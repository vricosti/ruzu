# Architectural Choices

This file records places where the Rust port deliberately does **not** follow the
C++ source literally, together with the reason and the price paid. It complements
[`CLAUDE.md`](CLAUDE.md), which states the rule; this file lists the exceptions
that were taken knowingly, so a reviewer comparing against upstream finds an
answer instead of a surprise.

Every entry must say: what upstream does, what the port does, why they differ,
and what it costs. An entry with no cost stated is an incomplete entry.

---

## 1. Scheduler → query cache: shared state handles

**Upstream.** `Scheduler` holds a `QueryCacheBase<QueryCacheParams>*`
(`vk_scheduler.h:300`), set by `RasterizerVulkan`'s constructor through
`SetQueryCache` (`vk_scheduler.h:90`, called at `vk_rasterizer.cpp:227`).
`Scheduler::EndRenderPass` then drives the query cache directly
(`vk_scheduler.cpp:392`):

```cpp
query_cache->CounterClose(VideoCommon::QueryType::StreamingByteCount);
query_cache->CounterEnable(VideoCommon::QueryType::ZPassPixelCount64, false);
query_cache->NotifySegment(false);
```

and `EndPendingOperations` calls `CounterReset(ZPassPixelCount64)` before ending
the render pass (`vk_scheduler.cpp:387`).

**The port.** `Scheduler` holds three `Arc<Mutex<…>>` handles to the exact pieces
used by these calls: `SamplesQueryState`, `TfbCounterState` and
`QueryRuntimeState`. `end_render_pass` performs close, pause and segment
notification in the same order as Eden; `end_pending_operations` resets samples
before ending the render pass.

**Why.** A literal `NonNull<QueryCache>` translation would reconstruct a second
`&mut QueryCache` while callers commonly already hold one, and conditional
rendering would similarly reconstruct a second `&mut Scheduler`. Mutexes prevent
data races but do not make aliased mutable references legal in Rust. Sharing the
three independently locked state objects preserves behavior without invoking
undefined behavior.

**Construction and teardown.** The rasterizer installs the handles only after
all fallible construction has succeeded. Its destructor finishes scheduler work
and clears the handles before destroying their Vulkan resource owners. Failed
construction therefore cannot leave a dangling scheduler registration.

**Cost.** Three shared-owner handles replace one non-owning C++ pointer, and the
scheduler spells out the three upstream calls against their state owners. The
actual method implementations remain in `query_cache.rs`; only the cross-owner
dispatch is adapted.

---

## 2. Samples query banks: `Arc<SamplesQueryBank>` with `Mutex<BankBase>`

**Upstream.** `SamplesQueryBank` derives from `VideoCommon::BankBase`
(`bank_base.h`) with plain members, lives by value in a
`std::deque<SamplesQueryBank>` inside `BankPool`, and is reached through a
`SamplesQueryBank* current_bank` plus bank ids.

**The port.** `BankPool<Arc<SamplesQueryBank>>`, with `BankBase` behind a
`parking_lot::Mutex` inside each bank
(`video_core/src/renderer_vulkan/query_cache.rs`).

**Why.** A query's results are resolved on the **fence thread**, in
`pop_unsynced_queries`, after the streamer's borrow has ended. Shared ownership
is what keeps the bank alive until then; a `Box` in the pool does not give that
guarantee. Once the bank is an `Arc`, access is shared-only, so mutating
`BankBase` needs interior mutability — hence the mutex.

**Note on address stability.** It is a *consequence*, not the reason.
`std::deque` is a segmented container: `push_back` never moves existing elements,
and the standard guarantees references to them stay valid, which is why upstream
can hold a raw `current_bank` pointer into it. Rust's `VecDeque`, which
`BankPool` is built on, is a single reallocating ring buffer and offers no such
guarantee. This is a **container difference, not a language limitation** — the
equivalent of `std::deque`'s guarantee is `VecDeque<Box<T>>`, and that is what
would have been used had lifetime across threads not been the binding constraint.

**Cost.** One uncontended mutex lock/unlock per slot reservation and per
reference close, where upstream has an atomic counter plus single-threaded
fields. Tens of nanoseconds per occlusion query, against the Vulkan calls
involved. Not measured.

---

## 3. Deferred GPU-side query pool reset

**Upstream.** `SamplesQueryBank::Reset()` does both halves at once: the
`BankBase` bookkeeping and the `vkCmdResetQueryPool` / `vkResetQueryPool`, using
the `Scheduler&` the bank holds as a member.

**The port.** `BankLike::reset` performs only the CPU-side half and sets
`pending_pool_reset`; the streamer records the GPU-side reset immediately after
`reserve_bank` returns, through `flush_pending_pool_reset`, before any slot of
that bank is handed out.

**Why.** `BankPool::reserve_bank` calls the reset through `BankLike`, and Rust
cannot pass `&mut Scheduler` down that call without reentrancy. Splitting keeps
`query_cache/bank_base.rs` a faithful port of `bank_base.h` and confines the
adaptation to the Vulkan bank, where the platform constraint actually lives.

**Ordering.** Preserved: the reset command is recorded before the first
`vkCmdBeginQuery` on that bank.

**Reentrancy note.** When `host_query_reset_supported` is false, recording the
reset calls `request_outside_renderpass`, which reaches the shared query states
described in §1. The reentrant `pause_counter` is a no-op, exactly as upstream's is:
`start_counter` publishes `state.current` only after recording `BeginQuery`,
mirroring upstream setting `has_started = true` last
(`vk_query_cache.cpp`, `SamplesStreamer::StartCounter`). `VK_EXT_host_query_reset`
is core in Vulkan 1.2, so this path is rare on current drivers.

---

## 4. Materialized query spans instead of `next_bank` chaining

**Upstream.** A query records `start_bank_id`, `size_banks`, `start_slot`,
`size_slots`, and `ApplyBankOp` / `ApplyBanksWideOp` re-walk the bank chain
through `SamplesQueryBank::next_bank` on every use.

**The port.** The walk is materialized once, when the report is taken, into a
`Vec<SamplesQuerySpan>` of `(bank, start, amount)` triples. Contiguous slots in
one bank are coalesced. `PopUnsyncedQueries` then merges the min/max range for
each bank across the entire pending report set before issuing
`vkGetQueryPoolResults`, matching upstream's `ApplyBanksWideOp`.

**Why.** Reports are resolved on the fence thread, where the streamer and its
bank pool are not reachable. The spans carry both the banks and the reference
ownership: each acquisition contributed one `AddReference`, and
`SamplesQuerySpan::drop` closes `amount` of them, which is upstream's
`Free` → `CloseReference` pairing.

**Consequence.** `SamplesQueryBank::next_bank` has no reader in the port, so the
field is not carried. Keeping it would be write-only state.

**Cumulative ownership.** Taking a report snapshots the entire slot history
since the last counter reset without draining it. Each snapshot adds its own
bank references, as upstream `WriteCounter` does when it copies `current_query`;
the history references are released by reset and report references by span drop.

**Cost.** One `Vec` allocation per report — typically one element, since a query
usually stays inside one bank — plus a small temporary range vector per flush.
Upstream allocates its pending query vector but does not materialize spans.

---

## Verification status of the above

- `cargo test -p video_core --lib`: 1464 passed, 0 failed, including focused
  regressions for `BankBase::close`, `is_dead`, and `BankPool::can_recycle_front`
  in `video_core/src/query_cache/bank_base.rs`.
- Successive samples reports are cumulative until `ResetCounter`, matching
  upstream `WriteCounter`; focused tests cover history snapshots and wide-range
  merging.
- `cargo build --bin ruzu`: clean.
- **Not covered:** no test exercises the real GPU path. Reference counting, bank
  recycling and the ranged readback are validated only by cross-reading against
  `~/Dev/emulators/eden/src/video_core/renderer_vulkan/vk_query_cache.cpp`. A run
  on a title that issues occlusion queries is still owed before treating this as
  settled.
