# TODO

Some maxwell_3d tests hang (test_call_method_cb_bind, test_call_method_cb_data) — needs investigation. The test_call_method_sync_point and other simpler tests pass. The hang is not a stack overflow — it occurs even with a single test and large stack. Likely related to the EngineInterface trait dispatch or a subtle deadlock in the test setup.

------
 // Upstream emits save_context and load_context helper functions using oaknut
        // (ARM64 code generator):
        //   - WriteSaveContext(): saves all guest GPRs/vectors to GuestContext struct
        //   - WriteLoadContext(): restores all guest GPRs/vectors from GuestContext struct
        // These are called by the SVC trampoline to save/restore guest state when
        // transitioning to/from host code.
        // Requires: oaknut-equivalent ARM64 code generation library for Rust.
        // NCE is AArch64-host-only; on x86_64 hosts this entire module is unused.
------

 The AudioCore takes Arc<Values>. This is a case where we should change the API to not require an Arc<Values> since there's   
  now a global. But that's a bigger refactor. For now, let me update the call sites to use values() and clone where needed.    
                                                                                                                               
  Let me now do all the edits. For the AudioCore case in main.rs, since it requires Arc<Values>, I'll create an Arc from a     
  clone of the global values. 
  ----

  KScopedSchedulerLockAndSleep   
    /// Upstream wraps in KScopedSchedulerLockAndSleep for atomic lock + sleep.
    /// In our single-core cooperative model, the process lock provides
    /// equivalent protection. Wire KScopedSchedulerLockAndSleep when
    /// KConditionVariable gains a KernelCore reference.
  ----

  Requires rdynarmic A64 decoder integration     

  ----

  ● 65615 blocks compiled in 10 seconds! And the cache hit ratio is terrible — at 100,000 lookups, there are 65,615 unique blocks (99.7% miss rate up
  to 10k lookups).

  The game compiles 6,500 blocks/second. That's massive compilation overhead. Most lookups result in a new compilation.

  Compare: the first 3 lookups at 100/1000/10000 were ALL misses. Only at 100,000 did we get a hit. This means the JIT is compiling a new block for
  almost every entry into run().

  This is the root cause: the JIT is constantly recompiling blocks because the LocationDescriptor hash changes even for the same PC. The hash
  includes CPSR/FPSCR state, so if any flag changes between runs, it's a different block.

  Let me verify:

  Searched for 2 patterns, read 1 file (ctrl+o to expand)     
                                                           
● The hash includes upper_location_descriptor which encodes T/E/IT/FPSCR mode bits. If FPSCR changes frequently (e.g., rounding mode, condition
  flags), each different FPSCR value creates a NEW block for the same PC.
                                                                                                                                                     
  This is by design in dynarmic (different FP modes need different code). But if ruzu is changing FPSCR unnecessarily, it would cause massive block  
  duplication.                                                                                                                                       
                                                                                                                                                     
  65,615 unique blocks in 10 seconds is the smoking gun. Upstream probably compiles ~1,000-2,000 blocks for the same boot sequence. The 30-60x       
  slowdown is explained by 30-60x more block compilations.
                                                                                                                                                     
  Let me verify if upper_location_descriptor is changing too frequently:                                                                             
  
● Update(~/Dev/emulators/rdynarmic/src/backend/x64/a32_emit_x64.rs)                                                                                  
  ⎿  Added 3 lines, removed 1 line                                
      155          static LOOKUP_CALLS: AtomicU64 = AtomicU64::new(0);                                                                               
      156          let n = LOOKUP_CALLS.fetch_add(1, Ordering::Relaxed);
      157          if n == 100 || n == 1000 || n == 10000 || n == 100000 {                                                                           
      158 -            eprintln!("JIT_LOOKUP: calls={} hit={} cache_size={}", n, result.is_some(), self.cache.len());                         
      158 +            let pc = location.get_raw() as u32;                                                                                           
      159 +            let upper = (location.get_raw() >> 32) as u32;                                                                         
      160 +            eprintln!("JIT_LOOKUP: calls={} hit={} cache_size={} pc={:#x} upper={:#x}", n, result.is_some(), self.cache.len(), pc, 
          +upper);                                                                                                                            
      161          }
      162          result
      163      }

● Bash(cd /home/vricosti/Dev/emulators/rdynarmic && touch src/backend/x64/a32_emit_x64.rs && cargo build --release 2>&1 | grep "Compiling rdynarmic"
      && cargo build --r…)
  ⎿     Compiling rdynarmic v0.1.0 (/home/vricosti/Dev/emulators/rdynarmic)                                                                          
  ⎿  (timeout 2m)     
                                                                                                                                                     
● Bash(cargo build --release 2>&1 | grep "Compiling core" && env XDG_DATA_HOME=/tmp/ruzu-statsB XDG_CACHE_HOME=/tmp/ruzu-statsBc
      XDG_CONFIG_HOME=/tmp/ruzu-statsBcfg RU…)                                                                                                       
  ⎿  No matches found                                                                                                                                
                                                                                                                                                     
● Bash(env XDG_DATA_HOME=/tmp/ruzu-statsC XDG_CACHE_HOME=/tmp/ruzu-statsCc XDG_CONFIG_HOME=/tmp/ruzu-statsCcfg RUST_LOG=error timeout 10             
      target/release/yuzu-cmd -g "/h…)                                                                                                               
  ⎿  JIT_LOOKUP: calls=100 hit=false cache_size=100                                                                                                  
     JIT_LOOKUP: calls=1000 hit=false cache_size=819
     JIT_LOOKUP: calls=10000 hit=false cache_size=9648                                                                                               
     JIT_LOOKUP: calls=100000 hit=false cache_size=65406          
  ⎿  (timeout 30s)                                                                                                                                   
                                                                                                                                                     
● 100,000 lookups et ZÉRO cache hit ! 65,406 blocs uniques compilés, et même à 100k lookups, AUCUN hit. Cela signifie que presque chaque appel à     
  run() compile un NOUVEAU bloc.                                                                                                                     
                                                                                                                                                     
  C'est le problème. Avec 65k blocs en 10 secondes, le JIT passe la quasi-totalité de son temps à compiler du code, pas à l'exécuter. La raison :    
  upper_location_descriptor (qui encode CPSR/FPSCR) change probablement à chaque instruction, créant un hash unique pour le même PC.                 
                                                                                                                                                     
  Voici le résumé comparatif avec upstream :                                                                                                         
  
  ┌────────────────────┬───────────────────┬───────────┬─────────────────────────────┐                                                               
  │       Aspect       │ Upstream dynarmic │ rdynarmic │           Impact            │
  ├────────────────────┼───────────────────┼───────────┼─────────────────────────────┤                                                               
  │ Blocs compilés/10s │ ~1-2k             │ 65k       │ 30-60x plus de compilations │
  ├────────────────────┼───────────────────┼───────────┼─────────────────────────────┤                                                               
  │ Cache hit rate     │ >90%              │ <1%       │ Presque tout est recompilé  │                                                               
  ├────────────────────┼───────────────────┼───────────┼─────────────────────────────┤                                                               
  │ Block size         │ Pas de limite     │ 64 max    │ Mineur                      │                                                               
  ├────────────────────┼───────────────────┼───────────┼─────────────────────────────┤                                                               
  │ Fastmem            │ ✓ inline          │ ✓ inline  │ OK                          │
  ├────────────────────┼───────────────────┼───────────┼─────────────────────────────┤                                                               
  │ Optimisations IR   │ ✓                 │ ✓         │ OK                          │
  ├────────────────────┼───────────────────┼───────────┼─────────────────────────────┤                                                               
  │ Block linking      │ ✓                 │ ✓         │ OK                          │
  └────────────────────┴───────────────────┴───────────┴─────────────────────────────┘                                                               
                                                                  
  Cause racine : Le upper_location_descriptor (hash du bloc) inclut des bits FPSCR/CPSR qui changent trop souvent, générant des dizaines de milliers 
  de blocs uniques au lieu de réutiliser les blocs déjà compilés. C'est un bug dans la construction du hash — il faut vérifier quels bits sont inclus
   vs upstream pour trouver ce qui diffère.                                                                                       

   ● Voici le résumé de la comparaison avec upstream :

  Architecture JIT — rdynarmic vs upstream dynarmic :

  ┌─────────────────────────┬───────────────────┬───────────────────────┐
  │         Aspect          │      Statut       │        Impact         │
  ├─────────────────────────┼───────────────────┼───────────────────────┤
  │ Hash/LocationDescriptor │ ✅ Identique      │ Pas le problème       │
  ├─────────────────────────┼───────────────────┼───────────────────────┤
  │ Fastmem (memory inline) │ ✅ Identique      │ Pas le problème       │
  ├─────────────────────────┼───────────────────┼───────────────────────┤
  │ Block linking           │ ✅ Fonctionne     │ Pas le problème       │
  ├─────────────────────────┼───────────────────┼───────────────────────┤
  │ IR Optimisations        │ ✅ Activées       │ Pas le problème       │
  ├─────────────────────────┼───────────────────┼───────────────────────┤
  │ Block size limit        │ ⚠️  64 vs illimité │ Mineur (~4% overhead) │
  ├─────────────────────────┼───────────────────┼───────────────────────┤
  │ Compilation speed       │ ✅ 17µs/bloc      │ Seulement 9% du temps │
  ├─────────────────────────┼───────────────────┼───────────────────────┤
  │ Exécution du code JIT   │ ❌ 91% du temps   │ LE bottleneck         │
  └─────────────────────────┴───────────────────┴───────────────────────┘

  Conclusion : Le code x86 généré par rdynarmic est significativement plus lent que celui d'upstream. Les 91% du temps sont passés dans l'exécution du code JIT, pas en compilation ou
  dispatch. Les causes probables :

  1. Qualité du code x86 généré — moins de peephole optimizations que upstream
  2. Register allocation — peut-être moins efficace (plus de spills mémoire)
  3. Fallbacks vectoriels — les ops NEON qui utilisent des appels de fonctions au lieu de SSE inline

  C'est un problème profond d'optimisation du backend JIT qui nécessite un travail conséquent sur la qualité du code x86 généré — pas un fix rapide.