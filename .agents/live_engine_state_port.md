# Live Engine State Port

## Completed slice

The common graphics buffer cache now reads the channel-bound live `Maxwell3D`.
The Vulkan and OpenGL draw-state snapshot adapters have been removed.

## Implemented prerequisite

`BufferCache<P, DT>` now contains `ChannelSetupCaches<BufferCacheChannelInfo>`.
The payload embeds `ChannelInfo`, and rasterizer create/bind/erase lifecycle
updates the common cache in both backends.

## Result

1. Graphics index, vertex, cbuf, transform-feedback, and dirty state come from
   the current channel's Maxwell3D.
2. `DeleteBuffer` invalidates the live Maxwell dirty flags.
3. `EngineState` was reduced to a compute-only `ComputeEngineState` bridge.
4. The remaining KeplerCompute bridge is a separate follow-up; it no longer
   carries or snapshots graphics state.
5. Rasterizer callbacks expose the active `DrawManager::DrawState` by scoped
   reference while Rust's borrow split temporarily moves `DrawManager` out of
   `Maxwell3D`; this avoids both the default-state regression and a per-draw
   clone.
