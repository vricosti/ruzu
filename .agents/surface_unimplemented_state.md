# Surface format conversion unimplemented

- kind: PixelFormatFromRenderTargetFormat
- format: 0x0
- upstream_fallback: A8B8G8R8Unorm
- upstream: surface.cpp reaches UNIMPLEMENTED_MSG before returning the fallback format
- rust: stopped before treating the fallback as an implemented mapping
