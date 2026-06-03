THIS FILE SHOULD NOT ME MODIFIED BY AN IA - ONLY BY A HUMAN

Mot clés a rechercher plus tard:
closely enough


● Before diving in, let me scope this properly — porting the lifecycle has deep dependency chains (image upload/download against
  the GL backend), and I want to honestly assess what's reachable.
     
  Searched for 2 patterns, read 1 file (ctrl+o to expand)

● Honest scope assessment before I start writing code: this is a much bigger fish than it looks, and I want you to make an informed
   call.

  Upstream surface (texture_cache.h is 2547 lines of templated implementation):
  - 18 stub methods in texture_cache.rs covering image lifecycle (refresh_contents, prepare_image, unregister_image, delete_image,
  join_images, blit_image, fill_graphics_image_views, fill_compute_image_views, find/insert/render-target paths, etc.)
  
  Hard dependencies that don't exist yet in ruzu:
  - Runtime::UploadStagingBuffer / DownloadStagingBuffer — GL persistent-mapped staging pool
  - Image::UploadMemory — glTexSubImage* per subresource, plus format-specific conversions (ASTC, BCn, depth-stencil)
  - Image::DownloadMemory (partial — see task #158 marked complete, but only a slice)
  - SwizzleTexture / UnswizzleTexture — Tegra block-linear ↔ linear conversion
  - LRU cache (lru_cache.Touch, lru_cache.MakeLRU)
  - Async ASTC decode pipeline (QueueAsyncDecode, TickAsyncDecode)
  - MSAA upload path / image-layout transitions

  Realistic estimate: ~2000-3000 lines of port work, spanning ~5-7 files, with the heaviest piece being block-linear ↔ linear
  swizzle (the Tegra texture format is non-trivial). Multi-day at minimum, and verifying correctness requires actually rendering
  frames — without a working GPU pipeline end-to-end, bugs are silent and hard to attribute.
  ----
  Oui, Vec<Option<Inst>> est acceptable comme étape intermédiaire, mais ce n’est pas équivalent à une vraie liste intrusive côté
  performance ni côté parité.

  Comparaison rapide :

  - Vec<Option<Inst>>
      - Avantages : indices stables, simple à porter, bonne localité mémoire, peu risqué.
      - Coût : les itérations doivent skipper les trous, les blocs très modifiés peuvent accumuler des tombstones, erase ne libère
        pas physiquement, certaines passes doivent rester disciplinées.
      - Performance probable : correcte pour maintenant, surtout si les shaders ont peu d’effacements par bloc.
  - Liste intrusive upstream-style
      - Avantages : erase physique O(1), identités stables, itération uniquement sur les instructions vivantes, meilleure parité
        avec Dynarmic/zuyu.
      - Coût : plus complexe en Rust, besoin d’arena avec handles stables + liens prev/next, plus de risques de bugs d’emprunt/
        aliasing si mal conçu.
      - Performance probable : meilleure dans les passes qui effacent beaucoup, mais moins cache-friendly qu’un Vec dense.

  Donc mon avis : Vec<Option<Inst>> est OK comme première version robuste pour éliminer les bugs d’indices qui shiftent. Mais si
  l’objectif est vraiment “upstream-faithful long-term”, la cible correcte est une arena stable avec liste intrusive logique :
  handles stables, prev/next, erase physique du chaînage, stockage arena séparé.

  Je ne vendrais pas Vec<Option<Inst>> comme version définitive. Je l’utiliserais pour stabiliser vite, ajouter les tests, puis
  migrer vers une InstArena intrusive propre si les passes IR continuent à diverger d’upstream ou si les tombstones deviennent
  visibles dans les profils.

  --
  Je ne passerais à la variante stricte que si on fait un chantier dédié: définir un owner handle sûr pour Rasterizer, Gpu, GpuThread, MemoryManager,
  Puller, Maxwell3D, puis migrer tout le monde dessus. Pour faire marcher l’émulateur avec une architecture saine, la priorité reste plutôt: garder ce
  compromis, finir les dirty flags/state tracker/texture path, et n’attaquer le modèle strict que quand les bugs fonctionnels visibles sont
  stabilisés.