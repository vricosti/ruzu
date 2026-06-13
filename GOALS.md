# Active Goal

Investiguer et corriger le rendu MK8D sur la branche actuelle `fable-fix-sync`.

  Contexte observé:
  - MK8D affiche toujours un fond rouge, puis clignote noir, puis passe à une animation noire.
  - Ensuite le son finit par se couper et le jeu bloque.
  - Le dernier correctif testé, drainage de `PendingJoinCopies` dans `OpenGLTextureCache::materialize_views`, ne change pas le symptôme.
  - Ne pas confondre deux problèmes possibles:
    1. rendu rouge/noir: probablement TextureCache/Framebuffer/Shader/GL state;
    2. blocage tardif audio + freeze: probablement guest/kernel/nvmap/IPC ou corruption guest.
  - Priorité: expliquer le rendu rouge/noir, pas le freeze tardif.

  Contraintes:
  - Travailler dans `/home/vricosti/Dev/emulators/ruzu`.
  - Upstream: `/home/vricosti/Dev/emulators/zuyu/src/`.
  - Respecter strictement `AGENTS.md`: lire upstream avant modification, préserver ownership/structure/order, mettre `DIFF.md` à jour pour tout
  changement d’implémentation.
  - Ne pas ajouter d’instrumentation lourde qui ralentit ou change fortement le timing.
  - Avant chaque run MK8D, vérifier qu’aucune instance `ruzu-cmd`/`yuzu-cmd` ne tourne déjà.
  - Ne pas envoyer L+R pendant le test.
  - Ne pas commit/push sans demande explicite.
  - Faire des capture du rendu a certians moments plutot que de demander a l'utilisateur ce qu'il voit;

  Objectif:
  Trouver pourquoi ruzu rend un fond rouge puis une cinématique noire alors que yuzu/zuyu affiche la cinématique MK8D correctement, et porter
  un correctif upstream-faithful.

  Plan recommandé:
  1. Revoir les changements non commités et les commits récents de `fable-fix-sync`, surtout:
     - `video_core/src/texture_cache/texture_cache.rs`
     - `video_core/src/renderer_opengl/gl_texture_cache.rs`
     - `video_core/src/renderer_opengl/gl_rasterizer.rs`
     - `video_core/src/buffer_cache/buffer_cache.rs`
     - `video_core/src/engines/maxwell_3d.rs`
  2. Comparer précisément avec upstream:
     - `/home/vricosti/Dev/emulators/zuyu/src/video_core/texture_cache/texture_cache.h`
     - `/home/vricosti/Dev/emulators/zuyu/src/video_core/renderer_opengl/gl_texture_cache.cpp`
     - `/home/vricosti/Dev/emulators/zuyu/src/video_core/renderer_opengl/gl_rasterizer.cpp`
  3. Valider ou réfuter l’hypothèse principale:
     - le port partiel de `JoinImages`/alias/copy/delete expose une image GL avant que son contenu soit copié ou rafraîchi;
     - différences connues: `RefreshContents`, rescale, sorted `join_copies_to_do`, `CopyImageMSAA`, `ReinterpretImage`, `EmulateCopyImage`,
     `broken_views/native_bgr`, sparse scan.
  4. Si `JoinImages` n’explique pas le rouge/noir, passer à la comparaison des frames:
     - identifier les render targets/present images utilisées juste avant le passage rouge -> noir;
     - comparer les formats, dimensions, GPU/CPU addr, framebuffer attachments, draw buffers, clear colors, sampler views;
     - chercher la première divergence ruzu vs zuyu dans la chaîne render-target -> texture view -> present.
  5. Utiliser des traces ciblées et bornées uniquement:
     - pas de `RUZU_TRACE_IOCALLOC_CTX` pendant les runs visuels, car ça ralentit trop;
     - préférer logs in-memory ou logs filtrés par addr/seq/time.
  6. Si modification:
     - porter la logique depuis upstream, pas un workaround;
     - ajouter un test ciblé si possible;
     - lancer au minimum `cargo check -p video_core`;
     - lancer MK8D sans traces lourdes pour validation visuelle;

  Résultat attendu:
  - Expliquer concrètement la cause du rouge/noir avec fichiers/fonctions concernés.
  - Appliquer un correctif minimal et upstream-faithful.
  - Mettre `DIFF.md` à jour sans entrée “fixed”.
  - Fournir:
    - fichiers modifiés;
    - différences upstream supprimées ou restantes;
    - commandes de test;
    - résultat MK8D;
    - risques restants.
