Avant de continuer a investiguer j'ai une question: comment on peut utiliser MoltenVk 1.4.2 alors qu'il n'est pas sorti, v1.4.2-rc1 est sorti il y a 2 jours ? Tu utilises le repository ? https://github.com/KhronosGroup/MoltenVK/releases/tag/v1.4.2-rc1
---
verifier que eden et reden utilisent les memes appels opengl glsl/vulkan sur ..., on pourra commencer
 
  par le titre, puis ensuite attendre au moins 45s que l'attract mode se declenche et comparer les 7 premieres
 
  secondes. I ldoit bien exister une sorte de driver virtuel qui se contente d'enregistrer les appels opengl/
  vulkan ? Le but est de voir si eden fait les choses differemment ou de maniere plus optimisée. si c'est
  pareil on pourra tester autre jeu.
  --
 CHECK:

 ⏺ Review complète. C'est la première fois que je peux comparer à eden lui-même plutôt qu'à zuyu — ça change la valeur des
  conclusions.

  Verdict
     
  Ce diff est d'une qualité de portage nettement supérieure à ce que j'avais relu sur dev/rendering-parity-snapshot. La
  quasi-totalité correspond à eden ligne à ligne, et il corrige deux vrais bugs. Une seule divergence mérite discussion.

  Bugs réellement corrigés 

  SetLayerZ lisait ses arguments à l'envers. L'ancien code faisait z_value = pop_u32(); padding = pop_u32(); layer_id = 
  pop_u64(). La signature eden est SetLayerZ(u64 layer_id, u64 z_value) — donc layer_id d'abord, en u64. L'ancienne lecture
  prenait les 4 octets de poids faible du layer_id pour le Z. Le nouveau code est correct.

  z_index était câblé à 0 dans la pile de composition. hardware_composer.rs poussait z_index: 0 pour chaque couche ; c'est
  maintenant layer_guard.z_index. Combiné au tri, ça restaure l'ordre d'empilement des calques — et le sort_by_key de Rust est
  stable, ce qui correspond au std::stable_sort d'eden.

  Au passage, le commentaire « This function currently does nothing but return a success error code » sur SetLayerVisibility
  était périmé : eden appelle bien m_container->SetLayerVisibility.

  Conformité vérifiée

  ┌────────────────────────────────────────────────────┬─────────────────────────────┬───────────────────┐
  │                      Élément                       │       Référence eden        │     Résultat      │
  ├────────────────────────────────────────────────────┼─────────────────────────────┼───────────────────┤
  │ configure_robustness2_features                     │ vulkan_device.cpp:1338-1340 │ exact             │
  ├────────────────────────────────────────────────────┼──────────────────────────────┼──────────────────────┤
  │ 10 formats ETC2/EAC                                │ vulkan_device.cpp:297-306    │ exact, même ordre    │
  ├────────────────────────────────────────────────────┼──────────────────────────────┼──────────────────────┤
  │ ComposeLocked                                      │ hardware_composer.cpp        │ ligne à ligne        │
  ├────────────────────────────────────────────────────┼──────────────────────────────┼──────────────────────┤
  │ ReleaseFramebuffersLocked                          │ idem                         │ exact                │
  ├────────────────────────────────────────────────────┼──────────────────────────────┼──────────────────────┤
  │ TryAcquireFramebufferLocked                        │ idem                         │ exact                │
  ├────────────────────────────────────────────────────┼──────────────────────────────┼──────────────────────┤
  │ Container::{Set,Get}LayerZIndex, SetLayerIsOverlay │ container.cpp:137-177        │ exact                │
  ├────────────────────────────────────────────────────┼──────────────────────────────┼──────────────────────┤
  │ RequestComposite / WaitForComposite                │ gpu.cpp:249-285              │ exact                │
  ├────────────────────────────────────────────────────┼──────────────────────────────┼──────────────────────┤
  │ WindowSystemType::Cocoa                            │ emu_window_sdl3_vk.cpp:75-76 │ exact, ordre compris │
  ├────────────────────────────────────────────────────┼──────────────────────────────┼──────────────────────┤
  │ Message « not synchronized »                       │ query_cache.h:274            │ exact                │
  └────────────────────────────────────────────────────┴──────────────────────────────┴──────────────────────┘

  Deux adaptations Rust sont justifiées plutôt que fautives : pending_composite_fence.swap(0) remplace le read-puis-clear non
  atomique d'eden, et try_acquire_framebuffer_locked reçoit frame_number en paramètre parce que la méthode est statique côté
  Rust.

  Le chemin MoltenVK de repli pointe désormais vers eden — j'ai vérifié que eden.app/Contents/Frameworks/libMoltenVK.dylib
  existe bien (10,5 Mo), donc le repli est fonctionnel et non mort.

  Le point à traiter — Medium

  push_unsynced_queries omet deux appels qu'eden fait (vk_query_cache.cpp:375-382) :

  void PushUnsyncedQueries() override {
      PauseCounter();
      current_bank->Close();
      { std::scoped_lock lk(flush_guard);
        pending_flush_sets.emplace_back(std::move(pending_flush_queries)); }
  }

  Le port ne fait que le déplacement sous verrou. pause_counter existe pourtant (query_cache.rs:220), et current_bank->Close()
  n'a aucun équivalent.

  C'est probablement sans conséquence : côté Rust, pending_flush_queries ne reçoit que des rapports issus de take_report, qui
  appelle déjà pause_counter. Les requêtes sont donc déjà terminées au moment de la poussée, alors qu'eden empile des indices
  pouvant encore être en vol. Mais « probablement » ne suffit pas ici — soit les appels manquants sont ajoutés, soit un
  commentaire explique pourquoi le modèle de rapports du port les rend inutiles. En l'état, un lecteur comparant à l'upstream
  verra un écart non justifié.

  Deux points mineurs

  should_wait_async_flushes ajoute un test par streamer qu'eden n'a pas : chez lui c'est un masque au niveau de la base
  (query_cache.h:509-512, !flushes_pending.empty() && front() != 0). Selon que la base portée en Rust suit ou non les streamers
  dans un masque, cet ajout est soit une adaptation nécessaire, soit une attente superflue. Je n'ai pas poussé la vérification
  jusqu'au bout — à confirmer.

  scripts/build-macos-app.sh positionne encore PKG_CONFIG_PATH pour opus, ffmpeg et openssl@3. C'est devenu redondant : le
  script fait déjà export PATH="$brew_prefix/bin:$PATH", ce qui suffit, et opus est désormais résolu par audio_core/build.rs.
  Inoffensif, mais c'est le symptôme que je traitais moi-même il y a quelques heures.

  Un détail de style : le chemin ~/Dev/emulators/eden/build/bin/eden.app est en dur dans du code suivi par git. C'est
  explicitement documenté comme un repli de développement, mais il ne fonctionnera que sur cette machine.
  À noter