Avant de continuer a investiguer j'ai une question: comment on peut utiliser MoltenVk 1.4.2 alors qu'il n'est pas sorti, v1.4.2-rc1 est sorti il y a 2 jours ? Tu utilises le repository ? https://github.com/KhronosGroup/MoltenVK/releases/tag/v1.4.2-rc1
 ---
 Le draw bloquant est un glMultiDraw*Indirect logiciel. Le port Rust n’emprunte pas encore l’équivalent complet
  de PrepareDraw, mais corriger ce sous-système maintenant serait un autre chantier.
---
• La cause est une course dans l’adaptation cooperative de WaitSynchronization, pas dans l’instruction JIT:

  1. le SVC met le thread en attente;
  2. le réveil écrit correctement r0/r1 dans son contexte sauvegardé;
  3. PhysicalCore::handoff_after_svc recopie ensuite l’ancien contexte JIT, contenant encore l’adresse des handles dans x1, et écrase le résultat du réveil.


• Problème

  SuperTuxKart démarre, affiche les trois icônes, puis se fige sans atteindre l’écran suivant.

  Résultats de l’investigation

  - Les précédentes corruptions AArch64 et lectures d’adresses basses/non mappées ont disparu.
  - L’erreur réseau Selected network interface '' not found n’est pas la cause principale.
  - Le blocage est désormais localisé au scheduler/fibres :
      - le thread invité 79 détient le mutex 0x815E32C0;
      - les threads 75, 80 et 81, dont le thread principal, attendent ce mutex;
      - le thread 79 reste RUNNABLE, mais sa migration entre les cœurs 0, 1 et 3 ne se termine pas correctement;
      - son context guard reste détenu par CPUCore_0, pendant qu’un autre cœur essaie de le reprendre;
      - CPUCore_2 est bloqué dans Fiber::yield_to;
      - CPUCore_0 est dans le chemin interrupt -> reschedule;
      - CPUCore_1 exécute un poll réseau HLE synchrone.

  - Des traces ont aussi montré un retour occasionnel sur la mauvaise fibre après un changement de thread.
  - J’ai rapproché plusieurs accès au thread courant de KScheduler du comportement upstream, mais STK se bloque toujours. Ces
    changements et les diagnostics sont encore non commités et ne constituent pas encore le correctif final.

  La prochaine étape est d’identifier précisément pourquoi le handoff schedule_impl_fiber -> switch_thread -> Fiber::yield_to
  reprend une fibre incorrecte ou conserve son context guard pendant une migration inter-cœur.

  Lancement de STK

  Compiler :

  cd /home/vricosti/Dev/emulators/ruzu_claude
  cargo build --release --bin ruzu-cmd

  Lancer avec Vulkan :

  DISPLAY=:0 RUST_LOG=error target/release/ruzu-cmd \
    --renderer vulkan \
    -g "/home/vricosti/Games/Emulators/Switch/common/roms/SuperTuxKart-1.5-switch/switch/stk.nro"

  Les trois icônes apparaissent après quelques secondes, puis le blocage se reproduit.
