Avant de continuer a investiguer j'ai une question: comment on peut utiliser MoltenVk 1.4.2 alors qu'il n'est pas sorti, v1.4.2-rc1 est sorti il y a 2 jours ? Tu utilises le repository ? https://github.com/KhronosGroup/MoltenVK/releases/tag/v1.4.2-rc1
 ---
 Le draw bloquant est un glMultiDraw*Indirect logiciel. Le port Rust n’emprunte pas encore l’équivalent complet
  de PrepareDraw, mais corriger ce sous-système maintenant serait un autre chantier.
---
• La cause est une course dans l’adaptation cooperative de WaitSynchronization, pas dans l’instruction JIT:

  1. le SVC met le thread en attente;
  2. le réveil écrit correctement r0/r1 dans son contexte sauvegardé;
  3. PhysicalCore::handoff_after_svc recopie ensuite l’ancien contexte JIT, contenant encore l’adresse des handles dans x1, et écrase le résultat du réveil.
