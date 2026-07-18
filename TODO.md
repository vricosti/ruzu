⏺ Finding 1 confirmed — unsigned_recip_estimate uses a "simple LUT-like approximation" and fp_recip_estimate32 just
  computes 1.0/f, neither matching ARM's 8-bit-mantissa estimate, and there's no FPSR handling. Your diagnosis is
  correct.

  My call on it: this is x64-only — on this Mac (Apple Silicon) the ARM64 backend runs, which you confirmed is correct,
  so it doesn't affect MK8D here. A faithful fix means porting ARM's RecipEstimate/RecipSqrtEstimate/Unsigned*
  reference algorithms + FP exponent handling + FPSR (~200 lines), and — critically — it can only be verified
  bit-accurately against the a32_diff oracle on an x64 host, which I can't run here. Writing it blind would violate the
  "don't claim correctness without verification" rule. So I've left it as a flagged follow-up rather than bundling an
  unverifiable numeric change into this set.
  ---
  Ce que faisait ma version (qui crashait)

  Je changeais drawableSize de la CAMetalLayer depuis le main thread → MoltenVK recréait le swapchain, mais la recréation de ruzu
  détruit l'ancien swapchain sans drainer les présentations en vol → MVKSwapchain::beginPresentation accède à de la mémoire libérée →
  SIGSEGV. C'est une divergence de present_manager.rs vs upstream (le present_done.Wait() ne protège pas contre ça sur MoltenVK, la
  destruction se fait pendant un present actif).

  Ce que fait mon fix actuel (sûr, mais pas équivalent)

  Je ne recrée pas le swapchain : il reste à la taille de boot, et Core Animation scale l'image à la nouvelle taille de fenêtre
  (contentsGravity). Pas de recréation → pas de crash. Mais : léger upscale (moins net) au lieu d'un re-rendu natif.

  Deux options

  1. Garder le scaling (actuel) : sûr, remplit la fenêtre, sans toucher à video_core. Léger flou en agrandissant.
  2. Vraie équivalence upstream : re-rendu à la résolution native au resize. Nécessite de corriger la recréation du swapchain de
  video_core/present_manager.rs pour attendre la fin des présentations en vol avant de détruire l'ancien swapchain (comme upstream).
  C'est une modif de l'arbre partagé video_core (édité par Codex) — à coordonner.
---
• Le scheduler de ruzu reproduit bien la barrière de fin de render pass, mais son modèle mélange commandes Vulkan immédiates et
  opérations différées, contrairement à upstream qui enregistre tout dans l’ordre dans le scheduler. Avant de conclure à cette
  divergence structurelle, j’ajoute une trace strictement ciblée sur 0x558FC0000: producteur, image/framebuffer Vulkan, fin de pass
  et première consommation B200.
