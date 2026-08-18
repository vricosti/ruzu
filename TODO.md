Avant de continuer a investiguer j'ai une question: comment on peut utiliser MoltenVk 1.4.2 alors qu'il n'est pas sorti, v1.4.2-rc1 est sorti il y a 2 jours ? Tu utilises le repository ? https://github.com/KhronosGroup/MoltenVK/releases/tag/v1.4.2-rc1
---
verifier que eden et reden utilisent les memes appels opengl glsl/vulkan sur MK8D, on pourra commencer
 
  par le titre, puis ensuite attendre au moins 45s que l'attract mode se declenche et comparer les 7 premieres
 
  secondes. I ldoit bien exister une sorte de driver virtuel qui se contente d'enregistrer les appels opengl/
  vulkan ? Le but est de voir si eden fait les choses differemment ou de maniere plus optimisée. si c'est
  pareil on pourra tester autre jeu.
  --
  cree un repertoire tools et a l'interieur cree un launcher ou tu lui passe un fichier de config qui va
  lancer un exe avec les paramètres fournis et une config qui lui dit de prendre des captures d'ecrans avec le timing, il doit pouvoir aussi prendre une video. Le binaire doit etre lancé puis avoir un timer
  interne pour savoir quand prendre la capture ecran pour etre le plus reproductible possible et avoir une
  image de reference.