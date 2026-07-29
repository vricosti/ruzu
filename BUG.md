# SuperTuxKart: extinction de l'audio par latence du scheduler

## Statut

Le probleme audio de SuperTuxKart est localise, mais le correctif definitif
n'est pas encore implemente.

Ce qui est prouve:

- la sortie audio hote fonctionne;
- le renderer audio HLE fonctionne tant que le guest lui fournit des buffers;
- le guest arrete de soumettre des wave buffers;
- le thread audio guest peut attendre entre environ 37 et 46 ms apres son
  reveil avant d'etre execute;
- le backend SDL Switch du jeu ne dispose que d'environ 46,4 ms de marge;
- reduire temporairement la periode de preemption de 10 ms a 5 ms supprime
  l'extinction de l'audio.

La frontiere du bug est donc le scheduling guest. La divergence precise qui
produit cette latence reste a isoler. Le test a 5 ms est une preuve
experimentale, pas un correctif acceptable.

## Symptome utilisateur

Le titre teste est:

```text
/Users/vricosti/Games/Emulators/Switch/roms/SuperTuxKart-1.5-switch/switch/stk.nro
```

Commande de reproduction:

```bash
cd /Users/vricosti/Games/Emulators/Switch/roms/SuperTuxKart-1.5-switch
/Users/vricosti/Dev/emulators/ruzu/target/release/ruzu-cmd \
    -r vulkan \
    -g switch/stk.nro
```

Le jeu produit d'abord du son, puis devient silencieux. Le menu principal doit
deja jouer sa musique; cliquer sur `OK` ou entrer dans une partie n'est pas
necessaire pour reproduire le defaut.

Le meme probleme a ete reproduit sous macOS ARM64 et Linux x64. Il ne s'agit
donc pas d'un defaut specifique a CoreAudio, MoltenVK ou Apple Silicon.

## Chaine causale verifiee

### 1. La sortie hote fonctionne

Le renderer ouvre correctement une sortie a 48 kHz. Le sink Cubeb reste actif,
sa file est stable et l'ADSP appelle le `DeviceSink` a la cadence attendue,
environ 200 mises a jour par seconde.

Apres la disparition du son, le sink continue de recevoir des blocs, mais ils
sont composes uniquement de zeros. Le probleme est donc situe avant Cubeb.

### 2. Le renderer audio HLE decode correctement

Avant le defaut, les traces montrent:

- PCM S16 stereo;
- source a 44,1 kHz;
- sortie a 48 kHz;
- pitch egal a 1;
- blocs de 240 echantillons par mise a jour ADSP;
- rotation normale des quatre slots de wave buffers HLE;
- progression normale du compteur d'echantillons consommes.

La musique `menutheme.ogg` est ouverte par le guest. Les appels Vorbis,
`alSourceQueueBuffers`, `alSourcePlay` et `playMusic` sont observes. Les
ressources du jeu et le chemin OpenAL ne sont donc pas absents.

### 3. Le guest cesse de fournir des wave buffers

Les appels de decodage s'arretent vers 22 secondes. Le renderer conserve une
voix active et demarree, mais son nombre de wave buffers devient nul:

```text
[AUDIO_VOICE_SKIP] index=0 id=0
  wave_buffer_count=0
  data_unmapped=false
  buffer_unmapped=false
  voice_dropped=false
  play_state=Started
  last_play_state=Started
  channel_count=2
  mix_id=0
  splitter_id=-1
```

`VoiceInfo::should_skip()` dans
`audio_core/src/renderer/voice/voice_info.rs` correspond a upstream. Une voix
sans wave buffer doit etre ignoree. Forcer le renderer a decoder cette voix
masquerait le symptome et violerait le contrat upstream.

### 4. Le backend SDL Switch a une marge tres faible

Le NRO embarque SDL 2.28.5 et utilise le backend audio Switch de devkitPro. Le
code de reference inspecte est:

```text
/tmp/devkitpro-sdl/src/audio/switch/SDL_switchaudio.c
commit 0738d3c
```

Ce backend utilise deux buffers de 1024 frames a 44,1 kHz:

```text
1024 / 44100 = 23,22 ms par buffer
2 buffers    = 46,44 ms de marge totale
```

`SWITCHAUDIO_PlayDevice` cherche un buffer `Free` ou `Done`, le remplit, le
soumet, puis attend sa transition d'etat. Lorsque aucun buffer n'est libre, il
cherche un buffer encore `Playing` et attend sa fin.

Si le thread audio n'est pas execute avant que les deux buffers soient
consommes, un appel a `audrvUpdate` peut ne laisser aucun buffer `Playing`.
`current` reste alors a `-1` et le backend accede a `buffer[-1]`. C'est un
comportement indefini dans la bibliotheque guest, qui explique pourquoi le
flux peut rester bloque definitivement au lieu de reprendre au reveil suivant.

Cette fragilite du SDL guest n'exonere pas l'emulateur: upstream ordonnance le
meme binaire assez rapidement pour ne pas epuiser les deux buffers.

### 5. Le thread audio est reveille mais ordonnance trop tard

Le thread audio SDL/libnx a ete identifie comme le thread guest 85:

- priorite guest 59;
- masque d'affinite `0xF`;
- attente dans `audrenWaitFrame`, autour du PC guest `0x80771EDC`.

La priorite 59 est correcte. Dans cette version du backend SDL Switch,
`SDL_THREAD_PRIORITY_TIME_CRITICAL` est traduit en `0x3B`, soit 59. Seule la
priorite SDL `HIGH` est traduite en `0x2B`, soit 43.

Les traces du scheduler montrent un delai reveil-vers-dispatch allant jusqu'a
environ 37,5 ms, avec des intervalles de reveil atteignant environ 46,3 ms.
Exemples mesures:

```text
37 523 us entre reveil et execution
37 517 us entre reveil et execution
37 493 us entre reveil et execution
46 334 us entre deux reveils dans le pire cas observe
```

Plusieurs workers de meme priorite, notamment les threads 79, 80, 81 et 83,
partagent les files. Avec une preemption toutes les 10 ms, trois ou quatre
tours de file produisent environ 40 ms de latence. Cela consomme presque toute
la marge de 46,4 ms du double buffer SDL. Un faible jitter supplementaire
suffit alors a epuiser les deux buffers.

Le code de ruzu conserve actuellement la periode upstream de 10 ms dans
`core/src/hle/kernel/kernel.rs::schedule_preemption_event`.

## Preuve decisive: contournement temporaire a 5 ms

L'experience suivante a ete realisee localement:

```diff
- let interval = std::time::Duration::from_millis(10);
+ let interval = std::time::Duration::from_millis(5);
```

Apres reconstruction en release et un run d'environ 55 secondes:

- l'audio est reste audible et normal;
- aucun `AUDIO_VOICE_SKIP` n'a ete observe;
- la voix est restee `in_use=1`, `skipped=0`, `active=2`;
- le renderer a continue de recevoir deux buffers soumis;
- les wave buffers sont restes valides jusqu'a la fin de la trace.

La correction temporaire a ete validee auditivement par l'utilisateur. La
source a ensuite ete remise a 10 ms.

Cette experience ne change ni le decodeur, ni le renderer audio, ni Cubeb, ni
SDL, ni les ressources du jeu. Son seul effet utile est de reduire le temps
maximum avant qu'un thread runnable de meme priorite repasse sur un coeur.
Elle demontre donc que le silence depend de la latence du scheduler.

Attention: un ancien `target/release/ruzu-cmd` peut encore contenir la variante
5 ms tant qu'une nouvelle compilation n'a pas remplace le binaire. L'etat
source documente et versionne reste a 10 ms.

## Pourquoi 5 ms n'est pas le correctif

Upstream utilise une preemption de 10 ms. Conserver 5 ms dans ruzu serait:

- une divergence de comportement;
- un moyen de cacher la cause exacte;
- une source de commutations de contexte supplementaires;
- un risque de regression de performance;
- un changement global pour contourner la fragilite d'un seul workload.

Le correctif fidele doit conserver 10 ms et restaurer le comportement complet
des files, migrations, handoffs et mises a jour du scheduler upstream.

## Mauvaises pistes et hypotheses invalidees

### Version SDL hote

Yuzu embarque statiquement SDL 2.30.12, commit
`8236e01a9f758d15927624925c6043f84d8a261f`. Ruzu utilise le SDL hote fourni
par `sdl2-compat` 2.32.70.

Cette difference n'est pas la cause: le buffering concerne SDL 2.28.5 embarque
dans le NRO, pas le SDL de la fenetre hote. Le defaut est aussi present sous
Linux.

### Cubeb, CoreAudio et la sortie hote

Le stream reste ouvert a 48 kHz, la cadence et la file sont normales, mais les
blocs recus deviennent nuls. Cubeb ne perd pas des echantillons valides; il
recoit deja du silence.

### Cadence ADSP et renderer HLE

Le renderer continue de tourner a environ 200 mises a jour par seconde. Il ne
se fige pas. Il cesse de decoder uniquement parce que le guest lui soumet zero
wave buffer.

### Decode PCM, resampling ou canaux

Le decode initial est correct et produit du son: S16 stereo, 44,1 vers 48 kHz,
pitch 1, progression des compteurs et rotation des buffers. Une erreur de
format permanente n'expliquerait pas un fonctionnement initial normal suivi
d'un arret de soumission guest.

### Ressources audio du jeu

`menutheme.ogg` est trouve et ouvert. Vorbis et OpenAL sont appeles. Le message:

```text
SFXBuffer: Positional audio is not supported with stereo files,
but /stk-data/sfx/car_revup.ogg is stereo
```

concerne un effet positionnel stereo et n'est pas fatal. Il n'explique pas
l'absence de musique dans le menu.

### `VoiceInfo::should_skip`

La fonction correspond a upstream. Le skip est une consequence correcte de
`wave_buffer_count=0`, pas la cause de cette valeur.

### Priorite guest perdue ou incorrecte

Le thread audio est bien a la priorite 59 demandee par le backend SDL Switch
pour `TIME_CRITICAL`. La priorite attendue n'est pas 43.

### Probleme macOS, MoltenVK ou QoS hote

Le defaut a ete reproduit sous Linux. Le chemin audio ne depend pas de
MoltenVK. Le test a 5 ms corrige le defaut sans modifier les priorites des
threads hotes.

### Echec de migration cause par `try_lock`

Cette hypothese venait de la lecture du chemin de migration. Elle est
invalidee: `KThreadLock` repose sur `SyncCell`, et `try_lock()` reussit sous le
contrat du scheduler lock. Ce chemin n'explique pas directement les retards.

### Perte de `last_scheduled_tick`

`KPriorityQueue::push_back` initialise temporairement cette valeur a zero, mais
`GlobalSchedulerContext` restaure immediatement le tick du `KThread` transmis.
Ce n'est pas la cause observee.

### Mauvais scheduler retourne par `System::scheduler_arc`

L'accesseur resout d'abord `kernel.current_scheduler()` a partir du TLS du
coeur hote. Dans le chemin runtime normal, le yield utilise donc le scheduler
du coeur courant.

### Reveil excessif de la condition variable

La boucle `SignalProcessWideKey` / `KConditionVariable` et la semantique du
nombre de threads reveilles ont ete comparees a upstream. Aucune divergence
locale expliquant le defaut n'a ete trouvee.

### Yield volontaire non applique

`PhysicalCore` rend la main au scheduler apres chaque SVC et les variantes
`SleepThread` de yield sont cablees. Cette piste n'explique pas a elle seule la
latence mesuree.

## Correctif definitif attendu

Le prochain travail doit rester centre sur le scheduler et conserver la
periode upstream de 10 ms:

1. Comparer litteralement les transitions `WAITING -> RUNNABLE`, la rotation
   des files de meme priorite, les migrations et le choix du prochain thread
   avec `GlobalSchedulerContext::PreemptThreads` et
   `KScheduler::RotateScheduledQueue` upstream.
2. Instrumenter sans bloquer la date de reveil, la file et le coeur cibles, les
   migrations, chaque thread choisi avant le thread 85 et la date effective de
   reprise.
3. Identifier pourquoi un thread priorite 59 runnable et affine a quatre
   coeurs attend trois ou quatre quanta complets.
4. Corriger la divergence precise plutot que specialiser le scheduler pour
   l'audio.
5. Ajouter un test de regression qui preserve 10 ms et verifie qu'un thread
   reveille de meme priorite est execute dans la borne produite par upstream.
6. Revalider STK sous macOS et Linux, puis verifier les performances et les
   autres titres sensibles au scheduling.

La correction de la bibliotheque guest est une autre option pour un build
specifique de STK: augmenter le nombre de buffers ou traiter le cas ou aucun
buffer n'est `Playing`. Cela rendrait le jeu plus robuste, mais ne corrigerait
pas la divergence de scheduling de l'emulateur.

## Artefacts de diagnostic

Les traces locales ayant servi a cette analyse sont:

```text
/tmp/stk-audio-decode-1785345113.log
/tmp/stk-audio-voice-1785345289.trace
/tmp/stk-audio-skip-1785345554.log
/tmp/stk-audio-sched-1785345778.log
/tmp/stk-audio-sched-1785345778.trace
/tmp/stk-audio-5ms-1785350000.log
```

Ces fichiers sous `/tmp` ne sont pas versionnes et peuvent disparaitre. Les
faits necessaires a la reprise de l'investigation sont donc reproduits dans ce
document.
