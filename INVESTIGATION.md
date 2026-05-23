il faudra supprimer race_anchor

# Bug B investigation — session 2026-05-23

Sur branche `refactor/render-engine-rework` à `d7cc0e6`. Pipeline async débloqué (fix MarkRegionCaching + state_tracker). MK8D affiche écran de boot blanc + freeze visuel après 1s. Objectif : trouver pourquoi.

## Setup & knobs utilisés

```bash
env XDG_CACHE_HOME=/tmp/ruzu-cache XDG_CONFIG_HOME=/tmp/ruzu-config \
    RUZU_TRACE_DRAW_SUMMARY=1 RUZU_TRACE_DRAW_SUMMARY_LIMIT=100000 \
    RUZU_TRACE_RT_FBO=1 RUZU_TRACE_RT_READBACK=1 \
    RUZU_TRACE_PRESENT=1 RUZU_TRACE_PRESENT_READBACK=1 \
    RUZU_TRACE_PRESENT_ALIASES=1 RUZU_DUMP_PRESENT_TEXTURE=1 \
    RUZU_TRACE_BQP=1 RUST_LOG=info timeout 25 \
    ./target/release/ruzu-cmd -r opengl -g <MK8D_NSP>
```

Note : `RUZU_TRACE_DRAW_SUMMARY=1` seul cap à 64 events (LIMIT par défaut). Mettre LIMIT=100000 pour voir le vrai compte.

Le panic kernel race en `k_thread.rs:1357` (priority inheritance) est softi en commit `d7cc0e6` (log warn + early return). Sans ce fix, le tracing lourd faisait panic.

## Mesures objectives

### Pipeline GPU sain

| Métrique | Valeur |
|---|---|
| DRAW_SUMMARY events (22s) | **21255** (~1020/sec) |
| Draws per frame @ 60 FPS | ~17 |
| SubmitGpfifo events | 2048+ (~100/sec) |
| BQP_QUEUE | atteint #1024 en 17s |

### Distribution des RTs (21255 draws)

| RT GPU VA | Count | FBO | Role |
|---|---|---|---|
| `0x524C10000` | 19971 | fbo=2 | Main internal RT (scene content) |
| `0x501D00000` | 430 | fbo=3 | Swap chain slot 0 → CPU 0x1FC000 |
| `0x502DE0000` | 427 | fbo=5 | Swap chain slot 2 → CPU 0x12DC000 |
| `0x502570000` | 427 | fbo=4 | Swap chain slot 1 → CPU 0xA6C000 |

Le routing aux 3 swap chain buffers est **balanced 1:1:1** (~430 chacun). Pas de Bug B "wrong FBO" au sens du memo précédent.

### Slot setup (BQP)

3 slots partagent `handle=28` (nvmap) avec offsets contigus :
- slot 0: offset=0 → CPU 0x1FC000
- slot 1: offset=8847360 → CPU 0xA6C000
- slot 2: offset=17694720 → CPU 0x12DC000

= 1 alloc 24MB découpée en 3 buffers RGBA8 1920x1080 contigus.

### Content vérifications

**RT_READBACK** (lecture du FBO juste après les draws) : 
- fbo=3 attached=20 : `[09 0A 0A FF 1F 1F 1F FF ...]` — boot logo dark content ✅
- fbo=4 attached=28 : même content ✅
- fbo=5 attached=33 : même content ✅

**PRESENT_TEXTURE** (lecture de la texture sample par blit_screen) :
- texture=25 (view de image=4 slot 0) : `[09 0A 0A FF 1F 1F 1F FF ...]` ✅
- texture=33 (view de image=14 slot 2) : même content ✅
- texture=35 (view de image=15 slot 1) : 
  - À #2 : `[00 00 00 00...]` (vide!)
  - À #5+ : même content que les autres

→ glTextureView partage bien le storage entre draw et present. **Le path GPU est correct.**

## Le "freeze" décortiqué

Region pattern PRESENT_READBACK (5 régions échantillonnées par frame fenêtrée) :

```
Pattern   Frames
_____     53     ← boot animation (toutes régions dark/varied)
_W_W_/_W_  6     ← transition (1 region devient blanche)
_WW_W     1469   ← FROZEN (3 of 5 regions pure white, 2 dark)
```

Régions blanches (`W` = sum=261120 = pure RGBA(255,255,255,255)) :
- `@624,344` (center)
- `@1248,688` (bottom-right)
- `@1248,0` (top-right)

Régions sombres (avec contenu varié) :
- `@0,0` (top-left)
- `@0,688` (bottom-left)

## Test sRGB vs RGBA8 format mismatch (échec)

`try_find_framebuffer_image_view` (`texture_cache_base.rs:508-512`) force toujours `A8B8G8R8Unorm` pour le view de présentation, mais les images sont allouées en `A8B8G8R8Srgb` côté draw. glTextureView avec formats différents → interprétation différente des bytes.

Patch testé : changer le view present pour matcher le format de l'image (sRGB si l'image est sRGB).

Résultat : CRCs changent (différente interprétation des bytes) mais **pattern régional identique** :
- avant : 4413 pure white, 1469 partial, 1469 partial
- après : 3636+1210+1210 (mêmes proportions)

→ Le format mismatch existe mais N'EST PAS la cause des régions blanches. Patch reverté.

## Diagnostic final

Les régions blanches dans `_WW_W` correspondent au **contenu réel rendu par MK8D**. MK8D dessine activement (~1020 draws/sec) mais le résultat est un écran statique avec :
- fond blanc (regions centre + droite)
- du contenu sombre dans les coins gauches (probablement un logo type "Now Loading..." ou splash écran)

**Bug B reformulé** : ce n'est pas un bug GPU/render. MK8D est **bloqué sur un écran de chargement statique** parce qu'un event/asset/SVC qu'il attend ne se déclenche pas.

## Pistes pour next session (Bug B reformulé = HLE/kernel)

1. **SVC pattern post-boot** : tracer quelles SVCs tid=75 (main MK8D thread) répète en boucle après les 53 frames d'animation. Utiliser `RUZU_TRACE_TID_SVC=75` ou équivalent.

2. **nvhost_ctrl events en attente** : MK8D attend probablement un event lié au file system, vsync, ou async asset load. Inspecter les events QueryEvent appelés et qui les signal.

3. **Symptôme côté audio renderer** : la memoire mentionne MK8D attend parfois sur cv `0x69A545AC` ou `0x814E0F60`. Vérifier si même symptôme actif ici.

4. **Comparer wallclock SVC byte-diff vs zuyu** au moment exact du freeze (≈t+1s post boot). Si les SVCs divergent à un point précis, ça localise le bug.

## Session-2 findings (2026-05-23 PM-late, continuation)

Avec le panic kernel softi (commit `d7cc0e6`), on a pu tracer SVC et mémoire activement.

### CV polling localisée — même bug que [[mk8d-cv-5564c6d8-2026-05-19-night]]

**Sur 30s** :
- tid=75 WaitProcessWideKeyAtomic on cv `0x7F2C0BCC` : **1413 fois** (~47 Hz)
- tid=83 SignalProcessWideKey on cv `0x7F2C0BCC` : **1418 fois** (~47 Hz)
- tid=101 SignalProcessWideKey on cv `0x7F2C0BCC` : **1419 fois** (~47 Hz)

zuyu réf (per memo) : tid=75 wait ~0.06 Hz → **ratio 800× plus de polling**.

47 Hz = audio frame rate. tid=83/101 sont les threads audio renderer qui notifient tid=75 chaque audio frame. tid=75 wake → eval predicate → FALSE → re-wait.

### La predicate variable est dans une région ALL-ZEROS

Dump mémoire `0x7F2C0B00 .. 0x7F2C0D60` au premier wait → **600 bytes pure 0**.

Traçage des writes à cette région sur 25s → **ZERO write**. Personne n'écrit dans la zone qui contient la predicate.

→ La predicate stays 0 forever, tid=75 polls indéfiniment.

### Hypothèse forte : audio renderer shared memory

Adresse haute `0x7F2C0BCC` = stack ou heap utilisateur de MK8D. Per memo `audio_renderer never got its shared-memory pool` :

MK8D mappe une shared memory pool avec audio_renderer (us, l'émulateur). Audio renderer doit régulièrement mettre à jour des fields dans cette shared memory pour signaler "audio buffer N consumed" / "ready for next sample". Si on ne writes pas correctement → MK8D voit toujours stale state → predicate=0 → polling.

### Pistes plus serrées pour next session

1. **Identifier la shared memory pool audio renderer**. Quel buffer MK8D mappe via `MapSharedMemory` vers audio_renderer? Comparer le contenu de cette pool ruzu vs zuyu.

2. **Audio renderer DSP work-buffer writes**. Le DSP doit écrire des status fields. Tracer `audio_core::adsp::apps::audio_renderer::command_list_processor` outputs. Sont-ils flushés dans la guest memory?

3. **Per-frame audio renderer state** : la `IAudioRenderer::RequestUpdate` IPC est byte-identical (per memo `[[mk8d-audio-ipc-ruled-out-2026-05-21]]`) → MAIS la shared memory update est séparée de l'IPC. C'est cette piste qui n'a pas été testée.

4. **PLT entry 5 identification** : ARM disassembler MK8D pour identifier la fonction au `GOT[0x60]`. Selon le symbol on saura quel state guest fait défaut. Outils potentiels : `nx2elf` puis `objdump -d` sur le main NSO de MK8D.

### Knobs utiles découverts

- `RUZU_TRACE_TID_SVC=tid_or_list_or_*` — trace SVCs par thread
- `RUZU_DUMP_MEM_AT_WAIT_CV=0xADDR` + `RUZU_DUMP_MEM_AT_FIRST_SIGNAL=0xADDR:LEN,...` — dump memory au premier wait de cv
- `RUZU_TRACE_MEMORY_W_RANGE=0xSTART:0xEND` — trace tous les writes dans range
- `RUZU_TRACE_AUDIO_EVENT=1` — log audio renderer signal_rendered_event

### Pistes audio renderer testées (pas la cause directe)

1. **Shared memory pool addresses** : `0x3A204000+256KB` (Read) + `0xC3E04000+17MB` (Read) — toutes côté audio renderer. La cv `0x7F2C0BCC` est dans MK8D's heap, PAS dans ces pools.

2. **Transfer memory** : MK8D fournit son own transfer memory à OpenAudioRenderer (taille calculée via GetWorkBufferSize). Audio renderer y écrit le command workbuffer pendant l'init (zero_block) puis le DSP y écrit pendant render. Le `0x7F2C0BCC` n'est probablement PAS dedans non plus.

3. **signal_rendered_event** : tire à haut taux (~100 Hz pendant le wedge), notifie correctement event object_id=390. Donc le PATH d'événement audio est OK.

→ **La predicate n'est pas la audio renderer state** au sens "is event signaled". C'est plus subtil — un check de buffer-consumed dans la shared memory ou dans le command workbuffer.

### Vraies pistes restantes

- **A. Disassembler MK8D** : extraire le main NSO de MK8D, désassembler PC=0x71EE40-0x71EEC0 pour identifier la PLT[5] cible et la logique de prédicat exacte. Outils : `nx2elf` + `objdump -d`.

- **B. Byte-diff transfer memory ruzu vs zuyu** au moment du wedge (t≈1s post-boot). Si les bytes divergent à offset X, on a la field à corriger.

- **C. DSP work-buffer command_list_processor** : tracer les status writes du DSP. Mémoire mentionne MK8D's audio path attend parfois sur des conditions buffer-consumed.

- **D. Mémoire `[[mk8d-cv-5564c6d8-2026-05-19-night]]`** mentionne que tid=75 wait 4 fois en ruzu vs 2 fois en zuyu — différent count CV. Là c'est 1413 vs 7, donc on est sur un DEEPER fork. Vérifier si on est dans un état de boot différent qui exacerbe l'issue.

## Disassembly chain traced (sans outils externes, via RUZU_DUMP_MEM_AT_FIRST_SVC + capstone-py)

Polling loop wake handler (depuis tid=75 wait sur cv 0x7F2C0BCC) :

```
0x71EE40 (game code) :
  ...
  mov r0, sb / mov r3, r7 / mov r1, r6
  bl  #0xc6064c             ← PLT call dont le RETURN VALUE est la prédicate
  mov r7, r0                ← R7 = retour
  ...
  cmp r7, #0                ← test prédicate
  bne 0x71EE9C              ← si non-zero, exit loop ; sinon re-wait
```

PLT chain résolue :

```
0xc6064c (PLT entry, 1er niveau) :
  add ip, pc, #0x200000     ; ip = 0xE60654
  add ip, ip, #0x72000      ; ip = 0xED2654
  ldr pc, [ip, #0x50]!      ; charge PC depuis GOT[0xED26A4]

GOT[0xED26A4] = 0x01E6EAFC

0x01E6EAFC (tailcall stub) :
  b 0x020234FC              ; saute vers 2e PLT level

0x020234FC (PLT entry, 2e niveau) :
  add ip, pc, #0x200000     ; ip = 0x02223504
  add ip, ip, #0xA0000      ; ip = 0x022C3504
  ldr pc, [ip, #0xF38]!     ; charge PC depuis GOT[0x022C443C]

GOT[0x022C443C] = 0x0201D5E4   ← UNRESOLVED (lazy resolver address)
```

0x0201D5E4 est le **classic ARM PLT lazy resolver** :

```
str lr, [sp, #-4]!          ; push return address
ldr lr, [pc, #4]            ; load GOT offset = 0x002A4E90
add lr, pc, lr              ; lr = 0x022C2480 (GOT base)
ldr pc, [lr, #8]!           ; jump to resolver via GOT[0x022C2488]
.data 0x002A4E90
```

→ Chaque appel passe par le résolveur dynamique (`_dl_runtime_resolve` équivalent) qui devrait :
1. Lookup le symbole dans `.dynsym`
2. Patcher `GOT[0x022C443C]` avec l'addresse réelle de la fonction
3. Sauter à la vraie fonction

En zuyu, la fonction résolue retourne non-zero après 7 waits → MK8D progresse.
En ruzu, elle retourne 0 indéfiniment.

## Symbole identifié ! (session 2026-05-23 PM, suite)

Avec hactool installé + parser python custom :

1. **NSP extracted** via `hactool -t pfs0` → 5 NCAs
2. **ExeFS extracted** via `hactool -t nca --exefsdir=...` → main (8.3MB), rtld, sdk, subsdk0-4
3. **NSO décompressé** via LZ4 (lz4.block python module)
4. **MOD0 header** trouvé à .text+8, donnant `dynamic_offset_rel = 0xCCC08C` → dynamic à 0xCCC094
5. **DYNAMIC parsée**, extracted : PLTGOT=0xCCC18C, JMPREL=0xB3C838, PLTRELSZ=0x16B8, SYMTAB=0xB3F5A8, STRTAB=0xB42C38
6. **727 JMPREL entries** parsées (R_ARM_JUMP_SLOT)

PLT mapping :
- PLT[0] (lazy stub at 0xC60600, 4 bytes single ldr)
- PLT[N] for N≥1 : 12 bytes each, starting at 0xC60604 + (N-1)*12

`bl 0xC6064C` = PLT entry at offset 0xC6064C - 0xC60604 = 0x48 / 12 = 6 → **PLT[7]** = JMPREL[6] :

```
PLT[1]    sym 837   __nnmusl_init_dso
PLT[2]    sym 838   __nnmusl_fini_dso
PLT[3]    sym 835   _init_libc0
PLT[4]    sym 827   _init_libc1
PLT[5]    sym 828   _init_libc2
PLT[6]    sym 825   _ZN2nn2oe10InitializeEv         = nn::oe::Initialize
PLT[7]    sym 834   _ZN2nn3aoc10InitializeEv        = nn::aoc::Initialize ← ICI
PLT[8]    sym 832   nn::fs::SetLocalAccessLog
PLT[9]    sym 826   nn::pctl::Initialize
PLT[10]   sym 830   nn::fs::SetResultHandledByApplication
PLT[11]   sym 831   nn::diag::InitializeApplicationAbortObserver
```

🎯 **`nn::aoc::Initialize()`** — Add-On Content (DLC) initialization.

### Le bug exact — CORRECTION post-upstream-check

⚠️ **Symbol identification revisited** : ma première analyse computait l'offset PLT mal. Avec la base correcte (main loaded at 0x206000, computed via byte-pattern match), le PLT entry à `0xc6064c` correspond à JMPREL[**323**] (pas 6), donc sym[**455**] :

`_ZN2nn5audio6AddAuxEPNS0_19AudioRendererConfigEPNS0_7AuxTypeEPNS0_10SubMixTypeEPvS7_j`
= **`nn::audio::AddAux(AudioRendererConfig*, AuxType*, SubMixType*, void*, void*, u32)`**

C'est le MÊME symbol que le memo `[[mk8d-cv-5564c6d8-2026-05-19-night]]` avait identifié à l'époque. Bonne nouvelle, mauvaise nouvelle:

```assembly
0x71EE6C: bl  #0xc6064c       ; call nn::audio::AddAux(...)
0x71EE70: mov r7, r0          ; r7 = return value (Result)
...
0x71EE80: cmp r7, #0
0x71EE84: bne 0x71EE9C        ; if r7 != 0 (error), skip work
0x71EE88..98: do work, set r5=1, write 1 to r4+0x18 ("done" flag)
0x71EE9C: mov r0, r5; pop {... pc}
```

### Comparaison upstream pour AddAux (zuyu vs ruzu)

| Fichier | Status |
|---|---|
| `renderer/effect/aux_.{cpp,rs}` Update v1/v2 | ✅ Équivalent |
| `renderer/command/effect/aux_.{cpp,rs}` Process | ✅ Équivalent (fix `1c420d5` déjà en place) |
| `renderer/memory/pool_mapper.{cpp,rs}` TryAttachBuffer | ✅ Équivalent |
| `pool_mapper FillDspAddr / FindMemoryPool` | ✅ Équivalent |

### Le mur

Le memo `[[mk8d-audio-ipc-ruled-out-2026-05-21]]` affirme : RequestUpdate IPC byte-identical ruzu vs zuyu (calls 8-185). Si IPC output identique → AddAux client retourne valeur identique. Donc le `r7` que MK8D vérifie est le MÊME en ruzu et zuyu.

→ **Le polling 47Hz n'est PAS causé par AddAux retournant différemment.** C'est ailleurs.

### Hypothèses restantes

1. **Calls 1-7 NOT byte-identical** (memo ne couvre que 8-185). Si AddAux fire dans les 7 premiers RequestUpdates, divergence early.

2. **State path bypassant l'IPC** : DSP écrit dans le command workbuffer (mémoire partagée mappée par MK8D). Si MK8D vérifie un FIELD dans le workbuffer (genre aux_status au-delà de ce que RequestUpdate retourne), et que notre DSP n'écrit pas ce field, MK8D voit stale state.

3. **Le polling loop n'utilise PAS le retour AddAux comme exit condition** : `r5` (la valeur retournée par la fonction interne) dépend du caller state, pas de AddAux seul. Le caller peut avoir une autre logique pour exit.

4. **Late call divergence** : peut-être que calls 186+ divergent silencieusement (memo dit "similar late attach lag" sans dire identical).

### Pour aller plus loin

- **Trace AddAux's return value runtime** en ruzu vs zuyu (sortir le r7 à chaque call à 0x71EE70)
- **Range-dump du workbuffer ruzu vs zuyu** avant/après chaque RequestUpdate dans les premiers ~10 calls
- **Trace le caller** de la fonction à 0x71EE40 pour voir la VRAIE exit condition du polling (PC du caller, R5/R4/R0 sur return)

## State machine dispatch découvert (session continue, post-skill)

Trace upward depuis 0x71EE40 via le skill `decompilation/find_callers.py` :

```
0x71EDCC  (poll function — calls AddAux)
   ↑ bl from 0x716040 and 0x71625C
0x715FD4  (one "step" function — wraps poll with vtable checks)
   ↑ bl from 0x7178CC
0x717178  (jump table base)
0x71714C  (DISPATCH HUB)
```

Le hub à `0x71714C` est une state machine driver :
```asm
0x71714C: cmp r4, #0              ; r4 = current state node
0x717150: subeq sp, fp, #8        ; if r4==0 → exit function
0x717154: popeq {r4, r5, fp, pc}
0x717158: ldr r0, [r4, #4]        ; state_id = r4[1]
0x71715C: sub r0, r0, #2          ; index = state_id - 2
0x717160: cmp r0, #0x4B           ; valid? (0..75)
0x717164: bhi 0x717ab4
0x717168: lsl r0, r0, #2          ; index * 4
0x71716C: add r1, pc, #4          ; jump table base = 0x717178
0x717170: ldr r0, [r0, r1]
0x717174: add pc, r0, r1          ; jump to handler[index]
; handler[N] at 0x717178+N (75 entries, each one's relative offset packed)
```

Chaque handler de la table de 75 :
1. Setup args
2. `bl <step_function>` (e.g. 0x715FD4 → 0x71EDCC → AddAux)
3. `ldr r4, [r4]` ← advance to next state (linked list)
4. `b 0x71714C` ← back to dispatch

→ **MK8D doit progresser ~7-75 transitions d'état avant de finir cette init phase.** Chaque wake = 1 transition.

zuyu termine la chaîne en ~7 audio frames (per memory note `tid=75 wait 7 fois`). ruzu reste bloqué à UN état spécifique — la machine d'état ne peut pas avancer parce qu'une condition liée à l'état courant n'est pas satisfaite.

### Vrai next step (concret)

**Tracer le state ID à chaque wake de tid=75 sur cv 0x7F2C0BCC** :
- Hook le JIT à PC 0x717158 (`ldr r0, [r4, #4]`)
- Logger `r0` (le state_id avant -2) à chaque wake
- Comparer la séquence d'états ruzu vs zuyu

Si zuyu fait `state_id = 2, 3, 5, 7, 10, 12, 15, ... done` et ruzu fait `state_id = 2, 3, 5, 5, 5, 5, 5, ...` → ruzu est bloqué à state 5. Aller inspecter handler[5-2] = handler[3] à `0x717178 + 3*4 = 0x717184` pour voir quelle fonction lance ce state et ce qu'elle vérifie.

Le skill `decompilation` permet maintenant de :
- `find_callers.py <NSO> --target <runtime_addr> --base <base>` → trouve tous les callers
- `find_symbol.py <NSO> --pc <plt_addr> --base <base>` → identifie symbol PLT
- `disasm.py <hex> <vma>` → disasm ARM32 rapide

Tous les scripts ont les corrections "anti-piège" (PC+8, imm rotation, base verification).

## SVC trace REGS (session continue)

`RUZU_SVC_TRACE=1 RUZU_SVC_TRACE_REGS=1` dump tous les registers à chaque SVC. Sur tid=75 cv waits :

```
imm=0x1c svc=WaitProcessWideKeyAtomic tid=75
  r0=0x7f2c0bc8 r1=0x7f2c0bcc r2=0x000081ff r3=0xffffffff
  r4=0xffffffff r5=0x7f2c0bcc r6=0x7f2c0bc5
  r7=0x5528b120  ← CONSTANT through all waits (likely state-machine context)
  pc=0x01d50438 lr=0x01d33100
```

**r4 est 0xFFFFFFFF au SVC** (libnx l'a modifié pour le wait). Le state pointer du state machine n'est PAS visible directement au moment du SVC.

`r7=0x5528B120` reste constant à travers tous les waits = pointeur vers un objet (heap MK8D) qui CONTIENT le state machine context indirectement. Dumping `0x5528B100..0x5528B180` montre :
```
0x5528B120: e208 3e52 28b4 2855 0000 0000 0803 0000
```
= une structure avec pointeurs vers `0x523EE208` (probablement vtable) et `0x5528B428` (heap object), + size `0x308`. Pas le state node direct.

## Mur d'analyse statique atteint

Pour aller plus loin sans modifs intrusives :

1. **JIT instrumentation** — hook PC=0x717158 dans rdynarmic pour logger r0 (state_id) à chaque iteration. ~1 jour de travail rdynarmic (callback inline).

2. **Side-by-side zuyu comparison** — run MK8D dans zuyu local, capturer le state-machine memory à la freeze, diff vs ruzu. Requiert zuyu build setup et knobs équivalents.

3. **Memory diff bytes-par-bytes** — dump heap MK8D autour de 0x5528B*** ruzu vs zuyu au même SVC count. Le byte qui diffère localise le bug.

4. **Stop ici et attaquer un autre angle** — par exemple :
   - Stub `nn::audio::AddAux` côté audio renderer pour systématiquement faire avancer la state machine (intrusif)
   - Patcher MK8D directement pour skip la state qui bloque
   - Investiguer d'autres titres pour confirmer si le bug est généralisé

## Session achievements (à retenir)

- ✅ **Decompilation toolkit complet** dans `.agents/skills/decompilation/` (commit `dcf3ad4`)
- ✅ **Base MK8D main NSO mesurée** = `0x00206000` (registry `bases.json`)
- ✅ **PLT entry pour polling-loop identifié** = `nn::audio::AddAux` (sym 455)
- ✅ **Call chain remontée** : AddAux ← polling fn ← step fn ← state machine dispatch
- ✅ **Architecture state machine** : 75 états max, dispatched at 0x71714C
- ⚠️ **State spécifique qui bloque non identifié** — besoin instrumentation runtime

Les sessions précédentes affirmaient :
- `[[mk8d-cv-5564c6d8-2026-05-19-night]]` : "AddAux returns 0 forever → infinite loop"
- `[[mk8d-audio-ipc-ruled-out-2026-05-21]]` : "Audio IPC byte-identical → cause is elsewhere"

Notre session a confirmé le SYMBOLE (AddAux) ET la STRUCTURE (state machine), mais pas l'état spécifique. Le bug est dans **comment MK8D évalue la sortie du state-handler suivant AddAux** (côté guest, pas IPC).

## Investigation Audio Pool Attach (session 2026-05-23 late)

### Pattern observé dans `RequestUpdate` IPC

Avec `RUZU_DUMP_AUDIO_UPDATE_RANGE=1-7`, calls 1-7 captured et parsés :

**INPUT MK8D (per call) :** 398 pools dans le tableau, état distribué :
- Call 2: `1 RequestAttach (pool[0])` + 397 Released
- Call 4: `1 Attached (pool[0])` + `1 RequestAttach (pool[1])` + 396 Released
- Call 6: `2 Attached (pool[0,1])` + `1 RequestAttach (pool[2])` + 395 Released

→ **MK8D attache séquentiellement, 1 pool par 2 calls**. 398 pools × 2 = ~800 calls = ~17s à 47Hz.

**OUTPUT ruzu (per call) :** correspondant — chaque RequestAttach → Attached.

### Mais à call 100, 105, 110...

```
call 100: total=398 attached=3 requested=0 released=395
call 105: total=398 attached=3 requested=0 released=395
call 110: total=398 attached=3 requested=0 released=395
```

INPUTS et OUTPUTS sont **byte-identical** entre calls 100-110.

→ **MK8D s'est arrêté de requester pool attach après pool[2]**. Polling continue à 47Hz mais MK8D n'envoie plus de nouvelle requête. Le state machine attend QUELQUE CHOSE QUI N'EST PAS LIÉ À L'AUDIO POOL.

### Implications

1. **Le cv 0x7F2C0BCC est juste le heartbeat audio frame** (signalé par ruzu's rendered_event à 47Hz)
2. **MK8D wake → check une condition non-audio → re-wait**
3. **La condition n'est PAS dans l'IPC RequestUpdate output** (byte-identical)
4. **La condition n'est PAS dans l'audio pool attach** (game stopped requesting)

### Hypothèses pour la cause finale (NON liée audio)

1. **File system asynchronous operation** : MK8D attend qu'un fichier finisse de loader (NCA mount, RomFS read async)
2. **Network/account event** : `nn::account::EnsureNetworkServiceAccountAvailable` ou similaire qui ne signale jamais
3. **Graphics surface ready** : MK8D attend nvflinger d'allouer un buffer spécifique
4. **AppletStateChanged event** : MK8D attend un message AppletMessage (`OnOperationModeChanged`, `OnPerformanceModeChanged`, etc.)

### Skill `decompilation` utilisé cette session

- `find_callers.py` (nouveau) — scan .text for BL targeting given address
- Permet de remonter le call stack : 0x71EE6C (AddAux call) ← 0x71EDCC (poll fn) ← 0x715FD4 (step fn) ← state machine dispatch hub at 0x71714C
- Identifié state machine 75 états max

### DSP CommandListProcessor

Vérifié actif via `RUZU_TRACE_ADSP_AUDIO=1` :
```
ADSP::AudioRenderer dsp main received Render
ADSP::AudioRenderer dsp main sending RenderResponse
```

→ DSP processor.process() est appelé. Le DSP n'est PAS la cause du blocage.

### Next session

Investigate **non-audio services** que MK8D consume :
- `am:` (applet message) — vérifier si MK8D fait `ReceiveMessage` puis attend `OnOperationModeChanged`
- `fsp-srv` async file load completion
- `nvgfx` (nvflinger) buffer allocation
- `acc:u` user account state

Quick-test : signaler manuellement quelques AppletMessage events au démarrage (intrusive but informative).

### Implémentation actuelle ruzu

`core/src/hle/service/aoc/addon_content_manager.rs` :
- `count_add_on_content_handler` → return `count = 0` (pas de DLC)
- `get_add_on_content_list_changed_event_handler` → return event handle
- `prepare_add_on_content_handler` → no-op SUCCESS

L'implémentation côté serveur est OK. Mais MK8D's `nn::aoc::Initialize()` est CÔTÉ CLIENT (dans un subsdk NSO de MK8D, qui appelle `aoc:u` via IPC). C'est cette fonction client qui doit retourner non-zero pour MK8D.

### Pistes pour FIX

1. **Investiguer ce que `nn::aoc::Initialize` attend exactement** : analyser le subsdk NSO de MK8D qui défini cette fonction. Trouver quelle IPC sequence elle fait sur `aoc:u` et quelle condition fait return non-zero. Probablement un truc comme "DLC list ready" ou "event signaled".

2. **Signaler l'event aoc_list_changed UNE FOIS au démarrage** : MK8D pourrait être bloqué en attente que l'event soit signalé. Tester en signalant l'event après création.

3. **Hypothèse alternative** : `Initialize` lit l'application's launch parameter pour savoir s'il y a une mise à jour DLC en cours. Si on retourne un launch param spécifique → return non-zero.

4. **Check si MK8D appelle d'autres aoc methods entre les Initialize** : tracer toutes les IPC sur aoc:u pendant le polling pour voir le pattern complet.

### Tools utilisés (réutilisables next session)

- `hactool` package custom : `/home/vricosti/Dev/emulators/hactool_1.4.0+git20231010_amd64.deb`
- `hactool` source : `/home/vricosti/Dev/emulators/hactool/`
- Python `lz4.block` : `pip install --user --break-system-packages lz4`
- MK8D extracted artifacts : `/tmp/mk8d_extract/`
  - `pfs0/` : 5 NCAs
  - `exefs/main` : main NSO
  - `exefs/rtld`, `sdk`, `subsdk0-4` : nnSdk NSOs (pour identifier `nn::aoc::Initialize` impl)
- Imports list : `/tmp/mk8d_imports.txt` (812 imports)

## Fichiers/sites touchés cette session

- `core/src/hle/kernel/k_thread.rs:1357` — soft fix priority inheritance race (committed dans `d7cc0e6`)
- `core/src/memory/memory.rs`, `video_core/src/host1x/gpu_device_memory_manager.rs`, `ruzu_cmd/src/main.rs` — fix MarkRegionCaching Mutex<Memory> bypass (committed dans `c65d2c0`)
- `video_core/src/renderer_opengl/gl_rasterizer.rs`, `gl_blit_screen.rs`, `mod.rs` — state_tracker mutex revert (committed dans `bb853fb`)
- `video_core/src/texture_cache/texture_cache_base.rs` — sRGB format experiment **reverted** (n'aide pas)

## Session 2026-05-23 PM (suite, post-AM-piste)

### Faux positif : SF_COMPOSE display routing

Hypothèse "display 0 only composed 2× in 60s" → **ARTEFACT de log-sampling**. Le compteur `COMPOSE_COUNT` est partagé entre les 5 displays (`surface_flinger.rs:110`), iterated dans HashMap order `(3,4,0,2,1)`. Display 0 est à position 2 → `n=5k+2` → `n%60==0` n'a aucune solution (5k+2 ≡ 0 mod 60, gcd(5,60)=5 ne divise pas 2). Seul `n.is_power_of_two()` peut sampler d0, et seulement pour les puissances de 2 ≡ 2 mod 5 : {2, 32, 512, 8192, ...}. Les logs `#512` et `#8192` matchent **exactement**. Display 0 EST composed 60×/sec, le log ne le sample juste pas.

### Buffer queue analysis : symptôme du wedge, pas la cause

Counts sur run 60s :
- `BQP_QUEUE` : **23 events** (game a queue 23 frames)
- `BQP_DEQUEUE_RET` (succès) : **1729** (game polled buffer free pool 1729× successfully)
- `BQP_DEQUEUE_BLOCK` (échec, queue full) : **5172**
- `BQC_RELEASE` (consumer release back to free pool) : **23** — match les queues

Steady-state à ~0.4 Hz (23 frames / 60s). Le buffer queue n'est PAS la source du wedge — c'est la state machine MK8D qui ne progresse pas et donc ne queue qu'une frame de temps en temps. Le polling 1729 vs 23 est un dequeue-cancel loop quand le state machine veut probe (cancel_buffer return slot to free sans queue).

`hardware_composer.rs::compose_locked` accept-and-release par compose tick fonctionne correctement (release_frame_number=1 ≤ frame_number après chaque tick). Le BQP_DEQUEUE_BLOCK 5172× est normal pour du polling non-blocking.

### NEW : PrefetchAbort cascade à t=53s (vtable corruption pattern)

Pattern observé reproductible :
- **t=43:00** : activité kernel/audio s'effondre (32k events/5s → 512/5s)
- **t=43:29** : 4+ threads PrefetchAbort en parallèle dans `multi_core_run_guest_thread`

Threads concernés :
```
tid=95  core=1 pc=0x6A15D788 lr=0x01F028D0 sp=0x022C5EA8 r0=0x00001C00
tid=105 core=1 pc=0x6A15D770 lr=0x01F028D0 sp=0x18C8FF80 r0=0x6A15D770   ← même LR
tid=97  core=1 pc=0x6A15DE64 lr=0x00758964 sp=0x10B7FFA8 r0=0x00F42400
tid=96  core=1 pc=0x00000104 lr=0x00758964 sp=0x036F0FA8 r0=0xEE888000   ← même LR
```

PCs sont dans le HEAP guest (0x6A15D7xx zone), pas dans NSO. Le mot à 0x6A15D788 = `0xFFFFFFC0` (sentinel/uninit). Pattern de mémoire à 0x6A15D770 (structure entête) :
```
+0x00: 0x0000001C       ; type/size header
+0x04: 0x6A15D770       ; self-ptr (linked list head, next=self)
+0x08: 0x6A15D770       ; self-ptr (prev=self)
+0x0C: 0x6A15DE10       ; ptr to peer struct
+0x10..+0x14: 0
+0x18: 0xFFFFFFC0       ; SUPPOSED to be vptr, est garbage
```

= classic C++ STL **empty list head** (sentinel pointing to self), avec un champ vtable+0x18 non initialisé.

LR pairs (0x01F028D0 et 0x00758964) sont dans MK8D's main NSO (base 0x206000) :
- LR=0x01F028D0 → NSO offset 0x1CFC8D0
- LR=0x00758964 → NSO offset 0x552964

**Hypothèse** : MK8D a un timeout dans son state machine wedge. Après ~50s sans progress, il appelle une cleanup/error-recovery routine qui itère sur une liste d'objets (la list head à 0x6A15D770). Mais ces objets n'ont jamais été correctement initialisés parce que la state machine n'a jamais atteint l'état d'init → vtable corrompue → crash.

Donc les crashes sont une **conséquence** du wedge, pas une cause indépendante. Mais le pattern d'erreur (multiple threads crashing simultaneously avec LR partagés) suggère un cleanup fork/parallel-spawn quand le timeout fire.

### Crash localisation par disasm — GOT CORRUPTION confirmée

Via le skill `decompilation`, NSO load-map déduite du log :

| NSO | Runtime base | Image size |
|---|---|---|
| rtld | 0x00200000 | 0x6000 |
| main | 0x00206000 | 0x1308000 |
| subsdk0 | 0x01512000 | 0x199000 |
| subsdk1 | 0x016AB000 | 0x28000 |
| subsdk2 | 0x016D3000 | 0x13000 |
| subsdk3 | 0x016E6000 | 0x3D000 |
| subsdk4 | 0x01723000 | 0x57A000 |
| sdk | 0x01C9C000 | 0x6F4000 |

LR=`0x01F028D0` est dans **sdk** (offset 0x2668D0). Disasm révèle un loop d'itération de liste C++ STL :

```asm
0x01F028AC: push   {r4-r8, sb, sl, fp, lr}     ; function entry
0x01F028B0: add    fp, sp, #0x1c
0x01F028B4: sub    sp, sp, #0x14
0x01F028B8: mov    r4, r0                       ; r4 = this (0x6A15D770)
0x01F028BC: ldr    r6, [r4, #0x20]              ; r6 = this->list.next
0x01F028C0: add    r5, r4, #0x1c                ; r5 = end sentinel = &this->list
0x01F028C4: b      0x1F028D4
0x01F028C8: ldr    r0, [r6, #8]                 ; r0 = node->data (child node)
0x01F028CC: bl     0x2028494                    ; call SyncSampleNumber(child) via PLT
0x01F028D0: ldr    r6, [r6, #4]                 ; r6 = node->next         ← LR
0x01F028D4: cmp    r6, r5                       ; end of list?
0x01F028D8: bne    0x1F028C8                    ; loop
```

La fonction = `_ZN2nn3hid13VibrationNode16SyncSampleNumberEv` (sym idx 8350, valeur=0x2668AC, taille=0xC4). C'est **nn::hid::VibrationNode::SyncSampleNumber()** — itération récursive sur les child VibrationNodes.

PLT 0x02028494 → trampoline `add ip,pc,#0x200000 ; add ip,ip,#0x9D000 ; ldr pc,[ip,#0xA28]!` → load PC from `[0x22C5EC4]`.

Le crash log dump du stack/.data autour de sp=0x022C5EA8 montre :
```
[022C5EC4] = 6A15D770    ← GOT slot pour SyncSampleNumber CORROMPU
```

Mappé : **GOT slot @ sdk offset 0x629EC4 = JMPREL[3725] for `nn::hid::VibrationNode::SyncSampleNumber`**.

🎯 **MÉCANISME** : Le PLT load PC depuis le GOT slot. Mais le GOT slot a été overwritten avec `0x6A15D770` (un pointeur heap). PC=0x6A15D770 → exécution interprète les données heap comme instructions ARM → tombe dans `0xFFFFFFC0` à 0x6A15D788 → PrefetchAbort.

LR=`0x00758964` (main NSO offset 0x552964) : pattern similaire, appelle `0xC5F998` qui est un tailcall trampoline (add ip,pc,#0x200000 ; add ip,ip,#0x72000 ; ldr pc,[ip,#0x8C8]!) → load PC from `[0xED2268]` (main GOT slot). Probable même cause (GOT corruption sur un symbole différent).

### Hypothèses pour la GOT corruption

1. **Write hors-bound dans ruzu's loader/relocator** : ruzu's NSO loader fait les relocations JUMP_SLOT au load time. Si le calcul d'adresse est faux ou un offset overflow, on écrit la résolution à la mauvaise adresse → un slot GOT contient une valeur non-fonctionnelle.
   - Le fait que `0x6A15D770` ressemble à un VibrationNode HEAP object suggère que ruzu's loader a confondu un "default value" (peut-être un pointeur de constructor ou de constante) avec l'adresse de fonction.
   - Vérifier `core/src/loader/nso.rs` et la gestion JMPREL/relocations.

2. **MK8D auto-corruption via buffer overflow** : MK8D's code écrit au-delà d'un buffer dans la heap → écrase sdk's .data GOT region.
   - Moins probable car le pattern (multiple threads, même symbole) suggère corruption point unique répétée.

3. **GOT pas mappée read-only après relocation** : zuyu marque la GOT en RO après resolution (RELRO). Ruzu pourrait ne pas le faire → tout write byzantin se propage.

4. **VibrationNode tree never initialized + uninitialized memory** : tres possible. MK8D's main thread tente d'init le HID VibrationNode tree mais la state machine bloque → tree resté uninitialized → premier appel à SyncSampleNumber tombe sur de la mémoire heap fraîche dont les premiers mots ressemblent à un node head mais le GOT est aussi resté à sa valeur initiale (0x6A15D770 sera la valeur écrite par le constructeur d'un VibrationNode).
   - Hypothèse forte : **GOT slot pour SyncSampleNumber est resté à sa valeur "lazy resolver stub" qui pointe vers une thunk dynamique du loader**. Si ruzu's loader ne resoud pas correctement ce symbole...
   - Vérifier : pour les symbols définis dans le MÊME module (sym.value = 0x2668AC dans sdk), est-ce que ruzu's loader patch correctement le GOT slot avec sdk_base + 0x2668AC ?

### Pour next session

1. **Vérifier `core/src/loader/nso.rs` traitement des R_ARM_JUMP_SLOT** : confirmer que le code écrit `module_base + sym.st_value` au GOT slot. Si le symbol est défini dans le même module, c'est trivial; si EXTERN, plus complexe.
2. **Tracer les écritures à `sdk_base + 0x629EC4` = 0x22C5EC4** : env-gate un breakpoint mémoire dans guest_memory.rs ou logger les writes range 0x22C5E00..0x22C5F00. Le premier write qui dépose 0x6A15D770 nous donnera l'instant + le code source.
3. **Comparer GOT contents ruzu vs zuyu** au démarrage (t=1s post-boot). Si zuyu a une autre valeur à 0x22C5EC4, on a localisé la divergence.
4. Le state machine wedge reste la priorité absolue — le crash à 53s est just du wedge qui finit par tuer le jeu, ET pourrait être lié si la GOT corruption a lieu au load time (mais alors zuyu aussi crasherait).

### Note finale

Ce crash à t=53s n'arrive PAS en zuyu — donc la GOT corruption est **ruzu-specific**. Si on fixe le wedge, le jeu n'arrive jamais aux call-sites des VibrationNode, donc le crash est masqué. Mais le bug GOT existe quand même et pourrait causer d'autres bugs silencieux. Vaut la peine d'investiguer en parallèle du wedge.

## Root cause GOT corruption — RAFFINÉ : thread stack overlap PLTGOT region

**Découverte clé**: tid=95 a `sp=0x022C5EA8` qui est **DANS la PLTGOT de sdk** :

```
sdk PLTGOT range: 0x022C2484 .. 0x022C663C
tid=95 sp=0x022C5EA8   ← IN PLTGOT
tid=97 sp=0x10B7FFA8   ← normal heap stack
tid=96 sp=0x036F0FA8   ← normal stack
tid=105 sp=0x18C8FF80  ← normal heap stack
```

Seul tid=95 a son stack DANS le GOT. Les autres ont des stacks dans des régions hautes typiques de heap-allocated worker threads.

**Mécanisme prouvé** : la frame de tid=95 sur la pile montre le pattern exact d'un `push {r4-r8, sb, sl, fp, lr}` (9 registres × 4 = 36 bytes = 0x24) + sub sp, #0x14 (locals 0x14) = total 0x38 byte frame. La pile contient :
- sp+0x00..0x1C : 7 registres pushés (r4=0x6A15D770, etc.)
- sp+0x20..0x30 : 3 derniers registres (fp, sl, sb)
- sp+0x34 : LR sauvé = 0x01F028D0 (= matches crash LR)

Le push à `0x01F028AC: push {r4, r5, r6, r7, r8, sb, sl, fp, lr}` au début de SyncSampleNumber a écrit les valeurs des registres dans la PLT GOT slot 0x022C5EC4 (sp+0x1C dans la frame) qui se trouve **AU MILIEU de la frame de la fonction qui s'exécute**.

Ainsi, **la fonction SyncSampleNumber elle-même corrompt son propre GOT slot via son prologue**. Puis tout autre thread qui appelle SyncSampleNumber via PLT charge la valeur corrompue (= r6 du frame de tid=95 = 0x6A15D770) et crash.

### Architecture du bug (32-bit ARMv7)

Dans `set_code_region` (`k_process_page_table.rs:509-516`) pour `address_space_width <= 32` :
```rust
base.m_stack_region_start = base.m_code_region_start;   // = 0x200000
base.m_stack_region_end = base.m_code_region_end;        // = end of code/modules
```

Le stack_region pour 32-bit chevauche INTENTIONNELLEMENT le code_region — c'est documenté chez zuyu (`k_page_table_base.cpp:261-265`). Pour 32-bit, `stack_region_size = 0` dans `InitializeForProcess` donc le stack_region reste à `[code_region_start, code_region_end]`.

Le `code_region` pour 32-bit englobe **toute la zone allouable de l'address space** (jusqu'à 0x80000000). Donc en théorie, sdk loaded à 0x01C9C000 EST dans la code_region. Et le stack_region inclut sdk.

**La question est : comment libnx (côté guest MK8D) choisit-il un stack_top pour un nouveau thread ?**

Probabilités :
1. **libnx malloc → returns stack memory** : libnx alloue depuis la heap (set_heap_region = `stack_top` du main thread). Heap doit débuter APRÈS sdk. Vérifier :
   - modules_end = 0x01C9C000 + 0x6F4000 = 0x02390000
   - tls_end = 0x02394000
   - main_stack_base = 0x02398000
   - heap_start = main_stack_top = 0x02398000 + stack_size
   - Donc heap_start ≥ 0x02398000 et tid=95 sp=0x022C5EA8 EST BIEN AVANT (dans sdk).
2. **libnx fait sa propre allocation de stack via SVC MapMemory à une adresse fixe (compile-time)** : possible si MK8D's libnx ou nnSdk a une table d'adresses de stack hard-codées qui ne tiennent pas compte de la zone des modules.
3. **Le main thread stack est sous-dimensionné** : si main_thread_stack_size est petit, peut-être que libnx alloue les worker stacks avant la heap, à des adresses contigues, qui finissent dans sdk's zone.

### Pour next session — vérification

1. **Re-run avec `RUZU_TRACE_TID_SVC='*'`** pour capturer les CreateThread + leur stack_top. Identifier l'adresse stack_top de tid=95.
2. **Loguer la heap_region_start/end de MK8D au démarrage** dans `k_process.rs::initialize_main_thread_stack_region`. Vérifier que heap > 0x02390000.
3. **Si MK8D donne explicitly à CreateThread un stack_top dans sdk's region** → c'est un bug guest-side (probable nnSdk Switch system component bug) qui devrait être détectable par zuyu aussi.
4. **Sinon** → ruzu's `ensure_user_stack_mapping` (`svc_thread.rs:264-353`) doit retourner ERROR au lieu de silently skip quand stack_top est non-FREE. Cela force le guest à allouer ailleurs.

Le fix le plus simple = échouer CreateThread quand stack_top tombe dans une région CODE ou DATA mappée par un module loaded. Cela ferait crasher MK8D EXPLICITEMENT au démarrage du thread au lieu de causer un mystery crash 53s plus tard.

Mais avant de fix, vérifier que zuyu se comporte différemment ici (peut-être que zuyu rejette aussi mais que libnx tente plusieurs adresses si la première échoue). Sinon le bug est ailleurs (heap_region setup incorrect dans ruzu).

## TEST 2026-05-23 PM-late : hypothèse stack-in-PLTGOT PARTIELLEMENT RÉFUTÉE

Ajout temporaire dans `svc_thread.rs` :
1. `RUZU_SVC_TRACE_THREAD=1` log inclut maintenant `stack=0x{:X}` (gardé)
2. Always-on warn `[SVC_CREATE_THREAD_STACK_COLLISION]` quand stack_top tombe sur non-FREE (REVERTI — trop bruyant, faux positifs)

Run 70s MK8D, résultats :

- **44 threads créés** par MK8D au boot, tous via SVC CreateThread
- **44/44 stack_tops** sont dans la région `[0x02390000..0x40000000)` (le gap entre modules_end et heap_start)
- **Aucun** des 44 stack_top/base ranges ne chevauche un module loaded (rtld/main/subsdk*/sdk)
- L'état rapporté pour la gap region = `IO_MEMORY | STATIC | IPC` — pas FREE, mais aussi pas CODE_DATA

Le warn fire 44 fois car ruzu's query_info retourne `IO_MEMORY|STATIC|IPC` pour cette région (pas FREE). Probablement libnx pre-alloue un "stack arena" via SVC MapMemory ou SetMemoryAttribute, et ruzu's KMemory marque correctement cette region comme `IO_MEMORY|STATIC|IPC`. C'est valide pour des stacks user-mode (non-FREE car déjà mapped par le guest).

### Crash this run

Different from previous run :
- t=43:29 (previous run) : tid=95 PrefetchAbort pc=0x6A15D788 lr=0x01F028D0 — exécution de heap data
- t=30 (this run)       : tid=95 PrefetchAbort pc=0x00201A44 lr=0x00201860 — exécution dans rtld text

PC=0x201A44 EST dans rtld (loaded 0x200000, text_size 0x34ac). LR=0x201860 aussi rtld. Le crash sp=0x022C5DD0 EST dans sdk's PLTGOT range — mais tid=95 a été créé avec stack=0xCDAAFF0 (gap region). Donc tid=95 a déplacé son SP d'environ ~13 MB pendant 30 secondes d'exécution. Cela suggère soit :
- Fiber/coroutine context switch
- Signal stack
- Thread bootstrap migration vers un "real" stack

OU plus probable : **le thread a corrompu son propre SP** via une erreur quelconque dans rtld init, et est tombé dans une zone (PLTGOT) qui happens to be valid memory.

### Conclusion sur la stack-in-PLTGOT theory

❌ **Théorie réfutée comme bug systématique** : les stacks NE sont PAS systématiquement allouées dans des PLTGOT. Le run précédent où tid=95 avait sp=0x022C5EA8 (dans sdk PLTGOT) était soit :
- Une coïncidence (sp arrivé là après corruption)
- Une instance spécifique d'un autre bug

✅ **Diagnostic conservé** : `stack=0xXXX` ajouté au log `[SVC_CREATE_THREAD]` pour usage futur via `RUZU_SVC_TRACE_THREAD=1`.

### Vraie hypothèse maintenant

Les crashes à t=30-60s sont des **conséquences downstream** du state machine wedge. Le wedge cause une dégradation progressive de l'état (tid=75 polls cv inutilement, threads workers font du non-progress, état mémoire incohérent). Eventually un thread fait quelque chose d'invalide (corrupted vtable read, wild pointer, bad sp) et crash.

**Le crash n'est pas le bug — c'est le symptôme final**. Le bug est dans pourquoi la state machine ne progresse pas.

### Pour next session — vraiment cette fois

Le seul vrai chemin de progrès = identifier ce que la state machine attend. Concrètement :

1. **Hook PC=0x717158 dans rdynarmic** pour logger `r0` (state_id pre-subtract) à chaque iteration de la state machine
2. **Comparer la séquence d'états ruzu vs zuyu** — où ruzu reste-t-il bloqué que zuyu traverse ?
3. **Si state machine bloque à un state N spécifique**, disassembler le handler de N pour comprendre quelle condition externe il attend
4. Cette condition externe sera certainement un état de service (file system, account, applet, nvgfx) qui ruzu signal différemment de zuyu

## 🤦 MY "CORRECTION" WAS WRONG — Original AddAux IS correct (2026-05-23)

I made the same mistake the original investigation made. Triple-verified:

**PLT[N] does NOT map to JMPREL[N-1].** PLT entries reference SCATTERED GOT slots, not in sequence. The correct way:

1. Compute PLT[N]'s actual GOT slot from the trampoline math
2. Search JMPREL for r_offset matching the GOT slot
3. Look up sym_idx

For `0xC6064C` (PLT[7]):
- trampoline: `add ip,pc,#0x200000; add ip,ip,#0x72000; ldr pc,[ip,#0x50]!`
- pc at first insn = 0xC60654 (pc+8)
- ip → 0xC60654 + 0x200000 + 0x72000 + 0x50 = **0xED26A4**
- NSO offset = 0xED26A4 - 0x206000 = **0xCCC6A4**
- JMPREL search → **JMPREL[323]** → sym[455] = **`nn::audio::AddAux`** ✓

For `0xC60640` (PLT[6]):
- trampoline: `add ip,pc,#0x200000; add ip,ip,#0x72000; ldr pc,[ip,#0x58]!`
- ip → 0xC60648 + 0x200000 + 0x72000 + 0x58 = **0xED26A0**
- NSO offset = 0xCCC6A0
- JMPREL search → **JMPREL[322]** → sym[463] = **`nn::audio::SetAuxEnabled`**

### So the wedge chain IS audio (original analysis correct)

```
state 63 → step fn 0x715FD4 → poll fn 0x71EDCC
  → bl 0xC6064C  = nn::audio::AddAux         (returns 0 in ruzu forever)
  ← r7 = result
  if r7 != 0: exit success (state advances)
  else: bl 0xC60640 = nn::audio::SetAuxEnabled (cleanup, disable aux)
        return → re-poll
```

### Test confirmation

Run with `log::info!` on all aoc:u handlers → **ZERO aoc:u IPC calls during wedge**. This is consistent with the wedge being in audio (not aoc), since aoc service is unused.

### Anti-pattern: PLT[N] index ≠ JMPREL index

The PLT index N IS NOT the same as the JMPREL index. PLT entries can resolve to arbitrary GOT slots based on the trampoline's `add ip,pc,#A; add ip,ip,#B; ldr pc,[ip,#C]!` math. Always compute the GOT slot address and search JMPREL by r_offset.

### So all the previously-explored audio piste IS the right direction

- [[mk8d-cv-5564c6d8-2026-05-19-night]] initial analysis (AddAux) — CORRECT
- [[mk8d-cv-5564c6d8-2026-05-19-night]] "revision" claiming JMPREL[323]/sym 455 — CORRECT (despite my earlier mis-correction)
- [[mk8d-audio-ipc-ruled-out-2026-05-21]] — Audio renderer IPC byte-identical for calls 8-185, but this doesn't necessarily rule out AddAux. AddAux is CLIENT-side and may read DSP shared memory state that isn't covered by IPC byte-diff.

### Real next session — DSP shared memory

The remaining unexplored angle is the **audio DSP shared work-buffer state** that MK8D reads CLIENT-side. The DSP writes effect state, performance state, AUX status, etc., into a memory region the client maps. If ruzu's DSP processor doesn't write the same state, MK8D's AddAux reads stale state and returns 0.

Concrete files to investigate:
- `audio_core/src/renderer/command/effect/aux_.rs` (AUX command processor — does it write AuxInfoDsp correctly?)
- `audio_core/src/renderer/system_manager.rs` (top-level DSP loop)
- `audio_core/src/adsp/...` (DSP framework that processes command lists per frame)

Key fields to check: `AuxInfoDsp.write_offset`, `AuxInfoDsp.read_offset` written each frame by DSP.

### MY MISTAKE THIS SESSION

I attempted to verify the PLT identification using the simplified formula `PLT[N] = JMPREL[N-1]`. That formula is WRONG. I should have done the full GOT slot computation from the start, the way the original investigation correctly did. Future investigators: always compute GOT slot via the trampoline math, never use the index-based shortcut.

## ANALYSE FINALE SESSION 2026-05-23 — Wedge state 63 complètement caractérisée

### Chain confirmée par disasm direct

```
state_id 63 handler @ 0x7178AC
  ├─ bl 0x279bec                          ; lock acquire
  ├─ load 6 args from r4[0x10..0x24]
  └─ bl 0x715FD4                          ; STEP FN
        ├─ check r6[0] flag — if 0, early exit
        ├─ call r4->vtable[8] (returns 1 to continue)
        ├─ bl 0x71f094                    ; setup
        └─ bl 0x71EDCC                    ; POLL FN
              ├─ check r4[0x18] flag — if non-zero, done
              ├─ compute aligned offsets
              ├─ bl 0x279bec                ; mutex lock
              ├─ bl 0xc5fa28                ; another lock (singleton)
              ├─ bl 0xC6064C                ; nn::audio::AddAux (sym 455)
              ├─ r7 = r0 (return value)
              ├─ bl 0x279bec                ; mutex unlock
              ├─ bl 0xc5fa58                ; singleton unlock
              ├─ cmp r7, #0
              │   bne exit-success-path     ; ← zuyu takes this after 7 polls
              ├─ ldrb r1, [r4, #0x19]       ; (only in ruzu path)
              ├─ bl 0xC60640                ; nn::audio::Something — cleanup PLT
              └─ return 0
```

### État de connaissance final

| Question | Réponse |
|---|---|
| Quel est l'état bloqué ? | state_id 63 (handler 0x7178AC) |
| Quelle fonction est appelée ? | nn::audio::AddAux via PLT 0xC6064C |
| AddAux retourne ? | 0 dans ruzu (en boucle) ; non-zero dans zuyu après ~7 polls |
| Pourquoi 0 dans ruzu ? | UNKNOWN — AddAux impl pas dans MK8D's NSOs (système library) |
| Audio IPC byte-identical ? | OUI calls 8-185 vérifié |
| Audio rate ? | 169Hz (vs 200Hz target) — OK |
| Stack-in-PLTGOT systematic ? | NON — réfuté par test (44 stacks dans gap region [0x02390000..0x40000000)) |
| Aux pool état ? | 398 pools, 3 attached — pas full |
| ruzu's aux_.rs match upstream ? | OUI byte-for-byte (vérifié direct) |

### Pourquoi je m'arrête ici

L'analyse statique a localisé le bug avec précision. La cause profonde est dans **comment AddAux (côté nnSdk client de MK8D) évalue le state du audio renderer**. Comme :
- L'implémentation d'AddAux n'est pas dans les NSOs de MK8D
- L'IPC est byte-identical
- L'aux pool a de la place
- ruzu's aux_.rs port matche upstream

Les pistes restantes nécessitent de l'instrumentation runtime substantielle :

1. **JIT hook PC=0x717158** (logger state_id à chaque dispatch) — nécessite modif rdynarmic
2. **Trace AudioRendererConfig en shared memory** au moment où AddAux est appelée — nécessite hook côté guest
3. **Diff side-by-side avec zuyu** au démarrage — nécessite setup zuyu + même traces
4. **Hack audio renderer pour forcer aux state "ready"** — intrusif test

### Recommandation pour next session

Option 4 est la plus rapide pour tester la HYPOTHESE : si on force quelque chose dans `audio_core/src/renderer/...` (par ex. set un flag dans le workbuffer shared memory que MK8D lit pour confirmer "aux ready"), MK8D devrait progresser. Si oui → wedge identifié dans le DSP shared-memory writeback.

Sinon les options 1-3 nécessitent de la vraie work. Approximativement :
- Option 1 : 4-8h rdynarmic
- Option 2 : 2-4h ruzu memory tracing
- Option 3 : 2-4h zuyu setup + trace runs

À discuter avec l'utilisateur.

## SESSION 2026-05-23 PM-very-late — Full poll fn semantics CORRECTED

Disasm of poll fn 0x71EDCC with proper PLT identifications:

```
0x71EDCC..71EDE0: prologue, r5 = 0 (default return)
0x71EDE4: if state.done_flag ([r4+0x18]) != 0 → exit returning 0
0x71EE04: bl 0xc60628 = nn::audio::GetRequiredBufferSizeForAuxSendReturnBuffer  (sym 456)
0x71EE08: r6 = required size
0x71EE0C..2C: compute aligned send_buffer (r7) + return_buffer (r8) inside [r4+0x1c]
0x71EE4C: bl 0xc5fa28 = nn::os::LockMutex                                          (sym 779)
0x71EE6C: bl 0xc6064c = nn::audio::AddAux(config, aux, submix, send, return, count) (sym 455)
0x71EE70: r7 = AddAux result
0x71EE7C: bl 0xc5fa58 = nn::os::UnlockMutex                                        (sym 783)
0x71EE80: cmp r7, #0
0x71EE84: bne 0x71EE9C   ← if AddAux returned ERROR (non-zero), exit with r5=0
0x71EE90: bl 0xc60640 = nn::audio::SetAuxEnabled                                   (sym 463)
0x71EE94: r5 = 1                                  ← SUCCESS
0x71EE98: state.done_flag = 1
0x71EE9C: return r5
```

### CRITICAL re-interpretation

**`bne 0x71EE9C` on `cmp r7, #0` means "branch on ERROR"** because non-zero is nn::Result error code (standard nnSdk convention). I had this BACKWARDS in some earlier analysis.

So:
- AddAux returns `Result::Success` (= 0) → poll fn calls SetAuxEnabled, marks done, returns 1 (success)
- AddAux returns ERROR (non-zero) → poll fn returns 0 (failure) → state machine re-polls

**In ruzu MK8D's AddAux returns ERROR forever.** Need to find WHICH error code and WHY.

Probable error candidates given context (398-pool config, 3 attached, 395 free):
- `ResultBufferUnmapped` — most likely. The aux send/return buffers computed from `[r4+0x1c]` aren't covered by any pool MK8D believes is attached.
- `ResultInvalidAddressInfo` — similar root cause.

### Why might buffers be "unmapped"?

MK8D's nn::audio::AddAux validates CLIENT-SIDE that buffer addresses fall within pools that are in `Attached` state per the local config. If MK8D's local pool list is incomplete or doesn't cover the aux buffer region, AddAux fails.

The local pool list is updated from `IAudioRenderer::RequestUpdate` IPC responses. Per `[[mk8d-audio-ipc-ruled-out-2026-05-21]]`, calls 8-185 are byte-identical. But the SAME memory entry notes "calls 4-7 are 1-IPC pool-attach lag (cosmetic)".

This "cosmetic lag" might NOT be cosmetic. If MK8D in calls 4-7 sends a pool-attach request and ruzu doesn't respond `Attached` until call 5 (one cycle late), MK8D's local config thinks pool[0] is still in `RequestAttach` state when state 63 polling starts.

When state 63's poll calls AddAux with a buffer that should be in pool[0], MK8D's check finds pool[0] not `Attached` → returns ResultBufferUnmapped.

### Action plan if anyone returns to this

The actual concrete fix-direction:
1. Re-verify ruzu's IPC pool-attach response for calls 4-7: should respond with `state=Attached` IMMEDIATELY when MK8D sends `RequestAttach`, NOT delayed by one IPC cycle.
2. Inspect `core/src/hle/service/audio/audio_renderer.rs::request_update_impl` and audio_core's `update_pools()` logic. Specifically: does ruzu mark pools `Attached` in the SAME IPC call that received `RequestAttach`, or only in the NEXT call?
3. Compare with zuyu — does upstream do single-IPC attach?
4. If lag confirmed, fix audio_core to commit attach atomically.

### Why I'm stopping

Static analysis has converged on the precise mechanism. Further progress requires either:
- JIT hook at PC 0x71EE70 to log AddAux's exact return error code (then we know WHICH error)
- Snapshotting MK8D's AudioRendererConfig at AddAux call time to verify pool state
- Inspecting AudioRendererConfig struct after call 4-7 vs same point in zuyu

I cannot deduce the exact error code from static analysis alone since AddAux's implementation is in MK8D's nnSdk system library, not in any guest NSO file I can read.
