# CLAUDE.md — Ruzu: Port de l'émulateur Zuyu (C++) vers Rust

## Objectif du projet

Porter l'émulateur Nintendo Switch **zuyu** (fork de yuzu, écrit en C++) vers **ruzu** (Rust), en respectant fidèlement l'architecture et l'implémentation originale C++. Le but final est de pouvoir exécuter :

```bash
./ruzu-cmd -g '/home/vricosti/Games/Emulators/Switch/common/roms/Mario Kart 8 Deluxe [NSP]/Mario Kart 8 Deluxe [0100152000022000][v0].nsp'
```

avec rendu OpenGL et sortie audio fonctionnels.

Pour comparer le comportement attendu, lancer la version C++ :

```bash
/home/vricosti/shared/zuyu/build/bin/zuyu-cmd -g '/home/vricosti/Games/Emulators/Switch/common/roms/Mario Kart 8 Deluxe [NSP]/Mario Kart 8 Deluxe [0100152000022000][v0].nsp'
```

---

## Règles de gestion des dépôts Git

### Code source C++ : LECTURE SEULE

**Ne JAMAIS modifier les fichiers dans `/home/vricosti/shared/zuyu/`.** Ce répertoire est la référence C++ et doit rester intact. Il sert uniquement à lire et comprendre l'implémentation originale.

### Dépôts Rust : travailler sur une branche

Pour les trois dépôts Rust suivants, **toujours créer une branche** avant de faire des modifications :

| Dépôt | Chemin |
|-------|--------|
| ruzu (émulateur) | `/home/vricosti/shared/ruzu/` |
| rdynarmic (port de dynarmic) | `/home/vricosti/shared/rdynarmic/` |
| rxbyak (port de xbyak) | `/home/vricosti/shared/rxbyak/` |

**Workflow Git obligatoire :**

```bash
cd /home/vricosti/shared/<depot>
git checkout -b claude/autonomous-porting    # Créer la branche de travail
# ... faire les modifications ...
git add -A
git commit -m "<message descriptif>"         # Commiter en local
```

**Règles strictes :**
- **Utiliser une seule branche `claude/autonomous-porting`** sur chaque dépôt — pas de branche par feature
- **Commiter régulièrement** en local avec des messages descriptifs
- **Ne JAMAIS pousser** (`git push`) — il n'y a pas de clés SSH configurées, et de toute façon on ne veut pas pousser sans review
- **Ne JAMAIS modifier la branche `main`** directement
- **Ne JAMAIS force-push ou rebase** des branches existantes

---

## Chemins du projet

| Élément | Chemin |
|---------|--------|
| Source C++ originale (lecture seule, référence) | `/home/vricosti/shared/zuyu/src/` |
| Destination Rust (implémentation) | `/home/vricosti/shared/ruzu/` |
| Binaire C++ de référence | `/home/vricosti/shared/zuyu/build/bin/zuyu-cmd` |
| ROM de test | `/home/vricosti/Games/Emulators/Switch/common/roms/Mario Kart 8 Deluxe [NSP]/Mario Kart 8 Deluxe [0100152000022000][v0].nsp` |

---

## Règle fondamentale

**Respecter l'implémentation C++ originale.** Chaque fichier C++ de zuyu doit avoir un équivalent Rust dans ruzu, avec la même logique, les mêmes structures, les mêmes algorithmes. Les adaptations idiomatiques Rust sont acceptées (Result au lieu d'exceptions, traits au lieu de classes abstraites, enums au lieu de switch/case, etc.) mais la structure globale et le découpage en modules doivent refléter le code C++.

**Ne PAS réinventer l'architecture.** Ne pas fusionner plusieurs fichiers C++ dans un seul fichier Rust. Ne pas découper un fichier C++ en plusieurs fichiers Rust sans raison technique. Le mapping doit être 1:1 autant que possible.

---

## Répertoires à porter (dans l'ordre de priorité)

Les répertoires suivants de `/home/vricosti/shared/zuyu/src/` doivent être portés. Les répertoires **exclus** sont `android/` et `yuzu/` (GUI Qt).

### Ordre de travail recommandé

1. **`common/`** → `ruzu-common/` — Types de base, utilitaires, settings, logging, compression, mémoire
2. **`core/`** → `ruzu-core/` (à créer si besoin, ou répartir dans les crates existantes selon le mapping ci-dessous)
3. **`shader_recompiler/`** → `ruzu-gpu/src/shader_recompiler/`
4. **`video_core/`** → `ruzu-gpu/`
5. **`audio_core/`** → `ruzu-audio/` (à créer)
6. **`hid_core/`** → `ruzu-hid/` (à créer)
7. **`input_common/`** → `ruzu-input/` (à créer)
8. **`network/`** → `ruzu-network/` (à créer)
9. **`web_service/`** → `ruzu-web-service/` (à créer)
10. **`frontend_common/`** → `ruzu-frontend-common/` (à créer)
11. **`dedicated_room/`** → `ruzu-dedicated-room/` (à créer)
12. **`yuzu_cmd/`** → `ruzu-cmd/` (à créer — point d'entrée CLI avec SDL)
13. **`tests/`** → tests unitaires intégrés dans chaque crate (`#[cfg(test)]`)

---

## Mapping détaillé zuyu → ruzu

### Mapping des répertoires C++ vers crates Rust

| Répertoire C++ (`zuyu/src/`) | Crate Rust (`ruzu/`) | Notes |
|------------------------------|----------------------|-------|
| `common/` | `ruzu-common/` | Types, utilitaires, settings, logging |
| `core/arm/` | `ruzu-cpu/` | CPU ARM (dynarmic bindings) |
| `core/crypto/` | `ruzu-crypto/` | Gestion des clés, AES, tickets |
| `core/hle/` | `ruzu-kernel/` + `ruzu-service/` | Kernel = kernel HLE, Service = services HOS |
| `core/loader/` | `ruzu-loader/` | Chargement NCA/NSP/NRO/NSO/XCI |
| `core/file_sys/` | `ruzu-loader/` (sous-module `vfs`) | VFS, partitions, content provider |
| `core/memory/` + `core/memory.cpp` | `ruzu-kernel/` (memory_manager) | Gestion mémoire du processus |
| `core/frontend/` | `ruzu-frontend-common/` | Interface frontend abstraite |
| `core/` (reste: core.cpp, core_timing, cpu_manager, etc.) | `ruzu-core/` (à créer) | Orchestrateur principal, timing, perf_stats |
| `video_core/` | `ruzu-gpu/` | GPU, renderers, texture/buffer cache |
| `shader_recompiler/` | `ruzu-gpu/src/shader_recompiler/` | Recompilation shaders |
| `audio_core/` | `ruzu-audio/` (à créer) | Audio DSP, input/output, rendering |
| `hid_core/` | `ruzu-hid/` (à créer) | Contrôleurs, capteurs, NFC/IR |
| `input_common/` | `ruzu-input/` (à créer) | Drivers input (SDL, etc.) |
| `network/` | `ruzu-network/` (à créer) | Multiplayer, rooms |
| `web_service/` | `ruzu-web-service/` (à créer) | Telemetry, annonces réseau |
| `frontend_common/` | `ruzu-frontend-common/` | Config frontend commune |
| `dedicated_room/` | `ruzu-dedicated-room/` (à créer) | Serveur room dédié |
| `yuzu_cmd/` | `ruzu-cmd/` (à créer) | Point d'entrée CLI SDL |

### Mapping des fichiers existants à vérifier/réorganiser

L'implémentation Rust existante dans `ruzu/` ne respecte pas toujours le mapping 1:1 avec le C++. **Avant d'implémenter de nouvelles fonctionnalités, vérifier que chaque fichier Rust existant correspond bien à son équivalent C++.** Si un fichier Rust mélange du code provenant de plusieurs fichiers C++, le découper pour restaurer le mapping 1:1.

---

## Méthodologie de portage — Par fichier

Pour chaque fichier C++ à porter, suivre cette procédure :

### Étape 1 : Analyser le fichier C++

```bash
# Lire le header C++ pour comprendre l'interface
cat /home/vricosti/shared/zuyu/src/<module>/<fichier>.h

# Lire l'implémentation
cat /home/vricosti/shared/zuyu/src/<module>/<fichier>.cpp
```

Identifier :
- Les classes, structs, enums
- Les méthodes publiques/privées
- Les dépendances (#include)
- Les patterns utilisés (RAII, observer, singleton, etc.)

### Étape 2 : Vérifier si un équivalent Rust existe déjà

```bash
# Chercher dans le code Rust existant
grep -r "NomDeLaClasse\|nom_de_la_fonction" /home/vricosti/shared/ruzu/
```

Si un fichier existe déjà, le comparer avec le C++ et le compléter/corriger.

### Étape 3 : Implémenter en Rust

- Créer le fichier au bon emplacement dans la crate Rust correspondante
- Respecter la logique et les structures du C++
- Adapter idiomatiquement pour Rust :

| C++ | Rust |
|-----|------|
| `class` avec héritage | `struct` + `trait` |
| `virtual` méthodes | `trait` objects (`dyn Trait`) ou generics |
| `std::shared_ptr<T>` | `Arc<T>` |
| `std::unique_ptr<T>` | `Box<T>` |
| `std::weak_ptr<T>` | `Weak<T>` |
| `std::optional<T>` | `Option<T>` |
| `std::variant<A,B>` | `enum` |
| `std::mutex` + lock | `Mutex<T>` (données dans le mutex) |
| Exceptions | `Result<T, E>` |
| `enum class` | `#[repr(u32)] enum` |
| `#define` / `constexpr` | `const` / `const fn` |
| Callbacks (`std::function`) | `Box<dyn Fn(...)>` ou closures génériques |
| RAII (destructeur) | `Drop` trait |
| Templates | Generics `<T>` |
| `namespace` | `mod` |
| `#include` | `use` |

### Étape 4 : Compiler et vérifier

```bash
cd /home/vricosti/shared/ruzu
cargo check 2>&1 | head -50
cargo build 2>&1 | head -50
```

Corriger les erreurs avant de passer au fichier suivant.

---

## Conventions Rust à suivre

### Nommage

- Noms de fichiers : `snake_case.rs` (un fichier par module C++)
- Structs/Enums : `PascalCase`
- Fonctions/méthodes : `snake_case`
- Constantes : `SCREAMING_SNAKE_CASE`
- Crates : `ruzu-*` (avec tiret dans Cargo.toml, underscore dans le code: `ruzu_common`)

### Structure d'une crate

```
ruzu-<nom>/
├── Cargo.toml
└── src/
    ├── lib.rs          # Racine du module, pub mod declarations
    ├── <fichier>.rs    # Un fichier par .h/.cpp C++ correspondant
    └── <sous-module>/  # Un sous-répertoire par sous-répertoire C++
        ├── mod.rs
        └── <fichier>.rs
```

### Gestion des erreurs

Chaque crate définit son propre type d'erreur :

```rust
use thiserror::Error;

#[derive(Debug, Error)]
pub enum AudioError {
    #[error("Device not found: {0}")]
    DeviceNotFound(String),
    // ...
}

pub type Result<T> = std::result::Result<T, AudioError>;
```

### Logging

Utiliser le crate `log` avec `env_logger` :

```rust
use log::{info, warn, error, debug, trace};
```

Le C++ utilise `LOG_INFO`, `LOG_WARNING`, etc. — mapper directement vers les macros `log`.

---

## Dépendances Rust recommandées

| Besoin | Crate Rust | Équivalent C++ |
|--------|-----------|----------------|
| Logging | `log` + `env_logger` | Common::Log |
| Sérialisation | `serde` + `serde_json` | nlohmann/json |
| Vulkan | `ash` | Vulkan C API |
| Fenêtrage/SDL | `sdl2` | SDL2 |
| Audio | `cpal` ou `sdl2::audio` | cubeb/SDL2 |
| Compression LZ4 | `lz4_flex` ou `lz4` | lz4 |
| Compression Zstd | `zstd` | zstd |
| Crypto AES | `aes`, `ctr`, `xts-mode` | mbedtls/openssl |
| Threading | `std::thread`, `crossbeam` | std::thread |
| Async | `tokio` (si nécessaire) | — |
| CLI args | `clap` | getopt |
| Bitfields | `bitflags` | — |
| CPU ARM JIT | `rdynarmic` | dynarmic |
| FFI (si besoin) | `bindgen` | — |
| Tests | `#[cfg(test)]` intégré | Catch2 |

---

## Points d'attention spécifiques

### 1. Dynarmic (CPU ARM) — Dépendance la plus critique

Le CPU ARM utilise dynarmic, qui a déjà été porté en Rust sous le nom **rdynarmic**. C'est la dépendance la plus importante du projet.

**Vérifications obligatoires :**
- L'implémentation de `rdynarmic` doit correspondre fidèlement à celle de dynarmic en C++. Avant d'avancer sur le reste du portage, comparer les interfaces et le comportement de rdynarmic avec le dynarmic C++ original.
- Les fichiers `ruzu-cpu/src/arm_dynarmic_32.rs` et `ruzu-cpu/src/arm_dynarmic_64.rs` doivent utiliser rdynarmic et reproduire exactement le même wiring que `zuyu/src/core/arm/dynarmic/arm_dynarmic_32.cpp` et `arm_dynarmic_64.cpp` (callbacks mémoire, configuration des coprocesseurs, gestion des exceptions, etc.).
- Toute divergence constatée entre rdynarmic et dynarmic C++ doit être signalée et corrigée dans rdynarmic avant de continuer.

### 2. Services HLE

Le répertoire `core/hle/service/` contient des dizaines de services Nintendo. Chaque service C++ (`core/hle/service/<nom>/`) doit avoir un fichier ou module correspondant dans `ruzu-service/src/`. Le framework IPC (`core/hle/ipc.h`, `core/hle/service/service.h`) est critique — le porter en premier.

### 3. GPU / Video Core

C'est le module le plus complexe. Ordre de portage recommandé :
1. `memory_manager.cpp` — Gestion mémoire GPU
2. `gpu.cpp` / `gpu_thread.cpp` — Thread GPU et command processing
3. `engines/` — Maxwell engines (3D, compute, DMA, etc.)
4. `renderer_vulkan/` — Renderer Vulkan (le plus important)
5. `buffer_cache/` et `texture_cache/` — Caches
6. `shader_cache.cpp` — Cache de shaders compilés

### 4. Shader Recompiler

Très complexe. Porter dans l'ordre :
1. `frontend/` — Décodage des instructions GPU Maxwell/Turing
2. `ir_opt/` — Passes d'optimisation IR
3. `backend/spirv/` — Génération SPIR-V pour Vulkan

### 5. File System / VFS

`core/file_sys/` implémente un VFS (Virtual File System) avec de nombreux formats Nintendo (NCA, NSP, XCI, PFS, RomFS, etc.). C'est critique pour le chargement des jeux. Vérifier que `ruzu-loader/` couvre bien tous les fichiers de `core/file_sys/` et `core/loader/`.

---

## Workflow de développement

### Avant de commencer un module

1. Lister les fichiers C++ du module : `ls /home/vricosti/shared/zuyu/src/<module>/`
2. Lister les fichiers Rust existants : `ls /home/vricosti/shared/ruzu/<crate>/src/`
3. Établir le mapping fichier par fichier
4. Identifier les fichiers manquants
5. Commencer par les fichiers sans dépendances internes, puis remonter

### Après chaque fichier porté

1. `cargo check` — doit compiler
2. Vérifier que le module `lib.rs` ou `mod.rs` exporte correctement le nouveau fichier
3. Si des tests existent dans `zuyu/src/tests/`, les porter aussi

### Progression

Maintenir un commentaire en tête de chaque fichier Rust indiquant :

```rust
//! Port de zuyu/src/<module>/<fichier>.h et zuyu/src/<module>/<fichier>.cpp
//! Status: [COMPLET | EN COURS | STUB]
//! Dernière synchro: <date>
```

---

## Ce qu'il ne faut PAS faire

- **Ne pas inventer de nouvelles abstractions** qui n'existent pas dans le C++
- **Ne pas fusionner des fichiers C++ distincts** dans un seul fichier Rust
- **Ne pas ignorer les fichiers "ennuyeux"** (constantes, types, enums) — ils sont nécessaires
- **Ne pas utiliser `todo!()` ou `unimplemented!()` partout** — implémenter réellement la logique
- **Ne pas skipper les méthodes** même si elles semblent mineures — elles peuvent être appelées ailleurs
- **Ne pas changer les algorithmes** sauf si le C++ utilise un pattern qui n'existe pas en Rust
- **Ne pas oublier `pub`** sur les items qui doivent être accessibles par d'autres crates
- **Ne pas oublier de déclarer les modules** dans `lib.rs` / `mod.rs`

---

## Vérification finale

Le projet est considéré fonctionnel quand :

1. `cargo build --release` compile sans erreur
2. `./target/release/ruzu-cmd -g '<chemin_rom>'` lance le jeu
3. Le rendu OpenGL affiche les frames correctement
4. Le son est audible
5. Le comportement est comparable à `zuyu-cmd` sur la même ROM