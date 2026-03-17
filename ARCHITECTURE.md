# Architecture

Ce document explique l'architecture actuelle du lifecycle thread/process et du dispatch CPU dans `ruzu`, en particulier la logique récente autour de la terminaison coopérative.

Il ne remplace pas l'upstream C++ comme source de vérité. Il décrit:

- ce que fait upstream conceptuellement
- comment le port Rust l'approxime aujourd'hui
- où vivent les ownership boundaries dans le code Rust
- quelles différences restent assumées ou encore temporaires

## Portee

Les fichiers principaux concernés sont:

- [`core/src/hle/kernel/k_thread.rs`](core/src/hle/kernel/k_thread.rs)
- [`core/src/hle/kernel/k_process.rs`](core/src/hle/kernel/k_process.rs)
- [`core/src/hle/kernel/k_scheduler.rs`](core/src/hle/kernel/k_scheduler.rs)
- [`core/src/hle/kernel/k_worker_task_manager.rs`](core/src/hle/kernel/k_worker_task_manager.rs)
- [`core/src/hle/kernel/svc_dispatch.rs`](core/src/hle/kernel/svc_dispatch.rs)
- [`core/src/hle/kernel/svc/svc_process.rs`](core/src/hle/kernel/svc/svc_process.rs)
- [`core/src/hle/kernel/svc/svc_thread.rs`](core/src/hle/kernel/svc/svc_thread.rs)
- [`core/src/hle/kernel/physical_core.rs`](core/src/hle/kernel/physical_core.rs)

## Vue D'Ensemble

Le modèle upstream n'est pas un runtime Rust coopératif. La terminaison d'un thread ou d'un process y est observée naturellement via:

- les retours de SVC
- les retours d'interruption ou de timer tick
- la libération du scheduler lock
- les chemins de wait/signal du noyau

Dans `ruzu`, la même sémantique n'existe pas encore partout. Le port s'appuie donc sur trois frontières explicites où une terminaison demandée peut être drainée de manière centralisée:

1. retour de `Yield` / sélection scheduler
2. retour de chaque SVC dans `svc_dispatch`
3. retour du quantum CPU dans `physical_core`

Le but est de couvrir les trois grandes familles de réentrée kernel:

- réentrée volontaire via SVC
- réentrée coopérative via scheduler
- réentrée implicite via fin de quantum CPU

## Modele Upstream

### Thread

Upstream sépare plusieurs étapes:

- `RequestTerminate()`: marque l'intention
- `Exit()`: lance le chemin de sortie du thread courant
- `Terminate()`: force ou attend la terminaison d'un autre thread
- `FinishTermination()`: finalise l'état, signale l'objet de synchro, nettoie les ressources

Le point important est l'ordre:

- la demande de terminaison ne détruit pas immédiatement le thread
- le thread est observé à une frontière d'ordonnancement ou de retour noyau
- la terminaison complète n'est visible qu'après `FinishTermination()`

### Process

Upstream suit aussi plusieurs phases:

- `Exit()` ou `Terminate()`
- `StartTermination()`
- `TerminateChildren(...)`
- `DoWorkerTaskImpl()`
- `FinishTermination()`

`TerminateChildren(...)` fait deux passes:

1. demander la terminaison des enfants
2. terminer ensuite les enfants restants

`DoWorkerTaskImpl()` existe pour sortir du chemin appelant immédiat et terminer le cleanup dans le bon contexte.

## Adaptation Rust Actuelle

### Ownership Rust

Le port utilise:

- `Arc<Mutex<KThread>>`
- `Arc<Mutex<KProcess>>`
- `Arc<Mutex<KScheduler>>`

Cette forme impose une contrainte absente du C++ upstream: beaucoup d'opérations doivent éviter de réentrer pendant qu'un `MutexGuard` est encore actif.

C'est la raison principale de plusieurs helpers Rust-side:

- `KProcess::exit_with_current_thread(...)`
- `KThread::terminate_thread(&Arc<Mutex<KThread>>)`
- la queue globale `KWorkerTaskManager`

Ces helpers ne cherchent pas à redessiner l'architecture. Ils existent pour préserver autant que possible l'ownership upstream malgré les contraintes de verrouillage Rust.

### Self References

`KThread` et `KProcess` ont maintenant une self-reference weak/upgradeable côté Rust. Cela permet:

- à `KThread::exit()` de se queue lui-même comme worker task
- à `KProcess::exit()` ou `terminate()` de reprogrammer `do_worker_task_impl()`

Sans cela, le lifecycle restait trop inline et s'éloignait du modèle upstream.

### KWorkerTaskManager

`KWorkerTaskManager` est maintenant une vraie queue asynchrone globale.

Son rôle actuel:

- exécuter les fins de terminaison hors du call path immédiat
- rapprocher `KThread::exit()` et `KProcess::exit()` du modèle upstream
- fournir `wait_for_idle()` et `wait_for_global_idle()` pour les tests

Ce que cela change:

- la fin de vie n'est plus forcée inline partout
- `do_worker_task_impl()` redevient un owner utile du cleanup process

## Frontieres De Drain

Le terme "drain" désigne ici: constater qu'un thread courant a `termination_requested` et déclencher son `exit()` complet à une frontière centrale de retour noyau/runtime.

### 1. Frontiere Scheduler

Fichier owner:

- [`core/src/hle/kernel/k_scheduler.rs`](core/src/hle/kernel/k_scheduler.rs)

Le helper local est:

- `exit_thread_if_termination_requested(...)`

Il est utilisé aujourd'hui dans:

- `yield_without_core_migration(...)`
- `wait_for_next_thread(...)`
- `select_next_thread_id(...)` pour les tests

Ce point couvre les chemins coopératifs où le thread courant repasse par le scheduler.

### 2. Frontiere SVC

Fichier owner:

- [`core/src/hle/kernel/svc_dispatch.rs`](core/src/hle/kernel/svc_dispatch.rs)

Le helper local est:

- `drain_current_thread_termination(ctx)`

Il est appelé une seule fois après `call32(...)` ou `call64(...)`, dans `call(...)`.

C'est volontairement centralisé:

- pas de duplication dans chaque handler SVC
- ownership conservé dans le vrai owner du retour SVC
- meilleure parité conceptuelle avec le moment où upstream relâche le scheduler lock après le handler

### 3. Frontiere Quantum CPU

Fichier owner:

- [`core/src/hle/kernel/physical_core.rs`](core/src/hle/kernel/physical_core.rs)

Le helper local est:

- `drain_current_thread_termination(...)`

Il est appelé dans le chemin `PhysicalCoreExecutionEvent::Halted(...)`, c'est-à-dire au retour du dispatch CPU après un quantum d'exécution guest sans SVC.

Dans le modèle coopératif de `ruzu`, c'est l'équivalent le plus proche du retour d'IRQ/timer tick upstream.

Après le drain:

- `exit()` est déclenché
- le scheduler reçoit `request_schedule()`
- `handoff_after_svc(...)` est réutilisé comme frontière de bascule vers le prochain runnable

Le nom du helper de handoff n'est pas encore idéal, mais l'ownership est correct: le basculement CPU suivant reste dans `physical_core`.

## Lifecycle Actuel

### KThread

Chemin simplifié aujourd'hui:

1. `request_terminate()` pose l'intention
2. une frontière de drain observe cette intention
3. `exit()` queue le worker task thread
4. le worker fait `finish_termination()`
5. le thread devient `TERMINATED` et `signaled`

Différence importante avec upstream:

- tous les chemins ne passent pas encore par un vrai `KSynchronizationObject::Wait()` upstream-compatible
- certains cas restent protégés pour éviter les deadlocks sous `MutexGuard`

### KProcess

Chemin simplifié aujourd'hui:

1. `exit()` ou `terminate()`
2. `start_termination(...)`
3. `terminate_children(...)`
4. `KWorkerTaskManager` exécute `do_worker_task_impl()`
5. `finish_termination()`

`terminate_children(...)` respecte maintenant l'ownership upstream:

- première passe: `request_terminate()`
- seconde passe: terminaison effective des enfants quand c'est sûr dans le runtime coopératif

La seconde passe reste plus conservatrice qu'upstream, parce qu'un thread Rust-side peut encore dépendre d'une future frontière de dispatch pour s'auto-drainer.

## Pourquoi Rust Force Ces Differences

Les écarts principaux ne viennent pas d'un choix de redesign, mais de contraintes mécaniques.

### 1. MutexGuard Et Reentrance

En C++, beaucoup de transitions se font en gardant plus de liberté sur l'ordre réel des appels.

En Rust, avec `Arc<Mutex<_>>`, il faut éviter:

- de rappeler une méthode qui reprend le même mutex indirectement
- de bloquer dans `wait()` alors qu'un guard critique est encore vivant
- de faire du cleanup profond depuis un contexte qui possède déjà l'objet parent

Conséquence:

- certains appels upstream doivent devenir des helpers d'orchestration
- certaines étapes doivent être décalées vers le worker task manager

### 2. Runtime Cooperatif

Le runtime actuel n'interrompt pas naturellement un thread guest n'importe où comme le fait upstream via ses frontières d'ordonnancement et d'interruptions.

Conséquence:

- il faut introduire des points explicites de drain
- un thread en "CPU pur" ne peut être observé qu'à la fin d'un quantum

### 3. Attente De Synchronisation Incomplete

Tant que `KSynchronizationObject::Wait()` et quelques morceaux scheduler/signal ne sont pas totalement au niveau upstream, certaines opérations qui seraient bloquantes en C++ doivent rester prudentes en Rust.

Conséquence:

- `terminate_thread(...)` existe en plus de `terminate(&mut self)`
- `terminate_children(...)` ne bloque pas encore agressivement sur tous les états

## Ce Qui Est Fidele

Les points suivants sont maintenant proches de l'intention upstream:

- ownership du cleanup process dans `k_process.rs`
- ownership du retour SVC dans `svc_dispatch.rs`
- ownership du retour de quantum CPU dans `physical_core.rs`
- séparation entre demande de terminaison et terminaison effective
- usage d'un worker task manager au lieu d'un cleanup totalement inline

## Ce Qui Reste Different

Les différences importantes encore connues sont:

- le drain reste explicite et coopératif, pas implicite partout comme upstream
- `handoff_after_svc(...)` est réutilisé aussi après la frontière CPU, ce qui est fonctionnel mais pas encore parfaitement nommé
- certains waits/terminations sont encore plus conservateurs que l'upstream pour éviter un deadlock Rust-side
- `KThread::Terminate()` et certaines attentes de synchro ne sont pas encore entièrement au niveau de sémantique C++
- toutes les frontières possibles de retour au noyau ne sont pas encore câblées

## Invariants Utiles Pour Les Changements Futurs

Si tu modifies cette zone, garder ces invariants:

- le drain doit rester centralisé par frontière majeure, pas dupliqué dans chaque handler
- le fichier owner doit rester le vrai owner conceptuel upstream du point de retour
- `request_terminate()` ne doit pas devenir un `exit()` immédiat
- `finish_termination()` doit rester la transition qui rend la terminaison visible
- le worker task manager doit rester le point de décalage pour le cleanup asynchrone
- ne pas réintroduire de lifecycle inline qui casse l'ownership `k_process.rs` ou `k_thread.rs`

## Frontieres Cibles Restantes

Si d'autres écarts doivent être fermés plus tard, il faut chercher de nouveaux hooks seulement sur des frontières communes de réentrée:

- retour d'exception si distinct du chemin `Halted(...)`
- autres sorties communes du dispatch CPU
- futurs chemins de wait/signal quand `KSynchronizationObject::Wait()` sera plus complet

Il ne faut pas:

- instrumenter chaque SVC individuellement
- disperser la logique de drain dans des helpers génériques sans owner clair
- faire porter ce comportement à des sous-systèmes mémoire ou IPC qui ne sont pas owners du dispatch

## Tests De Reference

Les tests qui documentent le comportement actuel sont:

- `call_drains_current_thread_termination_on_svc_return`
- `yield_exits_current_thread_when_termination_was_requested`
- `run_loop_drains_current_thread_termination_on_halt_boundary`

Ils couvrent respectivement:

- la frontière SVC
- la frontière scheduler coopérative
- la frontière de quantum CPU

## Resume

Le port Rust ne reproduit pas encore toute la mécanique implicite du kernel upstream, mais il a maintenant trois points de drain centraux qui couvrent les retours majeurs:

- scheduler
- SVC
- fin de quantum CPU

La différence essentielle avec Rust est que ces frontières doivent être rendues explicites pour rester sûres sous `Arc<Mutex<_>>` et runtime coopératif, tout en gardant l'ownership des comportements dans les mêmes fichiers conceptuels que l'upstream.
