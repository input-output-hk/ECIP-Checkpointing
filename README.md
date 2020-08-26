# checkpointing_code_plus_ecip

// TODO: Add information related to the ECIP proposal.

## Specification

- [OBFT-Checkpointing-Specification](https://github.com/input-output-hk/OBFT-Checkpointing-Specification)

## Code

* Build the Project

To build =blockchain-checkpoint-node=, run

#+begin_src sh
» nix build -f . blockchain-checkpoint-node.components.exes.blockchain-checkpoint-node
#+end_src

* Interactive Dev Env

To get a =nix-shell= populated with the appropriate haskell libs, and
incrementally build the project, run:

#+begin_src sh
» nix-shell
» cabal new-build blockchain-checkpoint-node
#+end_src

You can use =ghcid= (included in the shell) to check for errors on
real time:

#+begin_src sh
» ghcid
#+end_src

A =hoogle= database has been populated with the project's
dependencies. You can query it using the =hoogle= CLI:

#+begin_src sh
» hoogle Protocol
#+end_src

* Update Niv Dependencies

If you want to update a niv dependency (contained in =nix/sources.json=), use:

#+begin_src sh
» nix-shell
» niv update $dep_name
#+end_src
