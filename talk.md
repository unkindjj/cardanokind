
# Overview

Chairman tests:

  [ chairman ] <-> [ socket 1 ]    [ socket 2 ]    [ socket 3 ]
        |               ^               ^               ^
        |               |               |               |
        |               v               v               v
        |          [  node 1  ]    [  node 2  ]    [  node 3  ]
        |               |               |               |
        |               |               |               |
        v               v               v               v
  [ stdout c ]     [ stdout 1 ]    [ stdout 1 ]    [ stdout 1 ]

# Running

$ cabal build cardano-node cardano-cli cardano-node-chairman
$ cabal test cardano-node-chairman --test-show-details=direct

# Design considerations

* Cross-platform (Linux, MacOS, Windows)
* Build system portable (nix, cabal)
* Easy to debug (locally, in CI)
* Test isolation (locally between tests, multiple runs in CI)

# Features

* More annotations
* Annotating functions
* Missing annotation on exception
  * Caused by un-intercepted exceptions
  * Laziness causing un-intercepted exceptions
* Laziness triggering
* Workspaces
* Effectful assertions
* Deadline assertions
* Annotate on cleanup (not yet available; https://github.com/hedgehogqa/haskell-hedgehog/issues/399)
* Resource cleanup (see Integration/propertyOnce)
* Annotations in all the functions
* Launching executables with the right binary
  * Working across nix/straight cabal
  * cabal exec (not good, dirty output, triggers configure)
  * plan.json
  * environment variable

