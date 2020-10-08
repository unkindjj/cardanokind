{-# LANGUAGE CPP #-}

#if !defined(mingw32_HOST_OS)
#define UNIX
#endif

import           Cardano.Prelude

import           Hedgehog.Main (defaultMain)
#ifdef UNIX
import qualified Test.Cli.FilePermissions
#endif
import qualified Test.Cli.ITN
import qualified Test.Cli.Pioneers.Exercise1
import qualified Test.Cli.Pioneers.Exercise2
import qualified Test.Cli.Pioneers.Exercise3
import qualified Test.Cli.Pioneers.Exercise4

main :: IO ()
main =
  defaultMain
#ifdef UNIX
    [ Test.Cli.FilePermissions.tests
    , Test.Cli.ITN.tests
    , Test.Cli.Pioneers.Exercise1.tests
    , Test.Cli.Pioneers.Exercise2.tests
    , Test.Cli.Pioneers.Exercise3.tests
    , Test.Cli.Pioneers.Exercise4.tests
    ]
#else
    [ Test.Cli.ITN.tests
    , Test.Cli.Pioneers.Exercise1.tests
    , Test.Cli.Pioneers.Exercise2.tests
    , Test.Cli.Pioneers.Exercise3.tests
    , Test.Cli.Pioneers.Exercise4.tests
    ]
#endif
