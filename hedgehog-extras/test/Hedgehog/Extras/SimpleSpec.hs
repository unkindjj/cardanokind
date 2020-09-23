{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}

module Hedgehog.Extras.SimpleSpec
  ( spec
  ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Semigroup
import           HaskellWorks.Hspec.Hedgehog
import           Hedgehog
import           Prelude
import           System.FilePath ((</>))
import           Test.Hspec

import qualified Hedgehog as H
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H
import qualified Hedgehog.Extras.Test.Process as H
import qualified System.IO as IO
import qualified System.IO.Unsafe as IO

{- HLINT ignore "Reduce duplication" -}

via :: (MonadTest m, Show a, HasCallStack) => a -> m a
via a = do
  void $ H.annotate (show a)
  return a

spec :: Spec
spec = describe "Spec" $ do
  describe "Annotations" $ do
    it "Simple" $ requireTest $ do
      H.annotate "data"
      H.failure
    it "Simple returning value" $ requireTest $ do
      x <- forAll $ pure "data"
      x === "data"
      H.failure
    it "Simple returning value" $ requireTest $ do
      H.note_ "simple"
      H.noteShow_ @Int 1
      _ <- H.note "simple"
      _ <- H.noteShow @String "simple"
      _ <- H.noteShow @Int 1
      _ <- H.noteShowIO $ take 40 <$> IO.readFile "../cabal.project"
      _ <- H.noteShowM $ take 40 <$> H.evalIO (IO.readFile "../cabal.project")
      H.noteEach_ ["1", "2", "3"]
      H.noteShowEach_ ["1", "2", "3"]
      H.failure
  describe "Exceptions & annotations" $ do
    xit "Using liftIO unhelpfully disables annotations" $ requireTest $ do
      H.annotate "My annotation"
      x <- liftIO $ IO.readFile "missing-file-1"
      x === "expected value"
      return ()
    xit "Using evalIO is preserves annotations" $ requireTest $ do
      H.annotate "My annotation"
      x <- H.evalIO $ IO.readFile "missing-file-2"
      x === "expected value"
      return ()
    xit "Using Lazy IO seems fine" $ requireTest $ do
      H.annotate "My annotation"
      x <- H.evalIO $ IO.unsafeInterleaveIO $ IO.readFile "missing-file-3"
      x === "expected value"
      return ()
    xit "But evaluating lazy thunks can disable annotations too" $ requireTest $ do
      H.annotate "My annotation"
      x <- H.evalIO $ IO.unsafeInterleaveIO $ IO.readFile "missing-file-4"
      unless (x == "expected value") H.failure
    xit "Island annotation" $ requireTest $ do
      x <- via True
      x === False
  describe "Workspaces" $ do
    xit "Workspace" $ requireTest $ H.moduleWorkspace "tmp" $ \tempDir -> do
      H.writeFile (tempDir </> "file") "contents"
      H.failure
  describe "Useful IO functions" $ do
    xit "Testing IO" $ requireTest $ do
      H.annotate "My annotation"
      base <- H.getProjectBase
      x <- H.evalIO $ unlines . take 1 . lines <$> IO.readFile (base </> "NOTICE")
      x === "expected value"
      return ()
    xit "Testing IO" $ requireTest $ do
      H.annotate "My annotation"
      x <- H.readFile "hello"
      x === "expected value"
      return ()
    xit "Testing IO" $ requireTest $ do
      void $ H.listDirectory "."
      H.failure
