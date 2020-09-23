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

spec :: Spec
spec = describe "Spec" $ do
  describe "Exceptions & annotations" $ do
    it "Testing IO" $ requireTest $ do
      H.annotate "My annotation"
      x <- liftIO $ IO.readFile "missing-file-1"
      x === "expected value"
      return ()
    it "Testing IO" $ requireTest $ do
      H.annotate "My annotation"
      x <- H.evalIO $ IO.readFile "missing-file-2"
      x === "expected value"
      return ()
    it "Testing IO" $ requireTest $ do
      H.annotate "My annotation"
      x <- H.evalIO $ IO.unsafeInterleaveIO $ IO.readFile "missing-file-3"
      x === "expected value"
      return ()
    it "Testing IO" $ requireTest $ do
      H.annotate "My annotation"
      x <- H.evalIO $ IO.unsafeInterleaveIO $ IO.readFile "missing-file-4"
      unless (x == "expected value") H.failure
    it "Testing IO" $ requireTest $ do
      H.annotate "My annotation"
      base <- H.getProjectBase
      x <- H.evalIO $ unlines . take 1 . lines <$> IO.readFile (base </> "NOTICE")
      x === "expected value"
      return ()
    it "Testing IO" $ requireTest $ do
      H.annotate "My annotation"
      x <- H.readFile "hello"
      x === "expected value"
      return ()
    it "Testing IO" $ requireTest $ do
      void $ H.listDirectory "."
      H.failure
  describe "Workspaces" $ do
    it "Workspace" $ requireTest $ H.moduleWorkspace "tmp" $ \tempDir -> do
      H.writeFile (tempDir </> "file") "contents"
      H.failure
