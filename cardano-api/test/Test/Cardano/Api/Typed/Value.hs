{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Api.Typed.Value
  ( tests
  ) where

import           Cardano.Prelude
import           Data.Aeson
import qualified Data.Map.Strict as Map

import           Cardano.Api.Typed

import           Hedgehog (Property, discover, forAll, property, tripping, (===))
import           Test.Cardano.Api.Typed.Gen

import           Test.Tasty (TestTree)
import           Test.Tasty.Hedgehog.Group (fromGroup)

prop_roundtrip_Value_JSON :: Property
prop_roundtrip_Value_JSON =
  property $ do v <- forAll genValue
                tripping v encode eitherDecode


prop_roundtrip_Value_flatten_unflatten :: Property
prop_roundtrip_Value_flatten_unflatten =
  property $ do v <- forAll genValue
                valueFromNestedRep (valueToNestedRep v) === v

prop_roundtrip_Value_unflatten_flatten :: Property
prop_roundtrip_Value_unflatten_flatten =
    property $ do
      v <- forAll genValueNestedRep
      canonicalise v === valueToNestedRep (valueFromNestedRep v)

canonicalise :: ValueNestedRep -> ValueNestedRep
canonicalise (ValueNestedRep bundles) =
  ValueNestedRep $ mergeDuplicates bundles
 where
  mergeDuplicates :: [ValueNestedBundle] -> [ValueNestedBundle]
  mergeDuplicates bundles' =
    let cleanedBundles :: [ValueNestedBundle]
        cleanedBundles = removeEmptyNestedBundles bundles'

        folded :: (Quantity, [ValueNestedBundle])
        folded = foldl
                   (\(adaAcc, mintAcc) b ->
                     case b of
                       ValueNestedBundleAda q -> (adaAcc + q, mintAcc)
                       ValueNestedBundle pid aMap -> (adaAcc, ValueNestedBundle pid aMap : mintAcc)
                   )
                   (0,[])
                   cleanedBundles

        summedAda = fst folded
        summedMinted = snd folded

    in if summedAda == 0
       then sort summedMinted
       else ValueNestedBundleAda summedAda : sort summedMinted

  removeEmptyNestedBundles :: [ValueNestedBundle] -> [ValueNestedBundle]
  removeEmptyNestedBundles [] = []
  removeEmptyNestedBundles (vNb : rest) =
    case vNb of
      ValueNestedBundleAda v ->
        if v == 0
        then removeEmptyNestedBundles rest
        else vNb : removeEmptyNestedBundles rest
      ValueNestedBundle pid m ->
        -- All AssetNames have 0 quantity in a given PolicyId
        if all (\(_, quantity) -> quantity == 0) $ Map.toList m
        then removeEmptyNestedBundles rest
        else ValueNestedBundle pid (removeAssetWithZeroQuantity m) : removeEmptyNestedBundles rest

  removeAssetWithZeroQuantity :: Map AssetName Quantity -> Map AssetName Quantity
  removeAssetWithZeroQuantity m = Map.filter (\q -> q /= 0) m



-- -----------------------------------------------------------------------------

tests :: TestTree
tests = fromGroup $$discover

