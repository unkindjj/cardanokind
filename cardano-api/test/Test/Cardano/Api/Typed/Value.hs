{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Api.Typed.Value
  ( tests
  ) where

import           Cardano.Prelude
import           Data.Aeson

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

-- Note when going from ValueNestedRep -> Value (via fromValueNestedRep)
-- we merge maps, which combines all common keys. Therefore
-- we must generate an ValueNestedRep with no duplicate values.
-- Remember that Maps cannot have duplicate keys and therefore
-- we will never go from Value -> ValueNestedRep (via toValueNestedRep) to a
-- ValueNestedRep with duplicate values.
prop_roundtrip_Value_unflatten_flatten :: Property
prop_roundtrip_Value_unflatten_flatten =
    property $ do
      v <- forAll genValueNestedRep
      let v' = valueToNestedRep (valueFromNestedRep v)
      v `equiv` v'
  where
    equiv (ValueNestedRep a) (ValueNestedRep b) = sort a === sort b

-- -----------------------------------------------------------------------------

tests :: TestTree
tests = fromGroup $$discover

