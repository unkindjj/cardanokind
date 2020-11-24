{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Currency values
--
module Cardano.Api.Value
  ( Lovelace(..)

    -- * Multi-asset values
  , Quantity(..)
  , PolicyId(..)
  , AssetName(..)
  , AssetId(..)
  , Value
  , selectAsset
  , valueFromList
  , valueToList
  , filterValue
  , negateValue

    -- ** Ada \/ Lovelace specifically
  , quantityToLovelace
  , lovelaceToQuantity
  , selectLovelace
  , lovelaceToValue

    -- ** Alternative nested representation
  , ValueNestedRep(..)
  , ValueNestedBundle(..)
  , valueToNestedRep
  , valueFromNestedRep

    -- * Era-dependent use of multi-assert values
  , MintValue(..)
  , TxOutValue(..)
  , AdaOnlyInEra(..)
  , MultiAssetInEra(..)

    -- * Internal conversion functions
  , toShelleyLovelace
  ) where

import           Prelude

import           Data.Aeson hiding (Value)
import qualified Data.Aeson as Aeson
import           Data.Aeson.Types (Parser, toJSONKeyText)
import           Data.ByteString (ByteString)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Merge.Strict as Map
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.String (IsString)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import qualified Shelley.Spec.Ledger.Coin as Shelley

import           Cardano.Api.Eras
import           Cardano.Api.Script
import           Cardano.Api.SerialiseRaw (deserialiseFromRawBytesHex, serialiseToRawBytesHex)


-- ----------------------------------------------------------------------------
-- Lovelace
--

newtype Lovelace = Lovelace Integer
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum, Num, ToJSON, FromJSON)

instance Semigroup Lovelace where
  Lovelace a <> Lovelace b = Lovelace (a + b)

instance Monoid Lovelace where
  mempty = Lovelace 0

toShelleyLovelace :: Lovelace -> Shelley.Coin
toShelleyLovelace (Lovelace l) = Shelley.Coin l
--TODO: validate bounds


-- ----------------------------------------------------------------------------
-- Multi asset Value
--

newtype Quantity = Quantity Integer
  deriving newtype (Eq, Ord, Num, Show, ToJSON, FromJSON)

instance Semigroup Quantity where
  Quantity a <> Quantity b = Quantity (a + b)

instance Monoid Quantity where
  mempty = Quantity 0

lovelaceToQuantity :: Lovelace -> Quantity
lovelaceToQuantity (Lovelace x) = Quantity x

quantityToLovelace :: Quantity -> Lovelace
quantityToLovelace (Quantity x) = Lovelace x

newtype PolicyId = PolicyId ScriptHash
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString)

newtype AssetName = AssetName ByteString
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString)

instance ToJSON AssetName where
  toJSON (AssetName an) = Aeson.String $ Text.decodeUtf8 an

instance FromJSON AssetName where
  parseJSON = withText "AssetName" (return . AssetName . Text.encodeUtf8)

instance ToJSONKey AssetName where
  toJSONKey = toJSONKeyText (\(AssetName asset) -> Text.decodeUtf8 asset)

instance FromJSONKey AssetName where
  fromJSONKey = FromJSONKeyText (AssetName . Text.encodeUtf8)


data AssetId = AdaAssetId
             | AssetId !PolicyId !AssetName
  deriving (Eq, Ord, Show)

newtype Value = Value (Map AssetId Quantity)
  deriving Eq

instance Show Value where
  showsPrec d v = showParen (d > 10) $
    showString "valueFromList " . shows (valueToList v)

instance Semigroup Value where
  Value a <> Value b = Value (mergeAssetMaps a b)

instance Monoid Value where
  mempty = Value Map.empty

{-# NOINLINE mergeAssetMaps #-} -- as per advice in Data.Map.Merge docs
mergeAssetMaps :: Map AssetId Quantity
               -> Map AssetId Quantity
               -> Map AssetId Quantity
mergeAssetMaps =
    Map.merge
      Map.preserveMissing
      Map.preserveMissing
      (Map.zipWithMaybeMatched mergeQuantity)
  where
    mergeQuantity :: AssetId -> Quantity -> Quantity -> Maybe Quantity
    mergeQuantity _k a b =
      case a <> b of
        Quantity 0 -> Nothing
        c          -> Just c

instance ToJSON Value where
  toJSON = toJSON . valueToNestedRep

instance FromJSON Value where
  parseJSON v = valueFromNestedRep <$> parseJSON v


selectAsset :: Value -> (AssetId -> Quantity)
selectAsset (Value m) a = Map.findWithDefault mempty a m

valueFromList :: [(AssetId, Quantity)] -> Value
valueFromList = Value
              . Map.filter (/= 0)
              . Map.fromListWith (<>)

valueToList :: Value -> [(AssetId, Quantity)]
valueToList (Value m) = Map.toList m

-- | This lets you write @a - b@ as @a <> negateValue b@.
--
negateValue :: Value -> Value
negateValue (Value m) = Value (Map.map negate m)

filterValue :: (AssetId -> Bool) -> Value -> Value
filterValue p (Value m) = Value (Map.filterWithKey (\k _v -> p k) m)

selectLovelace :: Value -> Lovelace
selectLovelace = quantityToLovelace . flip selectAsset AdaAssetId

lovelaceToValue :: Lovelace -> Value
lovelaceToValue = Value . Map.singleton AdaAssetId . lovelaceToQuantity


-- ----------------------------------------------------------------------------
-- An alternative nested representation
--

-- | An alternative nested representation for 'Value' that groups assets that
-- share a 'PolicyId'.
--
newtype ValueNestedRep = ValueNestedRep [ValueNestedBundle]
  deriving (Eq, Ord, Show)

-- | A bundle within a 'ValueNestedRep' for a single 'PolicyId', or for the
-- special case of ada.
--
data ValueNestedBundle = ValueNestedBundle PolicyId (Map AssetName Quantity)
                       | ValueNestedBundleAda Quantity
  deriving (Eq, Ord, Show)


valueToNestedRep :: Value -> ValueNestedRep
valueToNestedRep v =
    -- unflatten all the non-ada assets, and add ada separately
    ValueNestedRep $
        [ ValueNestedBundleAda q | let q = selectAsset v AdaAssetId, q /= 0 ]
     ++ [ ValueNestedBundle pId qs | (pId, qs) <- Map.toList nonAdaAssets ]
  where
    nonAdaAssets :: Map PolicyId (Map AssetName Quantity)
    nonAdaAssets =
      Map.fromListWith (Map.unionWith (<>))
        [ (pId, Map.singleton aName q)
        | (AssetId pId aName, q) <- valueToList v ]

valueFromNestedRep :: ValueNestedRep -> Value
valueFromNestedRep (ValueNestedRep bundles) =
    valueFromList
      [ (aId, q)
      | bundle   <- bundles
      , (aId, q) <- case bundle of
                      ValueNestedBundleAda  q  -> [ (AdaAssetId, q) ]
                      ValueNestedBundle pId qs -> [ (AssetId pId aName, q)
                                                  | (aName, q) <- Map.toList qs ]
      ]

instance ToJSON ValueNestedRep where
  toJSON (ValueNestedRep bundles) = object $ map toPair bundles
    where
     toPair :: ValueNestedBundle -> (Text, Aeson.Value)
     toPair (ValueNestedBundleAda q) = ("lovelace", toJSON q)
     toPair (ValueNestedBundle pid assets) = (renderPolicyId pid, toJSON assets)

     renderPolicyId :: PolicyId -> Text
     renderPolicyId (PolicyId sh) = Text.decodeUtf8 (serialiseToRawBytesHex sh)

instance FromJSON ValueNestedRep where
  parseJSON =
      withObject "ValueNestedRep" $ \obj ->
        ValueNestedRep <$> sequenceA [ parsePid keyValTuple
                                   | keyValTuple <- HashMap.toList obj ]
    where
      parsePid :: (Text, Aeson.Value) -> Parser ValueNestedBundle
      parsePid ("lovelace", q) = ValueNestedBundleAda <$> parseJSON q
      parsePid (pid, q) =
        case deserialiseFromRawBytesHex AsScriptHash (Text.encodeUtf8 pid) of
          Just sHash -> ValueNestedBundle (PolicyId sHash) <$> parseJSON q
          Nothing -> fail $ "Failure when deserialising PolicyId: "
                         <> Text.unpack pid


-- ----------------------------------------------------------------------------
-- Era-dependent use of multi-assert values
--

data MintValue era where

     MintNothing :: MintValue era

     MintValue   :: MultiAssetInEra era -> Value -> MintValue era

deriving instance Eq   (MintValue era)
deriving instance Show (MintValue era)


data TxOutValue era where

     TxOutAdaOnly :: AdaOnlyInEra era -> Lovelace -> TxOutValue era

     TxOutValue   :: MultiAssetInEra era -> Value -> TxOutValue era

deriving instance Eq   (TxOutValue era)
deriving instance Show (TxOutValue era)


-- | Representation of whether only ada transactions are supported in a
-- particular era.
--
data AdaOnlyInEra era where

     AdaOnlyInByronEra   :: AdaOnlyInEra ByronEra
     AdaOnlyInShelleyEra :: AdaOnlyInEra ShelleyEra
     AdaOnlyInAllegraEra :: AdaOnlyInEra AllegraEra

deriving instance Eq   (AdaOnlyInEra era)
deriving instance Show (AdaOnlyInEra era)

-- | Representation of whether multi-asset transactions are supported in a
-- particular era.
--
data MultiAssetInEra era where

     -- | Multi-asset transactions are supported in the 'Mary' era.
     MultiAssetInMaryEra :: MultiAssetInEra MaryEra

deriving instance Eq   (MultiAssetInEra era)
deriving instance Show (MultiAssetInEra era)

