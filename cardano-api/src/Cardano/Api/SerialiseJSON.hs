
-- | JSON serialisation
--
module Cardano.Api.SerialiseJSON
  ( serialiseToJSON
  , ToJSON(..)
  , deserialiseFromJSON
  , prettyPrintJSON
  , FromJSON(..)
  , JsonDecodeError(..)
  ) where

import           Prelude

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS

import           Cardano.Api.HasTypeProxy
import           Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as Aeson
import           Data.Aeson.Encode.Pretty (encodePretty)


newtype JsonDecodeError = JsonDecodeError String

serialiseToJSON :: ToJSON a => a -> ByteString
serialiseToJSON = LBS.toStrict . Aeson.encode

prettyPrintJSON :: ToJSON a => a -> ByteString
prettyPrintJSON = LBS.toStrict . encodePretty

deserialiseFromJSON :: FromJSON a
                    => AsType a
                    -> ByteString
                    -> Either JsonDecodeError a
deserialiseFromJSON _proxy = either (Left . JsonDecodeError) Right
                           . Aeson.eitherDecodeStrict'

