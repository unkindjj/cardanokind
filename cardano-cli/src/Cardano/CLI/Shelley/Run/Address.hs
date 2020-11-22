{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.Shelley.Run.Address
  ( ShelleyAddressCmdError
  , renderShelleyAddressCmdError
  , runAddressCmd
  ) where

import           Cardano.Prelude hiding (putStrLn)
import           Prelude (String)

import           Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, hoistEither,
                     newExceptT)

import           Cardano.Api.Typed

import           Cardano.CLI.Shelley.Key (InputDecodeError, VerificationKeyOrFile,
                     VerificationKeyTextOrFile, VerificationKeyTextOrFileError (..),
                     readVerificationKeyOrFile, readVerificationKeyTextOrFileAnyOf,
                     renderVerificationKeyTextOrFileError)
import           Cardano.CLI.Shelley.Parsers (AddressCmd (..), AddressKeyType (..), OutputFile (..))
import           Cardano.CLI.Shelley.Run.Address.Info (ShelleyAddressInfoError, runAddressInfo)
import           Cardano.CLI.Types

data ShelleyAddressCmdError
  = ShelleyAddressCmdAddressInfoError !ShelleyAddressInfoError
  | ShelleyAddressCmdAesonDecodeError !FilePath !Text
  | ShelleyAddressCmdReadKeyFileError !(FileError InputDecodeError)
  | ShelleyAddressCmdReadFileException !(FileError ())
  | ShelleyAddressCmdVerificationKeyTextOrFileError !VerificationKeyTextOrFileError
  | ShelleyAddressCmdWriteFileError !(FileError ())
  deriving Show

renderShelleyAddressCmdError :: ShelleyAddressCmdError -> Text
renderShelleyAddressCmdError err =
  case err of
    ShelleyAddressCmdAddressInfoError addrInfoErr ->
      Text.pack (displayError addrInfoErr)
    ShelleyAddressCmdReadKeyFileError fileErr ->
      Text.pack (displayError fileErr)
    ShelleyAddressCmdVerificationKeyTextOrFileError vkTextOrFileErr ->
      renderVerificationKeyTextOrFileError vkTextOrFileErr
    ShelleyAddressCmdWriteFileError fileErr -> Text.pack (displayError fileErr)
    ShelleyAddressCmdAesonDecodeError fp decErr -> "Error decoding multisignature JSON object at: "
                                                   <> Text.pack fp <> " Error: " <> decErr
    ShelleyAddressCmdReadFileException fileErr -> Text.pack (displayError fileErr)

runAddressCmd :: AddressCmd -> ExceptT ShelleyAddressCmdError IO ()
runAddressCmd cmd =
  case cmd of
    AddressKeyGen kt vkf skf -> runAddressKeyGen kt vkf skf
    AddressKeyHash vkf mOFp -> runAddressKeyHash vkf mOFp
    AddressBuild payVk stkVk nw mOutFp -> runAddressBuild payVk stkVk nw mOutFp
    AddressBuildMultiSig useEra sFp nId mOutFp -> runAddressBuildScript useEra sFp nId mOutFp
    AddressInfo txt mOFp -> firstExceptT ShelleyAddressCmdAddressInfoError $ runAddressInfo txt mOFp

runAddressKeyGen :: AddressKeyType
                 -> VerificationKeyFile
                 -> SigningKeyFile
                 -> ExceptT ShelleyAddressCmdError IO ()
runAddressKeyGen kt (VerificationKeyFile vkeyPath) (SigningKeyFile skeyPath) =
    case kt of
      AddressKeyShelley         -> generateAndWriteKeyFiles AsPaymentKey
      AddressKeyShelleyExtended -> generateAndWriteKeyFiles AsPaymentExtendedKey
      AddressKeyByron           -> generateAndWriteKeyFiles AsByronKey
  where
    generateAndWriteKeyFiles asType = do
      skey <- liftIO $ generateSigningKey asType
      let vkey = getVerificationKey skey
      firstExceptT ShelleyAddressCmdWriteFileError
        . newExceptT
        $ writeFileTextEnvelope skeyPath (Just skeyDesc) skey
      firstExceptT ShelleyAddressCmdWriteFileError
        . newExceptT
        $ writeFileTextEnvelope vkeyPath (Just vkeyDesc) vkey

    skeyDesc, vkeyDesc :: TextEnvelopeDescr
    skeyDesc = "Payment Signing Key"
    vkeyDesc = "Payment Verification Key"


runAddressKeyHash :: VerificationKeyTextOrFile
                  -> Maybe OutputFile
                  -> ExceptT ShelleyAddressCmdError IO ()
runAddressKeyHash vkeyTextOrFile mOutputFp = do
  vkey <- firstExceptT ShelleyAddressCmdVerificationKeyTextOrFileError $
            readAddressVerificationKeyTextOrFile vkeyTextOrFile

  let hexKeyHash = foldSomeAddressVerificationKey
                     (serialiseToRawBytesHex . verificationKeyHash) vkey

  case mOutputFp of
    Just (OutputFile fpath) -> liftIO $ BS.writeFile fpath hexKeyHash
    Nothing -> liftIO $ BS.putStrLn hexKeyHash


runAddressBuild :: VerificationKeyTextOrFile
                -> Maybe (VerificationKeyOrFile StakeKey)
                -> NetworkId
                -> Maybe OutputFile
                -> ExceptT ShelleyAddressCmdError IO ()
runAddressBuild payVkeyTextOrFile mbStkVkeyOrFile nw mOutFp = do
    payVKey <- firstExceptT ShelleyAddressCmdVerificationKeyTextOrFileError $
                 readAddressVerificationKeyTextOrFile payVkeyTextOrFile

    addr <- case payVKey of
              AByronVerificationKey vk ->
                return (AddressByron (makeByronAddress nw vk))

              APaymentVerificationKey vk ->
                AddressShelley <$> buildShelleyAddress vk

              APaymentExtendedVerificationKey vk ->
                AddressShelley <$> buildShelleyAddress (castVerificationKey vk)

              AGenesisUTxOVerificationKey vk ->
                AddressShelley <$> buildShelleyAddress (castVerificationKey vk)

    let addrText = serialiseAddress (addr :: AddressAny)

    case mOutFp of
      Just (OutputFile fpath) -> liftIO $ Text.writeFile fpath addrText
      Nothing                 -> liftIO $ Text.putStrLn        addrText

  where
    buildShelleyAddress :: VerificationKey PaymentKey
                        -> ExceptT ShelleyAddressCmdError IO (Address ShelleyAddr)
    buildShelleyAddress vkey = do
      mstakeVKey <-
        case mbStkVkeyOrFile of
          Nothing -> pure Nothing
          Just stkVkeyOrFile ->
            firstExceptT ShelleyAddressCmdReadKeyFileError $
              fmap Just $ newExceptT $
                readVerificationKeyOrFile AsStakeKey stkVkeyOrFile

      let paymentCred  = PaymentCredentialByKey (verificationKeyHash vkey)
          stakeAddrRef = maybe NoStakeAddress
                               (StakeAddressByValue . StakeCredentialByKey
                                                    . verificationKeyHash)
                               mstakeVKey
          address      = makeShelleyAddress nw paymentCred stakeAddrRef

      return address


--
-- Handling the variety of address key types
--

-- TODO: if we could make unions like this an instance of the Key class then
-- it would simplify some of the code above
data SomeAddressVerificationKey
  = AByronVerificationKey           (VerificationKey ByronKey)
  | APaymentVerificationKey         (VerificationKey PaymentKey)
  | APaymentExtendedVerificationKey (VerificationKey PaymentExtendedKey)
  | AGenesisUTxOVerificationKey     (VerificationKey GenesisUTxOKey)

foldSomeAddressVerificationKey :: (forall keyrole. Key keyrole =>
                                   VerificationKey keyrole -> a)
                               -> SomeAddressVerificationKey -> a
foldSomeAddressVerificationKey f (AByronVerificationKey           vk) = f vk
foldSomeAddressVerificationKey f (APaymentVerificationKey         vk) = f vk
foldSomeAddressVerificationKey f (APaymentExtendedVerificationKey vk) = f vk
foldSomeAddressVerificationKey f (AGenesisUTxOVerificationKey     vk) = f vk

readAddressVerificationKeyTextOrFile
  :: VerificationKeyTextOrFile
  -> ExceptT VerificationKeyTextOrFileError IO SomeAddressVerificationKey
readAddressVerificationKeyTextOrFile vkTextOrFile =
    newExceptT $
      readVerificationKeyTextOrFileAnyOf bech32Types textEnvTypes vkTextOrFile
  where
    bech32Types =
      [ FromSomeType (AsVerificationKey AsByronKey)
                     AByronVerificationKey
      , FromSomeType (AsVerificationKey AsPaymentKey)
                     APaymentVerificationKey
      , FromSomeType (AsVerificationKey AsPaymentExtendedKey)
                     APaymentExtendedVerificationKey
      ]

    textEnvTypes =
      [ FromSomeType (AsVerificationKey AsByronKey)
                     AByronVerificationKey
      , FromSomeType (AsVerificationKey AsPaymentKey)
                     APaymentVerificationKey
      , FromSomeType (AsVerificationKey AsPaymentExtendedKey)
                     APaymentExtendedVerificationKey
      , FromSomeType (AsVerificationKey AsGenesisUTxOKey)
                     AGenesisUTxOVerificationKey
      ]

--
-- Multisig addresses
--

runAddressBuildScript
  :: UseCardanoEra
  -> ScriptFile
  -> NetworkId
  -> Maybe OutputFile
  -> ExceptT ShelleyAddressCmdError IO ()
runAddressBuildScript useEra (ScriptFile fp) nId mOutFp = do
  scriptLB <- handleIOExceptT (ShelleyAddressCmdReadFileException . FileIOError fp)
                $ LB.readFile fp
  withCardanoEra useEra $ \_era _eraStyle ->
    case useEra of
      UseByronEra -> liftIO $ putTextLn "Not implemented yet"
      UseShelleyEra -> do
        aScript :: SimpleScript ShelleyEra <-
         firstExceptT (ShelleyAddressCmdAesonDecodeError fp . Text.pack) . hoistEither $ decodeScript scriptLB
        mOutput mOutFp $ serialiseScriptAddress nId aScript
      UseAllegraEra -> do
        aScript :: SimpleScript AllegraEra <-
         firstExceptT (ShelleyAddressCmdAesonDecodeError fp . Text.pack) . hoistEither $ decodeScript scriptLB
        mOutput mOutFp $ serialiseScriptAddress nId aScript
      UseMaryEra -> do
        aScript :: SimpleScript MaryEra <-
         firstExceptT (ShelleyAddressCmdAesonDecodeError fp . Text.pack) . hoistEither $ decodeScript scriptLB
        mOutput mOutFp $ serialiseScriptAddress nId aScript

serialiseScriptAddress :: HasScriptFeatures era => NetworkId -> SimpleScript era -> Text
serialiseScriptAddress nId s =
  let payCred = makePaymentCredential s
  in serialiseAddress $ makeShelleyAddress nId payCred NoStakeAddress

decodeScript :: HasScriptFeatures era => LB.ByteString -> Either String (SimpleScript era)
decodeScript bs = eitherDecode bs

makePaymentCredential :: HasScriptFeatures era => SimpleScript era -> PaymentCredential
makePaymentCredential s = PaymentCredentialByScript . scriptHash $ SimpleScript s

mOutput :: Maybe OutputFile -> Text ->  ExceptT a IO ()
mOutput (Just (OutputFile oFp)) output = liftIO $ Text.writeFile oFp output
mOutput Nothing output = liftIO $ Text.putStr output
