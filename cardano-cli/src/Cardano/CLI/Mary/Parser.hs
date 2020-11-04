module Cardano.CLI.Mary.Parser
  ( Token (..)
  , Tokens
  , TParser
  , addition
  , applyAddSubtract
  , calculateValue
  , lexToken
  , lexTokens
  , preValueAddition
  , preValueLovelace
  , preValueMultiAsset
  , preValueParser
  , preValueSubtraction
  , preValToValue
  , stringToValue
  , subtraction
  , textToPolicyId
  , tokenToValue
  , valueTokenFullySpecified
  , valueTokenPolicyIdAndAssetId
  , valueTokenPolicyIdOnly
  ) where

import           Prelude

import           Control.Applicative (many, (<|>))
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.Word (Word64)
import           Text.Parsec (ParseError, Parsec, SourcePos, anyChar, getPosition, manyTill, parse,
                     token, try, (<?>))
import           Text.Parsec.Char (alphaNum, digit, hexDigit, letter, space, spaces, string)
import           Text.Parsec.String (Parser)
import           Text.ParserCombinators.Parsec.Combinator (eof, many1, notFollowedBy, sepBy1)

import           Cardano.Api.Typed (AssetId (..), AssetName (..), PolicyId (..), Quantity (..),
                     ScriptHash (..), Value, selectAsset, valueFromList)
import           Cardano.Crypto.Hash (hashFromStringAsHex)
import qualified Shelley.Spec.Ledger.Scripts as Shelley

{- HLINT ignore "Reduce duplication" -}

stringToValue :: String -> Either ParseError Value
stringToValue input = calculateValue <$> fullParse input
 where
  fullParse :: String -> Either ParseError [PreValue]
  fullParse str = parse lexTokens "" str >>= parse preValueParser ""

calculateValue :: [PreValue] -> Value
calculateValue preVals =
  let finalVal = mconcat . map preValToValue $ applyAddSubtract preVals
      ada = selectAsset finalVal AdaAssetId
  in if selectAsset finalVal AdaAssetId  < 0
     then error $ "Negative lovelace values are not allowed: " <> show ada
     else finalVal

applyAddSubtract :: [PreValue] -> [PreValue]
applyAddSubtract [] = []
applyAddSubtract [x] = [x]
applyAddSubtract (Subtraction : Lovelace w64 : rest) =
  Lovelace w64 : applyAddSubtract rest
applyAddSubtract (Subtraction : MultiAsset pId aId minted : rest) =
  MultiAsset pId aId (negate minted) : applyAddSubtract rest
applyAddSubtract (Addition : rest) = applyAddSubtract rest
applyAddSubtract (x : rest) = x : applyAddSubtract rest

textToPolicyId :: Text -> PolicyId
textToPolicyId hashText =
  case hashFromStringAsHex $ Text.unpack hashText of
    Just h -> PolicyId . ScriptHash $ Shelley.ScriptHash h
    Nothing -> error $ "PolicyId: " <> Text.unpack hashText <> " is not a hash."

preValToValue :: PreValue -> Value
preValToValue Addition = valueFromList []
preValToValue Subtraction = valueFromList []
preValToValue (Lovelace w64) =
  let quantity = Quantity w64
  in valueFromList [(AdaAssetId, quantity)]
preValToValue (MultiAsset pId aId minted) =
  let polId = textToPolicyId pId
      assetName = AssetName $ Text.encodeUtf8 aId
      assetId = AssetId polId assetName
      quantity = Quantity minted
  in valueFromList [(assetId , quantity)]

-- Parser

type TParser a = Parsec Tokens () a

data PreValue = Lovelace Integer
              | MultiAsset
                  Text
                  -- ^ PolicyId
                  Text
                  -- ^ AssetId
                  Integer
                  -- ^ Amount minted
              | Addition
              | Subtraction
              deriving Show

preValueParser :: TParser [PreValue]
preValueParser =
  many1 (   preValueLovelace
        <|> preValueMultiAsset
        <|> preValueAddition
        <|> preValueSubtraction
        )

tokenToTParser :: (Token -> Maybe a) -> TParser a
tokenToTParser f =
  token
    (show . snd)
    fst
    $ \(_,t) -> f t

preValueLovelace :: TParser PreValue
preValueLovelace =
  tokenToTParser (\t -> case t of
                          LovelaceT n -> Just $ Lovelace n
                          _ -> Nothing
                 )

preValueMultiAsset :: TParser PreValue
preValueMultiAsset =
  tokenToTParser (\t -> case t of
                          MultiAssetT pId aId aM -> Just $ MultiAsset pId aId aM
                          _ -> Nothing
                 )

preValueAddition :: TParser PreValue
preValueAddition =
  tokenToTParser (\t -> case t of
                          AdditionT -> Just Addition
                          _ -> Nothing
                 )


preValueSubtraction :: TParser PreValue
preValueSubtraction =
  tokenToTParser (\t -> case t of
                          SubtractionT -> Just Subtraction
                          _ -> Nothing
                 )

-- Lexer

type Tokens = [(SourcePos, Token)]

data Token = LovelaceT Integer
           | MultiAssetT
               Text
               -- ^ ScriptHash
               Text
               -- ^ AssetId
               Integer
               -- ^ AmountMinted
           | AdditionT
           | PeriodT
           | SubtractionT
           deriving (Eq, Ord, Show)

lexTokens :: Parser Tokens
lexTokens = spaces *> sepBy1 ((,) <$> getPosition <*> lexToken) spaces

lexToken :: Parser Token
lexToken =
      try (lovelaceToken <?> "Expecting \"Word64 lovelace\"")
  <|> (addition <?> "Expecting \"+\"")
  <|> (subtraction <?> "Expecting \"-\"")
  <|> (valueToken <?> "Expecting \"INT hexadecimal.STRING\"")
  <|> incorrectSyntax

-- Primitive Token Lexers

incorrectSyntax :: Parser Token
incorrectSyntax = do
  _ <- spaces
  incorrect <- many alphaNum
  _ <- manyTill anyChar eof
  fail $ "Incorrect syntax: " <> incorrect
       <> "\nExpecting \"Word64 lovelace\",\"+\" or \"INT hexadecimal.STRING\""

period :: Parser Token
period = PeriodT <$ string "."

word64 :: Parser Integer
word64 = do i <- uinteger
            if i > fromIntegral (maxBound :: Word64)
            then fail "Word64 max bound"
            else return i

uinteger :: Parser Integer
uinteger = do d <- many1 digit
              notFollowedBy alphaNum
              return $ read d

lovelaceToken :: Parser Token
lovelaceToken = do
  w64 <- word64 <?> "Word64"
  _ <- spaces
  _ <- string "lovelace"
  _ <- spaces
  return $ LovelaceT w64

valueToken :: Parser Token
valueToken =
      try valueTokenFullySpecified
  <|> try valueTokenPolicyIdAndAssetId
  <|> valueTokenPolicyIdOnly
  <* spaces

valueTokenFullySpecified :: Parser Token
valueTokenFullySpecified = do
  i <- try uinteger <?> "INT"
  let minted = fromInteger i
  _ <- spaces
  pId <- scriptHash
  _ <- period
  assetId <- try $ many (letter <|> digit)
  _ <- spaces
  return $ MultiAssetT pId (Text.pack assetId) minted

valueTokenPolicyIdAndAssetId :: Parser Token
valueTokenPolicyIdAndAssetId = do
  pId <- scriptHash
  _ <- period
  notFollowedBy space
  assetId <- many (letter <|> digit)
  _ <- spaces <|> eof
  notFollowedBy uinteger
  return $ MultiAssetT pId (Text.pack assetId) 1

valueTokenPolicyIdOnly :: Parser Token
valueTokenPolicyIdOnly = do
  i <- try uinteger <?> "INT"
  let minted = fromInteger i
  _ <- spaces
  pId <- scriptHash
  notFollowedBy period
  _ <- spaces
  return $ MultiAssetT pId (Text.pack "") minted

scriptHash :: Parser Text
scriptHash = Text.pack <$> many1 hexDigit

addition :: Parser Token
addition = (AdditionT <$ string "+") <* spaces

subtraction :: Parser Token
subtraction = (SubtractionT <$ string "-") <* spaces

-- Helpers

tokenToValue :: Token -> Value
tokenToValue AdditionT = valueFromList []
tokenToValue SubtractionT = valueFromList []
tokenToValue (LovelaceT w64) =
  let quantity = Quantity w64
  in valueFromList [(AdaAssetId, quantity)]
tokenToValue (MultiAssetT pId aId minted) =
  let polId = textToPolicyId pId
      assetName = AssetName $ Text.encodeUtf8 aId
      assetId = AssetId polId assetName
      quantity = Quantity minted
  in valueFromList [(assetId , quantity)]
tokenToValue PeriodT = valueFromList []

