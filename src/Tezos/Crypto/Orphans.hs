

{-# OPTIONS_GHC -Wno-orphans -Wno-missing-export-lists -Wno-missing-monadfail-instances #-}

module Tezos.Crypto.Orphans where

import Data.Char
import Prelude hiding (readEither, unlines, unwords)
import Text.ParserCombinators.ReadP (ReadP)
import Text.Read
import qualified Text.ParserCombinators.ReadP as P

import Data.Aeson
import qualified Data.Text as T

import Lorentz.Value
import Tezos.Address
import Tezos.Crypto
import qualified Tezos.Crypto.Ed25519 as Ed25519
import qualified Tezos.Crypto.Secp256k1 as Secp256k1
import qualified Tezos.Crypto.P256 as P256


-- | Parse something between the two given `Char`'s
betweenChars :: Char -> Char -> ReadP a -> ReadP a
betweenChars beforeChar afterChar =
  P.char beforeChar `P.between` P.char afterChar

-- | Parse something in parentheses
inParensP :: ReadP a -> ReadP a
inParensP = '(' `betweenChars` ')'

-- | Parse something in double-quotes: @"[something]"@
inQuotesP :: ReadP a -> ReadP a
inQuotesP = '"' `betweenChars` '"'

-- | Attempt to parse with given modifier, otherwise parse without
maybeLiftP :: (ReadP a -> ReadP a) -> ReadP a -> ReadP a
maybeLiftP liftP = liftM2 (<|>) liftP id

-- | Attempt to parse `inParensP`, else parse without
maybeInParensP :: ReadP a -> ReadP a
maybeInParensP = maybeLiftP inParensP

-- | Attempt to parse `inQuotesP`, else parse without
maybeInQuotesP :: ReadP a -> ReadP a
maybeInQuotesP = maybeLiftP inQuotesP

-- | Read an `Address`, inside or outside of @""@'s
readAddressP :: ReadP Address
readAddressP =
      maybeInParensP . maybeInQuotesP $ do
        ensureAddressPrefix
        addressStr <- P.munch1 isAlphaNum
        case parseAddress $ T.pack addressStr of
          Left err -> fail $ show err
          Right address' -> return address'
  where
    ensureAddressPrefix =
      (do {('t':'z':'1':_) <- P.look; return ()}) <|>
      (do {('K':'T':'1':_) <- P.look; return ()})

instance Read Address where
  readPrec = readP_to_Prec $ const readAddressP


instance Read PublicKey where
  readPrec = readP_to_Prec $ \_ ->
    maybeInQuotesP $ do
      eNonQuoteChars <- parsePublicKey . T.pack <$> P.munch1 isAlphaNum
      case eNonQuoteChars of
        Left err -> fail $ show err
        Right res -> return res

-- instance Read SecretKey where
--   readPrec = readP_to_Prec $ \_ ->
--     maybeInQuotesP $ do
--       eNonQuoteChars <- parseSecretKey . T.pack <$> P.munch1 isAlphaNum
--       case eNonQuoteChars of
--         Left err -> fail $ show err
--         Right res -> return res

instance Read Signature where
  readPrec = readP_to_Prec $ \_ ->
    maybeInQuotesP $ do
      eNonQuoteChars <- parseSignature . T.pack <$> P.munch1 isAlphaNum
      case eNonQuoteChars of
        Left err -> fail $ show err
        Right res -> return res

-- | Since `Ed25519.PublicKey` doesn't expose
-- many instances, we convert to `String` and
-- compare the results
instance Ord Ed25519.PublicKey where
  compare x y = show x `compare` (show y :: String)

-- | Since `Secp256k1.PublicKey` doesn't expose
-- many instances, we convert to `String` and
-- compare the results
instance Ord Secp256k1.PublicKey where
  compare x y = show x `compare` (show y :: String)

-- | Since `P256.PublicKey` doesn't expose
-- many instances, we convert to `String` and
-- compare the results
instance Ord P256.PublicKey where
  compare x y = show x `compare` (show y :: String)

deriving instance Ord PublicKey


-- instance ToJSON Ed25519.PublicKey where
--   toJSON = Aeson.String . formatPublicKey . PublicKeyEd25519
--   toEncoding = Aeson.text . formatPublicKey . PublicKeyEd25519

-- instance FromJSON Ed25519.PublicKey where
--   parseJSON =
--     Aeson.withText "PublicKey" $
--     either (fail . show) (pure . unPublicKey) . parsePublicKey


-- instance ToJSONKey Ed25519.PublicKey where
-- instance FromJSONKey Ed25519.PublicKey where

instance ToJSONKey PublicKey where
  -- toJSONKey = contramap _ toJSONKey
  -- toJSONKeyList = contramap (fmap _) toJSONKeyList

instance FromJSONKey PublicKey where
  -- fromJSONKey = fmap _ fromJSONKey
  -- fromJSONKeyList = fmap (fmap _) fromJSONKeyList


