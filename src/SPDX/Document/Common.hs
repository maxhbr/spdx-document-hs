{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module SPDX.Document.Common where

import           MyPrelude

import qualified Data.Aeson                    as A
import qualified Data.Aeson.Types              as A
import qualified Data.List                     as List
import qualified Data.Text                     as T
import qualified Distribution.Parsec           as SPDX
import qualified Distribution.SPDX             as SPDX

type SPDXID = String

data SPDXMaybe a
  = SPDXJust a
  | NOASSERTION
  | NONE
  deriving (Eq)
instance (Show a) => Show (SPDXMaybe a) where
  show (SPDXJust a) = show a
  show NOASSERTION  = "NOASSERTION"
  show NONE         = "NONE"
instance (A.FromJSON a) => A.FromJSON (SPDXMaybe a) where
  parseJSON = A.withText "SPDXMaybe" $ \case
    "NOASSERTION" -> pure NOASSERTION
    "NONE"        -> pure NONE
    text          -> fmap SPDXJust (A.parseJSON (A.String text))
spdxMaybeToMaybe :: SPDXMaybe a -> Maybe a
spdxMaybeToMaybe (SPDXJust a) = Just a
spdxMaybeToMaybe _            = Nothing

parseLicense :: String -> Maybe SPDX.LicenseExpression
parseLicense str = 
  case SPDX.eitherParsec str :: Either String SPDX.License of
    Left  err              -> Just . (`SPDX.ELicense` Nothing) $ case SPDX.eitherParsec str of
                              Right lic -> SPDX.ELicenseId lic
                              _         -> SPDX.ELicenseRef $ SPDX.mkLicenseRef' Nothing str
    Right SPDX.NONE        -> Nothing
    Right (SPDX.License l) -> Just l

parseLicenseExpression :: String -> SPDXMaybe SPDX.LicenseExpression
parseLicenseExpression "NOASSERTION" = NOASSERTION
parseLicenseExpression "NONE"        = NONE
parseLicenseExpression str = case parseLicense str of
  Just l -> SPDXJust l
  Nothing -> NONE

parseLicenses :: [String] -> Maybe SPDX.LicenseExpression
parseLicenses [] = Nothing
parseLicenses ls =
  let
    parseLicenses' :: [String] -> Maybe SPDX.LicenseExpression
    parseLicenses' [l     ] = parseLicense l
    parseLicenses' (l : ls) = case (parseLicenses' ls) of
      Just pls -> case parseLicense l of
        Just pl -> Just $ pl `SPDX.EAnd` pls
        Nothing -> Just pls
      Nothing -> parseLicense l
  in
    (parseLicenses' (List.nub ls))

renderSpdxLicense :: SPDX.LicenseExpression -> String
renderSpdxLicense (SPDX.ELicense l _) =
  let renderSpdxLicense' :: SPDX.SimpleLicenseExpression -> String
      renderSpdxLicense' (SPDX.ELicenseId  l') = show l'
      renderSpdxLicense' (SPDX.ELicenseRef l') = SPDX.licenseRef l'
  in  renderSpdxLicense' l
renderSpdxLicense (SPDX.EAnd l r) =
  unwords ["(", renderSpdxLicense l, "AND", renderSpdxLicense r, ")"]
renderSpdxLicense (SPDX.EOr l r) =
  unwords ["(", renderSpdxLicense l, "OR", renderSpdxLicense r, ")"]

renderLicenseExpression :: SPDXMaybe SPDX.LicenseExpression -> Maybe String
renderLicenseExpression (SPDXJust l) = Just $ renderSpdxLicense l
renderLicenseExpression _            = Nothing

data SPDXChecksumAlgorithm
  = SHA256
  | SHA1
  | SHA384
  | MD2
  | MD4
  | SHA512
  | MD6
  | MD5
  | SHA224
  deriving (Eq, Show, Generic)
instance A.FromJSON SPDXChecksumAlgorithm
data SPDXChecksum
--               "checksums" : {
--                 "description" : "The checksum property provides a mechanism that can be used to verify that the contents of a File or Package have not changed.",
--                 "type" : "array",
--                 "items" : {
--                   "type" : "object",
--                   "properties" : {
--                     "algorithm" : {
--                       "description" : "Identifies the algorithm used to produce the subject Checksum. Currently, SHA-1 is the only supported algorithm. It is anticipated that other algorithms will be supported at a later time.",
--                       "type" : "string",
--                       "enum" : [ "SHA256", "SHA1", "SHA384", "MD2", "MD4", "SHA512", "MD6", "MD5", "SHA224" ]
--                     },
--                     "checksumValue" : {
--                       "description" : "The checksumValue property provides a lower case hexidecimal encoded digest value produced using a specific algorithm.",
--                       "type" : "string"
--                     }
--                   },
--                   "description" : "A Checksum is value that allows the contents of a file to be authenticated. Even small changes to the content of the file will change its checksum. This class allows the results of a variety of checksum and cryptographic message digest algorithms to be represented."
--                 },
--                 "minItems" : 1
--               },
                  = SPDXChecksum
  { _SPDXChecksum_algorithm     :: SPDXChecksumAlgorithm
  , _SPDXChecksum_checksumValue :: String
  }
  deriving (Eq, Show)
instance A.FromJSON SPDXChecksum where
  parseJSON = A.withObject "SPDXChecksum"
    $ \v -> SPDXChecksum <$> v A..: "algorithm" <*> v A..: "checksumValue"
