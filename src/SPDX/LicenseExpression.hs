{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeFamilies              #-}

module SPDX.LicenseExpression
  ( MaybeLicenseExpression(..)
  , renderSpdxLicense
  , parseLicenseExpression
  , parseLicenseExpression'
  ) where

import           MyPrelude
import           SPDX.Document.Common

import qualified Data.Aeson           as A
import qualified Data.Aeson.Types     as A
import qualified Data.List            as List
import           Data.String          (IsString (..))
import qualified Data.Text            as T
import qualified Distribution.Parsec  as SPDX
import qualified Distribution.SPDX    as SPDX

newtype MaybeLicenseExpression =
  MLicExp
    { unMLicExp :: SPDXMaybe SPDX.LicenseExpression
    }
  deriving (Eq)

renderSpdxLicense :: SPDX.LicenseExpression -> String
renderSpdxLicense (SPDX.ELicense l _) =
  let renderSpdxLicense' :: SPDX.SimpleLicenseExpression -> String
      renderSpdxLicense' (SPDX.ELicenseId l')  = SPDX.licenseId l'
      renderSpdxLicense' (SPDX.ELicenseRef l') = SPDX.licenseRef l'
   in renderSpdxLicense' l
renderSpdxLicense (SPDX.EAnd l r) =
  unwords [renderSpdxLicense l, "AND", renderSpdxLicense r]
renderSpdxLicense (SPDX.EOr l r) =
  unwords ["(", renderSpdxLicense l, "OR", renderSpdxLicense r, ")"]

instance Show MaybeLicenseExpression where
  show (MLicExp (SPDXJust l)) = renderSpdxLicense l
  show (MLicExp me)           = show me

instance IsString MaybeLicenseExpression where
  fromString "NOASSERTION" = MLicExp NOASSERTION
  fromString "NONE" = MLicExp NONE
  fromString str =
    MLicExp $
    case SPDX.eitherParsec str :: Either String SPDX.License of
      Left err ->
        SPDXJust . (`SPDX.ELicense` Nothing) $
        case SPDX.eitherParsec str of
          Right lic -> SPDX.ELicenseId lic
          _         -> SPDX.ELicenseRef $ SPDX.mkLicenseRef' Nothing str
      Right SPDX.NONE -> NONE
      Right (SPDX.License l) -> SPDXJust l

instance A.ToJSON MaybeLicenseExpression where
  toJSON = A.toJSON . show

instance A.FromJSON MaybeLicenseExpression where
  parseJSON = fmap fromString . A.parseJSON

instance Semigroup MaybeLicenseExpression where
  (MLicExp NONE) <> mle2 = mle2
  (MLicExp NOASSERTION) <> mle2 = mle2
  mle1 <> (MLicExp NONE) = mle1
  mle1 <> (MLicExp NOASSERTION) = mle1
  (MLicExp (SPDXJust l1)) <> (MLicExp (SPDXJust l2)) =
    MLicExp . SPDXJust $ l1 `SPDX.EAnd` l2

instance Monoid MaybeLicenseExpression where
  mempty = MLicExp NOASSERTION

parseLicenseExpression :: String -> SPDXMaybe SPDX.LicenseExpression
parseLicenseExpression = unMLicExp . fromString

parseLicenseExpression' :: String -> Maybe SPDX.LicenseExpression
parseLicenseExpression' = spdxMaybeToMaybe . parseLicenseExpression
