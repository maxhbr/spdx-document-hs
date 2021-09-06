{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module SPDX.Document
  ( parseLicense, parseLicenses
  , renderSpdxLicense
  ) where

import MyPrelude

import qualified Distribution.SPDX as SPDX
import qualified Distribution.Parsec as SPDX

parseLicense :: String -> SPDX.LicenseExpression
parseLicense str = (`SPDX.ELicense` Nothing) $ case SPDX.eitherParsec str of
  Right lic -> SPDX.ELicenseId lic
  _         -> SPDX.ELicenseRef $ SPDX.mkLicenseRef' Nothing str

parseLicenses :: [String] -> Maybe SPDX.LicenseExpression
parseLicenses [] = Nothing
parseLicenses ls = let

  parseLicenses' :: [String] -> SPDX.LicenseExpression
  parseLicenses' [l] = parseLicense l
  parseLicenses' (l:ls) = parseLicense l `SPDX.EAnd` (parseLicenses' ls)
  in Just (parseLicenses' ls)

renderSpdxLicense :: SPDX.LicenseExpression -> String
renderSpdxLicense (SPDX.ELicense l _) = let
  renderSpdxLicense' :: SPDX.SimpleLicenseExpression -> String
  renderSpdxLicense' (SPDX.ELicenseId l') = show l'
  renderSpdxLicense' (SPDX.ELicenseRef l') = SPDX.licenseRef l'
  in renderSpdxLicense' l
renderSpdxLicense (SPDX.EAnd l r) = unwords ["(", renderSpdxLicense l, "AND", renderSpdxLicense r, ")"]
renderSpdxLicense (SPDX.EOr l r) = unwords ["(", renderSpdxLicense l, "OR", renderSpdxLicense r, ")"]