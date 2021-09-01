{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module SPDX.Document.SnippetInformation
  where

import MyPrelude

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
