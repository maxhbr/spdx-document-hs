{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import System.Exit
import Data.FileEmbed (embedFile)
import Data.Either
import Data.List (tails, isPrefixOf)
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as C
import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as A
import qualified Data.Yaml as Y
import qualified Data.Yaml.Aeson as Y
import qualified Data.Vector as V
import qualified Data.Map as Map
import System.IO.Temp (withSystemTempDirectory)
import System.Directory (doesFileExist)
import qualified Data.Graph.Inductive.Graph as G

import SPDX.Document

spdxFileBS :: B.ByteString
spdxFileBS = B.fromStrict $(embedFile "test/data/SPDXJSONExample-v2.2.spdx.json")

spdxYamlFileBS :: BS.ByteString
spdxYamlFileBS = $(embedFile "test/data/document.spdx.yml")

spdxSpec = do
  describe "SpdxCollector" $ do
    it "parsing Json is successfull" $ let
        spdxResult = A.eitherDecode spdxFileBS :: Either String SPDXDocument
        potentialError = case spdxResult of
          Right _ -> Nothing
          Left err -> Just err
      in do
      potentialError `shouldBe` Nothing
      isRight spdxResult `shouldBe` True
    it "parsing YML is successfull" $ let
        spdxYmlResult = Y.decodeEither' spdxYamlFileBS :: Either Y.ParseException SPDXDocument
        potentialYmlError = case spdxYmlResult of
          Right _ -> Nothing
          Left err -> Just (show err)
      in do 
      potentialYmlError `shouldBe` Nothing
      isRight spdxYmlResult `shouldBe` True

main :: IO ()
main = hspec $ spdxSpec
