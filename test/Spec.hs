{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Exception (evaluate)
import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as B
import Data.Either
import Data.FileEmbed (embedFile)
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.Query.BFS as G
import Data.List (isPrefixOf, tails)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified Data.Yaml as Y
import qualified Data.Yaml.Aeson as Y
import System.Directory (doesFileExist)
import System.Exit
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import Test.QuickCheck

import SPDX.Document
import SPDX.Document.Common

spdxFileBS :: B.ByteString
spdxFileBS =
  B.fromStrict $(embedFile "spdx-spec/examples/SPDXJSONExample-v2.3.spdx.json")

spdxYamlFileBS :: BS.ByteString
spdxYamlFileBS = $(embedFile "spdx-spec/examples/SPDXYAMLExample-2.3.spdx.yaml")

otherSpdxYamlFileBS :: BS.ByteString
otherSpdxYamlFileBS = $(embedFile "test/data/document.spdx.yml")

ternSpdxFileBS :: B.ByteString
ternSpdxFileBS =
  B.fromStrict $(embedFile "test/data/httpd_2.4.51.tern.fixed.spdx.json")

spdxSpec = do
  describe "SPDX License expression parsing" $ do
    it "parsing should work, license" $ do
      parseLicenseExpression "MIT" `shouldNotBe` NONE
      parseLicenseExpression "MIT" `shouldNotBe` NOASSERTION
      renderSpdxLicense (Maybe.fromJust (parseLicenseExpressionList' ["MIT"])) `shouldBe`
        "MIT"
    it "parsing should work, valid expression" $ do
      parseLicenseExpression "MIT AND Apache-2.0" `shouldNotBe` NONE
      parseLicenseExpression "MIT AND Apache-2.0" `shouldNotBe` NOASSERTION
      renderSpdxLicense (Maybe.fromJust (parseLicenseExpressionList' ["MIT AND Apache-2.0"])) `shouldBe`
        "MIT AND Apache-2.0"
      renderSpdxLicense (Maybe.fromJust (parseLicenseExpressionList' ["MIT", "Apache-2.0"])) `shouldBe`
        "MIT AND Apache-2.0"
    it "parsing should work, valid expression invalid names" $ do
      parseLicenseExpression "unknown-license-reference AND unknown" `shouldNotBe`
        NONE
      parseLicenseExpression "unknown-license-reference AND unknown" `shouldNotBe`
        NOASSERTION
  describe "SpdxCollector" $ do
    it "parsing Json is successfull" $
      let spdxResult = A.eitherDecode spdxFileBS :: Either String SPDXDocument
          potentialError =
            case spdxResult of
              Right _ -> Nothing
              Left err -> Just err
       in do potentialError `shouldBe` Nothing
             putStrLn (ppSpdxGraph (fromRight undefined spdxResult))
             isRight spdxResult `shouldBe` True
    it "parsing YML is successfull" $
      let spdxYmlResult =
            Y.decodeEither' spdxYamlFileBS :: Either Y.ParseException SPDXDocument
          potentialYmlError =
            case spdxYmlResult of
              Right _ -> Nothing
              Left err -> Just (show err)
       in do potentialYmlError `shouldBe` Nothing
             isRight spdxYmlResult `shouldBe` True
    it "parsing Json is successfull" $
      let spdxResult =
            A.eitherDecode ternSpdxFileBS :: Either String SPDXDocument
          potentialError =
            case spdxResult of
              Right _ -> Nothing
              Left err -> Just err
       in do potentialError `shouldBe` Nothing
             isRight spdxResult `shouldBe` True

main :: IO ()
main = hspec $ spdxSpec
