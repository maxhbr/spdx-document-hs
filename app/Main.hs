module Main where

import System.Environment (getArgs)
import SPDX.Document

main :: IO ()
main = do
    args <- getArgs
    mapM_ (\arg -> do
        doc <- parseSPDXDocument arg
        print doc
        ) args