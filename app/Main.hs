{-# LANGUAGE OverloadedStrings #-}
module Main where

import Protolude
import Control.Monad.Fail (fail)

import Gql2Py.Parser (parseSchemaDoc)
import Gql2Py.Printer (renderSchemaDoc)


main :: IO ()
main = do
  s <- readFile "../schema.graphql"
  let schema = parseSchemaDoc s
--  let text = renderSchemaDoc schema
  either (fail . show) f schema
  where
    f schema = do
      let py = renderSchemaDoc schema
      putStrLn py
--      either (fail . show) p py
--      where
--        p py = print py
