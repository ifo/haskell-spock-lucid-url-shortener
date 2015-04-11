{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Monoid
import Web.Spock.Safe
import Lucid
import qualified Data.Text.Lazy as L

import Templates.Helpers (renderHtmlStrict)

main :: IO ()
main =
  runSpock 3000 $ spockT id $ do

    get root $ -- equivalent of get "/"
      renderHtmlStrict $ p_ "Hello"

    get ("hello" <//> var) $ \name ->
      text ("Hello " <> name <> "!")

    getpost "param" $ do
      p <- param "url"
      case p of
        Nothing -> html "<form method='post'><input type='text' name='url' value='hello hi' /><button type='submit'>Submit</button></form>"
        Just url -> renderHtmlStrict $ ullis_ $ toHtml (url :: L.Text)

ullis_ :: (Monoid s, Term arg s, Term s result) => arg -> result
ullis_ p = ul_ $ li_ p
              <> li_ p
              <> li_ p

getpost url action = do
  get url action
  post url action
