{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# LANGUAGE EmptyDataDecls #-}

-- Demo code for Fay-to-JS compilation

import Language.Fay.Types
import Language.Fay.Prelude
import Language.Fay.FFI
import JQuery
-- import Language.Fay.Compiler
-- import Control.Monad
-- import System.Process

-- | Parts of Haskell that we know work off the bat:
--   Enums / custom types, lists, records, pattern matching, functions, higher
--   order functions, partial application, let statements

-- Foreign function interface
-- data Element
-- instance Foreign Element

alert :: Foreign a => a -> Fay ()
alert = ffi "window.alert(%1)"

-- DOM
{-
getInnerHTML :: Element -> Fay String
getInnerHTML = ffi "%1.innerHTML"

setInnerHTML :: Element -> String -> Fay ()
setInnerHTML = ffi "%1.innerHTML=%2"

theDocument :: Element
theDocument = ffi "window.document"

jQuery :: Element -> JQuery
jQuery = ffi "window.jQuery(%1)"

documentGetElements :: String -> Fay [Element]
documentGetElements = ffi "document.getElementsByTagName(%1)"
-}

consoleLog :: Foreign a => a -> Fay ()
consoleLog = ffi "window.console.log(%1)"

main :: Fay ()
main = do
  consoleLog "Entered main..."
  consoleLog $ return "Entered main..."
  body <- select "body"
  bodyContents <- getHtml body
  consoleLog $ getHtml body
  consoleLog bodyContents
  -- result <- documentGetElements "body"
  -- consoleLog result

