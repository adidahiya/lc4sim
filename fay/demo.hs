{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# LANGUAGE EmptyDataDecls #-}

-- Demo code for Fay-to-JS compilation

import Language.Fay.Prelude
import Language.Fay.FFI
import JQuery

-- ffi :: Foreign a => String -> a

alert :: Foreign a => a -> Fay ()
alert = ffi "window.alert(%1)"

------------------------------------------------------------------------------
-- HTML DOM elements & attributes
data Attr = Attr String String

data Elem = Elem String [Attr] [Elem]
          | CData String

buildAttr :: Attr -> String
buildAttr (Attr k v) = " " ++ k ++ "='" ++ v ++ "'"

buildElem :: Elem -> String
buildElem (CData s) = s
buildElem (Elem tag attrs childs) =
  "<" ++ tag ++ concatMap buildAttr attrs ++ ">" ++
  concatMap buildElem childs ++
  "</" ++ tag ++ ">"

-- Write elements to the DOM
writeRaw :: String -> Fay ()
writeRaw = ffi "document.write(%1)"

writeElem :: Elem -> Fay ()
writeElem = writeRaw . buildElem

-- Get the contents of an element and print them to the console
printElem :: Fay JQuery -> Fay ()
printElem f = f >>= getHtml >>= putStrLn


------------------------------------------------------------------------------
-- Test working with DOM elements and writing to the console
app :: Event -> Fay ()
app _ = do
  putStrLn "Start js app..."
  -- initEditor
  initFilepicker
  -- writeElem $ Elem "h4" [] [CData "test"]
  -- printElem $ select "body"
  -- left <- select ".left"
  -- right <- select ".right"
  -- rightContents <- getHtml right
  -- putStrLn rightContents
  -- setHtml rightContents left
  lineGutter <- select ".ace_gutter-cell"
  click onLineClick lineGutter
  putStrLn "...finished main."

initEditor :: Fay ()
initEditor = ffi "ace.edit('editor').setTheme('ace/theme/tomorrow')"

initFilepicker :: Fay ()
initFilepicker = ffi "filepicker.setKey('AKi1o3YU9SXWoWiVnrB8nz')"

onLineClick :: Event -> Fay ()
onLineClick e = do
  putStrLn "got line click"
  print e

theDocument :: Document
theDocument = ffi "window.document"

main :: Fay ()
main = documentReady app theDocument

