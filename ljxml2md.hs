{-# LANGUAGE OverloadedStrings #-}

module Main where

{- 
 - This should be compiled in a binary and piped in the XML format of LiveJournal /
 - Dremwidth. You'll get a markdown file with all metadata embedded as a YAML preface.
 -}

import qualified Data.Map        as M
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T

import Data.Text (Text)
import Data.Maybe
import Data.Either
import Data.Function
import Control.Arrow
import Text.Hamlet.XML
import Text.Pandoc
import Text.Pandoc.Builder
import Text.XML


main :: IO ()
main = do
    -- parse XML input
    input <- T.getContents
    let (Document prologue root epilogue) = parseText_ def input

    -- writer settings
    tmpl <- runIOorExplode $ compileDefaultTemplate "markdown"
    let wext = writerExtensions def
        writeMd = writeMarkdown
          (def { -- overrides to get YAML preface in the resulting MD
            writerTemplate   = Just tmpl, 
            writerExtensions = 
              enableExtension Ext_yaml_metadata_block wext } ) 

    -- root is the root element of the document, let's modify it
    let md = transform root

    -- print md
    (T.putStrLn . T.fromStrict) =<< (runIOorExplode $ writeMd md)

-- We'll turn out <document> into a Pandoc document
transform :: Element -> Pandoc
transform (Element _name _attrs children) = 
    mconcat $ map elemToDoc $ catMaybes $ map nodeToElem children

--   setMeta "id" ("5"::String) $ doc
--     $  header 1 (text "Hello!")
--     <> para (emph (text "hello world") <> text ".")

-- LJ doesn't sstore anything interesting in comments, attributes, etc.
-- We'll need text, of course, but that will come later in the traversal
nodeToElem :: Node -> Maybe Element
nodeToElem (NodeElement e) = Just e
nodeToElem _               = Nothing

getNodeText :: Node -> Maybe Text
getNodeText (NodeContent t) = Just t
getNodeText _               = Nothing

mHtmlToDoc :: Maybe Text -> Pandoc
mHtmlToDoc Nothing = mempty
mHtmlToDoc (Just t) = fromRight mempty . runPure $ readHtml def t

elemToDoc :: Element -> Pandoc
elemToDoc (Element name _attrs children) =
    case name of
        "event" -> listToMaybe children >>= getNodeText & mHtmlToDoc
        {- TODO: process more node types -}
        _       -> mempty

