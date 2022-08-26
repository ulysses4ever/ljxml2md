{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}

module Main where

{- 
 - This should be compiled in a binary (or run via runhaskell) and piped in 
 - the XML format of LiveJournal / Dreamwidth. You'll get a markdown file with all 
 - metadata embedded as a YAML preface. HTML text of the post is converted into MD.
 -}

import qualified Data.Map        as M
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T

import Data.Text (Text)
import Data.Maybe
import Data.Either
import Data.Function
import Debug.Trace
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

-- Transform an element by turning its children into Pandoc AST
transform :: Element -> Pandoc
transform (Element name _attrs children) = 
    mconcat .
    map elemToDoc .
    catMaybes $
    map nodeToElem children

-- List of XML elements we want to throw away
blockList :: [Text]
blockList = ["can_comment", "logtime", "personifi_tags"]

-- Some elements should be renamed for various external reasons
rename :: Text -> Text
rename = \case
    "subject" -> "title"
    "url"     -> "oldurl"
    "ditemid" -> "lj_url_id"
    "itemid"  -> "abs_id"
    other     -> other

-- Main workhorse: turn one LJ XML element into a piece of Pandoc AST
elemToDoc :: Element -> Pandoc
elemToDoc el@(Element name _attrs children) = fromMaybe mempty mRes
    where
    mChildText = listToMaybe children >>= mGetNodeText
    mRes = case name of
        "event" -> 
            fmap htmlToDoc mChildText 

        "props" ->
            Just . transform $ el

        _ | not $ nameTxt `elem` blockList -> 
            flip (setMeta $ rename nameTxt) mempty <$> mChildText 
            where
            nameTxt = nameLocalName name

        _   -> Nothing

-- LJ doesn't store anything interesting in comments, attributes, etc.
-- We'll need text, of course, but that will come later in the traversal
nodeToElem :: Node -> Maybe Element
nodeToElem (NodeElement e) = Just e
nodeToElem _               = Nothing

-- Plumbing
mGetNodeText :: Node -> Maybe Text
mGetNodeText (NodeContent t) = Just t
mGetNodeText _               = Nothing

htmlToDoc :: Text -> Pandoc
htmlToDoc t =
    fromRight (error "htmlToDoc: fail to parse HTML") . runPure $
    readHtml def t

