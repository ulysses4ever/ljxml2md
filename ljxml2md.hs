{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Map        as M
import           Text.Hamlet.XML
import           Text.XML
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import           Text.Pandoc
import           Text.Pandoc.Builder

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
transform (Element _name attrs children) = 
  setMeta "id" ("5"::String) $ doc
    $  header 1 (text "Hello!")
    <> para (emph (text "hello world") <> text ".")

{-

-- Leftover from xml-conduit example at https://www.yesodweb.com/book/xml

goNode :: Node -> [Node]
goNode (NodeElement e) = [NodeElement $ goElem e]
goNode (NodeContent t) = [NodeContent t]
goNode (NodeComment _) = [] -- hide comments
goNode (NodeInstruction _) = [] -- and hide processing instructions too

-- convert each source element to its XHTML equivalent
goElem :: Element -> Element
goElem (Element "para" attrs children) =
    Element "p" attrs $ concatMap goNode children
goElem (Element "em" attrs children) =
    Element "i" attrs $ concatMap goNode children
goElem (Element "strong" attrs children) =
    Element "b" attrs $ concatMap goNode children
goElem (Element "image" attrs _children) =
    Element "img" (fixAttr attrs) [] -- images can't have children
  where
    fixAttr mattrs
        | "href" `M.member` mattrs  = M.delete "href" $ M.insert "src" (mattrs M.! "href") mattrs
        | otherwise                 = mattrs
goElem (Element name attrs children) =
    -- don't know what to do, just pass it through...
    Element name attrs $ concatMap goNode children
-}
