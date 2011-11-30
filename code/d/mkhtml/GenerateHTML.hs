{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}

-- All of the following is ugly
-- Use an AST

module GenerateHTML (html) where

import Prelude hiding (replicate, all, concat)
import Data.Text (Text, all, pack, replicate, append, concat)
import Data.Text.Lazy (fromStrict)
import Text.XML (Document(Document), Prologue(Prologue), Element(Element), Name(Name), Node(NodeElement, NodeContent), def, parseText)

import Paragraphs

filterMaybe :: [Maybe a] → [a]
filterMaybe (Nothing:xs)  =    filterMaybe xs
filterMaybe ((Just x):xs) = x:(filterMaybe xs)
filterMaybe [] = []


blank :: Text -> Bool
blank = all (\c -> c == ' ' || c == '\t')

nbsp :: Char
nbsp = ' '

indent :: Int -> Text
indent i = replicate i (pack [nbsp])

mkName :: String -> Name
mkName n = Name (pack n) Nothing Nothing

mkAttr :: String -> String -> (Name, Text)
mkAttr at val = (mkName at, pack val)

mkIndented :: Int -> String -> Node -> [Node] -> Maybe Element
mkIndented i cls n start = Just $ Element (mkName "div") [mkAttr "class" "section"] [
    NodeElement $ Element (mkName "pre") [mkAttr "class" "indent"] [NodeContent $ indent i]
  , NodeElement $ Element (mkName "div") [mkAttr "class" cls]      (start++[n])
  ]


-- XML to be parsed must declare namespaces. Ours doesn't, so add it.
tryParse :: Text -> Node
tryParse s = case parseText def (fromStrict $ concat ["<div xmlns:e='http://eegg.github.com/htmlx' xmlns:m='http://eegg.github.com/macro'>", s, "</div>"]) of
  Left e                  -> error $ "parse Error " ++ (show e) ++ " on code: " ++ (show s)  -- NodeContent $ append s (pack [nbsp])
  Right (Document _ el _) -> NodeElement el

paragraphToHTML :: Paragraph -> Maybe Element
paragraphToHTML (CodeParagraph s)
  | blank s   = Nothing
  | otherwise = Just $ Element (Name (pack "displaycode") (Just $ "http://eegg.github.com/htmlx") (Just $ pack "e")) [] [NodeContent $ append s $ pack [nbsp]] -- stop xmlproc self-closing empty tags

paragraphToHTML (CommentParagraph i s) = mkIndented i "CommentParagraph" (tryParse s) []
paragraphToHTML (AssertionParagraph i s) = mkIndented i "AssertionParagraph" (tryParse s) []

paragraphsToHTML :: [Paragraph] -> [Maybe Element]
paragraphsToHTML ((CommentParagraph i s1):(AssertionParagraph _ s2):rest) =
  (mkIndented i "AssertionParagraph" (tryParse s2) [NodeElement $ Element (mkName "div") [mkAttr "class" "expl"] [tryParse s1]]):(paragraphsToHTML rest)
paragraphsToHTML (p:ps) = (paragraphToHTML p):(paragraphsToHTML ps)
paragraphsToHTML [] = []

htmlDoc :: [Node] → Document
htmlDoc es = Document (Prologue [] Nothing []) (Element (mkName "section") [
    mkAttr "class" "generated_listing"
  , mkAttr "xmlns" "http://www.w3.org/1999/xhtml"
  , mkNs "xi" "http://www.w3.org/2001/XInclude"
  , mkNs "e" "http://eegg.github.com/htmlx"
  , mkNs "m" "http://eegg.github.com/macro"
  ] es) []
  where mkNs ns url = ((Name (pack ns) (Just $ pack "xmlns") Nothing), pack url)

html :: [Paragraph] -> Document
html = htmlDoc . map NodeElement . filterMaybe . paragraphsToHTML