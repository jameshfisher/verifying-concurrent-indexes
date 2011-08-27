-- All of the following is ugly
-- Use an AST

module GenerateHTML (html) where

import Data.String.Utils

import Paragraphs

html = htmlWrap . join "\n" . map paragraphToHTML

blank [] = True
blank (' ':cs) = blank cs
blank ('\t':cs) = blank cs
blank _ = False

paragraphToHTML (CodeParagraph s) =
  if (blank s)
    then ""
    else "<e:displaycode>" ++ (htmlEscape s) ++ "&#160;</e:displaycode>" -- stops xmlproc self-closing empty tags

paragraphToHTML (CommentParagraph i s) =
  "<div class='section'><pre class='indent'>" ++ (join "" (replicate i "&#160;")) ++ "</pre><div class='CommentParagraph'>" ++ (htmlEscape s) ++ "</div></div>"

paragraphToHTML (AssertionParagraph i s) =
  "<div class='section'><pre class='indent'>" ++ (join "" (replicate i "&#160;")) ++ "</pre><div class='AssertionParagraph'>" ++ (htmlAssertion (htmlEscape s)) ++ "</div></div>"

paragraphToHTML (CommentedAssertionParagraph (CommentParagraph i s1) (AssertionParagraph j s2)) =
  "<div class='section'><pre class='indent'>" ++ (join "" (replicate i "&#160;")) ++ "</pre><div class='AssertionParagraph'>" ++
  "<div class='expl'>" ++ htmlEscape(s1) ++ "</div>" ++ (htmlAssertion (htmlEscape s2)) ++ "</div></div>"
  

htmlWrap s = htmlHeader ++ s ++ htmlFooter

htmlHeader = "<section class='generated_listing' xmlns='http://www.w3.org/1999/xhtml' xmlns:xi='http://www.w3.org/2001/XInclude' xmlns:e='http://eegg.github.com/htmlx' xmlns:m='http://eegg.github.com/macro'>"

htmlFooter = "</section>"


htmlAssertion [] = []
htmlAssertion ('\n':cs) = "<br />" ++ ind ++ (htmlAssertion rest)
  where indlength = length $ takeWhile (\c -> c == ' ') cs
        ind = "<span class='assertionIndent'> " ++ (join "" (replicate indlength "&#160;")) ++ "</span>"
        rest = dropWhile (\c -> c == ' ') cs
htmlAssertion (a:cs) = a:(htmlAssertion cs)

htmlEscape ('<':cs) = "&lt;" ++ (htmlEscape cs)
htmlEscape ('>':cs) = "&gt;" ++ (htmlEscape cs)
htmlEscape ('&':cs) = "&amp;" ++ (htmlEscape cs)
htmlEscape (c:cs) = c:(htmlEscape cs)
htmlEscape [] = []
