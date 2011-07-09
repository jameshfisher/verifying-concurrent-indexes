-- All of the following is ugly
-- Use an AST

module GenerateHTML (html) where

import Data.String.Utils

import Paragraphs

html = htmlWrap . join "\n" . map paragraphToHTML

paragraphToHTML (CodeParagraph s) =
  "<pre class='prettyprint'>" ++ s ++ "</pre>"

paragraphToHTML (CommentParagraph i s) =
  "<div class='section'><pre class='indent'>" ++ (join "" (replicate i "&nbsp;")) ++ "</pre><div class='CommentParagraph'>" ++ (htmlEscape s) ++ "</div><div class='clear'></div></div>"

paragraphToHTML (AssertionParagraph i s) =
  "<div class='section'><pre class='indent'>" ++ (join "" (replicate i "&nbsp;")) ++ "</pre><div class='AssertionParagraph'>" ++ (htmlAssertion (htmlEscape s)) ++ "</div><div class='clear'></div></div>"

paragraphToHTML (CommentedAssertionParagraph (CommentParagraph i s1) (AssertionParagraph j s2)) =
  "<div class='section'><pre class='indent'>" ++ (join "" (replicate i "&nbsp;")) ++ "</pre><div class='AssertionParagraph'>" ++
  "<div class='expl'>" ++ htmlEscape(s1) ++ "</div>" ++ (htmlAssertion (htmlEscape s2)) ++ "</div><div class='clear'></div></div>"
  

htmlWrap s = htmlHeader ++ s ++ htmlFooter

htmlHeader =
 "<!doctype html>\
  \<html>\
    \<head>\
      \<meta charset='utf-8' />\
    \</head>\
    \<link rel='stylesheet' type='text/css' href='google-code-prettify/prettify.css' />\
    \<link rel='stylesheet' type='text/css' href='main.css' />\
    \<script type='text/javascript' src='google-code-prettify/prettify.js'></script>\
    \<script type='text/javascript'>window.onload = function() { prettyPrint(); }</script>\
  \<body>"

htmlFooter = "</body></html>"


htmlAssertion [] = []
htmlAssertion ('\n':cs) = "<br />" ++ ind ++ (htmlAssertion rest)
  where indlength = length $ takeWhile (\c -> c == ' ') cs
        ind = "<span class='assertionIndent'>" ++ (join "" (replicate indlength "&nbsp;")) ++ "</span>"
        rest = dropWhile (\c -> c == ' ') cs
htmlAssertion (a:cs) = a:(htmlAssertion cs)

htmlEscape ('<':cs) = "&lt;" ++ (htmlEscape cs)
htmlEscape ('>':cs) = "&gt;" ++ (htmlEscape cs)
htmlEscape (c:cs) = c:(htmlEscape cs)
htmlEscape [] = []
