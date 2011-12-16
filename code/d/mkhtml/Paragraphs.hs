{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}

module Paragraphs (paragraphs, Paragraph(CodeParagraph, CommentParagraph, AssertionParagraph)) where

import Data.Text (Text, intercalate)

import LineTypes

data Paragraph =
    CodeParagraph           !Text
  | CommentParagraph   !Int !Text
  | AssertionParagraph !Int !Text

-- mostly a fold, but the folder can give up
semifoldl :: (a → b → Either a (a,a)) → a → [b] → [a]
semifoldl f = fold where
  fold a [] = []
  fold a (b:bs) = case f a b of
    Left   a      → fold m xs
    Right (a1,a2) → a1:(fold a2 bs)

-- Start a new paragraph
newParagraph :: TypedLine → Paragraph
newParagraph l = case l of
  CommentLine   i s → CommentParagraph   i s
  AssertionLine i s → AssertionParagraph i s
  CodeLine        s → CodeParagraph        s

nl :: Text → Text → Text
nl s1 s2 = intercalate "\n" [s1, s2]

reduceLine :: Paragraph → TypedLine → (Paragraph, Maybe Paragraph)
reduceLine para@(CommentParagraph i s1) (CommentLine i' s2)
  | i == i'   = (CommentParagraph i (nl s1 s2), Nothing                      )
  | otherwise = (para,                          Just $ CommentParagraph i' s2)

reduceLine para@(AssertionParagraph i s1) (AssertionLine i' s2)
  | i == i'   = (AssertionParagraph i (nl s1 s2), Nothing                        )
  | otherwise = (para,                            Just $ AssertionParagraph i' s2)

reduceLine (CodeParagraph s1) (CodeLine s2) = (CodeParagraph (nl s1 s2), Nothing)

reduceLine a b = (a, Just $ newParagraph b)


-- ugliness: appends to head of list of paras
-- Use Data.Sequence?
reducePara :: [Paragraph] → TypedLine → [Paragraph]
reducePara [] l = [newParagraph l]
reducePara (p:ps) l = newPs
  where (current, next) = reduceLine p l
        newPs = case next of
          Nothing       → current:ps
          Just newPara  → newPara:current:ps

reduceLinesRev :: [TypedLine] → [Paragraph]
reduceLinesRev ls = foldl reducePara [] ls

paragraphs :: [TypedLine] → [Paragraph]
paragraphs = reverse . reduceLinesRev
