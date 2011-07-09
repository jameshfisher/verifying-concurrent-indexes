module Paragraphs (paragraphs, Paragraph(CodeParagraph, CommentParagraph, AssertionParagraph, CommentedAssertionParagraph)) where

import LineTypes

data Paragraph =
    CodeParagraph String
  | CommentParagraph Int String
  | AssertionParagraph Int String
  | CommentedAssertionParagraph Paragraph Paragraph


-- Start a new paragraph
newParagraph (CommentLine i s) = (CommentParagraph i s)
newParagraph (AssertionLine i s) = (AssertionParagraph i s)
newParagraph (CodeLine s) = (CodeParagraph s)


-- Either merges line with current paragraph, or ends it and begins a new one
reduceLine :: Paragraph -> TypedLine -> (Paragraph, Maybe Paragraph)

-- Comments merged if at same indentation
reduceLine (CommentParagraph i s1) (CommentLine j s2) =
  if i == j
    then ((CommentParagraph i (s1 ++ "\n" ++ s2)), Nothing)
    else ((CommentParagraph i s1), Just (CommentParagraph j s2))

reduceLine (AssertionParagraph i s1) (AssertionLine j s2) =
  if i == j
    then ((AssertionParagraph i (s1 ++ "\n" ++ s2)), Nothing)
    else ((AssertionParagraph i s1), Just (AssertionParagraph j s2))

reduceLine a@(CodeParagraph s1) b@(CodeLine s2) =
  ((CodeParagraph (s1 ++ "\n" ++ s2)), Nothing)

reduceLine a b = (a, Just (newParagraph b))


-- ugliness: appends to head of list of paras
-- not sure how to avoid
reducePara :: [Paragraph] -> TypedLine -> [Paragraph]
reducePara [] l = [newParagraph l]
reducePara (p:ps) l = newPs
  where (current, next) = reduceLine p l
        newPs = case next of
          Nothing       -> current:ps
          Just newPara  -> newPara:current:ps

reduceLinesRev :: [TypedLine] -> [Paragraph]
reduceLinesRev ls = foldl reducePara [] ls


-- if a CommentParagraph immediately precedes an AssertionParagraph,
-- merge into CommentedAssertionParagraph
assertionComments :: [Paragraph] -> [Paragraph]
assertionComments (a@(CommentParagraph _ _):b@(AssertionParagraph _ _):rest) =
  (CommentedAssertionParagraph a b):(assertionComments rest)
assertionComments (a:rest) = a:(assertionComments rest)
assertionComments [] = []


paragraphs = assertionComments . reverse . reduceLinesRev
