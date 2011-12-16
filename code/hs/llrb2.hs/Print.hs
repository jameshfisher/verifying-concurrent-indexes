{-# LANGUAGE UnicodeSyntax #-}

module Print (Str(..), indent, red, sh) where

class (Show a)⇒ Str a where
  str ∷ a → Int → String → String

indent ∷ Int → String
indent = join . indentN where
  indentN = (flip replicate) "  "
  join = foldl (++) ""

ansi_wrap ∷ String → String → String
ansi_wrap c s = (ansi c) ++ s ++ ansi_reset where
  ansi code = '\27':'[':(code++"m")
  ansi_reset = ansi "0"

red ∷ String → String
red = ansi_wrap "31"

sh ∷ (Str a, Str b) ⇒ a → b → Int → String → String
sh l r i pre = (str r (i+1) "╭→") ++ (indent i) ++ pre ++ "\n" ++ (str l (i+1) "╰→")
