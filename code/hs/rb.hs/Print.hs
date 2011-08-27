module Print where

import Nodes

class Str a where
  str :: a -> Int -> String -> String

indent 0 = ""
indent n = "  " ++ indent (n-1)

sh l r i pre = (str r (i+1) "╭→") ++ (indent i) ++ pre ++ "\n" ++ (str l (i+1) "╰→")

instance Str BT where
  str (EBT v       l r) i pre = sh l r i (pre ++ show v)
  str (FBT v       l r) i pre = sh l r i (pre ++ show v)
  str (LBT v False l r) i pre = sh l r i (pre ++ show v)
  str (LBT v True  r l) i pre = sh l r i (pre ++ show v)
  str  ET               i pre = ""

ansi c = ['\27'] ++ "[" ++ c ++ "m"

instance Str RT where
  str (RT  v l r)       i pre = sh l r i (pre ++ (ansi "31") ++ (show v) ++ (ansi "0"))

instance Show BT where
  show b = str b 0 "─→"
instance Show RT where
  show r = str r 0 "─→"
