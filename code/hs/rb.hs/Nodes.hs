module Nodes where

data BT = FBT      RT Int RT
        | EBT      BT Int BT
        | LBT Bool RT Int BT -- False left-leaning; True right-leaning
        | ET

data RT = RT       BT Int BT
