{-# LANGUAGE UnicodeSyntax #-}

module DAG where

import Prelude hiding (null)
import Data.Set (Set, empty, insert, (\\), intersection, union, null)

data (Ord a)⇒ DAG a =
    Empty
  | (Set a, Set a) :> (DAG a)

infixr :>

instance (Show a, Ord a)⇒ Show (DAG a) where
  show Empty = ""
  show ((vs,ds) :> sub) = concat [show sub, show vs, ": ", show ds, "\n"]


data VerificationError a =
    NonexistentDependencies (Set a)
  | ReusedLabels (Set a)
  deriving (Show)

verify :: (Ord a)⇒ (DAG a) → Either (VerificationError a) (Set a)
verify g = case g of
  Empty → Right empty
  (vs,ds) :> subdag → do
    subVertices ← verify subdag
    verifyDeps ds subVertices
    verifyLabels vs subVertices
    Right $ union vs subVertices
    where
      verifyDeps ds subVs
        | null nonexistentDependencies = Right ()
        | otherwise                    = Left $ NonexistentDependencies nonexistentDependencies
        where nonexistentDependencies = ds \\ subVs

      verifyLabels labs subLabs
        | null reusedLabels = Right ()
        | otherwise         = Left $ ReusedLabels reusedLabels
        where reusedLabels = labs `intersection` subLabs

