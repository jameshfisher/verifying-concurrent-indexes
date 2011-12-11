{-# LANGUAGE UnicodeSyntax, GADTs #-}

module Remove where

import Nat (Nat, S, O)
import Nodes (B (..), R (..), RB(..), Tree(..))

mkB :: (Nat δ, Ord α)⇒ RB δ α → α → B δ α → B (S δ) α
mkB l v r = case l of
  RnotB l' → B3 l' v r
  BnotR l' → B2 l' v r



removeMinR :: (Nat δ, Ord α)⇒ R δ α → (α, RB δ α)
removeMinR (R l v r) = case l of
  Nil → (v, BnotR Nil)
  B3 ll lv lr → let (m, ll') = removeMinR ll in (m, RnotB $ R (mkB ll' lv lr) v r)
  B2 ll lv lr → case r of
    -- Nil → absurd
    B3 (R rll rlv rlr) rv rr → let (m, t') = removeMinR (R ll lv lr) in (m, RnotB $ R (mkB t' v rll) rlv (B2 rlr rv rr))
    B2 rl rv rr → let (m, t') = removeMinR (R ll lv lr) in (m, case t' of
      RnotB (R ll' lv' lr') → RnotB $ R (B2 ll' lv' lr') v r
      BnotR t'' → BnotR $ B3 (R t'' v rl) rv rr)

removeMinB :: (Nat δ, Ord α)⇒ B (S δ) α → (α, Either (B δ α) (B (S δ) α))
-- removeMinB Nil = absurd
removeMinB (B3 l v r) = let (m, l') = removeMinR l in (m, Right $ mkB l' v r)
removeMinB (B2 l v r) = let (m, t') = removeMinR (R l v r) in case t' of
  RnotB (R rl rv rr) → (m, Right $ B2 rl rv rr)
  BnotR t''          → (m, Left $ t'')



-- ANNOYING:
-- GHC doesn't match δ and (S δ) if removeR is defined on its own.
-- We have to define separate functions for δ and (S δ).
removeR :: (Nat δ, Ord α)⇒  R δ α → α → RB δ α
removeR t@(R Nil        _ _) = removeRbottom t
removeR t@(R (B2 _ _ _) _ _) = removeRtop t
removeR t@(R (B3 _ _ _) _ _) = removeRtop t


removeRbottom :: (Ord α)⇒ R O α → α → RB O α
removeRbottom t@(R Nil v Nil) x = case compare x v of
  LT → RnotB t
  EQ → BnotR Nil
  GT → RnotB t

removeRtop :: (Nat δ, Ord α)⇒  R (S δ) α → α → RB (S δ) α
removeRtop t@(R l v r) x = case compare x v of
  LT → case removeB l x of
    Right l' → RnotB $ R l' v r
    Left  l' → case r of
      B2 rl              rv rr → BnotR $ B3 (R  l'  v   rl)  rv  rr
      B3 (R rll rlv rlr) rv rr → RnotB $ R  (B2 l'  v   rll) rlv (B2 rlr rv rr)
  EQ → let (m, r') = removeMinB r in case r' of -- replace occurrences of v with m
    Right r'' → RnotB $ R l m r''
    Left  r'' → case l of
      B2 ll              lv lr → BnotR $ B3 (R  ll  lv  lr)  m   r''
      B3 (R lll llv llr) lv lr → RnotB $ R  (B2 lll llv llr) lv  (B2 lr m r'')
  GT → case removeB r x of  -- same as above, v not m
    Right r' → RnotB $ R l v r'
    Left  r' → case l of
      B2 ll              lv lr → BnotR $ B3 (R  ll  lv  lr)  v   r'
      B3 (R lll llv llr) lv lr → RnotB $ R  (B2 lll llv llr) lv  (B2 lr v r')


-- same problem as with removeR
removeB :: (Nat δ, Ord α)⇒ B (S δ) α → α → Either (B δ α) (B (S δ) α)
removeB t@(B2 _ _ (B2 _ _ _)) = removeBh2 t
removeB t@(B2 _ _ (B3 _ _ _)) = removeBh2 t
removeB t@(B3 _ _ (B2 _ _ _)) = removeBh2 t
removeB t@(B3 _ _ (B3 _ _ _)) = removeBh2 t
removeB t = removeBh1 t


removeBh1 :: (Nat δ, Ord α)⇒ B (S δ) α → α → Either (B δ α) (B (S δ) α)
-- removeBh1 Nil _ = absurd pattern
removeBh1 (B2 l v r) x = case removeR (R l v r) x of
  RnotB (R l v r) → Right $ B2 l v r
  BnotR t         → Left t
removeBh1 t@(B3 l@(R Nil lv Nil) v Nil) x = case compare x v of
  LT → Right $ mkB (removeRbottom l x) v Nil
  EQ → Right $ B2 Nil lv Nil
  GT → Right t


removeBh2 :: (Nat δ, Ord α)⇒ B (S (S δ)) α → α → Either (B (S δ) α) (B (S (S δ)) α)
-- removeBh2 Nil _ = absurd pattern
removeBh2 (B2 l v r) x = case removeR (R l v r) x of
  RnotB (R l v r) → Right $ B2 l v r
  BnotR t         → Left t

removeBh2 (B3 l@(R ll lv lr) v r) x = case compare x v of
  LT → Right $ mkB (removeR l x) v r
  EQ → let (m, r') = removeMinB r in case r' of  -- replace v with m
    Right r' → Right $ B3 l m r'
    Left  r' → case lr of  -- dropped height
      B2 lrl                lrv lrr → Right $ B2 ll lv (B3 (R lrl lrv lrr) m r')
      B3 (R lrll lrlv lrlr) lrv lrr → Right $ B3 (R ll lv (B2 lrll lrlv lrlr)) lrv (B2 lrr m r')
  GT → case removeB r x of  -- as above, v not m
    Right r' → Right $ B3 l v r'
    Left  r' → case lr of  -- dropped height
      B2 lrl                lrv lrr → Right $ B2 ll lv (B3 (R lrl lrv lrr) v r')
      B3 (R lrll lrlv lrlr) lrv lrr → Right $ B3 (R ll lv (B2 lrll lrlv lrlr)) lrv (B2 lrr v r')


-- the end-user function.
removeT :: (Ord α)⇒ Tree α → α → Tree α
removeT (Tree Nil) v = Tree Nil
removeT (Tree t@(B2 _ _ _)) v = case removeB t v of
  Left  t' → Tree t'
  Right t' → Tree t'
removeT (Tree t@(B3 _ _ _)) v = case removeB t v of
  Left  t' → Tree t'
  Right t' → Tree t'