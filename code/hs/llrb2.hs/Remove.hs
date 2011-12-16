{-# LANGUAGE UnicodeSyntax, GADTs #-}

module Remove where

import Nat (Nat, S)
import Nodes (B (..), R (..), RB(..), Tree(..), mkB, blk)



removeMin :: (Nat δ, Ord α)⇒ B (S δ) α → (α, Either (B δ α) (B (S δ) α))
removeMin t = case t of  -- Nil → absurd
  B3 l v r → let (m, l') = removeMinR l in (m, Right $ mkB l' v r)
  B2 l v r → (m, case t' of
    RnotB t'' → Right $ blk t''
    BnotR t'' → Left  $     t'') where (m, t') = removeMinR (R l v r)
  where
    removeMinR :: (Nat δ, Ord α)⇒ R δ α → (α, RB δ α)
    removeMinR (R l v r) = case l of
      Nil         → (v, BnotR Nil)
      B3 ll lv lr → (m, RnotB $ R (mkB ll' lv lr) v r) where (m, ll') = removeMinR ll
      B2 ll lv lr → case r of  -- Nil → absurd
        B3 (R rll rlv rlr) rv rr → let (m, t') = removeMinR (R ll lv lr) in (m, RnotB $ R (mkB t' v rll) rlv (B2 rlr rv rr))
        B2 rl              rv rr → let (m, t') = removeMinR (R ll lv lr) in (m, case t' of
          RnotB t'' → RnotB $ R  (blk t'')    v  r
          BnotR t'' → BnotR $ B3 (R t'' v rl) rv rr)


removeR :: (Nat δ, Ord α)⇒  R δ α → α → RB δ α
removeR t x = case t of
  R Nil        v Nil → case compare x v of { LT → RnotB t; EQ → BnotR Nil; GT → RnotB t }
  R (B2 _ _ _) _ _   → removeR' t x
  R (B3 _ _ _) _ _   → removeR' t x
  where
    removeR' :: (Nat δ, Ord α)⇒  R (S δ) α → α → RB (S δ) α
    removeR' (R l v r) x = case compare x v of
      LT → balRL l' v r  where     l'  = removeB l x
      EQ → balRR l  m r' where (m, r') = removeMin r
      GT → balRR l  v r' where     r'  = removeB r x

    balRR :: (Nat δ, Ord α)⇒ B (S δ) α → α → Either (B δ α) (B (S δ) α) → RB (S δ) α
    balRR l v r = case r of
      Right r' → RnotB $ R l v r'
      Left  r' → case l of
        B2 ll lv lr → BnotR $ B3 (R ll lv lr)  v   r'
        B3 ll lv lr → RnotB $ R  (blk ll)      lv  (B2 lr v r')

    balRL :: (Nat δ, Ord α)⇒ Either (B δ α) (B (S δ) α) → α → B (S δ) α → RB (S δ) α
    balRL l v r = case l of
      Right l' → RnotB $ R l' v r
      Left  l' → case r of
        B2 rl              rv rr → BnotR $ B3 (R  l'  v   rl)  rv  rr
        B3 (R rll rlv rlr) rv rr → RnotB $ R  (B2 l'  v   rll) rlv (B2 rlr rv rr)


removeB :: (Nat δ, Ord α)⇒ B (S δ) α → α → Either (B δ α) (B (S δ) α)
removeB t x = case t of
  B2 _ _ (B2 _ _ _) → removeB' t x
  B2 _ _ (B3 _ _ _) → removeB' t x
  B3 _ _ (B2 _ _ _) → removeB' t x
  B3 _ _ (B3 _ _ _) → removeB' t x
  _                 → case t of
    B2 l v r → case removeR (R l v r) x of
      RnotB (R l v r) → Right $ B2 l v r
      BnotR t         → Left t
    B3 l@(R Nil lv Nil) v Nil → case compare x v of
      LT → Right $ mkB (removeR l x) v Nil
      EQ → Right $ B2 Nil lv Nil
      GT → Right t
  where
    removeB' :: (Nat δ, Ord α)⇒ B (S (S δ)) α → α → Either (B (S δ) α) (B (S (S δ)) α)
    removeB' t x = case t of  -- Nil → absurd
      B2 l v r → case removeR (R l v r) x of
        RnotB (R l v r) → Right $ B2 l v r
        BnotR t         → Left t
      B3 l@(R ll lv lr) v r → case compare x v of
        LT → Right $ mkB (removeR l x) v r
        EQ → balBR l m r' where (m, r') = removeMin r
        GT → balBR l v r' where     r'  = removeB r x
        where
          balBR l@(R ll lv lr) v r = case r of
            Right r' → Right $ B3 l v r'
            Left  r' → case lr of
              B2 lrl                lrv lrr → Right $ B2 ll lv (B3 (R lrl lrv lrr) v r')
              B3 (R lrll lrlv lrlr) lrv lrr → Right $ B3 (R ll lv (B2 lrll lrlv lrlr)) lrv (B2 lrr v r')


-- the end-user function.
removeT :: (Ord α)⇒ Tree α → α → Tree α
removeT (Tree Nil) v = Tree Nil
removeT (Tree t@(B2 _ _ _)) v = case removeB t v of { Left  t' → Tree t'; Right t' → Tree t' }
removeT (Tree t@(B3 _ _ _)) v = case removeB t v of { Left  t' → Tree t'; Right t' → Tree t' }