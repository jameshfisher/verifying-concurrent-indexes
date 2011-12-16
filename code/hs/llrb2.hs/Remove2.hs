{-# LANGUAGE UnicodeSyntax, GADTs #-}

module Remove2 where

import Nat (Nat, S)
import Nodes (Nil(..), B2(..), B3(..), B(..), R(..), RB(..), Tree(..), mkB, blk, nil, b2, b3)

-- Create datatype for double-blacked root?


data (Nat δ, Ord α)⇒ B3orNil δ α = B3notNil (B3 δ α) | NilnotB3 (Nil δ α)

b3orNil2B :: (Nat δ, Ord α)⇒ B3orNil δ α → B δ α
b3orNil2B (NilnotB3 t) = Nil' t
b3orNil2B (B3notNil t) = B3'  t

mkB' :: (Nat δ, Ord α)⇒ Either (B3orNil δ α) (R δ α) → α → B δ α → B (S δ) α
mkB' (Right l) v r = B3' $ B3 l             v r
mkB' (Left  l) v r = B2' $ B2 (b3orNil2B l) v r

data (Nat δ, Ord α)⇒ B3orB2 δ α = B3notB2 (B3 δ α) | B2notB3 (B2 δ α)

b3orB22B (B3notB2 t) = B3' t
b3orB22B (B2notB3 t) = B2' t

data (Nat δ, Ord α)⇒ B3orR δ α = B3notR (B3 δ α) | RnotB3 (R δ α)


removeMin :: (Nat δ, Ord α)⇒ B3orB2 (S δ) α → (α, Either (B3orNil δ α) (B3orB2 (S δ) α))
removeMin t = case t of
  B3notB2 (B3 l v r) → (m, t) where
    (m, l') = removeMinR l
    t = case l' of
      Right l'' → Right $ B3notB2 $ B3 l'' v r
      Left  l'' → Right $ B2notB3 $ B2 (b3orNil2B l'') v r
  B2notB3 (B2 l v r) → (m, t) where
    (m, t') = removeMinR (R l v r)
    t = case t' of
      Right t'' → Right $ B2notB3 $ blk t''
      Left  t'' → Left  $               t''
  where
    removeMinR  :: (Nat δ, Ord α)⇒ R δ α → (α, Either (B3orNil δ α) (R δ α))
    removeMinR (R l v r) = case l of
      Nil' Nil          → (v, Left $ NilnotB3 Nil)
      B3' (B3 ll lv lr) → (m, Right $ R (mkB' ll' lv lr) v r) where (m, ll') = removeMinR ll
      B2' (B2 ll lv lr) → case r of  -- Nil → absurd
        B3' (B3 (R rll rlv rlr) rv rr) → let (m, t') = removeMinR (R ll lv lr) in (m, Right $ R (mkB' t' v rll) rlv (b2 rlr rv rr))
        B2' (B2 rl              rv rr) → let (m, t') = removeMinR (R ll lv lr) in (m, case t' of
          Right t'' → Right $ R  (B2' $ blk t'')    v  r
          Left  t'' → Left  $ B3notNil $ B3 (R (b3orNil2B t'') v rl) rv rr)


-- Either (B δ α) (B (S δ) α)
balRR :: (Nat δ, Ord α)⇒ B3orB2 (S δ) α → α → Either (B3orNil δ α) (B3orB2 (S δ) α) → B3orR (S δ) α
balRR l v r = case r of
  Right r' → RnotB3 $ R (b3orB22B l) v (b3orB22B r')
  Left  r' → case l of
    B2notB3 (B2 ll lv lr) → B3notR $ B3 (R ll lv lr)   v   (b3orNil2B r')
    B3notB2 (B3 ll lv lr) → RnotB3 $ R  (B2' $ blk ll) lv  (B2' $ B2 lr v (b3orNil2B r'))


balRL :: (Nat δ, Ord α)⇒ Either (B3orNil δ α) (B3orB2 (S δ) α) → α → B3orB2 (S δ) α → B3orR (S δ) α
balRL l v r = case l of
  Right l' → RnotB3 $ R (b3orB22B l') v (b3orB22B r)
  Left  l' → case r of
    B2notB3 (B2 rl              rv rr) → B3notR $ B3 (R  (b3orNil2B l')  v   rl)  rv  rr
    B3notB2 (B3 (R rll rlv rlr) rv rr) → RnotB3 $ R  (b2 (b3orNil2B l')  v   rll) rlv (b2 rlr rv rr)


bToB3orB2 :: (Nat δ, Ord α)⇒ B (S δ) α → B3orB2 (S δ) α
-- bToB3orB2 (Nil' t) = absurd
bToB3orB2 (B3' t) = B3notB2 t
bToB3orB2 (B2' t) = B2notB3 t

removeR' :: (Nat δ, Ord α)⇒  R (S δ) α → α → B3orR (S δ) α
removeR' (R l v r) x = case compare x v of
  LT → balRL l'' v r'  where l'' = removeB l' x
  EQ → balRR l'  m r'' where (m, r'') = removeMin r'
  GT → balRR l'  v r'' where r'' = removeB r' x
  where
    l' = bToB3orB2 l
    r' = bToB3orB2 r

data Either3 a b c = Case1 a | Case2 b | Case3 c

removeRbot :: (Nat δ, Ord α)⇒  R δ α → α → Either (Nil δ α) (R δ α)
removeRbot t@(R (Nil' l) v _) x = case compare x v of { EQ → Left l; _ → Right t }

removeRnotbot :: (Nat δ, Ord α)⇒  R (S δ) α → α → Either (B3 (S δ) α) (R (S δ) α)
removeRnotbot t@(R (B2'  _) _ _) x = case removeR' t x of { B3notR t' → Left t'; RnotB3 t' → Right t' }
removeRnotbot t@(R (B3'  _) _ _) x = case removeR' t x of { B3notR t' → Left t'; RnotB3 t' → Right t' }

removeR :: (Nat δ, Ord α)⇒  R δ α → α → Either3 (Nil δ α) (B3 δ α) (R δ α)
removeR t x = case t of
   R (Nil' _) _ _ → case removeRbot    t x of { Left t' → Case1 t'; Right t' → Case3 t' }
   R (B2'  _) _ _ → case removeRnotbot t x of { Left t' → Case2 t'; Right t' → Case3 t' }
   R (B3'  _) _ _ → case removeRnotbot t x of { Left t' → Case2 t'; Right t' → Case3 t }

--   where
--     removeR' :: (Nat δ, Ord α)⇒  R (S δ) α → α → RB (S δ) α
--     removeR' (R l v r) x = case compare x v of
--       LT → balRL l' v r  where     l'  = removeB l x
--       EQ → balRR l  m r' where (m, r') = removeMin r
--       GT → balRR l  v r' where     r'  = removeB r x

--     balRR :: (Nat δ, Ord α)⇒ B (S δ) α → α → Either (B δ α) (B (S δ) α) → RB (S δ) α
--     balRR l v r = case r of
--       Right r' → RnotB $ R l v r'
--       Left  r' → case l of
--         B2 ll lv lr → BnotR $ B3 (R ll lv lr)  v   r'
--         B3 ll lv lr → RnotB $ R  (blk ll)      lv  (B2 lr v r')

--     balRL :: (Nat δ, Ord α)⇒ Either (B δ α) (B (S δ) α) → α → B (S δ) α → RB (S δ) α
--     balRL l v r = case l of
--       Right l' → RnotB $ R l' v r
--       Left  l' → case r of
--         B2 rl              rv rr → BnotR $ B3 (R  l'  v   rl)  rv  rr
--         B3 (R rll rlv rlr) rv rr → RnotB $ R  (B2 l'  v   rll) rlv (B2 rlr rv rr)

removeB :: (Nat δ, Ord α)⇒ B3orB2 (S δ) α → α → Either (B3orNil δ α) (B3orB2 (S δ) α)
removeB = undefined