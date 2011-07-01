-- {-# OPTIONS --no-coverage-check #-}
-- {-# OPTIONS --universe-polymorphism #-}

open import Data.Bool using (Bool; true; false; if_then_else_)
open import Data.List
open import Data.Nat hiding (_≤_; _<_; _≟_; compare)
open import Data.Product
open import Data.Unit hiding (_≟_)

open import Level

open import Relation.Binary hiding (_⇒_)
open import Relation.Nullary

module LLRBTree2 
  (order : StrictTotalOrder Level.zero Level.zero Level.zero) where

ℓ : Level
ℓ = Level.zero

Setℓ = Set -- Set ℓ

{- Level-polymorphic
module LLRBTree2 
  {ℓ : Level}
  (order : StrictTotalOrder ℓ ℓ ℓ) where

-- Data.Unit.⊤ is not universe-polymorphic
record ⊤ {a : Level} : Set a where
  constructor tt
-}

{- TOO much trouble
module LLRBTree2 
  {c ℓ₁ ℓ₂ : Level}
  (order : StrictTotalOrder c ℓ₁ ℓ₂) where
-}

open module sto = StrictTotalOrder order

A : Setℓ
A = StrictTotalOrder.Carrier order

BoundsL = List A
BoundsR = List A

infix 5 _isleftof_
_isleftof_ : A → BoundsR → Setℓ
z isleftof []     = ⊤
z isleftof b ∷ β  = z < b × z isleftof β

infix 5 _isrightof_
_isrightof_ : A → BoundsL → Setℓ
z isrightof []     = ⊤
z isrightof b ∷ γ  = b < z × z isrightof γ

infix 5 _⇒ʳ_
data _⇒ʳ_ : BoundsR → BoundsR → Setℓ where
  ∎      : ∀ {γ} → γ ⇒ʳ γ
  keep_  : ∀ {γ γ' b} → γ ⇒ʳ γ' → b ∷ γ ⇒ʳ b ∷ γ'
  skip_  : ∀ {γ γ' b} → γ ⇒ʳ γ' → b ∷ γ ⇒ʳ γ'
  cover_,_  : ∀ {β β' x y} → x < y → x ∷ y ∷ β ⇒ˡ β'
         → x ∷ β ⇒ˡ β'

infix 5 _⇒ˡ_
data _⇒ˡ_ : BoundsL → BoundsL → Setℓ where
  ∎      : ∀ {β} → β ⇒ˡ β
  keep_  : ∀ {β β' b} → β ⇒ˡ β' → b ∷ β ⇒ˡ b ∷ β'
  skip_  : ∀ {β β' b} → β ⇒ˡ β' → b ∷ β ⇒ˡ β'
  cover_,_  : ∀ {γ γ' x y} → x < y → y ∷ x ∷ γ ⇒ʳ γ'
         → y ∷ γ ⇒ʳ γ'

⟦_⟧ˡ : ∀ {β β'} → β ⇒ˡ β' → (x : A) → x isleftof β → x isleftof β'
⟦ ∎          ⟧ˡ z p              = p
⟦ keep h     ⟧ˡ z (p₁ , p₂)      = p₁ , ⟦ h ⟧ˡ z p₂
⟦ skip h     ⟧ˡ z (_  , p)       = ⟦ h ⟧ˡ z p
⟦ cover  q , h ⟧ˡ z (p₁ , p)       = ⟦ h ⟧ˡ z (p₁ , trans p₁ q , p)

⟦_⟧ʳ : ∀ {γ γ'} → γ ⇒ʳ γ' → (x : A) → x isrightof γ → x isrightof γ'
⟦ ∎          ⟧ʳ z p              = p
⟦ keep h     ⟧ʳ z (p₁ , p₂)      = p₁ , ⟦ h ⟧ʳ z p₂
⟦ skip h     ⟧ʳ z (_  , p)       = ⟦ h ⟧ʳ z p
⟦ cover  q , h ⟧ʳ z (p₁ , p)       = ⟦ h ⟧ʳ z (p₁ , trans q p₁ , p)

------------------------------------------------------------------------

data Color : Set where
  red   : Color
  black : Color

data Tree' (β : BoundsL) (γ : BoundsR) : Color → ℕ → Setℓ where
  lf : Tree' β γ black 0
  nr : ∀ {n}(a : A) → a isleftof β → a isrightof γ
     → Tree' (a ∷ β) γ black n → Tree' β (a ∷ γ) black n
     → Tree' β γ red n
  nb : ∀ {leftSonColor n}(a : A) → a isleftof β → a isrightof γ
     → Tree' (a ∷ β) γ leftSonColor n → Tree' β (a ∷ γ) black n
     → Tree' β γ black (suc n)

infixl 3 _◁_
_◁_ : ∀ {β β' γ c n} → Tree' β γ c n → β ⇒ˡ β' → Tree' β' γ c n
lf          ◁ φ = lf
nr x pxl pxr l r ◁ φ = nr x (⟦ φ ⟧ˡ x pxl) pxr (l ◁ keep φ) (r ◁ φ)
nb x pxl pxr l r ◁ φ = nb x (⟦ φ ⟧ˡ x pxl) pxr (l ◁ keep φ) (r ◁ φ)

infixl 3 _◀_
_◀_ : ∀ {β γ γ' c n} → Tree' β γ c n → γ ⇒ʳ γ' → Tree' β γ' c n
lf          ◀ φ = lf
nr x pxl pxr l r ◀ φ = nr x pxl (⟦ φ ⟧ʳ x pxr) (l ◀ φ) (r ◀ keep φ)
nb x pxl pxr l r ◀ φ = nb x pxl (⟦ φ ⟧ʳ x pxr) (l ◀ φ) (r ◀ keep φ)

-- for saving t.c. time, replace deleteR by axiom

{-

postulate
  deleteR : ∀ {n β γ} → A → Tree' β γ red n → ∃ λ c' → Tree' β γ c' n
-}


extractMinR : ∀ {n β γ} → Tree' β γ red n → ∃₂ λ min c → min isleftof β × min isrightof γ × Tree' β (min ∷ γ) c n

{-
   (a)       -->  .
 -}
extractMinR (nr a pal par lf lf) = a , black , pal , par , lf

{-
         (c)
      [b]  t4    jump to (a)
   (a)
  t1 t2 t3
 -}
extractMinR (nr c pcl pcr (nb b (b<c , pb) pbr (nr a pal par t1 t2) t3) t4)
  with extractMinR (nr a pal par t1 t2)
... | x , c' , (x<b , x<c , pxl) , pxr , ta' = x , red , pxl , pxr , nr c pcl (x<c , pcr) (nb b (b<c , pb) (x<b , pbr) (ta') (t3 ◀ cover x<b , ∎)) (t4 ◀ cover x<c , ∎)

{-
     (b)            (c)
  [a]   [d]  -->  [b] [d]
      (c)
 -}
extractMinR (nr b pbl pbr (nb a (a<b , pal) par lf lf) (nb d pdl (b<d , pdr) (nr c (c<d , pcl) (b<c , pcr) lf lf) lf)) =
   a , red , pal , par , nr c pcl (trans a<b b<c , pcr) (nb b (b<c , pbl) (a<b , pbr) lf lf) (nb d pdl (c<d , trans a<b b<d , pdr) lf lf)

{-
     (b)             [d]
  [a]   [d]  -->  (b)
 -}
extractMinR (nr b pbl pbr (nb a (a<b , pal) par lf lf) (nb d pdl (b<d , pdr) lf lf)) =
   a , black , pal , par , nb d pdl (trans a<b b<d , pdr) (nr b (b<d , pbl) (a<b , pbr) lf lf) lf

{-
      (b)                (c)
  [a]     [d]  -->    [b]   [d]
        (c)        (a)
 t1 t2 t3 t4 t5   t1 t2 t3 t4 t5     Note: t1 is black
-}
extractMinR (nr b pbl pbr (nb a (a<b , pal) par (nb x1 px1l px1r t1l t1r) t2)
                    (nb d pdl (b<d , pdr) (nr c (c<d , pcl) (b<c , pcr) t3 t4) t5))
  with extractMinR (nr a (a<b , pal) par (nb x1 px1l px1r t1l t1r) t2)
... | x , c' , (x<b , pxl) , pxr , ta' = x , red , pxl , pxr , nr c pcl (trans x<b b<c , pcr)
    (nb b (b<c , pbl) (x<b , pbr) (ta' ◁ cover b<c , ∎) (t3 ◁ keep skip ∎ ◀ cover x<b , ∎))
    (nb d pdl (c<d , trans x<b b<d , pdr) (t4 ◀ keep cover x<b , skip ∎) (t5 ◀ cover c<d , keep keep cover x<b , skip ∎))

{-
      (b)               [b]
  [a]    [d]  -->    (a)   (d)
 t1 t2  t3 t4      t1 t2  t3 t4      Note: t1,t3 are black

case 1:  extractMinR a  returns black t1' (not a leaf!) : left rotate

    [b]              [d]
  t1'   (d)  -->  (b)
       t3 t4    t1' t3  t4

case 2:  extractMinR a  returns red a':   color flip

       [b]                (b)
    (a')    (d)   --> [a']    [d]
  t1' t2'  t3 t4     t1' t2' t3 t4

-}
extractMinR (nr b pbl pbr (nb a (a<b , pal) par (nb x1 px1l px1r t1l t1r) t2) (nb d pdl (b<d , pdr) (nb x3 (x3<d , px3l) (b<x3 , px3r) t3l t3r) t4))
  with extractMinR (nr a (a<b , pal) par (nb x1 px1l px1r t1l t1r) t2)
... | x , black , (x<b , pxl) , pxr , nb x1' (x1'<b , px1'l) (x<x1' , px1'r) t1l' t1r' = x , black , pxl , pxr ,
  nb d pdl (trans x<b b<d , pdr)
    (nr b (b<d , pbl) (x<b , pbr)
      (nb x1' (x1'<b , px1'l) (x<x1' , px1'r) t1l' t1r' ◁ cover b<d , ∎)
      (nb x3 (x3<d , px3l) (b<x3 , px3r) t3l t3r ◀ cover x<b , ∎))
    (t4 ◀ keep cover x<b , skip ∎)
... | x , red , (x<b , pxl) , pxr , nr a' pal' par' t1' t2' =
      x , red , pxl , pxr , nr b pbl (x<b , pbr) (nb a' pal' par' t1' t2') (nb d pdl (b<d , pdr) (nb x3 (x3<d , px3l) (b<x3 , px3r) t3l t3r) t4 ◀ cover x<b , ∎)

mutual

  deleteR : ∀ {n β γ} → A → Tree' β γ red n → ∃ λ c' → Tree' β γ c' n

  deleteR .{0} x (nr a pal par lf lf) with x ≟ a
  ... | yes _ = , lf
  ... | no  _ = , nr a pal par lf lf

  deleteR .{1} x (nr b pbl pbr (nb a (a<b , pal) par lf lf) (nb c pcl (b<c , pcr) lf lf)) with x ≟ a
  ... | yes _ = , nb c pcl pcr (nr b (b<c , pbl) pbr lf lf) lf
  ... | no  _ with x ≟ b
  ... | yes _ = , nb c pcl pcr (nr a (trans a<b b<c , pal) par lf lf) lf
  ... | no  _ with x ≟ c
  ... | yes _ = , nb b pbl pbr (nr a (a<b , pal) par lf lf) lf
  ... | no  _ = , nr b pbl pbr (nb a (a<b , pal) par lf lf) (nb c pcl (b<c , pcr) lf lf)

  -- 1.5
  deleteR .{1} x (nr b pbl pbr (nb a (a<b , pal) par lf lf) (nb d pdl (b<d , pdr) (nr c (c<d , pcl) (b<c , pcr) lf lf) lf)) with x ≟ a
  ... | yes _ = , nr c pcl pcr (nb b (b<c , pbl) pbr lf lf) (nb d pdl (c<d , pdr) lf lf)
  ... | no  _ with x ≟ b
  ... | yes _ = , nr c pcl pcr (nb a (trans a<b b<c , pal) par lf lf) (nb d pdl (c<d , pdr) lf lf)
  ... | no  _ with x ≟ c
  ... | yes _ = , nr b pbl pbr (nb a (a<b , pal) par lf lf) (nb d pdl (trans b<c c<d , pdr) lf lf)
  ... | no  _ = , nr b pbl pbr (nb a (a<b , pal) par lf lf) (nb c pcl (b<c , pcr) lf lf)

  -- 1.6
  deleteR .{1} x (nr c pcl pcr (nb b (b<c , pbl) pbr (nr a (a<b , a<c , pal) par lf lf) lf) (nb d pdl (c<d , pdr) lf lf)) with  x ≟ a
  ... | yes _ = , nr c pcl pcr (nb b (b<c , pbl) pbr lf lf) (nb d pdl (c<d , pdr) lf lf)
  ... | no  _ with x ≟ b
  ... | yes _ = , nr c pcl pcr (nb a (trans a<b b<c , pal) par lf lf) (nb d pdl (c<d , pdr) lf lf)
  ... | no  _ with x ≟ c
  ... | yes _ = , nr b pbl pbr (nb a (a<b , pal) par lf lf) (nb d pdl (trans b<c c<d , pdr) lf lf)
  ... | no  _ = , nr b pbl pbr (nb a (a<b , pal) par lf lf) (nb c pcl (b<c , pcr) lf lf)

  deleteR .{1} x (nr c pcl pcr (nb b (b<c , pbl) pbr (nr a (a<b , a<c , pal) par lf lf) lf) (nb e pel (c<e , per) (nr d (d<e , pdl) (c<d , pdr) lf lf) lf)) with  x ≟ a
  ... | yes _ = , nr c pcl pcr (nb b (b<c , pbl) pbr lf lf) (nb e pel (c<e , per) (nr d (d<e , pdl) (c<d , pdr) lf lf) lf)
  ... | no  _ with x ≟ b
  ... | yes _ = , nr c pcl pcr (nb a (trans a<b b<c , pal) par lf lf) (nb e pel (c<e , per) (nr d (d<e , pdl) (c<d , pdr) lf lf) lf)
  ... | no  _ with x ≟ c
  ... | yes _ = , nr b pbl pbr (nb a (a<b , pal) par lf lf) (nb d pdl (trans b<c c<d , pdr) (nr c (c<d , pcl) (b<c , pcr) lf lf) lf)
  ... | no  _ = , nr c pcl pcr (nb b (b<c , pbl) pbr (nr a (a<b , trans a<b b<c , pal) par lf lf) lf) (nb e pel (trans c<d d<e , per) lf lf)

  -- 2.4
  deleteR x (nr d pdl pdr
                    (nb b pbl pbr
                        (nb a pal par al ar) (nb c pcl pcr cl cr))
                    (nb f pfl pfr
                        (nb e pef per el er) (nb g pgl pgr gl gr))) with compare x d

  -- 2.4.2
  deleteR x (nr d pdl pdr
                    (nb b (b<d , pbl) pbr
                        (nb a pal par al ar)
                        (nb c (c<d , pcl) (b<c , pcr) cl cr))
                    (nb f pfl (d<f , pfr)
                        (nb e (e<f , pel) (d<e , per) el er)
                        (nb g pgl pgr gl gr)))
      | tri≈ _ x≈d _ with deleteR x (nr d (d<f , pdl) (b<d , pdr)
                                        (nb c (c<d , pcl) (b<c , pcr) cl cr ◁ cover d<f , ∎)
                                        (nb e (e<f , pel) (d<e , per) el er ◀ cover b<d , ∎))
  ... | red   , nr r (r<f , prl) (b<r , prr) rl rr = ,
    let a' = nb a pal par al ar ◁ cover b<r , keep keep skip ∎
        rl' = rl ◁ keep skip ∎
        b' = nb b (b<r , pbl) pbr a' rl'
        rr' = rr ◀ keep skip ∎
        g' = nb g pgl pgr gl gr ◀ cover r<f ,  keep keep skip ∎
        f' = nb f pfl (r<f , pfr) rr' g'
    in nr r prl prr b' f'
  ... | black , r = ,
    let a' = nb a pal par al ar ◁ keep cover d<f , skip ∎
        b' = nr b (trans b<d d<f , pbl) pbr a' r
        g' = nb g pgl pgr gl gr ◀ keep skip ∎
    in nb f pfl pfr b' g'

  -- 2.4.1
  deleteR x (nr d pdl pdr
                    (nb b pbl pbr
                        (nb a pal par al ar)
                        (nb c pcl pcr cl cr))
                    (nb f pfl (d<f , pfr)
                        (nb e pel per el er)
                        (nb g pgl pgr gl gr)))
      | tri< x<d ̸x≈d ̸x>d with deleteR x (nr b pbl pbr
                                                (nb a pal par al ar)
                                                (nb c pcl pcr cl cr))
  ... | red   , (nr r prl prr rl rr) = , nr d pdl pdr
                                            (nb r prl prr rl rr)
                                            (nb f pfl (d<f , pfr)
                                                (nb e pel per el er)
                                                (nb g pgl pgr gl gr))
  ... | black , r = ,
    let r' = r ◁ cover d<f , ∎
        d' = nr d (d<f , pdl) pdr r' (nb e pel per el er)
        g' = nb g pgl pgr gl gr ◀ keep skip ∎
    in nb f pfl pfr d' g'

  -- 2.4.3
  deleteR x (nr d pdl pdr
                    (nb b pbl pbr
                        (nb a pal par al ar)
                        (nb c pcl pcr cl cr))
                    (nb f pfl (d<f , pfr)
                        (nb e pel per el er)
                        (nb g pgl pgr gl gr)))
      | tri> _ _ x>d with deleteR x (nr f pfl (d<f , pfr)
                                        (nb e pel per el er)
                                        (nb g pgl pgr gl gr))
  ... | red   , nr r prl prr rl rr = , nr d pdl pdr
                                          (nb b pbl pbr
                                              (nb a pal par al ar)
                                              (nb c pcl pcr cl cr))
                                          (nb r prl prr rl rr)
  ... | black , nb r prl prr rl rr = , nb d pdl pdr
                                          (nr b pbl pbr
                                              (nb a pal par al ar)
                                              (nb c pcl pcr cl cr))
                                          (nb r prl prr rl rr)

  -- 2.3
  deleteR x (nr f pfl pfr
                    (nb d pdl pdr
                        (nr b pbl pbr a c)
                        e)
                    (nb j pjl pjr
                        (nr h phl phr g i)
                        k)) with compare x d

  -- 2.3.1
  deleteR x (nr f pfl pfr
                    (nb d pdl pdr
                        (nr b pbl pbr a c)
                        e)
                    (nb j pjl pjr
                        (nr h phl phr g i)
                        k))
      | tri< x<d _ _ with deleteR x (nr b pbl pbr a c)
  ... | _ , r = , nr f pfl pfr (nb d pdl pdr r e) (nb j pjl pjr (nr h phl phr g i) k)

  -- 2.3.2a
  deleteR x (nr f pfl pfr
                    (nb d (d<f , pdl) pdr
                          (nr b (b<d , b<f , pbl) pbr a c)
                          e)
                    (nb j pjl pjr
                          (nr h phl phr g i)
                          k))
      | tri≈ _ x≈d _ with deleteR x (nr d (d<f , pdl) (b<d , pdr) c ( e ◀ cover b<d , ∎ ))
  ... | black  , r = ,
    let a' = a ◁ keep skip ∎
        b' =  nb b (trans b<d d<f , pbl) pbr a' r 
    in nr f pfl pfr b' (nb j pjl pjr (nr h phl phr g i) k)
  ... | red    , nr r (r<f , prl) (b<r , prr) rl rr = ,
    let rr' = rr ◀ keep skip ∎
        a' = a ◁ cover b<r , keep keep skip ∎
        b' = nr b (b<r , b<f , pbl) pbr a' rl
        r' = nb r (r<f , prl) prr b' rr' 
    in nr f pfl pfr r' (nb j pjl pjr (nr h phl phr g i) k)

  deleteR x (nr f pfl pfr
                    (nb d pdl pdr
                          (nr b pbl pbr a c)
                          e)
                    (nb j pjl pjr
                          (nr h phl phr g i)
                          k))
      | tri> _ _ x>d with compare x f

  -- 2.3.2b
  deleteR x (nr f pfl pfr
                    (nb d (d<f , pdl) pdr
                          (nr b (b<d , b<f , pbl) pbr a c)
                          e)
                    (nb j pjl pjr
                          (nr h phl phr g i)
                          k))
      | tri> _ _ x>d | tri< x<f _ _ with deleteR x (nr d (d<f , pdl) (b<d , pdr)
                                                         c
                                                         (e ◀ cover b<d , ∎))
  ... | black  , r = ,
    let a' = a ◁ keep skip ∎
        b' =  nb b (trans b<d d<f , pbl) pbr a' r 
    in nr f pfl pfr b' (nb j pjl pjr (nr h phl phr g i) k)
  ... | red    , nr r (r<f , prl) (b<r , prr) rl rr = ,
    let rr' = rr ◀ keep skip ∎
        a' = a ◁ cover b<r , keep keep skip ∎
        b' = nr b (b<r , b<f , pbl) pbr a' rl
        r' = nb r (r<f , prl) prr b' rr' 
    in nr f pfl pfr r' (nb j pjl pjr (nr h phl phr g i) k)

  -- 2.3.3
  deleteR x (nr f pfl pfr
                      (nb d (d<f , pdl) pdr
                            (nr b (b<d , b<f , pbl) pbr a c)
                            e)
                      (nb j pjl (f<j , pjr)
                          (nr h (h<j , phl) (f<h , phr) g i)
                          k))
      | tri> _ _ x>d | tri≈ _ x≈f _ with deleteR x (nr f (f<h , f<j , pfl) (d<f , pfr)
                                                       (e ◁ cover f<j , cover f<h , ∎)
                                                       (g ◀ cover d<f , ∎))
  ... | black , r = ,
    let k' = k ◀ keep cover d<f , skip ∎
        i' = i ◀ keep cover d<f , skip ∎
        h' = nr h (h<j , phl) (trans d<f f<h , phr) r i'
        j' = nb j pjl (trans d<f f<j , pjr) h' k'
        b' = nb b (b<d , b<f , pbl) pbr a c ◁ keep skip ∎
    in nr d pdl pdr b' j'
  ... | red   , nr r (r<h , r<j , prl) (d<r , prr) rl rr = ,
    let rl' = rl ◁ keep skip skip ∎
        rr' = rr ◀ keep skip ∎
        k'  = k ◀ cover r<j , keep keep skip ∎
        i'  = i ◀ cover r<h , keep keep skip ∎
        h'  = nr h (h<j , phl) (r<h , phr) rr' i'
        j'  = nb j pjl (r<j , pjr) h' k'
        a'  = a ◁ keep cover d<r , keep keep skip ∎
        c'  = c ◁ cover d<r , keep keep skip ∎
        b'  = nr b (b<d , trans b<d d<r , pbl) pbr a' c'
        d'  = nb d (d<r , pdl) pdr b' rl'
    in nr r prl prr d' j'

  deleteR x (nr f pfl pfr
                    (nb d pdl pdr
                          (nr b pbl pbr a c)
                          e)
                    (nb j pjl pjr
                          (nr h phl phr g i)
                          k))
      | tri> _ _ x>d | tri> _ _ x>f with compare x j

  -- 2.3.4
  deleteR x (nr f pfl pfr
                    (nb d pdl pdr
                          (nr b pbl pbr a c)
                          e)
                    (nb j pjl pjr
                          (nr h phl phr g i)
                          k))
      | tri> _ _ x>d | tri> _ _ x>f | tri< x<j _ _ with deleteR x (nr h phl phr g i)
  ... | _ , r = , nr f pfl pfr (nb d pdl pdr (nr b pbl pbr a c) e) (nb j pjl pjr r k)

  -- 2.3.5a
  deleteR x (nr f pfl pfr
                    (nb d pdl pdr
                          (nr b pbl pbr a c)
                          e)
                    (nb j pjl (f<j , pjr)
                          (nr h (h<j , phl) (f<h , phr) g i)
                          k))
      | tri> _ _ x>d | tri> _ _ x>f | tri≈ _ x≈j _ with deleteR x (nr j pjl (h<j , f<j , pjr)
                                                                        i
                                                                        (k ◀ cover h<j , ∎))
  ... | red   , nr r prl (h<r , f<r , prr) rl rr = ,
    let rr' = rr ◀ keep skip ∎
        g'  = g ◁ cover h<r , keep keep skip ∎
        h'  = nr h (h<r , phl) (f<h , phr) g' rl
        r'  = nb r prl (f<r , prr) h' rr'
    in nr f pfl pfr (nb d pdl pdr (nr b pbl pbr a c) e) r'
  ... | black , r = ,
    let g' = g ◁ keep skip ∎
        h' = nb h phl (f<h , phr) g' r
    in nr f pfl pfr (nb d pdl pdr (nr b pbl pbr a c) e) h'

  -- 2.3.5b
  deleteR x (nr f pfl pfr
                    (nb d pdl pdr
                          (nr b pbl pbr a c)
                          e)
                    (nb j pjl (f<j , pjr)
                          (nr h (h<j , phl) (f<h , phr) g i)
                          k))
      | tri> _ _ x>d | tri> _ _ x>f | tri> _ _ x>j with deleteR x (nr j pjl (h<j , f<j , pjr)
                                                                        i
                                                                        (k ◀ cover h<j , ∎))
  ... | red   , nr r prl (h<r , f<r , prr) rl rr = ,
    let rr' = rr ◀ keep skip ∎
        g'  = g ◁ cover h<r , keep keep skip ∎
        h'  = nr h (h<r , phl) (f<h , phr) g' rl
        r'  = nb r prl (f<r , prr) h' rr'
    in nr f pfl pfr (nb d pdl pdr (nr b pbl pbr a c) e) r'
  ... | black , r = ,
    let g' = g ◁ keep skip ∎
        h' = nb h phl (f<h , phr) g' r
    in nr f pfl pfr (nb d pdl pdr (nr b pbl pbr a c) e) h'

  -- 2.2
  deleteR x (nr d pdl pdr
                    (nb b pbl pbr
                        (nb a pal par al ar)
                        c)
                    (nb h phl phr
                        (nr f pfl pfr e g) i)) with compare x d

  -- 2.2.1
  deleteR x (nr d pdl pdr
                    (nb b pbl pbr
                        (nb a pal par al ar)
                        c)
                    (nb h phl (d<h , phr)
                        (nr f (f<h , pfl) (d<f , pfr) e g)
                        i))
      | tri< x<d _ _ with deleteR x (nr b pbl pbr (nb a pal par al ar) c ◁ cover d<f , ∎)
  ... | red   , nr r prl prr rl rr = ,
    let r' = nb r prl prr rl rr ◁ keep skip ∎
    in nr d pdl pdr r' (nb h phl (d<h , phr) (nr f (f<h , pfl) (d<f , pfr) e g) i)
  ... | black , r = ,
    let e' = e ◁ keep skip ∎
        d' = nb d (d<f , pdl) pdr r e'
        g' = g ◀ keep skip ∎
        i' = i ◀ cover f<h , keep keep skip ∎
        h' = nb h phl (f<h , phr) g' i'
    in nr f pfl pfr d' h'

  -- 2.2.2
  deleteR x (nr d pdl pdr
                    (nb b (b<d , pbl) pbr
                        (nb a pal par al ar)
                        c)
                    (nb h phl (d<h , phr)
                        (nr f (f<h , pfl) (d<f , pfr) e g)
                        i))
      | tri≈ _ x≈d _ with deleteR x (nr d (d<f , pdl) (b<d , pdr)
                                          (c ◁ cover d<f , ∎)
                                          (e ◁ keep skip ∎ ◀ cover b<d , ∎))
  ... | red   , nr r prl (b<r , prr) rl rr = ,
    let a'  = nb a pal par al ar ◁ cover b<r , keep keep cover d<f , skip ∎
        b'  = nr b (b<r , trans b<d d<f , pbl) pbr a' rl
        rr' = rr ◀ keep skip ∎
        r'  = nb r prl prr b' rr'
        g'  = g ◀ keep skip ∎
        i'  = i ◀ cover f<h , keep keep skip ∎
        h'  = nb h phl (f<h , phr) g' i'
    in nr f pfl pfr r' h'
  ... | black , r = ,
    let a' = nb a pal par al ar ◁ keep cover d<f , skip ∎
        b' = nb b (trans b<d d<f , pbl) pbr a' r
        g' = g ◀ keep skip ∎
        i' = i ◀ cover f<h , keep keep skip ∎
        h' = nb h phl (f<h , phr) g' i'
    in nr f pfl pfr b' h'

  deleteR x (nr d pdl pdr
                    (nb b pbl pbr
                        (nb a pal par al ar)
                        c)
                    (nb h phl phr
                        (nr f pfl pfr e g) i))
      | tri> _ _ x>d with compare x h

  -- 2.2.3
  deleteR x (nr d pdl pdr
                    (nb b pbl pbr
                        (nb a pal par al ar)
                        c)
                    (nb h phl phr
                        (nr f pfl pfr e g) i))
      | tri> _ _ x>d | tri< x<h _ _ with deleteR x (nr f pfl pfr e g)
  ... | _ , r = , nr d pdl pdr (nb b pbl pbr (nb a pal par al ar) c) (nb h phl phr r i)

  -- 2.2.4a
  deleteR x (nr d pdl pdr
                    (nb b pbl pbr
                        (nb a pal par al ar)
                        c)
                    (nb h phl phr
                        (nr f (f<h , pfl) (d<f , pfr) e g) i))
      | tri> _ _ x>d | tri≈ _ x≈h _ with deleteR x (nr h phl (f<h , phr)
                                                         g
                                                         (i ◀ cover f<h , ∎))
  ... | red   , nr r prl (f<r , d<r , prr) rl rr = ,
    let e'  = e ◁ cover f<r , keep keep skip ∎
        f'  = nr f (f<r , pfl) (d<f , pfr) e' rl
        rr' = rr ◀ keep skip ∎
        r'  = nb r prl (d<r , prr) f' rr'
    in nr d pdl pdr (nb b pbl pbr (nb a pal par al ar) c) r'
  ... | black , r = ,
    let e' = e ◁ keep skip ∎
        f' = nb f pfl (d<f , pfr) e' r
    in nr d pdl pdr (nb b pbl pbr (nb a pal par al ar) c) f'

  -- 2.2.4b
  deleteR x (nr d pdl pdr
                    (nb b pbl pbr
                        (nb a pal par al ar)
                        c)
                    (nb h phl phr
                        (nr f (f<h , pfl) (d<f , pfr) e g) i))
      | tri> _ _ x>d | tri> _ _ x>h with deleteR x (nr h phl (f<h , phr)
                                                         g
                                                         (i ◀ cover f<h , ∎))
  ... | red   , nr r prl (f<r , d<r , prr) rl rr = ,
    let e'  = e ◁ cover f<r , keep keep skip ∎
        f'  = nr f (f<r , pfl) (d<f , pfr) e' rl
        rr' = rr ◀ keep skip ∎
        r'  = nb r prl (d<r , prr) f' rr'
    in nr d pdl pdr (nb b pbl pbr (nb a pal par al ar) c) r'
  ... | black , r = ,
    let e' = e ◁ keep skip ∎
        f' = nb f pfl (d<f , pfr) e' r
    in nr d pdl pdr (nb b pbl pbr (nb a pal par al ar) c) f'

  -- 2.1
  deleteR x (nr f pfl pfr
                    (nb d pdl pdr
                        (nr b pbl pbr a c)
                        e)
                    (nb h phl phr
                        (nb g pgl pgr gl gr)
                        i)) with compare x d

  -- 2.1.1
  deleteR x (nr f pfl pfr
                    (nb d pdl pdr
                        (nr b pbl pbr a c)
                        e)
                    (nb h phl phr
                        (nb g pgl pgr gl gr)
                        i))
      | tri< x<d _ _ with deleteR x (nr b pbl pbr a c)
  ... | _ , r = , nr f pfl pfr (nb d pdl pdr r e) (nb h phl phr (nb g pgl pgr gl gr) i)

  deleteR x (nr f pfl pfr
                    (nb d pdl pdr
                        (nr b pbl pbr a c)
                        e)
                    (nb h phl phr
                        (nb g pgl pgr gl gr)
                        i))
      | tri> _ _ x>d with compare x f

  -- 2.1.2
  deleteR x (nr f pfl pfr
                    (nb d pdl pdr
                        (nr b (b<d , b<f , pbl) pbr a c)
                        e)
                    (nb h phl phr
                        (nb g pgl pgr gl gr)
                        i))
      | tri> _ _ x>d | tri< x<f _ _ with deleteR x (nr d pdl (b<d , pdr)
                                                         c
                                                         (e ◀ cover b<d , ∎))
  ... | red , nr r (r<f , prl) (b<r , prr) rl rr = ,
    let rr' = rr ◀ keep skip ∎
        a'  = a ◁ cover b<r , keep keep skip ∎
        b'  = nr b (b<r , b<f , pbl) pbr a' rl
        r'  = nb r (r<f , prl) prr b' rr'
    in nr f pfl pfr r' (nb h phl phr (nb g pgl pgr gl gr) i)
  ... | black , r = ,
    let a' = a ◁ cover b<f , keep skip skip ∎
        b' = nb b (b<f , pbl) pbr a' r
    in nr f pfl pfr b' (nb h phl phr (nb g pgl pgr gl gr) i)

  -- 2.1.3
  deleteR x (nr f pfl pfr
                    (nb d (d<f , pdl) pdr
                        (nr b (b<d , b<f , pbl) pbr a c)
                        e)
                    (nb h phl (f<h , phr)
                        (nb g pgl pgr gl gr)
                        i))
      | tri> _ _ x>d | tri≈ _ x≈f _ with deleteR x (nr f (f<h , pfl) (d<f , pfr)
                                                         (e ◁ cover f<h , ∎)
                                                         (nb g pgl pgr gl gr ◀ cover d<f , ∎))
  ... | _ , r = ,
    let b' = nb b (b<d , b<f , pbl) pbr a c ◁ keep skip ∎
        i' = i ◀ keep cover d<f , skip ∎
        h' = nb h phl (trans d<f f<h , phr) r i'
    in nr d pdl pdr b' h'

  -- 2.1.4
  deleteR x (nr f pfl pfr
                    (nb d (d<f , pdl) pdr
                        (nr b (b<d , b<f , pbl) pbr a c)
                        e)
                    (nb h phl (f<h , phr)
                        (nb g pgl pgr gl gr)
                        i))
      | tri> _ _ x>d | tri> _ _ x>f with deleteR x (nr h phl (f<h , phr)
                                                       (nb g pgl pgr gl gr)
                                                       i
                                                   ◀ cover d<f , ∎)
  ... | red , nr r prl (f<r , d<r , prr) rl rr = ,
    let b'  = nb b (b<d , b<f , pbl) pbr a c ◁ keep skip ∎
        e'  = e ◁ cover f<r , ∎
        f'  = nr f (f<r , pfl) (d<f , pfr) e' rl
        rr' = rr ◀ keep skip ∎
        r'  = nb r prl (d<r , prr) f' rr'
    in nr d pdl pdr b' r'
  ... | black , r = ,
    let b' = nb b (b<d , b<f , pbl) pbr a c ◁ keep skip ∎
        f' = nb f pfl (d<f , pfr) e r
    in nr d pdl pdr b' f'

  deleteR x (nr f pfl pfr
                    (nb d pdl pdr
                        (nr b (b<d , b<f , pbl) pbr a c)
                        e)
                    (nb h phl phr
                        (nb g pgl pgr gl gr)
                        i))
      | tri≈ _ x≈d _ with deleteR x (nr d pdl (b<d , pdr)
                                        c
                                        (e ◀ cover b<d , ∎))
  ... | red , nr r (r<f , prl) (b<r , prr) rl rr = ,
    let rr' = rr ◀ keep skip ∎
        a'  = a ◁ cover b<r , keep keep skip ∎
        b'  = nr b (b<r , b<f , pbl) pbr a' rl
        r'  = nb r (r<f , prl) prr b' rr'
    in nr f pfl pfr r' (nb h phl phr (nb g pgl pgr gl gr) i)
  ... | black , r = ,
    let a' = a ◁ cover b<f , keep skip skip ∎
        b' = nb b (b<f , pbl) pbr a' r
    in nr f pfl pfr b' (nb h phl phr (nb g pgl pgr gl gr) i)



-- the returned bit z indicates whether the tree's black height has shrunk
deleteB : ∀ {n β γ} → A → Tree' β γ black (suc n) → ∃ λ z → Tree' β γ black (if z then n else (suc n))

-- case terminal node
deleteB x (nb a pal par lf lf) with x ≟ a
... | yes _ = true , lf                    -- shrunk (black height reduced)
... | no  _ = false , nb a pal par lf lf   -- not shrunk (black height preserved)

-- case 2-node: color red and call deleteR
deleteB x (nb b pbl pbr (nb a pal par al ar) br)
  with deleteR x (nr b pbl pbr (nb a pal par al ar) br)
... | red   , nr r prl prr rl rr = false , nb r prl prr rl rr -- red --> black
... | black , nb r prl prr rl rr = true  , nb r prl prr rl rr -- already black ==> shrunk

-- case 3-node
deleteB x (nb b pbl pbr (nr a pal par al ar) br) with compare x b

-- delete in left (red) subtree
deleteB x (nb b pbl pbr (nr a pal par al ar) br) | tri< x<b x≈b x>b with (deleteR x (nr a pal par al ar))
-- ... | , nr r pr rl rr = false , nb b pb (nr r pr rl rr) br
... | _ , bl' = false , nb b pbl pbr bl' br -- whatever comes back, no shrinking

-- would delete in right (black) subtree, but it is a leaf
deleteB x (nb b pbl pbr (nr a pal par al ar) lf) | tri> x<b x≈b x>b =
  false , (nb b pbl pbr (nr a pal par al ar) lf)

-- delete in right (black) subtree
deleteB x (nb h phl phr (nr b pbl pbr bl br) (nb i pil pir il ir)) | tri> x<h x≈h x>h
  with (deleteB x (nb i pil pir il ir))

-- no shrinkage, just reassemble
deleteB x (nb h phl phr (nr b pbl pbr bl br) (nb i pil pir il ir)) | tri> x<h x≈h x>h | false , r =
  false , nb h phl phr (nr b pbl pbr bl br) r

-- if there was shrinkage, we need to merge with right brother or parts of it
{- case: right brother f is a 2-node
             [h]              [b]
   (b)                     [a]         [h]
[a]     [f]           --->         (f)
     [d]   [g]   [r]             [d] [g]  [r]
-}
deleteB x (nb h phl phr (nr b (b<h , pbl) pbr a (nb {leftSonColor = black} f pfl pfr d g)) (nb i pil pir il ir)) | tri> x<h x≈h x>h | true  , r =
  false , nb b pbl pbr
             (a ◁ keep skip ∎)
             (nb h phl (b<h , phr)
                 (nr f pfl pfr d g ◁ ∎)
                 (r ◀ cover b<h , ∎))

{- case: right brother f is a 3-node
             [h]                    [f]
   (b)                        (b)
[a]     [f]           ---> [a]  [d]      [h]
    (d)
  [c] [e] [g]   [r]           [c] [e]  [g] [r]
-}
deleteB x (nb h phl phr (nr b (b<h , pbl) pbr a (nb f (f<h , pfl) (b<f , pfr) (nr d pdl pdr c e) g)) (nb i pil pir il ir)) | tri> x<h x≈h x>h | true  , r =
  false , nb f pfl pfr
             (nr b (b<f , pbl) pbr
                 (a ◁ cover b<f , keep keep skip ∎)
                 (nb d pdl pdr c e ◁ keep skip ∎))
             (nb h phl (f<h , phr)
                 (g ◀ keep skip ∎)
                 (r ◀ cover f<h , ∎))

-- delete root

{- case root is a terminal 3-node -}
deleteB x (nb d pdl pdr (nr b (b<d , pbl) pbr lf lf) lf) | tri≈ _ x≈d _ = false , nb b pbl pbr lf lf

{- case right son is a 2-node, left-right grandchild is a 2-node
         [d]
  (b)
[a]  [c]      [h]     call extractMinR (h)
  [cl] [cr] [f] [i]                  [f] [i]
-}
deleteB x (nb d pdl pdr (nr b (b<d , pbl) pbr a (nb {leftSonColor = black} c pcl pcr cl cr)) (nb {leftSonColor = black} h phl phr f i)) | tri≈ _ x≈d _ with extractMinR (nr h phl phr f i)
... | min , black , pminl , (d<min , pminr) , r  =
  false , let a' = a ◁ keep skip ∎
              c' =  nr c pcl pcr cl cr 
              r'' = r ◀ skip cover b<d , ∎
              d' = nb d pdl (b<d , pdr) c' r''
          in nb b pbl pbr a' d'
... | min , red   , pminl , (d<min , pminr) , nr r prl prr rl rr  =
  false , let r' = nb r prl prr rl rr ◀ keep skip ∎
              b' = nr b (b<d , pbl) pbr a (nb c pcl pcr cl cr) ◁ cover d<min , skip ∎
          in nb min pminl pminr b' r'

{- case right son is a 2-node, left-right grandchild is a 3-node
            [d]
    (b)
[a]     [c]       [h]     call extractMinR (h)
    (cl)   [cr] [f] [i]                  [f] [i]
[clr]  [crr]
-}
deleteB x (nb d pdl pdr (nr b (b<d , pbl) pbr a (nb c (c<d , pcl) (b<c , pcr) (nr cl pcll pclr cll clr) cr)) (nb {leftSonColor = black} h phl phr f i)) | tri≈ _ x≈d _ with extractMinR (nr h phl phr f i)
... | min , black , pminl , (d<min , pminr) , r  =
  false , let a' = a ◁ cover b<c , keep keep skip ∎
              cl' = nb cl pcll pclr cll clr ◁ keep skip ∎
              b' =  nr b (b<c , pbl) pbr a' cl' 
              cr' = cr ◀ keep skip ∎
              r' = r ◀ skip cover c<d , ∎
              d' =  nb d pdl (c<d , pdr) cr' r'
          in nb c pcl pcr b' d'
... | min , red   , pminl , (d<min , pminr) , nr r prl prr rl rr  =
  false , let r' = nb r prl prr rl rr ◀ keep skip ∎
              b' = nr b (b<d , pbl) pbr a (nb c (c<d , pcl) (b<c , pcr) (nr cl pcll pclr cll clr) cr) ◁ cover d<min , skip ∎
          in nb min pminl pminr b' r'

{- case right son is a 3-node
       [d]
  (b)
[a] [c]      [h]
          (f)        call extractMinR (f)
        [e] [g] [i]               [e] [g]
-}
deleteB x (nb d pdl pdr (nr b pbl pbr a c) (nb h phl (d<h , phr) (nr f (f<h , pfl) (d<f , pfr) e g) i)) | tri≈ _ x≈d _ with extractMinR (nr f (f<h , pfl) (d<f , pfr) e g)
... | result with extractMinR (nr f (f<h , pfl) (d<f , pfr) e g)
... | min , _ , (min<h , pminl) , (d<min , pminr) , r =
  false , let r' = r ◀ keep skip  ∎
              i' = i ◀ cover min<h , keep keep skip ∎
              h' =  nb h phl (min<h , phr) r' i' 
              b' = nr b pbl pbr a c ◁ cover d<min , skip ∎
          in nb min pminl pminr b' h'


