module Remove where

import Nodes

---------------
-- BALANCING --
---------------

-- balancing RT
-- ============

-- balance a RT.  Takes Bool indicating side with imbalance.
-- May return RT or BT, but always at correct height.

balR :: RT -> Bool -> Either RT BT

-- missing Black on LHS
balR (RT {-h-1-}l v {-h-}r) False = case {-h-}r of -- [l < v < r@[rl@[rll < rlv < rlr] < rv < rr]]
  {-h-}EBT       {-h-1-}rl                                rv {-h-1-}rr -> Right $ {-h-}LBT True {-h-1-}(RT  {-h-1-}rl rv {-h-1-}rr)  v    {-h-1-}l                                  -- color flip.
  {-h-}LBT True  {-h-1-}rr                                rv {-h-1-}rl -> Right $ {-h-}FBT      {-h-1-}(RT  {-h-1-}l  v  {-h-1-}rl )  rv  {-h-1-}rr                                 -- single rotate.
  {-h-}LBT False {-h-1-}rl@(RT {-h-1-}rll rlv {-h-1-}rlr) rv {-h-1-}rr -> Left  $ {-h-}RT         {-h-}(EBT {-h-1-}l  v  {-h-1-}rll)  rlv   {-h-}(EBT      {-h-1-}rlr rv {-h-1-}rr )  -- double rotate.  
  {-h-}FBT       {-h-1-}rl@(RT {-h-1-}rll rlv {-h-1-}rlr) rv {-h-1-}rr -> Left  $ {-h-}RT         {-h-}(EBT {-h-1-}l  v  {-h-1-}rll)  rlv   {-h-}(LBT True {-h-1-}rr  rv {-h-1-}rlr)  -- double rotate.
  {-h-}ET -> error "balR must be passed tree with non-ET sibling"  -- implies h = 0; l at height -1; impossible.

-- missing Black on RHS
balR (RT {-h-}l v {-h-1-}r) True = case {-h-}l of
  {-h-}EBT       {-h-1-}ll                                lv {-h-1-}lr                                -> Right $ {-h-}LBT False {-h-1-}(RT {-h-1-}ll lv {-h-1-}lr)   v   {-h-1-}r                          -- color flip.
  {-h-}LBT False {-h-1-}ll                                lv {-h-1-}lr                                -> Right $ {-h-}FBT       {-h-1-}ll                            lv  {-h-1-}(RT {-h-1-}lr v {-h-1-}r)  -- single rotate.
  {-h-}LBT True  {-h-1-}lr@(RT {-h-1-}lrl lrv {-h-1-}lrr) lv {-h-1-}ll                                -> Left  $ {-h-}RT          {-h-}(EBT {-h-1-}ll lv {-h-1-}lrl) lrv {-h-}(EBT {-h-1-}lrr v {-h-1-}r)  -- double rotate.
  {-h-}FBT       {-h-1-}ll                                lv {-h-1-}lr@(RT {-h-1-}lrl lrv {-h-1-}lrr) -> Left  $ {-h-}RT          {-h-}(LBT False ll lv lrl)         lrv {-h-}(EBT {-h-1-}lrr v {-h-1-}r)  -- double rotate.
  {-h-}ET -> error "balR must be passed tree with non-ET sibling"

-- balancing a black tree always returns a black tree.
-- However, it may return it at height h-1.  The bool indicates whether it is fixed.
balB :: BT -> Bool -> (BT, Bool)


-- balancing EBT
-- =============

-- missing black on LHS
balB (EBT {-h-2-}l v {-h-1-}r) False = case {-h-1-}r of
  {-h-1-}EBT       {-h-2-}rl                             rv {-h-2-}rr -> ({-h-1-}(LBT True {-h-2-}(RT {-h-2-}rl rv {-h-2-}rr) v   {-h-2-}l                                   ), False) -- color flip and pass up
  {-h-1-}LBT True  {-h-2-}(RT {-h-2-}rrl rrv {-h-2-}rrr) rv {-h-2-}rl -> (  {-h-}(EBT      {-h-1-}(EBT {-h-2-}l v {-h-2-}rl)  rv  {-h-1-}(EBT      {-h-2-}rrl rrv {-h-2-}rrr)), True ) -- single rotate and blacken the red
  {-h-1-}LBT False {-h-2-}(RT {-h-2-}rll rlv {-h-2-}rlr) rv {-h-2-}rr -> (  {-h-}(EBT      {-h-1-}(EBT {-h-2-}l v {-h-2-}rll) rlv {-h-1-}(EBT      {-h-2-}rlr rv  {-h-2-}rr )), True ) -- double rotate
  {-h-1-}FBT       {-h-2-}(RT {-h-2-}rll rlv {-h-2-}rlr) rv {-h-2-}rr -> (  {-h-}(EBT      {-h-1-}(EBT {-h-2-}l v {-h-2-}rll) rlv {-h-1-}(LBT True {-h-2-}rr  rv  {-h-2-}rlr)), True ) -- double rotate
  {-h-1-}ET -> error "balB must be passed a tree with a non-ET sibling"

-- missing black on RHS
balB (EBT {-h-1-}l v {-h-2-}r) True = case {-h-1-}l of
  {-h-1-}EBT       {-h-2-}ll                             lv {-h-2-}lr                             -> ( {-h-1-}(LBT False {-h-2-}(RT  {-h-2-}ll  lv  {-h-2-}lr ) v   {-h-2-}r                         ),  False)  -- color flip, pass up
  {-h-1-}LBT False {-h-2-}(RT {-h-2-}lll llv {-h-2-}llr) lv {-h-2-}lr                             -> (   {-h-}(EBT       {-h-1-}(EBT {-h-2-}lll llv {-h-2-}llr) lv  {-h-1-}(EBT {-h-2-}lr  v {-h-2-}r)), True )  -- single rotate
  {-h-1-}LBT True  {-h-2-}(RT {-h-2-}lrl lrv {-h-2-}lrr) lv {-h-2-}ll                             -> (   {-h-}(EBT       {-h-1-}(EBT {-h-2-}ll  lv  {-h-2-}lrl) lrv {-h-1-}(EBT {-h-2-}lrr v {-h-2-}r)), True )  -- double rotate
  {-h-1-}FBT       {-h-2-}ll                             lv {-h-2-}(RT {-h-2-}lrl lrv {-h-2-}lrr) -> (   {-h-}(EBT       {-h-1-}(LBT False {-h-2-}ll lv {-h-2-}lrl) lrv {-h-1-}(EBT {-h-2-}lrr v {-h-2-}r)), True) -- double
  {-h-1-}ET -> error "balB must be passed a tree with a non-ET sibling"


-- balancing LBT
-- =============

-- LBT False
         
--missing on LHS
balB (LBT False {-h-2-}(RT {-h-2-}ll lv {-h-2-}lr) v {-h-1-}r) False = ({-h-}EBT {-h-1-}(EBT {-h-2-}ll lv {-h-2-}lr) v {-h-1-}r, True) -- this should never be called because the RT would be fixed up in previous call
--missing on RHS; case reduce
balB (LBT False {-h-1-}(RT {-h-1-}ll lv {-h-1-}lr) v {-h-2-}r) True = case (balR (RT {-h-1-}lr v {-h-2-}r) True) of
  Left  {-RT h-1-}newr -> ({-h-}LBT True {-h-1-}newr lv {-h-1-}ll,   True)
  Right {-BT h-1-}newr -> ({-h-}EBT      {-h-1-}ll   lv {-h-1-}newr, True)


-- LBT True

--missing on LHS
balB (LBT True {-h-1-}(RT {-h-1-}rl rv {-h-1-}rr) v {-h-2-}l) False = case (balR (RT {-h-2-}l v {-h-1-}rl) False) of
  Left  {-RT h-1-}newl -> ({-h-}LBT False {-h-1-}newl rv {-h-1-}rr, True)
  Right {-BT h-1-}newl -> ({-h-}EBT       {-h-1-}newl rv {-h-1-}rr, True)

-- missing on RHS
balB (LBT True {-h-2-}(RT {-h-2-}rl rv {-h-2-}rr) v {-h-1-}l) True = ({-h-}EBT {-h-1-}l v {-h-1-}(EBT {-h-2-}rl rv {-h-2-}rr), True)


-- balancing FBT
-- =============

-- in both cases we can just recolor.  But again this should not be called because the RT would have been fixed in prev recursive call

-- missing on LHS
balB (FBT {-h-2-}(RT {-h-2-}ll lv {-h-2-}lr) v {-h-1-}r)                           False = (LBT True  {-h-1-}r v {-h-1-}(EBT {-h-2-}ll lv {-h-2-}lr), True)

-- missing on RHS
balB (FBT {-h-1-}l                           v {-h-2-}(RT {-h-2-}rl rv {-h-2-}rr)) True  = (LBT False {-h-1-}l v {-h-1-}(EBT {-h-2-}rl rv {-h-2-}rr), True)


--------------
-- REMOVING --
--------------

findMaxR (RT l v ET) = v
findMaxR (RT l v t) = findMaxB t

findMaxB (EBT l v ET) = v
findMaxB (EBT l v t) = findMaxB t

findMaxB (LBT False l v ET) = v
findMaxB (LBT False l v t) = findMaxB t

findMaxB (LBT True t v l) = findMaxR t

findMaxB (FBT l v r) = findMaxR r

do_balR :: RT -> Bool -> Bool -> Either RT BT
do_balR t _    True  = Left t
do_balR t side False = balR t side

do_balB :: BT -> Bool -> Bool -> (BT, Bool)
do_balB t _ True = (t, True)
do_balB t side False = balB t side

rem_auxR :: RT -> Int -> Either RT BT

rem_auxR (RT l v r) x = case compare x v of
  LT -> do_balR (RT newl v r) False y
    where (newl, y) = rem_auxB l x
  GT -> do_balR (RT l v newr) True y
    where (newr, y) = rem_auxB r x
  EQ -> case l of
    ET -> Right ET -- r is ET as well
    _  -> do_balR (RT newl mx r) False y -- r is not ET
      where mx = findMaxB l
            (newl, y) = rem_auxB l mx

rem_auxB :: BT -> Int -> (BT, Bool)

rem_auxB ET _ = (ET, True)

rem_auxB (EBT l v r) x = case compare x v of
  LT -> do_balB (EBT newl v r) False y
    where (newl, y) = rem_auxB l x
  GT -> do_balB (EBT l v newr) True y
    where (newr, y) = rem_auxB r x
  EQ -> case l of
    ET -> (ET, False) -- r is ET too
    _  -> do_balB (EBT newl mx r) False y
      where mx = findMaxB l
            (newl, y) = rem_auxB l mx

rem_auxB (LBT False l@(RT ll lv lr) v r) x = case compare x v of
  LT -> case rem_auxR l x of
    Left newl  -> (LBT False newl v r, True) -- returned RT
    Right newl -> (EBT newl v r, True) -- returned BT
  GT -> do_balB (LBT False l v newr) True y
    where (newr, y) = rem_auxB r x
  EQ -> case r of
    ET -> (EBT ll lv lr, True) -- just use LHS and blacken
    _  -> let mx = findMaxR l in case rem_auxR l mx of 
      Left  newl -> (LBT False newl mx r, True) -- grab max from LHS
      Right newl -> (EBT       newl mx r, True) -- 

rem_auxB (LBT True r@(RT rl rv rr) v l) x = case compare x v of
  LT -> do_balB (LBT True r v newl) False y
    where (newl, y) = rem_auxB l x
  GT -> case rem_auxR r x of
    Left newr  -> (LBT True newr v l, True)
    Right newr -> (EBT l v newr, True)
  EQ -> case l of
    ET -> (EBT rl rv rr, True) -- just use RHS and blacken
    _  -> do_balB (LBT True r mx newl) False y
          where mx = findMaxB l
                (newl, y) = rem_auxB l x

rem_auxB (FBT l v r) x = case compare x v of
  LT -> case rem_auxR l x of
    Left newl -> (FBT newl v r, True)
    Right newl -> (LBT True r v newl, True)
  GT -> case rem_auxR r x of
    Left newr -> (FBT l v newr, True)
    Right newr -> (LBT False l v newr, True)
  EQ -> let mx = findMaxR l in case rem_auxR l mx of
    Left newl  -> (FBT newl mx r, True)
    Right newl -> (LBT True r mx newl, True)


remove :: BT -> Int -> BT
remove t x = let (new, _) = rem_auxB t x in new