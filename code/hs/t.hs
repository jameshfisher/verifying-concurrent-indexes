data T a = N | T (T a) a (T a) deriving (Show)
o n f t w = case t of {N -> n w; T l v r -> f (compare w v) w l v r}
op n e = g where g = o n (\ord w l v r -> case ord of {LT-> T (g l w) v r; GT-> T l v (g r w); EQ-> e l v r})
search = o (const False) (\ord w l _ r -> case ord of {LT-> search l w; EQ-> True; GT-> search r w})
insert = op (\v-> T N v N) T
remove = op (const N) (\l _ r -> m l r) where m N = id; m (T l v r) = \r'-> T l v (m r r')
