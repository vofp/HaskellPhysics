{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Physics_combine where

import Physics_types

class Combine a where
    -- | Combine 2 Objects
    simpleCombine :: a -> a -> a

instance Combine (Env a) where
    simpleCombine (Env _ a1 f1 l1) (Env _ a2 f2 l2) = Env 0 (a1++a2) (f1++f2) (l1++l2)

instance Combine Sphere where
    simpleCombine (Sphere n1 s1 p1 v1 f1) (Sphere n2 s2 p2 v2 f2) = Sphere n s p v f
        where n = n1 ++ n2
              s = (s1+s2)/2
              p = simpleCombine p1 p2
              v = simpleCombine v1 v2
              f = (\x -> simpleCombine (f1 x) (f2 x))

-- | Combine vectors
instance Combine (Vector) where
    simpleCombine (a,b,c) (x,y,z) = (a+x,b+y,c+z)