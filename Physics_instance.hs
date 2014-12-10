module Physics_instance where

import Data.List
import Physics_types
import Physics_objects (magnitude)

-- | Showing the Sphere
instance Show Sphere where
    show (Sphere n s p v _) = "Sphere " ++ n ++ " " ++ show s ++ " " ++ show p ++ " " ++ show v

-- | Showing the Wall
instance Show Wall where
    show (Wall n s p v ) = "Wall " ++ n ++ " " ++ show s ++ " " ++ show p ++ " " ++ show v

instance Show Element where
    show (Sp s) = show s 
    show (Wa w) = show w 

instance Show a => Show (Env a) where
    show (Env t a _ _) = "Env " ++ show t ++ " " ++ show a

instance Eq Sphere where
    (Sphere n1 s1 p1 v1 _) == (Sphere n2 s2 p2 v2 _) = n1 == n2 
                                                    && s1 == s2 
                                                    && (magnitude (0,0,0) p1) == (magnitude (0,0,0) p2) 
                                                    && (magnitude (0,0,0) v1) == (magnitude (0,0,0) v2)

instance Eq Wall where
    (Wall n1 s1 p1 v1) == (Wall n2 s2 p2 v2) = n1 == n2 
                                                    && s1 == s2 
                                                    && (magnitude (0,0,0) p1) == (magnitude (0,0,0) p2) 
                                                    && (magnitude (0,0,0) v1) == (magnitude (0,0,0) v2)

instance Eq Element where
    (Sp s1) == (Sp s2) = s1 == s2
    (Wa w1) == (Wa w2) = w1 == w2
    _       == _       = False 

instance Ord Sphere where
    compare (Sphere n1 s1 p1 v1 _) (Sphere n2 s2 p2 v2 _)
            | n == EQ && s == EQ && p == EQ = v
            | n == EQ && s == EQ            = p
            | n == EQ                       = s
            | otherwise                     = n  
        where n = compare n1 n2 
              s = compare s1 s2
              p = compare (magnitude (0,0,0) p1) (magnitude (0,0,0) p2)
              v = compare (magnitude (0,0,0) v1) (magnitude (0,0,0) v2) 


instance Ord a => Eq (Env a) where
    (Env t a _ _) == (Env t2 a2 _ _) = t == t2 && sort a == sort a2
