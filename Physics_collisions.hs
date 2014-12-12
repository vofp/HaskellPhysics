{-# LANGUAGE MultiParamTypeClasses #-}

module Physics_collisions where


import qualified Data.Vec3 as V

import Physics_objects
import Physics_types

class Collision a b where
    -- | Checks if there is a collision between 2 objects
    collision :: a -> b -> Bool
    -- | Resolve the collision between 2 objects
    bounce :: a -> b -> (a,b)

instance Collision Element Element where
    collision (Sp s1) (Sp s2) = collision s1 s2
    collision (Sp s1) (Wa w2) = collision s1 w2
    collision (Wa w1) (Sp s2) = collision s2 w1
    collision (Wa w1) (Wa w2) = collision w1 w2

    bounce (Sp s1) (Sp s2) = (Sp n1, Sp n2)
      where (n1, n2) = bounce s1 s2
    bounce (Sp s1) (Wa w2) = (Sp n1, Wa n2)
      where (n1, n2) = bounce s1 w2
    bounce (Wa w1) (Sp s2) = (Wa n2, Sp n1)
      where (n1, n2) = bounce s2 w1
    bounce (Wa w1) (Wa w2) = (Wa n1, Wa n2)
      where (n1, n2) = bounce w1 w2

-- | See if Collision happened between 2 spheres
instance Collision Sphere Sphere where
    collision (Sphere _ s1 p1 _ _) (Sphere _ s2 p2 _ _) = magnitude p1 p2 <= s1 + s2
    bounce s1 s2 = (newS1, newS2)
        where x = V.normalize $ (V.TUVec3 (getPos s1)) V.<-> (V.TUVec3 (getPos s2))
              v1 = V.TUVec3 $ getVelo s1
              x1 = x V..* v1
              v1x = x V..^ x1
              v1y = v1 V.<-> v1x
              m1 = 4/3* pi * (getSize s1)^3
              xt = x V..^ (-1)
              v2 = V.TUVec3 $ getVelo s2
              x2 = xt V..* v2
              v2x = xt V..^ x2
              v2y = v2 V.<-> v2x
              m2 = 4/3* pi * (getSize s2)^3
              vel1 = (v1x V..^ ((m1-m2)/(m1+m2))) V.<+> (v2x V..^ ((2*m2)/(m1+m2))) V.<+> v1y
              vel2 = (v1x V..^ ((2*m1)/(m1+m2))) V.<+> (v2x V..^ ((m2-m1)/(m1+m2))) V.<+> v2y
              newS1 = update (getPos s1) (V.toXYZ vel1) s1
              newS2 = update (getPos s2) (V.toXYZ vel2) s2


-- | See if Collision happened between a sphere and wall
instance Collision Sphere Wall where
    collision (Sphere _ s1 (x,y,z) _ _) (Wall _ _ (x0,y0,z0) (a,b,c)) =  (a*x+b*y+c*z+d)/sqrt(a*a+b*b+c*c) <= s1
        where d = -(a*x0+b*y0+c*z0)
    bounce s w = (newS,w)
        where (Wall _ _ _ p) = w
              n_vector = V.normalize $ V.TUVec3 p
              v = V.TUVec3 $ getVelo s
              reflected = n_vector V.><  (n_vector V.>< v)
              newVelo = v V.<-> (reflected V..^ 2)
              newS = update (getPos s) (V.toXYZ newVelo) s

instance Collision Wall Sphere where
    collision w s = collision s w
    bounce w s = (w2,s2)
      where (s2,w2) = bounce s w

instance Collision Wall Wall where
    collision _ _ = False
    bounce a b = (a,b)
