{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}

module Physics where

import Data.List 
import Data.Maybe
import qualified Data.Vec3 as V

type AccelFunc    = Double -> AccelVector
type Setting      = Element -> AccelFunc
type Vector       = (Double,Double,Double)
type AccelVector  = Vector
type LogType      = String
type Name         = String
type NormalVector = Vector
type Position     = Vector
type Size         = Double
type Time         = Double
type Velocity     = Vector

data Env a  = Env Time [a] [Setting] [Log]
data Log    = LogColSS Time Sphere Sphere
            | LogColSW Time Sphere Wall
data Sphere = Sphere Name Size Position Velocity AccelFunc
data Wall   = Wall Name Size Position NormalVector
data Element = Sp Sphere
             | Wa Wall

class Object a where 
    -- | update the Position and Velocity
    update     :: Position -> Velocity -> a -> a

    -- | Grab Info about the object
    getName    :: a -> Name
    getSize    :: a -> Size
    getPos     :: a -> Position
    getVelo    :: a -> Velocity
    getAccel   :: a -> AccelFunc
    normalize  :: a -> (Name, Size, Position, Velocity)

    -- | Pull Object out of Env by name
    getObjName :: Name -> Env a -> Maybe a
    getObjName s (Env t a _ _) = getObjNamelist s a

    -- | Pull Object out of list by name
    getObjNamelist :: Name -> [a] -> Maybe a
    getObjNamelist s []     = Nothing
    getObjNamelist s (x:xs)
        | s == getName x = Just x
        | otherwise      = getObjNamelist s xs

    -- | Get info about an object in Env
    getInfoObj :: (a -> b) -> Name -> Env a -> Maybe b
    getInfoObj f n e = case (getObjName n e) of
                        Nothing  -> Nothing
                        (Just p) -> Just (f p)
    getSizeObj  :: Name -> Env a -> Maybe Size
    getSizeObj  = getInfoObj getSize
    getPosObj   :: Name -> Env a -> Maybe Position
    getPosObj   = getInfoObj getPos
    getVeloObj  :: Name -> Env a -> Maybe Velocity
    getVeloObj  = getInfoObj getVelo
    getAccelObj :: Name -> Env a -> Maybe AccelFunc
    getAccelObj = getInfoObj getAccel

    -- | Progress Time for an object
    stepTime :: Time -> Time -> [Setting] -> a -> a

instance Object Element where
    getAccel (Sp s)   = getAccel s
    getAccel (Wa s)   = getAccel s
    getName (Sp s)    = getName s
    getName (Wa s)    = getName s
    getPos (Sp s)     = getPos s
    getPos (Wa s)     = getPos s
    getSize (Sp s)    = getSize s
    getSize (Wa s)    = getSize s
    getVelo (Sp s)    = getVelo s
    getVelo (Wa s)    = getVelo s
    normalize (Sp s)  = normalize s
    normalize (Wa s)  = normalize s
    update p v (Sp s) = Sp (update p v s)
    update p v (Wa s) = Wa (update p v s)

    stepTime t1 t2 s p = update newPos newVelo p 
        where f          = getAccel p
              t          = t2 - t1
              a          = avgAccel t1 t2 4 f
              (ax,ay,az) = simpleCombine (foldr simpleCombine (0,0,0) [g p t | g <- s]) a
              (px,py,pz) = getPos p
              (vx,vy,vz) = getVelo p
              newPos     = stepPos t (px,py,pz) (vx,vy,vz) (ax,ay,az)
              newVelo    = (vx+ax*t,vy+ay*t,vz+az*t)

instance Object Sphere where
    -- new = Sphere
    update p v (Sphere n s _ _ f) = (Sphere n s p v f)
    getName (Sphere n _ _ _ _) = n
    getSize (Sphere _ s _ _ _) = s
    getPos (Sphere _ _ p _ _) = p
    getVelo (Sphere _ _ _ v _) = v
    getAccel (Sphere _ _ _ _ f) = f
    normalize (Sphere n s p v _) = (n, s, p, v)

    stepTime t1 t2 s p = update newPos newVelo p 
        where f          = getAccel p
              t          = t2 - t1
              a          = avgAccel t1 t2 4 f
              (ax,ay,az) = simpleCombine (foldr simpleCombine (0,0,0) [g (Sp p) t | g <- s]) a
              (px,py,pz) = getPos p
              (vx,vy,vz) = getVelo p
              newPos     = stepPos t (px,py,pz) (vx,vy,vz) (ax,ay,az)
              newVelo    = (vx+ax*t,vy+ay*t,vz+az*t)

instance Object Wall where
    -- new  n s p v _ = Wall  n s p v
    update _ _ (Wall n s p v) = (Wall n s p v)
    getName (Wall n _ _ _) = n
    getSize (Wall _ s _ _) = s
    getPos (Wall _ _ p _) = p
    getVelo (Wall _ _ _ _) = (0.0,0.0,0.0)
    getAccel (Wall _ _ _ _) = \x -> (0.0,0.0,0.0)
    normalize (Wall n s p v) = (n, s, p, v)

    stepTime t1 t2 s p = update newPos newVelo p 
        where f          = getAccel p
              t          = t2 - t1
              a          = avgAccel t1 t2 4 f
              (ax,ay,az) = simpleCombine (foldr simpleCombine (0,0,0) [g (Wa p) t | g <- s]) a
              (px,py,pz) = getPos p
              (vx,vy,vz) = getVelo p
              newPos     = stepPos t (px,py,pz) (vx,vy,vz) (ax,ay,az)
              newVelo    = (vx+ax*t,vy+ay*t,vz+az*t)

-- | Find distance between two points
magnitude :: Vector -> Vector -> Double
magnitude (a,b,c) (x,y,z) = sqrt (i*i + j*j + k*k)
                                where i = x - a
                                      j = y - b
                                      k = z - c

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
              --xt = x V..^ (-1)
        --where p1         = getPos s1
        --      (x1,y1,z1) = getVelo s1
        --      p2         = getPos s2
        --      (x2,y2,z2) = getVelo s2
        --      nx1        = -x1
        --      ny1        = -y1
        --      nz1        = -z1
        --      nx2        = -x2
        --      ny2        = -y2
        --      nz2        = -z2
        --      newS1      = update p1 (nx1,ny1,nz1) s1
        --      newS2      = update p2 (nx2,ny2,nz2) s2

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

-- | Create a basic acceleration function with a vector
accelFunc :: AccelVector -> AccelFunc
accelFunc (a,b,c) = \t -> (t*a,t*b,t*c)

-- | Basic gravity function
gravityVelo :: Setting
gravityVelo (Sp _) = \t -> (0,0,9.8)
gravityVelo (Wa _) = \t -> (0,0,0.0)

-- | Basic not accelerating function
nothingVelo :: AccelFunc
nothingVelo = \t -> (0,0,0)

-- | Find the avg Acceleration vector over a period of time 
avgAccel :: Time -> Time -> Int -> AccelFunc -> AccelVector
avgAccel t1 t2 i f = avg [f t | let rt = (t2-t1)/(fromIntegral i), j <- [0..i], let t = t1 + rt * (fromIntegral j)]

-- | Avg of an array of vectors
avg :: [AccelVector] -> AccelVector
avg v = foldr (\(a,b,c) (x,y,z) -> (a/l+x,b/l+y,c/l+z)) (0,0,0) v
      where l = fromIntegral (length v)

-- | Step the time of the Env
stepTimeEnv :: Object a => Time -> Env a -> Env a
stepTimeEnv st (Env t1 a f l) = Env t2 (map (stepTime t1 t2 f) a) f l
                          where t2 = t1 + st

-- | Step multiple times
stepsTimeEnv :: (Collision a a, Eq a, Object a) => Int -> Time -> Env a -> Env a
stepsTimeEnv 0 t e = e
stepsTimeEnv n t e = stepsTimeEnv (n-1) t e3
                        where e2 = stepTimeEnv t e
                              e3 = resolveCollisions e2

-- | detect all collisions
detectCollisions :: (Collision a a, Eq a) => [a] -> Bool
detectCollisions a = foldr (||) False [collision e1 e2 | e1 <- a, e2 <- a, e1 /= e2]

-- | list all collisions
listCollisions :: (Collision a a, Eq a, Object a) => [a] -> [(String, String)]
listCollisions a = [(s1, s2)| e1 <- a, e2 <- a, e1 /= e2, collision e1 e2, let s1 = getName e1, let s2 = getName e2, s1 < s2 ]

-- | resolve all collisions
resolveCollisions :: (Collision a a, Eq a, Object a) => Env a -> Env a
resolveCollisions (Env t a f l) = (Env t a2 f l)
  where c  = listCollisions a
        a2 = foldl (resolveCollision) a c

-- | Resolve a Collision
resolveCollision :: (Collision a a, Object a) => [a] -> (String, String) -> [a]
resolveCollision a (s1,s2) = a2
  where o1      = fromJust $ getObjNamelist s1 a
        o2      = fromJust $ getObjNamelist s2 a
        (n1,n2) = bounce o1 o2
        a2 = replaceObj n1 $ replaceObj n2 a

-- | Adds object to an array but removes old version
replaceObj :: Object a => a -> [a] -> [a]
replaceObj n []     = [n]
replaceObj n (x:xs) | (getName n) == (getName x) = replaceObj n xs
                   | otherwise                  = x:(replaceObj n xs)

-- | uses Time, Position, Velocity, and acceleration to figure out new location
stepPos :: Time -> Position -> Velocity -> AccelVector -> Position
stepPos t (px,py,pz) (vx,vy,vz) (ax,ay,az) = (px+vx*t+0.5*ax*t*t,py+vy*t+0.5*ay*t*t,pz+vz*t+0.5*az*t*t)
