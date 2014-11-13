{-# LANGUAGE MultiParamTypeClasses #-}

module Physics where

import Data.List

type AccelFunc    = Double -> AccelVector
type AccelVector  = (Double,Double,Double)
type LogType      = String
type Name         = String
type NormalVector = (Double,Double,Double)
type Position     = (Double,Double,Double)
type Size         = Double
type Time         = Double
type Velocity     = (Double,Double,Double)

data Env a  = Env Time [a] 
data Log    = LogColSS Time Sphere Sphere
            | LogColSW Time Sphere Wall
data Sphere = Sphere Name Size Position Velocity AccelFunc
data Wall   = Wall Name Size Position NormalVector
data Element = Sp Sphere
             | Wa Wall

class Object a where 
    -- new        :: Name -> Size -> Position -> Velocity -> AccelFunc -> a
    update     :: Position -> Velocity -> a -> a
    getName    :: a -> Name
    getSize    :: a -> Size
    getPos     :: a -> Position
    getVelo    :: a -> Velocity
    getAccel   :: a -> AccelFunc
    normalize  :: a -> (Name, Size, Position, Velocity)

    getObjName :: Name -> Env a -> Maybe a
    getObjName s (Env t a) = getObjNamelist s a

    getObjNamelist :: Name -> [a] -> Maybe a
    getObjNamelist s []     = Nothing
    getObjNamelist s (x:xs)
        | s == getName x   = Just x
        | otherwise = getObjNamelist s xs

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

    stepTime :: Time -> Time -> a -> a
    stepTime t1 t2 p = update newPos newVelo p 
        where f          = getAccel p
              (ax,ay,az) = avgAccel t1 t2 4 f
              t          = t2 - t1
              (px,py,pz) = getPos p
              (vx,vy,vz) = getVelo p
              newPos     = (px+vx*t+0.5*ax*t*t,py+vy*t+0.5*ay*t*t,pz+vz*t+0.5*az*t*t)
              newVelo    = (vx+ax*t,vy+ay*t,vz+az*t)

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

instance Object Sphere where
    -- new = Sphere
    update p v (Sphere n s _ _ f) = (Sphere n s p v f)
    getName (Sphere n _ _ _ _) = n
    getSize (Sphere _ s _ _ _) = s
    getPos (Sphere _ _ p _ _) = p
    getVelo (Sphere _ _ _ v _) = v
    getAccel (Sphere _ _ _ _ f) = f
    normalize (Sphere n s p v _) = (n, s, p, v)

instance Object Wall where
    -- new  n s p v _ = Wall  n s p v
    update p v (Wall n s _ _) = (Wall n s p v)
    getName (Wall n _ _ _) = n
    getSize (Wall _ s _ _) = s
    getPos (Wall _ _ p _) = p
    getVelo (Wall _ _ _ _) = (0.0,0.0,0.0)
    getAccel (Wall _ _ _ _) = \x -> (0.0,0.0,0.0)
    normalize (Wall n s p v) = (n, s, p, v)

magnitude :: (Double,Double,Double) -> (Double,Double,Double) -> Double
magnitude (a,b,c) (x,y,z) = sqrt (i*i + j*j + k*k)
                                where i = x - a
                                      j = y - b
                                      k = z - c

class Collision a b where
    collision :: a -> b -> Bool

-- | See if Collision happened between 2 spheres
-- >>> collision (Sphere "Test" 1 (0,0,0) (0,0,0) gravityVelo) (Sphere "Test2" 1 (1,0,0) (0,0,0) gravityVelo)
-- True
-- 
-- >>> collision (Sphere "Test" 1 (0,0,0) (0,0,0) gravityVelo) (Sphere "Test2" 1 (2,0,0) (0,0,0) gravityVelo)
-- True
-- 
-- >>> collision (Sphere "Test" 1 (0,0,0) (0,0,0) gravityVelo) (Sphere "Test2" 1 (3,0,0) (0,0,0) gravityVelo)
-- False
instance Collision Sphere Sphere where
    collision (Sphere _ s1 p1 _ _) (Sphere _ s2 p2 _ _) = magnitude p1 p2 <= s1 + s2

-- | See if Collision happened between a sphere and wall
-- >>> collision (Sphere "Test" 1 (2,0,0) (0,0,0) gravityVelo) (Wall "Test2" 1 (0,0,0) (1,0,0))
-- False
-- 
-- >>> collision (Sphere "Test" 1 (2,0,0) (0,0,0) gravityVelo) (Wall "Test2" 1 (0,0,0) (0,1,0))
-- True
-- 
-- >>> collision (Sphere "Test" 1 (2,0,0) (0,0,0) gravityVelo) (Wall "Test2" 1 (0,0,0) (0,0,1))
-- True
instance Collision Sphere Wall where
    collision (Sphere _ s1 (x,y,z) _ _) (Wall _ _ (x0,y0,z0) (a,b,c)) =  (a*x+b*y+c*z+d)/sqrt(a*a+b*b+c*c) <= s1
        where d = -(a*x0+b*y0+c*z0)

instance Collision Wall Sphere where
    collision w s = collision s w

-- | Showing the Sphere
-- >>> Sphere "Test" 1 (0,0,0) (0,0,0) gravityVelo
-- Sphere Test 1.0 (0.0,0.0,0.0) (0.0,0.0,0.0)

instance Show Sphere where
    show (Sphere n s p v _) = "Sphere " ++ n ++ " " ++ show s ++ " " ++ show p ++ " " ++ show v

instance Show a => Show (Env a) where
    show (Env t a) = "Env " ++ show t ++ " " ++ show a

instance Eq Sphere where
    (Sphere n1 s1 p1 v1 _) == (Sphere n2 s2 p2 v2 _) = n1 == n2 
                                                    && s1 == s2 
                                                    && (magnitude (0,0,0) p1) == (magnitude (0,0,0) p2) 
                                                    && (magnitude (0,0,0) v1) == (magnitude (0,0,0) v2)

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
    (Env t a) == (Env t2 a2) = t == t2 && sort a == sort a2

-- | Create a basic acceleration function with a vector
-- >>> (accelFunc (1,1,1)) 2
-- (2.0,2.0,2.0)
accelFunc :: AccelVector -> AccelFunc
accelFunc (a,b,c) = \t -> (t*a,t*b,t*c)

-- | Basic gravity function
-- >>> gravityVelo 100
-- (0.0,0.0,9.8)
gravityVelo :: AccelFunc
gravityVelo = \t -> (0,0,9.8)

-- | Find the avg Acceleration vector over a period of time 
-- >>> avgAccel 0 2 2 gravityVelo
-- (0.0,0.0,9.8)
avgAccel :: Time -> Time -> Int -> AccelFunc -> AccelVector
avgAccel t1 t2 i f = avg [f t | let rt = (t2-t1)/(fromIntegral i), j <- [0..i], let t = t1 + rt * (fromIntegral j)]

-- | Avg of an array of vectors
-- >>> avg [(0,0,0),(2,2,2)]
-- (1.0,1.0,1.0)
avg :: [AccelVector] -> AccelVector
avg v = foldr (\(a,b,c) (x,y,z) -> (a/l+x,b/l+y,c/l+z)) (0,0,0) v
      where l = fromIntegral (length v)

-- | Step the time of the Env
-- >>> let s = Sphere "Test" 1 (0,0,0) (0,0,0) gravityVelo
-- >>> let e = Env 0 [s]
-- >>> let e1 = stepTimeEnv 1 e
-- >>> e1
-- Env 1.0 [Sphere Test 1.0 (0.0,0.0,4.9) (0.0,0.0,9.8)]
-- 
-- >>> let e2 = stepTimeEnv 2 e
-- >>> e2
-- Env 2.0 [Sphere Test 1.0 (0.0,0.0,19.6) (0.0,0.0,19.6)]
-- 
-- >>> stepTimeEnv 1 e1 == e2
-- True
stepTimeEnv :: Object a => Time -> Env a -> Env a
stepTimeEnv st (Env t1 a) = Env t2 (map (stepTime t1 t2) a)
                          where t2 = t1 + st

stepPos :: Time -> Position -> Velocity -> AccelVector -> Position
stepPos t (px,py,pz) (vx,vy,vz) (ax,ay,az) = (px+vx*t+ax*t*t,py+vy*t+ay*t*t,pz+vz*t+az*t*t)

-- | Get an Sphere from an Env
-- >>> let s = Sphere "Test" 1 (0,0,0) (0,0,0) gravityVelo
-- >>> let e = Env 0 [s]
-- >>> getObjName "Test" e
-- Just Sphere Test 1.0 (0.0,0.0,0.0) (0.0,0.0,0.0)
-- 
-- >>> getObjName "Missing" e
-- Nothing