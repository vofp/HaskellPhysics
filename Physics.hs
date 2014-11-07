module Physics where

import Data.List

type Name = String

type Time = Double


type Position = (Double,Double,Double)

data Sphere = Sphere Name Position Velocity AccelFunc
-- | Empty

class Proj a where 
    getName  :: a -> String
    getPos   :: a -> Position
    getVelo  :: a -> Velocity
    getAccel :: a -> AccelFunc

instance Proj Sphere where
    getName (Sphere n _ _ _) = n
    getPos (Sphere _ p _ _) = p
    getVelo (Sphere _ _ v _) = v
    getAccel (Sphere _ _ _ f) = f

data Env = Env Time [Sphere]

type Velocity = (Double,Double,Double)
type AccelVector = (Double,Double,Double)

type AccelFunc = Double -> AccelVector

-- | Showing the Sphere
-- >>> Sphere "Test" (0,0,0) (0,0,0) gravityVelo
-- Sphere Test (0.0,0.0,0.0) (0.0,0.0,0.0)

instance Show Sphere where
    show (Sphere n p v _) = "Sphere " ++ n ++ " " ++ show p ++ " " ++ show v

instance Show Env where
    show (Env t a) = "Env " ++ show t ++ " " ++ show a

instance Eq Sphere where
    (Sphere n p v _) == (Sphere n2 p2 v2 _) = n == n2 && p == p2 && v == v2
    -- Empty            == Empty               = True
    -- _                == _                   = False

instance Ord Sphere where
    (Sphere n p v _) <= (Sphere n2 p2 v2 _) = n <= n2
    -- Empty            == Empty               = EQ
    -- Empty            == _                   = LT
    -- _                == _                    = GT

instance Eq Env where
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
-- >>> let s = Sphere "Test" (0,0,0) (0,0,0) gravityVelo
-- >>> let e = Env 0 [s]
-- >>> let e1 = stepTimeEnv 1 e
-- >>> e1
-- Env 1.0 [Sphere Test (0.0,0.0,4.9) (0.0,0.0,9.8)]
-- 
-- >>> let e2 = stepTimeEnv 2 e
-- >>> e2
-- Env 2.0 [Sphere Test (0.0,0.0,19.6) (0.0,0.0,19.6)]
-- 
-- >>> stepTimeEnv 1 e1 == e2
-- True
stepTimeEnv :: Time -> Env -> Env
stepTimeEnv st (Env t1 a) = Env t2 (map (stepTimeSphere t1 t2) a)
                          where t2 = t1 + st

stepTimeSphere :: Time -> Time -> Sphere -> Sphere
-- stepTimeSphere _  _  Empty                              = Empty
stepTimeSphere t1 t2 (Sphere n (px,py,pz) (vx,vy,vz) f) = Sphere n newPos newVelo f
                                              where (ax,ay,az) = avgAccel t1 t2 4 f
                                                    t          = t2 - t1
                                                    newPos     = (px+vx*t+0.5*ax*t*t,py+vy*t+0.5*ay*t*t,pz+vz*t+0.5*az*t*t)
                                                    newVelo    = (vx+ax*t,vy+ay*t,vz+az*t)

stepPos :: Time -> Position -> Velocity -> AccelVector -> Position
stepPos t (px,py,pz) (vx,vy,vz) (ax,ay,az) = (px+vx*t+ax*t*t,py+vy*t+ay*t*t,pz+vz*t+az*t*t)

-- | Get an Sphere from an Env
-- >>> let s = Sphere "Test" (0,0,0) (0,0,0) gravityVelo
-- >>> let e = Env 0 [s]
-- >>> getObjName "Test" e
-- Just Sphere Test (0.0,0.0,0.0) (0.0,0.0,0.0)
-- 
-- >>> getObjName "Missing" e
-- Nothing

getObjName :: String -> Env -> Maybe Sphere
getObjName s (Env t a) = getObjNamelist s a

getObjNamelist :: String -> [Sphere] -> Maybe Sphere
getObjNamelist s []     = Nothing
getObjNamelist s ((Sphere s2 p v f):xs)
    | s == s2   = Just (Sphere s2 p v f)
    | otherwise = getObjNamelist s xs

