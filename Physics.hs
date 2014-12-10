{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}

module Physics where

import Data.List 
import Data.Maybe
import qualified Data.Vec3 as V

import Physics_collisions
import Physics_types
import Physics_objects
import Physics_combine
import Physics_instance


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

updateLog :: (Collision a a, Object a) => [(String, String)] -> Env a -> [Log] -> [Log]
updateLog []     e l = l
updateLog (x:xs) e l = (createLog t a b)++l
    where (s1,s2) = x
          (Env t _ _ _) = e
          a = fromJust $ getObjName s1 e
          b = fromJust $ getObjName s2 e

-- | Adds object to an array but removes old version
replaceObj :: Object a => a -> [a] -> [a]
replaceObj n []     = [n]
replaceObj n (x:xs) | (getName n) == (getName x) = replaceObj n xs
                   | otherwise                  = x:(replaceObj n xs)

-- | uses Time, Position, Velocity, and acceleration to figure out new location
stepPos :: Time -> Position -> Velocity -> AccelVector -> Position
stepPos t (px,py,pz) (vx,vy,vz) (ax,ay,az) = (px+vx*t+0.5*ax*t*t,py+vy*t+0.5*ay*t*t,pz+vz*t+0.5*az*t*t)
