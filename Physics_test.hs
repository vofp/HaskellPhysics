{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}

module Physics_test where

import Data.List 
import Data.Maybe 
import Physics
import Physics_collisions
import Physics_types
import Physics_objects
import Physics_combine

-- | See if Collision happened between 2 spheres
-- >>> collision (Sphere "Test" 1 (0,0,0) (0,0,0) nothingVelo) (Sphere "Test2" 1 (1,0,0) (0,0,0) nothingVelo)
-- True
-- 
-- >>> collision (Sphere "Test" 1 (0,0,0) (0,0,0) nothingVelo) (Sphere "Test2" 1 (2,0,0) (0,0,0) nothingVelo)
-- True
-- 
-- >>> collision (Sphere "Test" 1 (0,0,0) (0,0,0) nothingVelo) (Sphere "Test2" 1 (3,0,0) (0,0,0) nothingVelo)
-- False
--
-- >>> bounce (Sphere "Test" 1 (1,3,0) (0,-1,0) nothingVelo) (Sphere "Test2" 1 (1,1,0) (0,1,0) nothingVelo)
-- (Sphere Test 1.0 (1.0,3.0,0.0) (0.0,1.0,0.0),Sphere Test2 1.0 (1.0,1.0,0.0) (0.0,-1.0,0.0))
--
-- >>> bounce (Sphere "Test" 1 (2,0,0) (-1,0,0) nothingVelo) (Sphere "Test2" 1 (0,0,0) (1,0,0) nothingVelo)
-- (Sphere Test 1.0 (2.0,0.0,0.0) (1.0,0.0,0.0),Sphere Test2 1.0 (0.0,0.0,0.0) (-1.0,0.0,0.0))
--
-- >>> bounce (Sphere "Test" 1 (0,0,2) (0,0,-1) nothingVelo) (Sphere "Test2" 1 (0,0,0) (0,0,1) nothingVelo)
-- (Sphere Test 1.0 (0.0,0.0,2.0) (0.0,0.0,1.0),Sphere Test2 1.0 (0.0,0.0,0.0) (0.0,0.0,-1.0))
--
-- >>> bounce (Sphere "Test" 1 (1,0,0) (-1,0,0) nothingVelo) (Wall "Test2" 1 (0,0,0) (1,0,0))
-- (Sphere Test 1.0 (1.0,0.0,0.0) (1.0,0.0,0.0),Wall Test2 1.0 (0.0,0.0,0.0) (1.0,0.0,0.0))
--

-- | See if Collision happened between a sphere and wall
-- >>> collision (Sphere "Test" 1 (2,0,0) (0,0,0) nothingVelo) (Wall "Test2" 1 (0,0,0) (1,0,0))
-- False
-- 
-- >>> collision (Sphere "Test" 1 (2,0,0) (0,0,0) nothingVelo) (Wall "Test2" 1 (0,0,0) (0,1,0))
-- True
-- 
-- >>> collision (Sphere "Test" 1 (2,0,0) (0,0,0) nothingVelo) (Wall "Test2" 1 (0,0,0) (0,0,1))
-- True

-- | Combine vectors
-- >>> let s = [gravityVelo]
-- >>> let p = Sp (Sphere "Test" 1 (0,0,0) (0,0,0) nothingVelo)
-- >>> let a = (0.0,0.0,0.0)
-- >>> let t = 1.0
-- >>> simpleCombine (foldr simpleCombine (0,0,0) [g p t | g <- s]) a
-- (0.0,0.0,9.8)

-- | Showing the Sphere
-- >>> Sphere "Test" 1 (0,0,0) (0,0,0) nothingVelo
-- Sphere Test 1.0 (0.0,0.0,0.0) (0.0,0.0,0.0)

-- | Showing the Wall
-- >>> Wall "Test" 1 (0,0,0) (0,0,0)
-- Wall Test 1.0 (0.0,0.0,0.0) (0.0,0.0,0.0)

-- | Create a basic acceleration function with a vector
-- >>> (accelFunc (1,1,1)) 2
-- (2.0,2.0,2.0)


-- | Basic gravity function
-- >>> gravityVelo (Sp (Sphere "Test" 1 (2,0,0) (0,0,0) nothingVelo)) 100
-- (0.0,0.0,9.8)

-- | Basic not accelerating function
-- >>> nothingVelo 100
-- (0.0,0.0,0.0)

-- | Find the avg Acceleration vector over a period of time 
-- >>> avgAccel 0 2 2 (accelFunc (0,0,9.8))
-- (0.0,0.0,9.8)

-- | Avg of an array of vectors
-- >>> avg [(0,0,0),(2,2,2)]
-- (1.0,1.0,1.0)

-- | Step the time of the Env
-- >>> let s = Sphere "Test" 1 (0,0,0) (0,0,0) nothingVelo
-- >>> let e = Env 0 [s] [gravityVelo] []
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
-- 
-- >>> let s2 = Sphere "Test2" 1 (2,0,0) (0,0,0) nothingVelo
-- >>> let w = Wall "Test3" 1 (0,0,0) (1,0,0)
-- >>> let e3 = Env 0 [(Sp s2),(Wa w)] [gravityVelo] []
-- >>> let e4 = stepTimeEnv 1 e3
-- >>> e4
-- Env 1.0 [Sphere Test2 1.0 (2.0,0.0,4.9) (0.0,0.0,9.8),Wall Test3 1.0 (0.0,0.0,0.0) (1.0,0.0,0.0)]

-- | Step multiple times
-- >>> let s = Sphere "Test" 1 (0,0,0) (0,0,0) nothingVelo
-- >>> let e = Env 0 [s] [gravityVelo] []
-- >>> stepsTimeEnv 1 1 e
-- Env 1.0 [Sphere Test 1.0 (0.0,0.0,4.9) (0.0,0.0,9.8)]
-- 
-- >>> stepsTimeEnv 2 1 e
-- Env 2.0 [Sphere Test 1.0 (0.0,0.0,19.6) (0.0,0.0,19.6)]
-- 
-- >>> stepsTimeEnv 64 0.03125 e
-- Env 2.0 [Sphere Test 1.0 (0.0,0.0,19.600000000000012) (0.0,0.0,19.599999999999998)]

-- | detect all collisions
-- >>> detectCollisions [Sp (Sphere "Test" 1 (0,0,0) (0,0,0) nothingVelo), Sp (Sphere "Test2" 1 (1,0,0) (0,0,0) nothingVelo)]
-- True
-- 
-- >>> detectCollisions [Sp (Sphere "Test" 1 (0,0,0) (0,0,0) nothingVelo), Sp (Sphere "Test2" 1 (2,0,0) (0,0,0) nothingVelo)]
-- True
-- 
-- >>> detectCollisions [Sp (Sphere "Test" 1 (0,0,0) (0,0,0) nothingVelo), Sp (Sphere "Test2" 1 (3,0,0) (0,0,0) nothingVelo)]
-- False
-- 
-- >>> detectCollisions [Sp (Sphere "Test" 1 (0,0,0) (0,0,0) nothingVelo), Sp (Sphere "Test2" 1 (15,0,0) (0,0,0) nothingVelo), Sp (Sphere "Test3" 1 (2,0,0) (0,0,0) nothingVelo)]
-- True
-- 
-- >>> detectCollisions [Sp (Sphere "Test" 1 (0,0,0) (0,0,0) nothingVelo), Sp (Sphere "Test2" 1 (15,0,0) (0,0,0) nothingVelo), Sp (Sphere "Test3" 1 (5,0,0) (0,0,0) nothingVelo)]
-- False

-- | list all collisions
-- >>> listCollisions [Sp (Sphere "Test" 1 (0,0,0) (0,0,0) nothingVelo), Sp (Sphere "Test2" 1 (1,0,0) (0,0,0) nothingVelo)]
-- [("Test","Test2")]
-- 
-- >>> listCollisions [Sp (Sphere "Test" 1 (0,0,0) (0,0,0) nothingVelo), Sp (Sphere "Test2" 1 (2,0,0) (0,0,0) nothingVelo)]
-- [("Test","Test2")]
-- 
-- >>> listCollisions [Sp (Sphere "Test" 1 (0,0,0) (0,0,0) nothingVelo), Sp (Sphere "Test2" 1 (3,0,0) (0,0,0) nothingVelo)]
-- []
-- 
-- >>> listCollisions [Sp (Sphere "Test" 1 (0,0,0) (0,0,0) nothingVelo), Sp (Sphere "Test2" 1 (15,0,0) (0,0,0) nothingVelo), Sp (Sphere "Test3" 1 (2,0,0) (0,0,0) nothingVelo)]
-- [("Test","Test3")]
-- 
-- >>> listCollisions [Sp (Sphere "Test" 1 (0,0,0) (0,0,0) nothingVelo), Sp (Sphere "Test2" 1 (15,0,0) (0,0,0) nothingVelo), Sp (Sphere "Test3" 1 (5,0,0) (0,0,0) nothingVelo)]
-- []
-- 
-- >>> listCollisions [Sp (Sphere "Test" 1 (0,0,0) (0,0,0) nothingVelo), Sp (Sphere "Test2" 1 (-1,0,0) (0,0,0) nothingVelo), Sp (Sphere "Test3" 1 (2,0,0) (0,0,0) nothingVelo)]
-- [("Test","Test2"),("Test","Test3")]
-- 
-- >>> listCollisions [Sp (Sphere "Test" 1 (0,0,0) (0,0,0) nothingVelo), Sp (Sphere "Test2" 5 (-1,0,0) (0,0,0) nothingVelo), Sp (Sphere "Test3" 1 (2,0,0) (0,0,0) nothingVelo)]
-- [("Test","Test2"),("Test","Test3"),("Test2","Test3")]
--

-- | resolve all collisions
-- >>> resolveCollisions (Env 0 [Sp (Sphere "Test" 1 (0,0,0) (-1,0,0) nothingVelo), Sp (Sphere "Test2" 1 (-2,0,0) (1,0,0) nothingVelo), Sp (Sphere "Test3" 1 (5,0,0) (0,0,0) nothingVelo)] [] [])
-- Env 0.0 [Sphere Test3 1.0 (5.0,0.0,0.0) (0.0,0.0,0.0),Sphere Test2 1.0 (-2.0,0.0,0.0) (-1.0,0.0,0.0),Sphere Test 1.0 (0.0,0.0,0.0) (1.0,0.0,0.0)]
-- 
-- >>> let a = [Sp (Sphere "Test" 1 (0,0,0) (1,0,0) nothingVelo), Sp (Sphere "Test2" 1 (-2,0,0) (2,0,0) nothingVelo), Sp (Sphere "Test3" 1 (2,0,0) (0,0,0) nothingVelo)]
-- >>> let e = (Env 0 a [] [])
-- >>> listCollisions a
-- [("Test","Test2"),("Test","Test3")]
-- >>> resolveCollisions e
-- Env 0.0 [Sphere Test2 1.0 (-2.0,0.0,0.0) (1.0,0.0,0.0),Sphere Test3 1.0 (2.0,0.0,0.0) (2.0,0.0,0.0),Sphere Test 1.0 (0.0,0.0,0.0) (0.0,0.0,0.0)]
-- 

-- | Resolve a Collision
-- >>> let a = [Sp (Sphere "Test" 1 (0,0,0) (1,0,0) nothingVelo), Sp (Sphere "Test2" 1 (-2,0,0) (2,0,0) nothingVelo), Sp (Sphere "Test3" 1 (2,0,0) (0,0,0) nothingVelo)]
-- >>> a
-- [Sphere Test 1.0 (0.0,0.0,0.0) (1.0,0.0,0.0),Sphere Test2 1.0 (-2.0,0.0,0.0) (2.0,0.0,0.0),Sphere Test3 1.0 (2.0,0.0,0.0) (0.0,0.0,0.0)]
-- 
-- >>> resolveCollision a ("Test","Test3")
-- [Sphere Test2 1.0 (-2.0,0.0,0.0) (2.0,0.0,0.0),Sphere Test3 1.0 (2.0,0.0,0.0) (1.0,0.0,0.0),Sphere Test 1.0 (0.0,0.0,0.0) (0.0,0.0,0.0)]

-- | Get an Sphere from an Env
-- >>> let s = Sphere "Test" 1 (0,0,0) (0,0,0) nothingVelo
-- >>> let e = Env 0 [s] [gravityVelo] []
-- >>> getObjName "Test" e
-- Just Sphere Test 1.0 (0.0,0.0,0.0) (0.0,0.0,0.0)
-- 
-- >>> getObjName "Missing" e
-- Nothing

