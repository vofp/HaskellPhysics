module Physics_objects where

import Physics_types
import Physics_combine

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

-- | Find the avg Acceleration vector over a period of time 
avgAccel :: Time -> Time -> Int -> AccelFunc -> AccelVector
avgAccel t1 t2 i f = avg [f t | let rt = (t2-t1)/(fromIntegral i), j <- [0..i], let t = t1 + rt * (fromIntegral j)]

-- | Avg of an array of vectors
avg :: [AccelVector] -> AccelVector
avg v = foldr (\(a,b,c) (x,y,z) -> (a/l+x,b/l+y,c/l+z)) (0,0,0) v
      where l = fromIntegral (length v)
      
-- | uses Time, Position, Velocity, and acceleration to figure out new location
stepPos :: Time -> Position -> Velocity -> AccelVector -> Position
stepPos t (px,py,pz) (vx,vy,vz) (ax,ay,az) = (px+vx*t+0.5*ax*t*t,py+vy*t+0.5*ay*t*t,pz+vz*t+0.5*az*t*t)
