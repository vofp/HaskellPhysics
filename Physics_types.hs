module Physics_types where

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

data Env a  = Env Time [a] [Setting] [Log a]
data Log a = LogCol Time a a
data Sphere = Sphere Name Size Position Velocity AccelFunc
data Wall   = Wall Name Size Position NormalVector
data Element = Sp Sphere
             | Wa Wall