module ASTMovi where

-- Identificadores de Variable
type Variable = String

-- Expresiones Aritmeticas
data Expf = Const Double
            | Var Variable
            | UMinus Expf
            | Plus Expf Expf
            | Minus Expf Expf
            | Times Expf Expf
            | Div Expf Expf
            | Sen Expf
            | Cos Expf
            | Tan Expf
            | Pot Expf Expf
            | Log Expf
            | Rnd Expf Integer
            | Floor Expf
            | Ceil Expf
            | Rangle Point
            | Gangle Point
            | Dist Point
 deriving Show
--https://hackage.haskell.org/package/base-4.19.0.0/docs/GHC-Real.html#v:round

-- Expresiones Booleanas
data BoolExp = BTrue
             | BFalse
             | Eq Expf Expf
             | Lt Expf Expf
             | Gt Expf Expf
             | And BoolExp BoolExp
             | Or BoolExp BoolExp
             | Not BoolExp
 deriving Show

 -- Definimos nuestros nuevos tipos
type Point = (Expf,Expf)
type PointAllow = (Point,Bool)
type ListF = [Expf]
type Sensor = String
type Vel = Expf
type Time = Expf

data ListPoint = LPoint [Point]
               | Path Expf Variable ListF
    deriving Show

data Obstacle = LPointAllow [PointAllow]
              | Obs ListPoint [Integer]
    deriving Show
    
-- Comandos
data Comm = Skip
          | Letf Variable Expf
          | Seq Comm Comm
          | Cond BoolExp Comm Comm
          | While BoolExp Comm
          | Fd Time Vel
          | Turn Time Vel
          | TurnAbs Double Vel
          | Lookat Point Vel
          | Goline Point Vel Vel
          | LookatAbs Point Vel
          | GolineAbs Point Vel Vel
          | Follow ListPoint Vel Vel 
          | FollowSmart Obstacle Obstacle Vel Vel
 deriving Show
