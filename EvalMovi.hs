module EvalMovi (eval, getEnvVar) where

import ASTMovi
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)

import Control.Monad.IO.Class (MonadIO, liftIO)

import Control.Concurrent
import GHC.IO.Handle
import System.IO

-- Definimos los dos posibles tipos de error
data Error = UndefVar | DivByZero

-- Estados
type EnvV = [(Variable,Double)]
type EnvAng = Double
type Env = (EnvV, EnvAng)

-- Estado nulo
initState :: Env
initState = ([],0.0)

-- Monada de estado
newtype State a = State { runState :: Env -> Either Error (a, Env, String, String) }

instance Monad State where
    return x = State (\s -> Right (x, s, "", ""))
    m >>= f = State (\s -> do (v, s', lg, p) <- runState m s
                              (v', s'', lg', p') <- runState (f v) s'
                              return (v', s'', lg ++ lg', p ++ p'))

-- Clase para representar mónadas con estado de variables.
class Monad m => MonadState m where
    lookfor :: Variable -> m Double
    update :: Variable -> Double -> m ()
    delete :: Variable -> m () -- Para borrar una variable del estado
    varexist :: Variable -> m Bool -- Para determinar si una variable existe en el estado
    addAngle :: Double -> m ()
    getAngle :: m Double

instance MonadState State where
    -- Dos operaciones para analizar sensores
    -- Dos operaciones para actualizar y tomar valores de variables
    lookfor var = State (\s -> either Left (\v -> Right (v, s, "", "")) (lookfor' var (getEnvVar s)))
                    where lookfor' var []               = Left UndefVar
                          lookfor' var ((var', val):ss) | var == var' = Right val
                                                        | otherwise   = lookfor' var ss
    update var val = State (\s -> Right ((), (update' var val (getEnvVar s), getEnvAngle s), "", ""))
                     where update' var val [] = [(var, val)]
                           update' var val ((var', val'):ss) | var == var' = (var, val):ss
                                                             | otherwise   = (var', val'):update' var val ss

    -- Dos operaciones utiles para el Path: delete y varexist
    delete var = State (\s -> Right ((), (delete' var (getEnvVar s), getEnvAngle s), "", ""))
                    where delete' var [] = []
                          delete' var ((var', val'):ss) | var == var' = ss
                                                        | otherwise   = (var', val'):delete' var ss
    varexist var = State (\s -> Right (varexist' var (getEnvVar s), s, "", ""))
                    where varexist' var []               = False
                          varexist' var ((var', val):ss) | var == var' = True
                                                         | otherwise   = varexist' var ss
    -- Dos operaciones utiles para analizar el angulo del movil
    addAngle var = State (\s -> Right ((), (getEnvVar s, correctAngle (var + getEnvAngle s)), "", ""))
    getAngle = State (\s -> Right (getEnvAngle s, s, "", ""))

---------------------------------------------------------------------
-- Funciones auxiliares para desempaquetar tupla de tres elementos --
---------------------------------------------------------------------
getEnvVar :: (EnvV, EnvAng) -> EnvV
getEnvVar (a, _) = a

getEnvAngle :: (EnvV, EnvAng) -> EnvAng
getEnvAngle (_, a) = a

correctAngle :: Double -> Double
correctAngle var
                 | var < 0 = correctAngle (var + 360.0)
                 | var < 360.0 = var
                 | var >= 360.0 = correctAngle (var - 360.0)

-- Clase para representar mónadas que lanzan errores.
class Monad m => MonadError m where
    throw :: m a

instance MonadError State where
    throw = State (\_ -> Left DivByZero)

-- Clase para representar mónadas que marca la traza.
class Monad m => MonadTrace m where
    trace :: String -> Double -> Double -> m ()

instance MonadTrace State where
    trace str v t = State (\s -> Right ((), s, "", str ++ " | Vel: " ++ show v ++ " | Time: " ++ show t ++ "\n"))

-- Clase para representar mónadas que marca la traza.
class Monad m => MonadLogo m where
    logo :: String -> Double -> Double -> m ()

instance MonadLogo State where
    logo str v t = State (\s -> Right ((), s, str ++ " " ++ show (v*t) ++ "\n", ""))

-- Para calmar al GHC
instance Functor State where
    fmap = liftM


instance Applicative State where
    pure = return
    (<*>) = ap

-- Evalua un programa en el estado nulo
-- Matcheamos los errores UndefVar y DivByZero para tener un mensaje de error distinto en cada tipo de error
eval :: Comm -> (Env, String, String)
eval p = case runState (stepCommStar p) initState of
    Right (v, s, lg, tr) -> (s, lg, tr)
    Left UndefVar     -> error "Variable no definida"
    Left DivByZero    -> error "Division por cero"

-- Evalua multiples pasos de un comnado en un estado,
-- hasta alcanzar un Skip
stepCommStar :: (MonadState m, MonadError m, MonadTrace m, MonadLogo m) => Comm -> m Comm
stepCommStar Skip = return Skip
stepCommStar p = do p' <- stepComm p
                    stepCommStar p'

-- Evalua un paso de una primitiva en un estado dado
stepComm :: (MonadState m, MonadError m, MonadTrace m, MonadLogo m) => Comm -> m Comm
stepComm (Letf v e)         = do val <- evalFloatExp e
                                 update v val
                                 return Skip
stepComm (Seq Skip c1)      =   return c1
stepComm (Seq l r)          = do stepComm l
                                 return r
stepComm (Cond b c0 c1)     = do bval <- evalBoolExp b
                                 if bval then return c0
                                         else return c1
stepComm (While b c)     = do bval <- evalBoolExp b
                              if bval then return (Seq c (While b c))
                                      else return Skip
stepComm (Fd time vel)      = do end <- evalBoolExp (Or (Eq time (Const 0)) (Lt time (Const 0)))
                                 if end
                                 then return Skip
                                      else do v <- evalFloatExp vel
                                              t <- evalFloatExp time
                                              trace "Fd" v t
                                              logo "fd" v t
                                              return (Fd (Minus time (Const 1.0)) vel)

stepComm (Turn time vel) = do v <- evalFloatExp vel
                              t <- evalFloatExp time
                              trace "Turn" v t
                              logo "rt" v t
                              addAngle (v*t)
                              return Skip

stepComm (TurnAbs ang vel) = do angAct <- getAngle
                                v <- evalFloatExp vel
                                return (Turn (Const ((ang - angAct)/v)) vel)

stepComm (Lookat p v)      = do px <- evalFloatExp (fst p)
                                py <- evalFloatExp (snd p)
                                return (Turn (Const (180 / pi * atan2 px py)) v)

stepComm (Goline p v1 v2)  = do dist <- evalFloatExp (Dist p)
                                v2d <- evalFloatExp v2
                                return (Seq (Lookat p v1) (Fd (Const (dist/v2d)) v2))

stepComm (GolineAbs p v1 v2)  = do px <- evalFloatExp (fst p)
                                   py <- evalFloatExp (snd p)
                                   v2d <- evalFloatExp v2
                                   dist <- evalFloatExp (Dist p)
                                   return (Seq (TurnAbs (180 / pi * atan2 px py) v1) (Fd (Const (dist/v2d)) v2))

stepComm (Follow (LPoint []) v1 v2) = return Skip
stepComm (Follow (LPoint [p]) v1 v2) = return (GolineAbs p v1 v2)
stepComm (Follow (LPoint (p:q:ps)) v1 v2) = return (Seq (GolineAbs p v1 v2) (Follow (LPoint ((Minus (fst q) (fst p),Minus (snd q) (snd p)):ps)) v1 v2))
stepComm (Follow (Path exp v list) v1 v2) = do lp <- evalListPoint (Path exp v list)
                                               return (Follow lp v1 v2)


stepComm (FollowSmart (LPointAllow []) contingency v1 v2) = return Skip
-- Si queda un solo punto en el camino y no esta obstaculizado va hacia el punto
stepComm (FollowSmart (LPointAllow [(p,True)]) contingency v1 v2) = return (GolineAbs p v1 v2)
-- Si quedan mas de 1 punto y no esta obstaculizado va hacia el punto y continua el camino
stepComm (FollowSmart (LPointAllow ((p,True):(q,b):xs)) contingency v1 v2) = return (Seq (GolineAbs p v1 v2) (FollowSmart (LPointAllow (((Minus (fst q) (fst p),Minus (snd q) (snd p)), b):xs)) contingency v1 v2))
-- Si esta obstaculizado y no tiene camino de contingencia se saltea el punto y va al siguiente
stepComm (FollowSmart (LPointAllow ((p,False):(q,b):xs)) (LPointAllow []) v1 v2) = return (FollowSmart (LPointAllow (((Minus (fst q) (fst p),Minus (snd q) (snd p)), b):xs)) (LPointAllow []) v1 v2)
-- Si esta obstaculizado y es el ultimo punto del camino termina
stepComm (FollowSmart (LPointAllow [(p,False)]) contingency v1 v2) = return Skip
-- Si esta obstaculizado y no es el ultimo punto del camino toma el camino de contingencia
stepComm (FollowSmart (LPointAllow ((p,False):xs)) (LPointAllow ((c,b):cs)) v1 v2) = return (FollowSmart (LPointAllow  ((((Plus (fst c) (fst p), Plus (snd c) (snd p)),b) : cs) ++ xs)) (LPointAllow ((c,b):cs)) v1 v2)
stepComm (FollowSmart (Obs (LPoint lpoint) list) lpa v1 v2) = stepComm (FollowSmart (transformObs (Obs (LPoint lpoint) list)) lpa v1 v2)
stepComm (FollowSmart lpa (Obs (LPoint lpoint) list) v1 v2) = stepComm (FollowSmart lpa (transformObs (Obs (LPoint lpoint) list)) v1 v2)

-- Funciones auxiliares
consListPoint :: Point -> ListPoint -> ListPoint
consListPoint x (LPoint xs) = LPoint (x : xs)

-- Transforma una lista de puntos a una lista de obstaculos con pares ordenados
transformObs :: Obstacle -> Obstacle
transformObs (Obs (LPoint lpoint) list) = LPointAllow [(point, i `elem` list) | (point , i) <- zip lpoint [0..]]
transformObs (LPointAllow [xsAlow]) = LPointAllow [xsAlow]

-- Evalua ListPoint
evalListPoint :: (MonadState m, MonadError m, MonadTrace m, MonadLogo m) => ListPoint -> m ListPoint
evalListPoint (Path exp v list) = case list of
                                        [] -> return (LPoint [])
                                        x:xs -> do b <- varexist v
                                                   if b then do
                                                       do valv <- lookfor v
                                                          valx <- evalFloatExp x
                                                          update v valx
                                                          valy <- evalFloatExp exp
                                                          update v valv
                                                          return (consListPoint (Const valx,Const valy) (Path exp v xs))
                                                   else do
                                                          valx <- evalFloatExp x
                                                          update v valx
                                                          valy <- evalFloatExp exp
                                                          delete v
                                                          return (consListPoint (Const valx,Const valy) (Path exp v xs))
evalListPoint (LPoint xs) = return (LPoint xs)

-- Evalua una expresion decimal, sin efectos laterales
evalFloatExp :: (MonadState m, MonadError m) => Expf -> m Double
evalFloatExp (Const n)   = return n
evalFloatExp (Var v)     = do lookfor v
evalFloatExp (UMinus e)  = do val <- evalFloatExp e
                              return (negate val)
evalFloatExp (Plus l r)  = do lval <- evalFloatExp l
                              rval <- evalFloatExp r
                              return (lval + rval)
evalFloatExp (Minus l r) = do lval <- evalFloatExp l
                              rval <- evalFloatExp r
                              return (lval - rval)
evalFloatExp (Times l r) = do lval <- evalFloatExp l
                              rval <- evalFloatExp r
                              return (lval * rval)
evalFloatExp (Div l r)   = do lval <- evalFloatExp l
                              rval <- evalFloatExp r
                              if rval == 0 then throw
                              else return (lval/rval)
evalFloatExp (Cos e)  = do val <- evalFloatExp e
                           return (cos val)
evalFloatExp (Sen e)  = do val <- evalFloatExp e
                           return (sin val)
evalFloatExp (Tan e)  = do val <- evalFloatExp e
                           return (tan val)
evalFloatExp (Log e)  = do val <- evalFloatExp e
                           return (log val)
evalFloatExp (Pot a b)  = do vala <- evalFloatExp a
                             valb <- evalFloatExp b
                             return (vala ** valb)
evalFloatExp (Ceil e)  = do vale <- evalFloatExp e
                            return (fromIntegral (ceiling vale))
evalFloatExp (Floor e)  = do vale <- evalFloatExp e
                             return (fromIntegral (floor vale))
evalFloatExp (Rnd e n)  = do vale <- evalFloatExp e
                             return (roundTo vale n)
evalFloatExp (Dist p)       = do px <- evalFloatExp (fst p)
                                 py <- evalFloatExp (snd p)
                                 return (sqrt (px**2+py**2))
evalFloatExp (Gangle p)     = do x <- evalFloatExp (fst p)
                                 y <- evalFloatExp (snd p)
                                 return (180/pi * atan2 x y)
evalFloatExp (Rangle p)     = do x <- evalFloatExp (fst p)
                                 y <- evalFloatExp (snd p)
                                 return (atan2 x y)

roundTo :: Double -> Integer -> Double
roundTo x n = fromInteger (round (x * (10^n))) / (10.0^^n)

evalBoolExp :: (MonadState m, MonadError m, MonadTrace m) => BoolExp -> m Bool
evalBoolExp BTrue     = return True
evalBoolExp BFalse    = return False
evalBoolExp (Eq l r)  = do lval <- evalFloatExp l
                           rval <- evalFloatExp r
                           return (lval == rval)
evalBoolExp (Lt l r)  = do lval <- evalFloatExp l
                           rval <- evalFloatExp r
                           return (lval < rval)
evalBoolExp (Gt l r)  = do lval <- evalFloatExp l
                           rval <- evalFloatExp r
                           return (lval > rval)
evalBoolExp (And l r) = do lval <- evalBoolExp l
                           rval <- evalBoolExp r
                           return (lval && rval)
evalBoolExp (Or l r)  = do lval <- evalBoolExp l
                           rval <- evalBoolExp r
                           return (lval || rval)
evalBoolExp (Not b)   = do bval <- evalBoolExp b
                           return (not bval)