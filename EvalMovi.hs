module EvalMovi (eval, getEnvVar) where

import ASTMovi
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)

import Control.Monad.IO.Class (MonadIO, liftIO)

import Control.Concurrent
import GHC.IO.Handle
import System.IO

-- Definimos los dos posibles tipos de error
data Error = UndefVar | DivByZero | UndefSensor | Block

-- Estados
type EnvV = [(Variable,Double)]
type EnvS = [(Sensor,Bool)]
type EnvAng = Double
type EnvSucces = Bool
type EnvPoint = Point
type Env = (EnvV, EnvS, EnvAng, EnvSucces, EnvPoint)

-- Estado nulo
initState :: Env
initState = ([],[("front", False)], 0.0, True, (Const 0.0,Const 0.0))

-- Monada de estado
newtype State a = State { runState :: Env -> Either Error (a, Env, String) }

instance Monad State where
    return x = State (\s -> Right (x, s, ""))
    m >>= f = State (\s -> do (v, s', tr) <- runState m s
                              (v', s'', tr') <- runState (f v) s'
                              return (v', s'', tr ++ tr'))

-- Clase para representar mónadas con estado de variables.
class Monad m => MonadState m where
    lookfor :: Variable -> m Double
    update :: Variable -> Double -> m ()
    delete :: Variable -> m () -- Para borrar una variable del estado
    varexist :: Variable -> m Bool -- Para determinar si una variable existe en el estado
    sensorUpdate :: Sensor -> Bool -> m()
    sensorState :: Sensor -> m Bool
    addAngle :: Double -> m ()
    getAngle :: m Double
    putSuccess :: Bool -> m()
    getSuccess :: m Bool
    putPoint :: Point -> m()
    getPoint :: m Point

instance MonadState State where
    -- Dos operaciones para analizar sensores
    sensorUpdate sen val = State (\s -> Right ((), (getEnvVar s,sensorUpdate' sen val (getEnvSensor s), getEnvAngle s, getEnvSuccess s, getEnvPoint s), ""))
                            where sensorUpdate' sen val [] = [(sen, val)]
                                  sensorUpdate' sen val ((sen', val'):ss) | sen == sen' = (sen, val):ss
                                                                          | otherwise   = (sen', val'):sensorUpdate' sen val ss
    sensorState sen = State (\s -> either Left (\v -> Right (v, s, "")) (sensorState' sen (getEnvSensor s)))
                        where sensorState' sen [] = Left UndefSensor
                              sensorState' sen ((sensor,state):ss) | sen == sensor = Right state
                                                                   | otherwise     = sensorState' sen ss
    -- Dos operaciones para actualizar y tomar valores de variables
    lookfor var = State (\s -> either Left (\v -> Right (v, s, "")) (lookfor' var (getEnvVar s)))
                    where lookfor' var []               = Left UndefVar
                          lookfor' var ((var', val):ss) | var == var' = Right val
                                                        | otherwise   = lookfor' var ss
    update var val = State (\s -> Right ((), (update' var val (getEnvVar s), getEnvSensor s, getEnvAngle s, getEnvSuccess s, getEnvPoint s), ""))
                     where update' var val [] = [(var, val)]
                           update' var val ((var', val'):ss) | var == var' = (var, val):ss
                                                             | otherwise   = (var', val'):update' var val ss
    -- Dos operaciones utiles para el Path: delete y varexist
    delete var = State (\s -> Right ((), (delete' var (getEnvVar s), getEnvSensor s, getEnvAngle s, getEnvSuccess s, getEnvPoint s), ""))
                    where delete' var [] = []
                          delete' var ((var', val'):ss) | var == var' = ss
                                                        | otherwise   = (var', val'):delete' var ss
    varexist var = State (\s -> Right (varexist' var (getEnvVar s), s, ""))
                    where varexist' var []               = False
                          varexist' var ((var', val):ss) | var == var' = True
                                                         | otherwise   = varexist' var ss
    -- Dos operaciones utiles para analizar el angulo del movil
    addAngle var = State (\s -> Right ((), (getEnvVar s, getEnvSensor s, correctAngle (var + getEnvAngle s), getEnvSuccess s, getEnvPoint s), ""))
    getAngle = State (\s -> Right (getEnvAngle s, s, ""))
    -- Dos operaciones utiles para analizar el exito del ultimo comando
    putSuccess val = State (\s -> Right ((), (getEnvVar s, getEnvSensor s, getEnvAngle s, val, getEnvPoint s), ""))
    getSuccess = State (\s -> Right (getEnvSuccess s, s, ""))
    -- Dos operaciones utiles para el punto objetivo en los comandos inteligentes
    putPoint point = State (\s -> Right ((), (getEnvVar s, getEnvSensor s, getEnvAngle s, getEnvSuccess s, point), ""))
    getPoint = State (\s -> Right (getEnvPoint s, s, ""))

---------------------------------------------------------------------
-- Funciones auxiliares para desempaquetar tupla de tres elementos --
---------------------------------------------------------------------
getEnvVar :: (EnvV, EnvS, EnvAng, EnvSucces, EnvPoint) -> EnvV
getEnvVar (a, _, _, _, _) = a

getEnvSensor :: (EnvV, EnvS, EnvAng, EnvSucces, EnvPoint) -> EnvS
getEnvSensor (_, a, _, _, _) = a

getEnvAngle :: (EnvV, EnvS, EnvAng, EnvSucces, EnvPoint) -> EnvAng
getEnvAngle (_, _, a, _, _) = a

getEnvSuccess :: (EnvV, EnvS, EnvAng, EnvSucces, EnvPoint) -> EnvSucces
getEnvSuccess (_, _, _, a, _) = a

getEnvPoint :: (EnvV, EnvS, EnvAng, EnvSucces, EnvPoint) -> EnvPoint
getEnvPoint (_, _, _, _, a) = a

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

class Monad m => MonadErrorSensor m where
    throwSensor :: m a

instance MonadErrorSensor State where
    throwSensor = State (\_ -> Left Block)

-- Clase para representar mónadas que marca la traza.
class Monad m => MonadTrace m where
    trace :: String -> Double -> Double -> m ()

instance MonadTrace State where
    trace str v t = State (\s -> Right ((), s, str ++ " | Vel: " ++ show v ++ " | Time: " ++ show t ++ "\n"))

-- Clase para representar mónadas que marca la traza.
class Monad m => MonadLogo m where
    logo :: String -> Double -> Double -> m ()

instance MonadLogo State where
    logo str v t = State (\s -> Right ((), s, str ++ show (v*t) ++ "\n"))

-- Para calmar al GHC
instance Functor State where
    fmap = liftM

instance Applicative State where
    pure = return
    (<*>) = ap

-- Evalua un programa en el estado nulo
-- Matcheamos los errores UndefVar y DivByZero para tener un mensaje de error distinto en cada tipo de error
eval :: Comm -> (Env, String)
eval p = case runState (stepCommStar p) initState of
    Right (v, s, str) -> (s, str)
    Left UndefVar     -> error "Variable no definida"
    Left DivByZero    -> error "Division por cero"
    Left UndefSensor  -> error "Sensor no definido"
    Left Block        -> error "Interrupcion sin salida"

-- Evalua multiples pasos de un comnado en un estado,
-- hasta alcanzar un Skip
{-
stepCommStar :: Comm -> IO ()
stepCommStar Skip = return ()
stepCommStar p = do putStrLn(comm2str p ++ "\n")
                    threadDelay 1000000
                    x <- ifReadyDo stdin getChar
                    if x == Just '\n'
                                then do stepComm (USensor "front" True)
                                        p' <- stepComm p
                                        stepComm (USensor "front" False)
                                        stepCommStar p'
                                else do do p' <- stepComm p
                                           stepCommStar p'
-}

stepCommStar :: (MonadState m, MonadError m, MonadTrace m, MonadLogo m, MonadErrorSensor m) => Comm -> m Comm
stepCommStar Skip = return Skip
stepCommStar p = do p' <- stepComm p
                    stepCommStar p'

ifReadyDo :: Handle -> IO a -> IO (Maybe a)
ifReadyDo hnd x = hReady hnd >>= f
   where f True = x >>= return . Just
         f _    = return Nothing

--Funcion auxiliar para imprimir comandos
comm2str :: Comm -> String
comm2str (Fd vel time) = "PFoward | Vel: " ++ show vel ++ " | Time: " ++ show time
comm2str (Turn vel time) = "PTurn | Vel: " ++ show vel ++ " | Time: " ++ show time
comm2str (FollowSmart lp contingency vel1 vel2) = "PFollowSmart | Listpoint: " ++ show lp ++ " | Contingency: " ++ show contingency ++ " | Vel F: " ++ show vel1 ++ " | Vel T: " ++  show vel2

-- Evalua un paso de una primitiva en un estado dado
stepComm :: (MonadState m, MonadError m, MonadTrace m, MonadLogo m, MonadErrorSensor m) => Comm -> m Comm
{-
stepComm (USensor sen val) = do sensorUpdate sen val
                                return (Skip)-}
stepComm Skip               = return (Skip)
stepComm (Letf v e)         = do val <- evalFloatExp e
                                 update v val
                                 return Skip
stepComm (Seq Skip c1)      = return c1
stepComm (Seq l r)          = do stepComm l
                                 return r
stepComm (Cond b c0 c1)     = do bval <- evalBoolExp b
                                 if bval then return (c0)
                                         else return (c1)
stepComm (While b c)     = do bval <- evalBoolExp b
                              if bval then return (Seq c (While b c))
                                      else return (Skip)
stepComm (Fd time vel)      = do end <- evalBoolExp (Or (Eq time (Const 0)) (Lt time (Const 0)))
                                 if end
                                 then return Skip
                                      else do v <- evalFloatExp vel
                                              t <- evalFloatExp time
                                              trace "Fd" v t
                                              logo "fd" v t
                                              return (Fd (Minus time (Const 1.0)) (vel))

stepComm (Turn time vel) = do v <- evalFloatExp vel
                              t <- evalFloatExp time
                              trace "Turn" v t
                              logo "rt" v t
                              addAngle (v*t)
                              return (Skip)

stepComm (TurnAbs ang vel) = do angAct <- getAngle
                                v <- evalFloatExp vel
                                return (Turn (Const ((ang - angAct)/v)) vel)

stepComm (Lookat p v)      = do px <- evalFloatExp (fst p)
                                py <- evalFloatExp (snd p)
                                return (Turn (Const (180 / pi * (atan2 px py))) v)

stepComm (Goline p v1 v2)  = do dist <- evalFloatExp (Dist p)
                                v2d <- evalFloatExp (v2)
                                return (Seq (Lookat p v1) (Fd (Const (dist/v2d)) v2))

stepComm (GolineAbs p v1 v2)  = do px <- evalFloatExp (fst p)
                                   py <- evalFloatExp (snd p)
                                   v2d <- evalFloatExp (v2)
                                   dist <- evalFloatExp (Dist p)
                                   return (Seq (TurnAbs (180 / pi * (atan2 px py)) v1) (Fd (Const (dist/v2d)) v2))

{-
stepComm (GolineAbsSmart p v1 v2)  = do px <- evalFloatExp (fst p)
                                        py <- evalFloatExp (snd p)
                                        v2d <- evalFloatExp (v2)
                                        dist <- evalFloatExp (Dist p)
                                        return (Seq (TurnAbs (180 / pi * (atan2 px py)) v1) (FdSmart (Const (dist/v2d)) v2 (Const 0.0)))
-}

stepComm (Follow (LPoint []) v1 v2) = return Skip
stepComm (Follow (LPoint [p]) v1 v2) = return (GolineAbs p v1 v2)
stepComm (Follow (LPoint (p:q:ps)) v1 v2) = return (Seq (GolineAbs p v1 v2) (Follow (LPoint ((Minus (fst q) (fst p),Minus (snd q) (snd p)):ps)) v1 v2))

stepComm (FollowSmart (LPointAllow []) contingency v1 v2) = return (Skip)
-- Si queda un solo punto en el camino y no esta obstaculizado va hacia el punto
stepComm (FollowSmart (LPointAllow [(p,True)]) contingency v1 v2) = return (GolineAbs p v1 v2)
-- Si quedan mas de 1 punto y no esta obstaculizado va hacia el punto y continua el camino
stepComm (FollowSmart (LPointAllow ((p,True):(q,b):xs)) contingency v1 v2) = return (Seq (GolineAbs p v1 v2) (FollowSmart (LPointAllow (((Minus (fst q) (fst p),Minus (snd q) (snd p)), b):xs)) contingency v1 v2))
-- Si esta obstaculizado y no tiene camino de contingencia se saltea el punto y va al siguiente
stepComm (FollowSmart (LPointAllow ((p,False):(q,b):xs)) (LPointAllow []) v1 v2) = return (FollowSmart (LPointAllow (((Minus (fst q) (fst p),Minus (snd q) (snd p)), b):xs)) (LPointAllow []) v1 v2)
-- Si esta obstaculizado y es el ultimo punto del camino termina
stepComm (FollowSmart (LPointAllow [(p,False)]) contingency v1 v2) = return (Skip)
-- Si esta obstaculizado y no es el ultimo punto del camino toma el camino de contingencia
stepComm (FollowSmart (LPointAllow ((p,False):xs)) (LPointAllow ((c,b):cs)) v1 v2) = return (FollowSmart (LPointAllow  ((((Plus (fst c) (fst p), Plus (snd c) (snd p)),b) : cs) ++ xs)) (LPointAllow ((c,b):cs)) v1 v2)

-- Funciones auxiliares
consListPoint :: Point -> ListPoint -> ListPoint
consListPoint x (LPoint xs) = LPoint (x : xs)

recalculatePoint :: Point -> ListPoint -> Point
recalculatePoint p (LPoint []) = p
recalculatePoint (px,py) (LPoint ((cx,cy):cs)) = recalculatePoint (Minus px cx,Minus py cy) (LPoint cs)

-- Evalua Obstacle
evalObs :: (MonadState m, MonadError m, MonadTrace m, MonadErrorSensor m) => Obstacle -> m Obstacle
evalObs (Obs (LPoint lpoint) list) = return (LPointAllow [(point, i `elem` list) | (point , i) <- zip lpoint [0..]])
evalObs (LPointAllow [xsAlow]) = return (LPointAllow [xsAlow])

-- Evalua ListPoint
evalListPoint :: (MonadState m, MonadError m, MonadTrace m, MonadErrorSensor m) => ListPoint -> m ListPoint
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

evalBoolExp :: (MonadState m, MonadError m, MonadTrace m, MonadErrorSensor m) => BoolExp -> m Bool
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