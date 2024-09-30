module Parser where

import Text.ParserCombinators.Parsec
import Text.Parsec.Token
import Text.Parsec.Language (emptyDef)
import ASTMovi

-- Funcion para facilitar el testing del parser.
totParser :: Parser a -> Parser a
totParser p = do
                  whiteSpace movi
                  t <- p
                  eof
                  return t

-- Analizador de Tokens
-- https://wiki.haskell.org/Parsing_a_simple_imperative_language
movi :: TokenParser u
movi = makeTokenParser (emptyDef   { commentStart  = "/*"
                                  , commentEnd    = "*/"
                                  , commentLine   = "//"
                                  , reservedNames = ["true","false","skip","if","then",
                                                     "else","end", "while","do", "in", "dist",
                                                     "fd", "bk", "turn", "lookat", "goline",
                                                     "path", "follow", "followsmart", "sen","cos",
                                                     "tan", "pot", "log", "round", "floor","ceil",
                                                     "def_obstacles"]
                                  , reservedOpNames = [  "+"
                                                       , "-"
                                                       , "*"
                                                       , "/"
                                                       , "<"
                                                       , ">"
                                                       , "&"
                                                       , "|"
                                                       , "="
                                                       , ";"
                                                       , ","
                                                       , "~"
                                                       , ":="
                                                       , "("
                                                       , ")"
                                                       , "["
                                                       , "]"
                                                       ]
                                   }
                                 )

-- Parsear generador de obstculs

----------------------------------
--- Parser de expressiones flotantes
-----------------------------------
{--
chainl p op x
parsea 0 o más ocurrencias de p separadas por op
Retorna el valor que se obtiene al aplicar todas las
funciones retornadas por op a los valores retornados
por p
--}

floatexp :: Parser Expf
floatexp  = chainl1 term addopp
term = chainl1 factor multopp

factor = try (parens movi floatexp)
         <|> try (do reservedOp movi "-"
                     f <- factor
                     return (UMinus f))
         <|> try (do f <- float movi
                     return (Const f))
         <|> try (do str <- identifier movi
                     return (Var str))
         <|> try (do reservedOp movi "cos"
                     reservedOp movi "("
                     exp <- floatexp
                     reservedOp movi ")"
                     return (Cos exp))
         <|> try (do reservedOp movi "sen"
                     reservedOp movi "("
                     exp <- floatexp
                     reservedOp movi ")"
                     return (Sen exp))
         <|> try (do reservedOp movi "tan"
                     reservedOp movi "("
                     exp <- floatexp
                     reservedOp movi ")"
                     return (Tan exp))
         <|> try (do reservedOp movi "log"
                     reservedOp movi "("
                     exp <- floatexp
                     reservedOp movi ")"
                     return (Log exp))
         <|> try (do reservedOp movi "pot"
                     reservedOp movi "("
                     e <- floatexp
                     reservedOp movi ","
                     x <- floatexp
                     reservedOp movi ")"
                     return (Pot e x))
         <|> try (do reservedOp movi "round"
                     reservedOp movi "("
                     x <- floatexp
                     reservedOp movi ","
                     i <- integer movi
                     reservedOp movi ")"
                     return (Rnd x i))
         <|> try (do reservedOp movi "ceil"
                     reservedOp movi "("
                     x <- floatexp
                     reservedOp movi ")"
                     return (Ceil x))
         <|> try (do reservedOp movi "floor"
                     reservedOp movi "("
                     x <- floatexp
                     reservedOp movi ")"
                     return (Floor x))
         <|> try (do reserved movi "dist"
                     reservedOp movi "("
                     p <- point
                     reservedOp movi ")"
                     return (Dist p))
         <|> try (do reserved movi "gangle"
                     reservedOp movi "("
                     p <- point
                     reservedOp movi ")"
                     return (Gangle p))
         <|> try (do reserved movi "rangle"
                     reservedOp movi "("
                     p <- point
                     reservedOp movi ")"
                     return (Rangle p))
-- TODO between "(" ")" -> floatexp 
{--         <|> try (do n <- integer movi
                     f <- (n::Double)
                     return (Const f))
--}

multopp = do try (reservedOp movi "*")
             return Times
          <|> do try (reservedOp movi "/")
                 return Div

addopp = do try (reservedOp movi "+")
            return Plus
         <|> do try (reservedOp movi "-")
                return Minus

-----------------------------------
--- Parser de expresiones booleanas
------------------------------------
boolexp :: Parser BoolExp
boolexp  = chainl1 boolexp2 (try (do reservedOp movi "|"
                                     return Or))

boolexp2 = chainl1 boolexp3 (try (do reservedOp movi "&"
                                     return And))

boolexp3 = try (parens movi boolexp)
           <|> try (do reservedOp movi "~"
                       b <- boolexp3
                       return (Not b))
           <|> floatcomp
           <|> boolvalue

floatcomp = try (do i <- floatexp
                    c <- compopp
                    j <- floatexp
                    return (c i j))

compopp = try (do reservedOp movi "="
                  return Eq)
          <|> try (do reservedOp movi "<"
                      return Lt)
          <|> try (do reservedOp movi ">"
                      return Gt)

boolvalue = try (do reserved movi "true"
                    return BTrue)
            <|> try (do reserved movi "false"
                        return BFalse)


-----------------------------------
--- Parser de punto y de lista de puntos
-----------------------------------

point :: Parser Point
point = try (do reservedOp movi "("
                x1 <- floatexp
                reservedOp movi ","
                x2 <- floatexp
                reservedOp movi ")"
                return (x1,x2))

listpoint :: Parser ListPoint
listpoint = try (do reservedOp movi "["
                    ps <- sepBy point (char ',')
                    reservedOp movi "]"
                    return (LPoint ps))
            <|> try (do reserved movi "path"
                        reservedOp movi "("
                        fun <- floatexp
                        reservedOp movi ","
                        var <- identifier movi
                        reservedOp movi ","
                        reservedOp movi "["
                        list <- sepBy floatexp (char ',')
                        reservedOp movi "]"
                        reservedOp movi ")"
                        return (Path fun var list))

-----------------------------------
--- Parser de obstaculos
-----------------------------------

obstacle :: Parser Obstacle
obstacle = try (do reserved movi "def_obstacles"
                   reservedOp movi "("
                   lpoint <- listpoint
                   reservedOp movi ","
                   reservedOp movi "["
                   list <- sepBy (natural movi) (char ',')
                   reservedOp movi "]"
                   return LPointAllow [(point, elem i list) | (point , i) <- zip (getPoints lpoint) [0..]]

--return (Obs lpoint list))

-- Funcion auxiliar
getPoints :: ListPoint -> [Point]
getPoints (LPoint [xs]) = [xs]

-- Evalua Obstacle
evalObs :: Obstacle -> Obstacle
evalObs (Obs (LPoint lpoint) list) = LPointAllow [(point, elem i list) | (point , i) <- zip lpoint [0..]]

{-
TransormAux :: Obstacle -> Obstacle
TransormAux Obs (LPoint xs) [] = Allowed (LPoint xs)
TransormAux Obs (LPoint x:xs) y:ys = if y < 0 then TransormAux Obs (LPoint x:xs) ys
                                              else if y = 0 then lConcatAllow [(x,False)] TransormAux Obs (LPoint xs) (resta1 ys)
                                                            else lConcatAllow [(x,True)] TransormAux Obs (LPoint xs) (resta1 ys)

lConcatAllow ::  Obstacle -> Obstacle -> Obstacle
lConcatAllow (LPointAllow xs) (LPointAllow ys) = (LPointAllow (xs++ys))

Allowed :: ListPoint -> Obstacle
Allowed ListPoint [] = LPointAllow []
Allowed ListPoint (x:xs) = lConcatAllow (x,True) (Allowed xs)

resta1 :: [Int] -> [Int]
resta1 [] = []
resta1 x:xs = (x-1): resta1 xs

ordenaylimpia :: [Int] -> [Int]
ordenaylimpia xs = ordenaylimpiaaux (ordena xs) 

ordenaylimpiaaux :: [Int] -> [Int]
ordenaylimpiaaux [] = []
ordenaylimpiaaux x:xs = if x < 0 then ordenaylimpiaaux xs else (x:xs)

ordena :: [Int] -> [Int]
ordena [] = []
ordena xs = m : ordena (delete m xs)
    where m = minimum xs                                      
--}

-----------------------------------
--- Parser de comandos
-----------------------------------
comm :: Parser Comm
comm = chainl1 comm2 (try (do reservedOp movi ";"
                              return Seq))

comm2 = try (do reserved movi "skip"
                return Skip)
        <|> try (do reserved movi "if"
                    reservedOp movi "("
                    cond <- boolexp
                    reservedOp movi ")"
                    reserved movi "then"
                    case1 <- comm
                    reserved movi "else"
                    case2 <- comm
                    reserved movi "end"
                    return (Cond cond case1 case2))
        <|> try (do reserved movi "while"
                    reservedOp movi "("
                    cond <- boolexp
                    reservedOp movi ")"
                    reserved movi "do"
                    c <- comm
                    reserved movi "end"
                    return (While cond c))
        <|> try (do str <- identifier movi
                    reservedOp movi ":="
                    e <- floatexp
                    return (Letf str e))
        <|> try comm3

comm3 :: Parser Comm
comm3 = try (do reserved movi "fd"
                reservedOp movi "("
                dist <- floatexp
                reservedOp movi ","
                time <- floatexp
                reservedOp movi ")"
                return (Fd time (Div dist time)))
        <|> try (do reserved movi "bk"
                    reservedOp movi "("
                    dist <- floatexp
                    reservedOp movi ","
                    time <- floatexp
                    reservedOp movi ")"
                    return (Fd time (UMinus (Div dist time))))
        <|> try (do reserved movi "turn"
                    reservedOp movi "("
                    ang <- floatexp
                    reservedOp movi ","
                    time <- floatexp
                    reservedOp movi ")"
                    return (Turn time (Div ang time)))
        <|> try comm4

comm4 :: Parser Comm
comm4 = try (do reserved movi "lookat"
                reservedOp movi "("
                p <- point
                reservedOp movi ","
                v <- floatexp
                reservedOp movi ")"
                return (Lookat p v))
        <|> try (do reserved movi "goline"
                    reservedOp movi "("
                    p <- point
                    reservedOp movi ","
                    v1 <- floatexp
                    reservedOp movi ","
                    v2 <- floatexp
                    reservedOp movi ")"
                    return (Goline p v1 v2))
        <|> try (do reserved movi "follow"
                    reservedOp movi "("
                    list <- listpoint
                    reservedOp movi ","
                    v1 <- floatexp
                    reservedOp movi ","
                    v2 <- floatexp
                    reservedOp movi ")"
                    return (Follow list v1 v2))
        <|> try (do reserved movi "followsmart"
                    reservedOp movi "("
                    list <- obstacle
                    reservedOp movi ","
                    listcontingency <- obstacle
                    reservedOp movi ","
                    v1 <- floatexp
                    reservedOp movi ","
                    v2 <- floatexp
                    reservedOp movi ")"
                    return (FollowSmart list listcontingency v1 v2))

------------------------------------
-- Funcion de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)
