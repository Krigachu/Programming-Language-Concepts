--JORKRI Interpreter

module Evaluator where
import Parsing

--data Expr = Program | Line | Condition | Exp | Factor

type Environment = [ (String, Factor) ]
type State = (Program, Environment)
type CState = (Condition, Environment)
type EState = (Exp, Environment)
type FState = (Factor, Environment)


isValue :: Factor -> Bool
isValue (JKInt _) = True
isValue (JKReal _) = True
isValue JKTrue = True
isValue JKFalse = True
isValue _ = False

deparse :: Factor -> String 
deparse (JKInt n) = show n
deparse (JKReal n) = show n
deparse (JKTrue) = "true"
deparse (JKFalse) = "false"
deparse _ = "Unknown"

addVariable :: String -> Int -> Environment -> Environment
addVariable s e [] = (s, JKInt e):[] 
addVariable s e ((y,x):env) | s == y = ((y, JKInt e):env)
                            | otherwise = (y,x) : addVariable s e env

--getting variable from environment, maybe type could be used here
getVariable :: String -> Environment -> Int
getVariable s [] = error "variable used before definition... u big dumb"
getVariable s ((y,e):env) | s == y = evalFactor (e, env)
                          | otherwise = getVariable s env

--eval :: State -> State
--eval ((JKProgram l p), env) = (evalLine (l, env))
--eval ((JKFinalLine l), env) = (evalLine (l, env))

--Deal with if statements
dealIf :: Program -> Environment -> Environment
dealIf p env = snd $evalLine(p, env)
--Deal with while loops
dealWhile :: Condition -> Environment -> Program -> Environment
dealWhile c env p | evalCondition (c, env) == True = dealWhile c (snd (evalLine (p, env))) p
                  | otherwise = env

evalLine :: State -> State
evalLine (JKProgram (JKTypeEqualLine t s e) prog, env) = evalLine(prog, addVariable s (evalExp (e, env)) env)
evalLine (JKFinalLine (JKTypeEqualLine t s e), env) = (JKFinalLine (JKTypeEqualLine t s e), addVariable s (evalExp (e, env)) env)
evalLine (JKProgram (JKEqualLine s e) prog, env) = evalLine(prog, addVariable s (evalExp (e, env)) env)
evalLine (JKFinalLine (JKEqualLine s e), env) = (JKFinalLine (JKEqualLine s e), addVariable s (evalExp (e, env)) env)
evalLine (JKProgram (JKIf c p) prog, env) | evalCondition(c, env) == True = evalLine(prog, dealIf p env )
                                          | otherwise = evalLine(prog, env)
evalLine (JKFinalLine (JKIf c p), env) | evalCondition(c, env) == True = evalLine(p, env)
                                       | otherwise = (JKFinalLine (JKIf c p), env)
evalLine (JKProgram (JKWhile c p) prog, env) | evalCondition(c, env) == True = evalLine(prog, dealWhile c env p)
                                             | otherwise = evalLine(prog, env)
evalLine (JKFinalLine (JKWhile c p), env) | evalCondition(c, env) == True = (JKFinalLine (JKWhile c p), dealWhile c env p)
                                          | otherwise = (JKFinalLine (JKWhile c p), env)
--evalLine (JKProgram (JKFor f f2 p) prog, env) = dealFor
--evalLine (JKFinalLine (JKFor f f2 p), env) = dealFor
evalLine (JKProgram (JKOutput e) prog, env) = evalLine (prog, env)
-- do print ( evalExp (e) )
-- evalState <- evalLine (prog, env)
evalLine (JKFinalLine (JKOutput e), env) = (JKFinalLine (JKOutput e), env)
-- do print ( evalExp (e) )
-- evalState <- (JKFinalLine (JKOutput e), env)

evalCondition :: CState -> Bool
evalCondition (JKCompareLess e1 e2, env) | evalExp (e1, env) < evalExp (e2, env) = True
                                         | otherwise = False
evalCondition (JKCompareMore e1 e2, env) | evalExp (e1, env) > evalExp (e2, env) = True
                                         | otherwise = False
evalCondition (JKCompareEqual e1 e2, env) | evalExp (e1, env) == evalExp (e2, env) = True
                                          | otherwise = False
evalCondition (JKAnd c1 c2, env) = evalCondition (c1, env) && evalCondition (c2, env)
evalCondition (JKOr c1 c2, env) = evalCondition (c1, env) || evalCondition (c2, env)
evalCondition (JKNot c, env) = not ( evalCondition (c, env) )

evalExp :: EState -> Int
evalExp (Bracket e, env) = evalExp (e, env)
evalExp (JKAdd e1 e2, env) = evalExp (e1, env) + evalExp(e2, env)
evalExp (JKSubtract e1 e2, env) = evalExp (e1, env) - evalExp (e2, env)
evalExp (JKMultiply e1 e2, env) = evalExp (e1, env) * evalExp (e2, env)
--evalExp (JKDivide e1 e2) = evalExp (e1) / evalExp (e2)
evalExp (JKPower e1 e2, env) = evalExp (e1, env) ^ evalExp (e2, env)
evalExp (JKDiv e1 e2, env) = evalExp (e1, env) `div` evalExp (e2, env)
evalExp (JKMod e1 e2, env) = evalExp (e1, env) `mod` evalExp (e2, env)
evalExp (JKFactor e, env) = evalFactor (e, env)

evalFactor :: FState -> Int
evalFactor (JKInt i, env) = i
--evalFactor (JKReal d) = d
--evalFactor (JKFalse) = False
--evalFactor (JKTrue) = True
evalFactor (JKVar s, env) = getVariable s env

--evalType (TyBool) = Bool
--evalType (TyStr) = String
--evalType (TyInt) = Int
--evalType (TyReal) = Real
