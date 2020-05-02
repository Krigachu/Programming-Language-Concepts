--JORKRI Interpreter

module Evaluator where
import Parsing

data Expr = Program | Line | Condition | Exp | Factor

type Environment = [ (String, Factor) ]
type State = (Expr, Environment)

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

addVariable :: String -> Exp -> Environment -> Environment
addVariable s e [] = (s,e):[] 
addVariable s e ((y,x):env) | s == y = ((y,e):env)
                            | otherwise = addVariable s e env

--getting variable from environment, maybe type could be used here
getVariable :: String -> Environment -> Exp
getVariable s [] = error "u big dumb"
getVariable s ((y,e):env) | s == y = e
                          | otherwise = getVariable s env

--eval :: State -> State
--eval ((JKProgram l p), env) = (evalLine (l, env))
--eval ((JKFinalLine l), env) = (evalLine (l, env))

--Deal with if statements
dealIf :: Program -> Environment -> Environment
dealIf(p, env) = snd $evalLine(p, env)
--Deal with while loops
dealWhile :: Condition -> Environment -> Program -> Environment
dealWhile c env p | c == True = dealWhile c (snd (evalLine (p env))) p
                  | otherwise = env

evalLine :: State -> State
evalLine (JKProgram (JKTypeEqualLine t s e) prog, env) = evalLine(prog, addVariable s e env)
evalLine (JKFinalLine (JkTypeEqualLine t s e), env) = (JKFinalLine (JkTypeEqualLine t s e), addVariable s e env)
evalLine (JKProgram (JKEqualLine s e) prog, env) = evalLine(prog, addVariable(s e env))
evalLine (JKFinalLine (JKEqualLine s e), env) = (JKFinalLine (JKEqualLine s e), addVariable s e env)
evalLine (JKProgram (JKIf c p) prog, env) | evalCondition(c, env) == True = evalLine(prog, dealIf (p, env) )
                                          | otherwise = evalLine(prog, env)
evalLine (JKFinalLine (JKIf c p), env) | evalCondition(c, env) == True = evalLine(p, env)
                                       | otherwise = (JKFinalLine (JKIf c p), env)
evalLine (JKProgram (JKWhile c p) prog, env) | evalCondition(c, env) == True = evalLine(prog, dealWhile c env p)
                                             | otherwise = evalLine(prog, env)
evalLine (JKFinalLine (JKWhile c p), env) | evalCondition(c, env) == True = (JKFinalLine (JKWhile c p), dealWhile c env p)
                                          | otherwise = (JKFinalLine (JKWhile c p), env)
--evalLine (JKProgram (JKFor f f2 p) prog, env) = dealFor
--evalLine (JKFinalLine (JKFor f f2 p), env) = dealFor
evalLine (JKProgram (JKOutput e) prog, env) = do print ( evalExp (e) )
                                                 evalState <- evalLine (prog, env)
evalLine (JKFinalLine (JKOutput e), env) = do print ( evalExp (e) )
                                              evalState <- (JKFinalLine (JKOutput e), env)

evalCondition :: State -> State
evalCondition (JKCompareLess e1 e2, ) | evalExp (e1) < evalExp (e2) = True
                                    | otherwise = False
evalCondition (JKCompareMore e1 e2) | evalExp (e1) > evalExp (e2) = True
                                    | otherwise = False
evalCondition (JKCompareEqual e1 e2) | evalExp (e1) == evalExp (e2) = True
                                     | otherwise = False
evalCondition (JKAnd c1 c2) = evalCondition (c1) && evalCondition (c2)
evalCondition (JKOr c1 c2) = evalCondition (c1) || evalCondition (c2)
evalCondition (JKNot c) = not ( evalCondition (c) )

evalExp :: State -> State
evalExp (Bracket e) = evalExp (e)
evalExp (JKAdd e1 e2) = evalExp (e1) + evalExp(e2)
evalExp (JKSubtract e1 e2) = evalExp (e1) - evalExp (e2)
evalExp (JKMultiply e1 e2) = evalExp (e1) * evalExp (e2)
--evalExp (JKDivide e1 e2) = evalExp (e1) / evalExp (e2)
evalExp (JKPower e1 e2) = evalExp (e1) ^ evalExp (e2)
evalExp (JKDiv e1 e2) = evalExp (e1) `div` evalExp (e2)
evalExp (JKMod e1 e2) = evalExp (e1) `mod` evalExp (e2)
evalExp (JKFactor e) = evalFactor (e)

evalFactor :: State -> State
evalFactor (JKInt i) = i
--evalFactor (JKReal d) = d
--evalFactor (JKFalse) = False
--evalFactor (JKTrue) = True
evalFactor (JKVar s) = getVariable

--evalType (TyBool) = Bool
--evalType (TyStr) = String
--evalType (TyInt) = Int
--evalType (TyReal) = Real
