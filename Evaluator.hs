--JORKRI Interpreter

module Evaluator where
import Parsing


type Environment = [ (String, Int) ]
type ListEnvironment = [ (String, [Int]) ]

type State = (Program, Environment, ListEnvironment)
type CState = (Condition, Environment, ListEnvironment)
type EState = (Exp, Environment, ListEnvironment)
type FState = (Factor, Environment, ListEnvironment)


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
addVariable s e [] = (s, e):[] 
addVariable s e ((y,x):env) | s == y = ((y, e):env)
                            | otherwise = (y,x) : addVariable s e env

--getting variable from environment, maybe type could be used here
getVariable :: String -> Environment -> Int
getVariable s [] = error "variable used before definition... u big dumb"
getVariable s ((y,e):env) | s == y = e
                          | otherwise = getVariable s env

addList :: String -> ListEnvironment -> ListEnvironment
addList s [] = (s, []):[] 
addList s ((y,x):env) | s == y = ((y, []):env)
                      | otherwise = (y,x) : addList s env

addToList :: String -> Int -> ListEnvironment -> ListEnvironment
addToList s e [] = error "uh oh, list not here"
addToList s e ((y,x):env) | s == y = ((y,(x++(e:[]))):env)
                          | otherwise = (y,x) : addToList s e env

--getting List from environment, maybe type could be used here
getList :: String -> ListEnvironment -> [Int]
getList s [] = error "u big dumb"
getList s ((y,e):env) | s == y = e
                      | otherwise = getList s env

--indexes start from 0
index :: String -> Int -> ListEnvironment -> Int
index s e [] = error "uh oh, list ain't here"
index s e ((y,x):env) | s == y = x !! e
                      | otherwise = index s e env

third (_, _, e) = e
second (_, e, _) = e

--Deal with if statements
dealIf :: Program -> Environment -> ListEnvironment -> (Environment, ListEnvironment)
dealIf p env lenv = (second $evalLine(p, env, lenv), third $evalLine(p, env, lenv))
--Deal with while loops
dealWhile :: Condition -> Environment -> ListEnvironment -> Program -> (Environment, ListEnvironment)
dealWhile c env lenv p | evalCondition (c, env, lenv) == True = dealWhile c (second (evalLine (p, env, lenv))) (third (evalLine (p, env, lenv))) p
                       | otherwise = (env, lenv)

doProgram :: Program -> IO String
doProgram (prog) = outProgram(evalLine (prog, [], []))

outProgram :: State -> IO String
outProgram ((JKFinalLine (JKOutput e)), env, lenv) = printList((getList e lenv))

printList :: [Int] -> IO String
printList [] = return("")
printList (x:xs) = do print(x)
                      printList xs

evalLine :: State -> State
evalLine (JKProgram (JKInstantiateList s) prog, env, lenv) = evalLine(prog, env, addList s lenv)
evalLine (JKFinalLine (JKInstantiateList s), env, lenv) = (JKFinalLine (JKInstantiateList s), env, addList s lenv)
evalLine (JKProgram (JKConcat s e) prog, env, lenv) = evalLine(prog, env, addToList s (evalExp (e, env, lenv)) lenv)
evalLine (JKFinalLine (JKConcat s e), env, lenv) = (JKFinalLine (JKConcat s e), env, addToList s (evalExp (e, env, lenv)) lenv)
evalLine (JKProgram (JKTypeEqualLine t s e) prog, env, lenv) = evalLine(prog, addVariable s (evalExp (e, env, lenv)) env, lenv)
evalLine (JKFinalLine (JKTypeEqualLine t s e), env, lenv) = (JKFinalLine (JKTypeEqualLine t s e), addVariable s (evalExp (e, env, lenv)) env, lenv)
evalLine (JKProgram (JKEqualLine s e) prog, env, lenv) = evalLine(prog, addVariable s (evalExp (e, env, lenv)) env, lenv)
evalLine (JKFinalLine (JKEqualLine s e), env, lenv) = (JKFinalLine (JKEqualLine s e), addVariable s (evalExp (e, env, lenv)) env, lenv)
evalLine (JKProgram (JKIf c p) prog, env, lenv) | evalCondition(c, env, lenv) == True = evalLine(prog, fst (dealIf p env lenv), snd (dealIf p env lenv))
                                                | otherwise = evalLine(prog, env, lenv)
evalLine (JKFinalLine (JKIf c p), env, lenv) | evalCondition(c, env, lenv) == True = evalLine(p, env, lenv)
                                             | otherwise = (JKFinalLine (JKIf c p), env, lenv)
evalLine (JKProgram (JKWhile c p) prog, env, lenv) | evalCondition(c, env, lenv) == True = evalLine(prog, fst (dealWhile c env lenv p), snd (dealWhile c env lenv p))
                                                   | otherwise = evalLine(prog, env, lenv)
evalLine (JKFinalLine (JKWhile c p), env, lenv) | evalCondition(c, env, lenv) == True = (JKFinalLine (JKWhile c p), fst (dealWhile c env lenv p), snd (dealWhile c env lenv p))
                                                | otherwise = (JKFinalLine (JKWhile c p), env, lenv)
--evalLine (JKProgram (JKFor f f2 p) prog, env) = dealFor
--evalLine (JKFinalLine (JKFor f f2 p), env) = dealFor
evalLine (JKProgram (JKOutput e) prog, env, lenv) = evalLine (prog, env, lenv)
-- do print ( evalExp (e) )
-- evalState <- evalLine (prog, env)
evalLine (JKFinalLine (JKOutput e), env, lenv) = (JKFinalLine (JKOutput e), env, lenv)
-- do print ( evalExp (e) )
-- evalState <- (JKFinalLine (JKOutput e), env)

evalCondition :: CState -> Bool
evalCondition (JKCompareLess e1 e2, env, lenv) | evalExp (e1, env, lenv) < evalExp (e2, env, lenv) = True
                                         | otherwise = False
evalCondition (JKCompareMore e1 e2, env, lenv) | evalExp (e1, env, lenv) > evalExp (e2, env, lenv) = True
                                         | otherwise = False
evalCondition (JKCompareEqual e1 e2, env, lenv) | evalExp (e1, env, lenv) == evalExp (e2, env, lenv) = True
                                          | otherwise = False
evalCondition (JKAnd c1 c2, env, lenv) = evalCondition (c1, env, lenv) && evalCondition (c2, env, lenv)
evalCondition (JKOr c1 c2, env, lenv) = evalCondition (c1, env, lenv) || evalCondition (c2, env, lenv)
evalCondition (JKNot c, env, lenv) = not ( evalCondition (c, env, lenv) )

evalExp :: EState -> Int
evalExp (Bracket e, env, lenv) = evalExp (e, env, lenv)
evalExp (JKAdd e1 e2, env, lenv) = evalExp (e1, env, lenv) + evalExp(e2, env, lenv)
evalExp (JKSubtract e1 e2, env, lenv) = evalExp (e1, env, lenv) - evalExp (e2, env, lenv)
evalExp (JKMultiply e1 e2, env, lenv) = evalExp (e1, env, lenv) * evalExp (e2, env, lenv)
--evalExp (JKDivide e1 e2) = evalExp (e1) / evalExp (e2)
evalExp (JKPower e1 e2, env, lenv) = evalExp (e1, env, lenv) ^ evalExp (e2, env, lenv)
evalExp (JKDiv e1 e2, env, lenv) = evalExp (e1, env, lenv) `div` evalExp (e2, env, lenv)
evalExp (JKMod e1 e2, env, lenv) = evalExp (e1, env, lenv) `mod` evalExp (e2, env, lenv)
evalExp (JKIndex s e, env, lenv) = index s (evalExp(e, env, lenv)) lenv
evalExp (JKFactor e, env, lenv) = evalFactor (e, env, lenv)

evalFactor :: FState -> Int
evalFactor (JKInt i, env, lenv) = i
--evalFactor (JKReal d) = d
--evalFactor (JKFalse) = False
--evalFactor (JKTrue) = True
evalFactor (JKVar s, env, lenv) = getVariable s env

--evalType (TyBool) = Bool
--evalType (TyStr) = String
--evalType (TyInt) = Int
--evalType (TyReal) = Real
