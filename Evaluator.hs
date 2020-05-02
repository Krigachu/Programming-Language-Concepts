--JORKRI Interpreter

module Evaluator where
import Parsing

data Expr = Program | Line | Condition | Exp | Factor

type Environment = [ (String, Factor) ]
type State = (Program  ,Environment)

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

--addVariable :: Environment -> Environment
--deal with conflicting variables

eval :: State -> State
eval ((JKProgram l p), env) = (evalLine (l, env))
eval ((JKFinalLine l), env) = (evalLine (l, env))

evalLine :: State -> State
evalLine (JKProgram (JKTypeEqualLine t s e) prog, env) = eval(prog, env ++ [(s, evalExp (e) )])
evalLine (JKFinalLine (JkTypeEqualLine t s e), env) = (JKFinalLine (JkTypeEqualLine t s e), env ++ [(s, evalExp(e))])
evalLine (JKProgram (JKEqualLine s e) prog, env) = eval()
evalLine (JKFinalLine (JKEqualLine s e), env) = 
evalLine (JKProgram (JKIf c p) prog, env) = 
evalLine (JKFinalLine (JKIf c p), env) = evalCondition (c) eval (p)
evalLine (JKProgram (JKWhile c p) prog, env) = evalCondition (c) eval (p)
evalLine (JKFinalLine (JKWhile c p), env) = 
evalLine (JKProgram (JKFor f f2 p) prog, env) =
evalLine (JKFinalLine (JKFor f f2 p), env) =
evalLine (JKProgram (JKOutput e)) = print ( evalExp (e) )

evalCondition :: State -> State
evalCondition (JKCompareLess e1 e2) | evalExp (e1) < evalExp (e2) = True
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
--evalFactor (JKVar s) = s

--evalType (TyBool) = Bool
--evalType (TyStr) = String
--evalType (TyInt) = Int
--evalType (TyReal) = Real
