--JORKRI Interpreter

module Evaluator where
import Parsing

data Frame = HCompare Expr Environment 
           | CompareH Expr
           | HAdd Expr Environment | AddH Expr
           | HIf Expr Expr Environment | HLet String ToyType Expr Environment
           | HApp Expr Environment | AppH Expr
type Kontinuation = [ Frame ]
type State = (Expr,Environment,Kontinuation)


isValue :: Expr -> Bool
isValue (JKInt _) = True
isValue (JKReal _) = True
isValue JKTrue = True
isValue JKFalse = True
isValue _ = False

deparse :: Expr -> String 
deparse (JKInt n) = show n
deparse (JKReal n) = show n
deparse (JKTrue) = "true"
deparse (JKFalse) = "false"
deparse _ = "Unknown"


eval (JKProgram l p) = evalLine (l) ++ eval (p)
eval (JKFinalLine l) = evalLine (l)


evalLine (JKTypeEqualLine t s e) = 
evalLine (JKEqualLine s e) = 
evalLine (JKIf c p) = 
evalLine (JKWhile c p) = 
evalLine (JKFor f f2 p) = 
evalLine (JKOutput e) = print evalExp (e)

evalCondition (JKCompareLess e1 e2) | evalExp (e1) < evalExp (e2) = True
                                    | otherwise = False
evalCondition (JKCompareMore e1 e2) | evalExp (e1) > evalExp (e2) = True
                                    | otherwise = False
evalCondition (JKCompareEqual e1 e2) | evalExp (e1) == evalExp (e2) = True
                                     | otherwise = False
evalCondition (JKAnd c1 c2) = evalCondition (c1) && evalCondition (c2)
evalCondition (JKOr c1 c2) = evalCondition (c1) || evalCondition (c2)
evalCondition (JKNot c) = not evalCondition (c)

evalExp (Bracket e) = evalExp (e)
evalExp (JKAdd e1 e2) = evalExp (e1) + evalExp(e2)
evalExp (JKSubtract e1 e2) = evalExp (e1) - evalExp (e2)
evalExp (JKMultiply e1 e2) = evalExp (e1) * evalExp (e2)
evalExp (JKDivide e1 e2) = evalExp (e1) / evalExp (e2)
evalExp (JKPower e1 e2) = evalExp (e1) ^ evalExp (e2)
evalExp (JKDiv e1 e2) = evalExp (e1) `div` evalExp (e2)
evalExp (JKMod e1 e2) = evalExp (e1) `mod` evalExp (e2)
evalExp (JKFactor e) = evalFactor (e)

evalFactor (JKInt i) = i
evalFactor (JKReal d) = d
evalFactor (JKFalse) = False
evalFactor (JKTrue) = True
evalFactor (JKVar s) = s