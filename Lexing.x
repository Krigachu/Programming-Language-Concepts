{ 
module Lexing where 
}

%wrapper "posn" 
$digit = 0-9     
-- digits 
$alpha = [a-zA-Z]    
-- alphabetic characters

tokens :-
$white+       ;
  "--".*        ; 
  Bool            { tok (\p s -> TokenTypeBool p)} 
  Str             { tok (\p s -> TokenTypeStr p) }
  Real            { tok (\p s -> TokenTypeReal p) }
  Int             { tok (\p s -> TokenTypeInt p) }
  "->"            { tok (\p s -> TokenTypeArr p) }
  $digit+         { tok (\p s -> TokenInt p (read s)) }
  $digit+.$digit+  { tok (\p s -> TokenReal p (read s)) }
  true            { tok (\p s -> TokenTrue p) }
  false           { tok (\p s -> TokenFalse p) }
  \+\+              { tok (\p s -> TokenConcat p) }
  \<              { tok (\p s -> TokenLessThan p) }
  \>              { tok (\p s -> TokenMoreThan p) }
  \+              { tok (\p s -> TokenPlus p) }
  \-               { tok (\p s -> TokenSubtract p) }
  \*               { tok (\p s -> TokenMultiply p) }
  \/               { tok (\p s -> TokenDivide p) }
  if              { tok (\p s -> TokenIf p) }
  then            { tok (\p s -> TokenThen p) }
  else            { tok (\p s -> TokenElse p) }
  \:              { tok (\p s -> TokenHasType p) }
  let             { tok (\p s -> TokenLet p ) }
  not             { tok (\p s -> TokenNot p) }
  ==              { tok (\p s -> TokenEquiv p) }
  =               { tok (\p s -> TokenEq p )}
  in              { tok (\p s -> TokenIn p )}
  &&              { tok (\p s -> TokenAnd p)}
  \|\|             { tok (\p s -> TokenOr p)}
  \(              { tok (\p s -> TokenLParen p) }
  \)              { tok (\p s -> TokenRParen p) }
  ¬               { tok (\p s -> TokenEndLine p) }
  $alpha [$alpha $digit \_ \’]*   { tok (\p s -> TokenVar p s) } 

{
-- Each action has type :: AlexPosn -> String -> MDLToken 

-- Helper function
tok f p s = f p s

-- The token type: 
data ToyToken = 
  TokenTypeBool AlexPosn         | 
  TokenTypeStr  AlexPosn         |
  TokenTypeInt AlexPosn          |
  TokenTypeReal AlexPosn         |
  TokenTypeArr  AlexPosn         |
  TokenInt AlexPosn Int          |
  TokenReal AlexPosn Double      |
  TokenTrue AlexPosn             |
  TokenFalse AlexPosn            |
  TokenLessThan AlexPosn         |
  TokenMoreThan AlexPosn         |
  TokenConcat AlexPosn           |
  TokenPlus AlexPosn             |
  TokenSubtract AlexPosn         |
  TokenMultiply AlexPosn         |
  TokenDivide AlexPosn           |
  TokenIf AlexPosn               |
  TokenThen AlexPosn             |
  TokenElse AlexPosn             |
  TokenHasType AlexPosn          |
  TokenLet AlexPosn              |
  TokenNot AlexPosn              |
  TokenEquiv AlexPosn            |
  TokenEq AlexPosn               |
  TokenIn AlexPosn               |
  TokenAnd AlexPosn              |
  TokenOr AlexPosn               |
  TokenLParen AlexPosn           |
  TokenRParen AlexPosn           |
  TokenEndLine AlexPosn          |
  TokenVar AlexPosn String
  deriving (Eq,Show) 

tokenPosn :: ToyToken -> String
tokenPosn (TokenTypeBool (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenTypeStr  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenTypeInt  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenTypeReal  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenTypeArr  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenInt  (AlexPn a l c) _) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenReal  (AlexPn a l c) _) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenTrue  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenFalse  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLessThan  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenMoreThan  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenConcat  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenPlus  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenSubtract  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenMultiply  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenDivide  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenIf (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenThen (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenElse (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenHasType (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLet (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenNot  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenEquiv  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenEq  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenIn  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenAnd  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenOr  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLParen (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRParen (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenEndLine  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenVar (AlexPn a l c) _) = show(l) ++ ":" ++ show(c)

}