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
  List            { tok (\p s -> TokenTypeList p) }
  \" [$alpha $white $digit \_ \’]* \"   { tok (\p s ->TokenStr p (read s)) }
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
  \^              { tok (\p s -> TokenPower p)}
  div             { tok (\p s -> TokenDiv p)}
  mod             { tok (\p s -> TokenMod p)}
  print           { tok (\p s -> TokenPrint p)}
  for             { tok (\p s -> TokenFor p) }
  while           { tok (\p s -> TokenWhile p) }
  if              { tok (\p s -> TokenIf p) }
  then            { tok (\p s -> TokenThen p) }
  else            { tok (\p s -> TokenElse p) }
  \:              { tok (\p s -> TokenHasType p) }
  not             { tok (\p s -> TokenNot p) }
  ==              { tok (\p s -> TokenEquiv p) }
  =               { tok (\p s -> TokenEq p )}
  in              { tok (\p s -> TokenIn p )}
  &&              { tok (\p s -> TokenAnd p)}
  \|\|            { tok (\p s -> TokenOr p)}
  \[              { tok (\p s -> TokenLSquare p)}
  \]              { tok (\p s -> TokenRSquare p)}
  \{              { tok (\p s -> TokenLCurlyBr p)}
  \}              { tok (\p s -> TokenRCurlyBr p)}
  \(              { tok (\p s -> TokenLParen p) }
  \)              { tok (\p s -> TokenRParen p) }
  \!               { tok (\p s -> TokenEndLine p) }
  $alpha [$alpha $digit \_ \’]*   { tok (\p s -> TokenVar p s) } 

{
-- Each action has type :: AlexPosn -> String -> MDLToken 

-- Helper function
tok f p s = f p s

-- The token type: 
data Token = 
  TokenTypeBool AlexPosn         | 
  TokenTypeStr  AlexPosn         |
  TokenTypeInt AlexPosn          |
  TokenTypeReal AlexPosn         |
  TokenTypeList AlexPosn         |
  TokenStr AlexPosn String       |
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
  TokenPower AlexPosn            |
  TokenDiv AlexPosn              |
  TokenMod AlexPosn              |
  TokenPrint AlexPosn            |
  TokenFor AlexPosn              |
  TokenWhile AlexPosn            |
  TokenIf AlexPosn               |
  TokenThen AlexPosn             |
  TokenElse AlexPosn             |
  TokenHasType AlexPosn          |
  TokenNot AlexPosn              |
  TokenEquiv AlexPosn            |
  TokenEq AlexPosn               |
  TokenIn AlexPosn               |
  TokenAnd AlexPosn              |
  TokenOr AlexPosn               |
  TokenLSquare AlexPosn          |
  TokenRSquare AlexPosn          |
  TokenLCurlyBr AlexPosn         |
  TokenRCurlyBr AlexPosn         |
  TokenLParen AlexPosn           |
  TokenRParen AlexPosn           |
  TokenEndLine AlexPosn          |
  TokenVar AlexPosn String
  deriving (Eq,Show) 

tokenPosn :: Token -> String
tokenPosn (TokenTypeBool (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenTypeStr  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenTypeInt  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenTypeReal  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenTypeList  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenStr  (AlexPn a l c) _) = show(l) ++ ":" ++ show(c)
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
tokenPosn (TokenPower  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenDiv  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenMod  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenPrint  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenFor (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenWhile (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenIf (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenThen (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenElse (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenHasType (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenNot  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenEquiv  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenEq  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenIn  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenAnd  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenOr  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLSquare (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRSquare (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLCurlyBr (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRCurlyBr (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLParen (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRParen (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenEndLine  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenVar (AlexPn a l c) _) = show(l) ++ ":" ++ show(c)

}