{ 
module Parsing where 
import Lexing 
}

%name parseCalc 
%tokentype { Token } 
%error { parseError }
%token 
    Bool   { TokenTypeBool _ }
    Str    { TokenTypeStr _ } 
    Real    { TokenTypeReal _ } 
    Int    { TokenTypeInt _ }
    str    { TokenStr _ $$ }
    int    { TokenInt _ $$ } 
    real    { TokenReal _ $$ }
    true   { TokenTrue _ }
    false  { TokenFalse _ }
    '++'   { TokenConcat _ }
    '<'    { TokenLessThan _ }
    '>'    { TokenMoreThan _ }
    '+'    { TokenPlus _ }
    '-'    { TokenSubtract _ }
    '*'    { TokenMultiply _ }
    '/'    { TokenDivide _ }
    '^'    { TokenPower _ }
    var    { TokenVar _ $$ }
    for    { TokenFor _ }
    while  { TokenWhile _ }
    if     { TokenIf _ }
    then   { TokenThen _ }
    else   { TokenElse _ }
    ':'    { TokenHasType _ }
    not    { TokenNot _ }
    '=='   { TokenEquiv _ }
    '='    { TokenEq _ }
    in     { TokenIn _ }
    '&&'   { TokenAnd _ }
    '||'   { TokenOr _ }
    '['    { TokenLSquare _ }
    ']'    { TokenRSquare _ }
    '{'    { TokenLCurlyBr _ }
    '}'    { TokenRCurlyBr _ }
    '('    { TokenLParen _ } 
    ')'    { TokenRParen _ }
    '¬'    { TokenEndLine _ } 


%right in
%nonassoc if
%nonassoc then
%nonassoc else
%nonassoc int true false var '(' ')'
%left '<'
%left '>'
%left '-'
%left '+'


%% 

Control : for Prim in Prim '{' Control '}'              { TmFor $2 $4 $6 }
        | if '(' Condition ')' then '{' Control '}'     { TmIf $3 $7 }
        | while '(' Condition ')' then '{' Control '}'  { TmWhile $3 $7 } 
        | Condition                                           { $1 }


Exp : Prim '+' Exp                             { TmAdd $1 $3 }
    | Prim '-' Exp                             { TmSubtract $1 $3 }
    | Prim '*' Exp                             { TmMultiply $1 $3 }
    | Prim '/' Exp                             { TmDivide $1 $3 }
    | Prim '^' Exp                             { TmPower $1 $3 }
    | '(' Exp ')'                              { Bracket $2 }
    | Exp '=' Exp                              { TmEquals $1 $3}
    | Prim                                     { $1 }

-- Exp1 : str '++' str

Condition : Exp '<' Exp                      { TmCompareLess $1 $3 }
          | Exp '>' Exp                      { TmCompareMore $1 $3 }
          | Exp '==' Exp                     { TmCompareEqual $1 $3 }
          | Condition '&&' Condition           { TmAnd $1 $3 }
          | Condition '||' Condition           { TmOr $1 $3 }
          | not Condition                      { TmNot $2 }
          | Exp                                { $1 }

-- questionable var in numbers, Tm? change?!

Prim : int                                      { TmInt $1 }
     | real                                     { TmReal $1 }
     | false                                    { TmFalse $1 }
     | true                                     { TmTrue $1}
     | var                                      { TmVar $1 }

-- Stringz : str                                { TmStr $1 } 

Type : Bool            { TyBool } 
     | Str          { TyStr }
     | Real            { TyReal }
     | Int             { TyInt } 


{ 
parseError :: [Token] -> a
parseError [] = error "Unknown Parse Error" 
parseError (t:ts) = error ("Parse error at line:column " ++ (tokenPosn t))

data TokenType = TyInt | TyBool | TyStr | TyReal
   deriving (Show,Eq)

type Environment = [ (String,Expr) ]

data Control = TmFor Prim Prim Control | TmIf Condition Control | TmWhile Condition Control | Condition deriving (Show, Eq)

data Expr = TmAdd Prim Expr | TmSubtract Prim Expr | TmDivide Prim Expr | TmMultiply Prim Expr | TmPower Prim Expr |TmEquals Expr Expr | Bracket Expr | Prim deriving (Show,Eq)

data Condition = TmCompareLess Expr Expr | TmCompareMore Expr Expr | TmCompareEqual Expr Expr | TmAnd Condition Condition | TmOr Condition Condition | TmNot Condition | Expr deriving (Show,Eq)


--  Cl String TokenType Expr Environment
data Prim = TmInt Int | TmReal Double | TmTrue Bool | TmFalse Bool | TmVar String deriving (Show,Eq)
} 
