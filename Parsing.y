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

Control : for Numb in Numb '{' Control '}'              { TmFor $2 $4 $6 }
        | if '(' Condition ')' then '{' Control '}'     { TmIf $3 $7 }
        | while '(' Condition ')' then '{' Control '}'  { TmWhile $3 $7 } 
        | Exp                                           { $1 }


Exp : Numb '+' Exp                             { TmAdd $1 $3 }
    | Numb '-' Exp                             { TmSubtract $1 $3 }
    | Numb '*' Exp                             { TmMultiply $1 $3 }
    | Numb '/' Exp                             { TmDivide $1 $3 }
    | Numb '^' Exp                             { TmPower $1 $3 }
    | '(' Exp ')'                               { $2 }
    | Numb                                      { $1 }
    | Exp '=' Exp                               { TmEquals $1 $3}

-- Exp1 : str '++' str

Condition : Exp '<' Exp                      { TmCompareLess $1 $3 }
          | Exp '>' Exp                      { TmCompareMore $1 $3 }
          | Exp '==' Exp                     { TmCompareEqual $1 $3 }
          | Condition '&&' Condition           { TmAnd $1 $3 }
          | Condition '||' Condition           { TmOr $1 $3 }
          | not Condition                      { TmNot $2 }

-- questionable var in numbers, Tm? change?!

Numb : int                                      { TmInt $1 }
     | real                                     { TmReal $1 }
     | var                                      { TmVar $1 }

-- Stringz : str                                { TmStr $1 }

Prim : true                                      { TmTrue }
     | false                                     { TmFalse } 

Type : Bool            { TyBool }
     | Str             { TyStr } 
     | Real            { TyReal }
     | Int             { TyInt } 


{ 
parseError :: [Token] -> a
parseError [] = error "Unknown Parse Error" 
parseError (t:ts) = error ("Parse error at line:column " ++ (tokenPosn t))

data TokenType = TyInt | TyBool | TyStr | TyReal
   deriving (Show,Eq)

type Environment = [ (String,Expr) ]

data Control = TmFor Numb Numb Control | TmIf Condition Control | TmWhile Condition Control | Expr

data Condition = TmCompareLess Expr Expr | TmCompareMore Expr Expr | TmCompareEqual Expr Expr | TmAnd Condition Condition | TmOr Condition Condition | TmNot Condition deriving (Show,Eq)

data Expr = TmAdd Numb Expr | TmSubtract Numb Expr | TmDivide Numb Expr | TmMultiply Numb Expr | Numb | TmEquals Expr Expr | Cl String TokenType Expr Environment
    deriving (Show,Eq)

data Prim = TmTrue | TmFalse deriving (Show,Eq)

data Numb = TmVar String | TmInt Int | TmReal Double deriving (Show,Eq)
} 
