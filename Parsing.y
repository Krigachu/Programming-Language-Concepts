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
    List   { TokenTypeList _}
    index  { TokenIndex _ }
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
    div    { TokenDiv _ }
    mod    { TokenMod _ }
    OUTPUT { TokenOutput _ }
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
    '!'    { TokenEndLine _ }
    var    { TokenVar _ $$ } 


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

Program : Line Program                          { JKProgram $1 $2 }
        | Line                                  { JKFinalLine $1 }

Line : List var '!'                          { JKInstantiateList $2}
     | var '++' Exp '!'                     { JKConcat $1 $3}
     | Type var '=' Exp '!'               { JKTypeEqualLine $1 $2 $4 }
     | var '=' Exp '!'                    { JKEqualLine $1 $3 }
     | if '(' Condition ')' then '{' Program '}'     { JKIf $3 $7 }
     | while '(' Condition ')' then '{' Program '}'  { JKWhile $3 $7 }
     | for '(' Factor in Factor ')' then '{' Program '}'       { JKFor $3 $5 $9 }
     | OUTPUT '(' var ')' '!'                     { JKOutput $3 }

Exp : Exp '+' Exp                             { JKAdd $1 $3 }
    | Exp '-' Exp                             { JKSubtract $1 $3 }
    | Exp '*' Exp                             { JKMultiply $1 $3 }
    | Exp '/' Exp                             { JKDivide $1 $3 }
    | Exp '^' Exp                             { JKPower $1 $3 }
    | Exp div Exp                             { JKDiv $1 $3 }
    | Exp mod Exp                             { JKMod $1 $3 }
    | '(' Exp ')'                             { Bracket $2 }
    | var index Exp                           { JKIndex $1 $3 }
    | Factor                                  { JKFactor $1 }

Condition : Exp '<' Exp                      { JKCompareLess $1 $3 }
          | Exp '>' Exp                      { JKCompareMore $1 $3 }
          | Exp '==' Exp                     { JKCompareEqual $1 $3 }
          | Condition '&&' Condition           { JKAnd $1 $3 }
          | Condition '||' Condition           { JKOr $1 $3 }
          | not '(' Condition ')'              { JKNot $3 }

Factor : int                                      { JKInt $1 }
       | real                                     { JKReal $1 }
       | false                                    { JKFalse }
       | true                                     { JKTrue }
       | var                                      { JKVar $1 }

Type : Bool            { TyBool } 
     | Str             { TyStr }
     | Real            { TyReal }
     | Int             { TyInt } 

{ 
parseError :: [Token] -> a
parseError [] = error "Unknown Parse Error" 
parseError (t:ts) = error ("Parse error at line:column " ++ (tokenPosn t))

data TokenType = TyInt 
               | TyBool 
               | TyStr 
               | TyReal
               deriving (Show,Eq)

data Factor = JKInt Int
            | JKReal Double
            | JKFalse
            | JKTrue
            | JKVar String
            deriving (Show, Eq)

data Exp = JKAdd Exp Exp
         | JKSubtract Exp Exp
         | JKMultiply Exp Exp
         | JKDivide Exp Exp
         | JKPower Exp Exp
         | JKDiv Exp Exp
         | JKMod Exp Exp
         | Bracket Exp
         | JKIndex String Exp
         | JKFactor Factor
         deriving (Show, Eq)

data Condition = JKCompareLess Exp Exp 
               | JKCompareMore Exp Exp 
               | JKCompareEqual Exp Exp 
               | JKAnd Condition Condition 
               | JKOr Condition Condition 
               | JKNot Condition 
               deriving (Show, Eq)

data Line = JKInstantiateList String
          | JKConcat String Exp
          | JKTypeEqualLine TokenType String Exp
          | JKEqualLine String Exp
          | JKIf Condition Program
          | JKWhile Condition Program
          | JKFor Factor Factor Program
          | JKOutput String
          deriving (Show, Eq)

data Program = JKProgram Line Program
             | JKFinalLine Line
             deriving (Show ,Eq)
}