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

Line : Type var '=' Factor '¬'               { JKTypeEqualLine $1 $2 $4 }
     | var '=' Factor '¬'                    { JKEqualLine $1 $3 }
     | if '(' Factor ')' then '{' Factor '}'     { JKIf $3 $7 }
     | while '(' Factor ')' then '{' Factor '}'  { JKWhile $3 $7 }
     | for Factor in Factor '{' Factor '}'       { JKFor $2 $4 $6 }

Factor : '(' Factor ')'                           { Bracket $2 }
       | int                                      { JKInt $1 }
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
            | Bracket Factor
            deriving (Show, Eq)

data Line = JKTypeEqualLine TokenType String Factor
          | JKEqualLine String Factor
          | JKIf Factor Factor
          | JKWhile Factor Factor
          | JKFor Factor Factor Factor
          deriving (Show, Eq)

data Program = JKProgram Line Program
             | JKFinalLine Line
             deriving (Show ,Eq)
}