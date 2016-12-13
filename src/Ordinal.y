{
module Ordinal where

import Data.Char

}

%name parse
%tokentype { Token }
%error { parseError }

%token
    int     { TokenInt $$ }
    omega   { TokenOmega }
    '='     { TokenEq }
    '+'     { TokenPlus }
    '*'     { TokenMul }
    '^'     { TokenExp }
    '('     { TokenLPar }
    ')'     { TokenRPar }

%nonassoc   '='
%left       '+'
%left       '*'
%right      '^'
%%

Eq      : Sum '=' Sum       { OrdComp $1 $3 }
Sum     : Mult              { $1 }
        | Sum '+' Mult      { Sum $1 $3 }
Mult    : Exp               { $1 }
        | Mult '*' Exp      { Mul $1 $3 }
Exp     : Term              { $1 }
        | Term '^' Mult     { Exp $1 $3 }
Term    : int               { Atom $1 }
        | omega             { Omega }
        | '(' Sum ')'       { $2 }

{
{- Data structure -}

data Token
         = TokenInt Int
         | TokenOmega
         | TokenEq
         | TokenPlus
         | TokenMul
         | TokenExp
         | TokenLPar
         | TokenRPar
         deriving Show

data Ordinal = Atom Int
             | Omega
             | Sum Ordinal Ordinal
             | Mul Ordinal Ordinal
             | Exp Ordinal Ordinal

instance Show Ordinal where
    show (Atom n)     = show n
    show Omega        = "w"
    show (Sum x y) = writeOrdinal x ++ " + " ++ writeOrdinal y
    show (Mul x y) = writeOrdinal x ++ " * " ++ writeOrdinal y
    show (Exp x y) = writeOrdinal x ++ " ^ " ++ writeOrdinal y

data OrdinalComparison = OrdComp Ordinal Ordinal

instance Read OrdinalComparison where
    readsPrec _ s = [(parse $ lexer s, "")]

instance Show OrdinalComparison where
    show (OrdComp x y) = show x ++ " = " ++ show y

isOrdinalSimple :: Ordinal -> Bool
isOrdinalSimple (Atom _) = True
isOrdinalSimple Omega    = True
isOrdinalSimple _        = False

writeOrdinal :: Ordinal -> String
writeOrdinal x
             | isOrdinalSimple x = show x
             | otherwise         = "(" ++ show x ++ ")"

{- Error function -}

parseError :: [Token] -> a
parseError _ = error "Ordinal parse error"

{- Lexer -}

lexer :: String -> [Token]
lexer [] = []
lexer ('=':cs) = TokenEq : lexer cs
lexer ('+':cs) = TokenPlus : lexer cs
lexer ('*':cs) = TokenMul : lexer cs
lexer ('^':cs) = TokenExp : lexer cs
lexer ('(':cs) = TokenLPar : lexer cs
lexer (')':cs) = TokenRPar : lexer cs
lexer ('w':cs) = TokenOmega : lexer cs
lexer (c:cs)
    | isSpace c = lexer cs
    | isDigit c = TokenInt n : lexer rest
      where
        n = read $ c : ns
        (ns, rest) = span isDigit cs

}
