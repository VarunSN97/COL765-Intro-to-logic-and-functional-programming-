module Ast where

type Id = String

data Typ = Int | Boolean
		deriving (Eq,Show)

data FTyp = SINGLE Typ | ARROW Typ Typ
		deriving (Eq,Show)

data VarDecl = Decl Id Expr
        deriving (Eq,Show)

data UnOp = Negate | Not
        deriving (Eq,Show)

data BinOp = Plus | Minus | Times | Equals | LessThan | GreaterThan | And | Or | Xor | Implies
        deriving (Eq,Show)

data Expr =
             BinExpr BinOp Expr Expr
           | UnExpr UnOp Expr
           | Ite Expr Expr Expr
           | Let VarDecl Expr
		   | FunExp Expr Expr Typ FTyp Expr
		   | Fn Expr Typ Expr
		   | AppExp Expr Expr
		   | BoolConst Bool
		   | IntConst Integer 
		   | VarExpr Id
        deriving (Eq,Show)
