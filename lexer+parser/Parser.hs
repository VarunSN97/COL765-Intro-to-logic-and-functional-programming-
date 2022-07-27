module Parser (
 AST (..),
 parse,
 beginning,
)where

import Lexer

data AST = Number String | Const String | Id String | AddExp AST AST | MinusExp AST AST | MulExp AST AST | Negate AST | Lessthan AST AST | Greaterthan AST AST | Not AST | Equals AST AST | Xor AST AST | Or AST AST | And AST AST | Implies AST AST | Assignment AST AST | LetIn AST AST | ITE AST AST AST | BinExp AST | UnaryExp AST deriving (Eq,Show)

beginning:: String -> AST
beginning str = parse (scanTokens str)

checker :: [PosToken] -> Bool
checker tokens = case (reverse tokens) of
                 ((EOF_ a "`"):xs)->True
                 otherwise-> error $ "Syntax error :EOF Missing"

parse :: [PosToken] -> AST
parse tokens 
 |(checker tokens) = let (ast, tokens')=parseStmt tokens
                     in case tokens' of
                        ((EOF_ a "`"):xs)-> ast
                        otherwise -> error $ "Syntax error :" ++ (showPosn (head tokens')) ++ " :Multiple Statements found"
                               


parseStmt :: [PosToken] -> (AST, [PosToken])
parseStmt (IF_ _ "if":tokens)=
 let (ast, tokens') =bExpr tokens
 in case tokens' of
    (THEN_ _ "then":rest1)->let (ast2,rest2)=bExpr rest1
                         in case rest2 of
                            (ELSE_ _ "else":rest3)->let (ast3, rest4)=bExpr rest3
                                                 in case rest4 of
                                                    (FI_ _ "fi":xs)->(ITE ast ast2 ast3, xs)
                                                    ([])->error  "Syntax Error:(Missing 'fi'): if <expr> then <expr> else <expr> fi"
						    (x:xs)->error $ "Syntax Error:" ++ showPosn x ++ "(Missing 'fi'): if <expr> then <expr> else <expr> fi"
                            ([])->error "Syntax Error:(Missing 'else'): if <expr> then <expr> else <expr> fi"
                            (x:xs)->error $ "Syntax Error:" ++ showPosn x ++ "(Missing 'else'): if <expr> then <expr> else <expr> fi"
    ([])->error "Syntax Error:(Expected 'then'): if <expr> then <expr> else <expr> fi"
    (x:xs)->error $ "Syntax Error:" ++ showPosn x++ "(Expected 'then'): if <expr> then <expr> else <expr> fi"
parseStmt (THEN_ a "then":tokens)=error $ "Syntax Error:"++ showPosn (THEN_ a "then") ++ "(Invalid statement) if <expr> then <expr> else <expr> fi"
parseStmt (ELSE_ a "else":tokens)=error $ "Syntax Error:"++ showPosn (ELSE_ a "else") ++ "(Invalid statement) if <expr> then <expr> else <expr> fi"
parseStmt (FI_ a "fi":tokens)=error $ "Syntax Error:" ++ showPosn (FI_ a "fi") ++ "(Invalid statement) if <expr> then <expr> else <expr> fi"

parseStmt ((LET_ _ "let"):(ID_ _ a):(ASSIGNMENT_ _ ":="):tokens)=
 let (ast, tokens')=bExpr tokens
 in case tokens' of
    (IN_ _ "in":rest1)->let (ast2,rest2)=bExpr rest1
                        in case rest2 of
                           (END_ _ "end":xs)->( LetIn (Assignment (Id a) ast) ast2,xs)
                           (x:xs)->error $ "Syntax error :" ++ showPosn x ++"(Missing 'end') let var := <expr> in <expr> end"
		           ([])->error "Syntax error :(Missing 'end') let var := <expr> in <expr> end"
    (x:xs)->error $ "Syntax error :" ++ showPosn x ++"(Missing 'in')let var := <expr> in <expr> end"
    ([])->error "Syntax error :(Missing 'in')let var := <expr> in <expr> end"
parseStmt (IN_ a "in":tokens)=error $ "Syntax error :"++ showPosn (IN_ a "in") ++"(Invalid statement)let var := <expr> in <expr> end"
parseStmt (END_ a "end":tokens)=error $ "Syntax error :"++ showPosn (END_ a "end")++ "(Invalid statement)let var := <expr> in <expr> end"

parseStmt (x:xs) = let (ast, tokens) = bExpr (x:xs) in (ast,tokens)

bExpr :: [PosToken]->(AST,[PosToken])
bExpr tokens = let (ast,tokens')=bTerm tokens 
               in case tokens' of
                  (IMPLIES_ _ "IMPLIES":rest1)->let (ast2, rest2)=bExpr rest1 
                                             in (BinExp(Implies ast ast2),rest2)
                  (_)->(ast,tokens')

bTerm :: [PosToken]->(AST,[PosToken])
bTerm tokens = bTerm' . bNt $ tokens

bTerm' :: (AST,[PosToken])->(AST,[PosToken])
bTerm' (lhs, []) = (lhs,[])
bTerm' (lhs,lookaheadtokens)= case (lookaheadtokens) of
                              ((AND_ _ "AND"):tokens) -> let (rhs,tokens')=bNt tokens in bTerm'(BinExp (And lhs rhs),tokens')
                              ((OR_ _ "OR"):tokens) -> let (rhs,tokens')=bNt tokens in bTerm'(BinExp (Or lhs rhs),tokens')
                              ((XOR_ _ "XOR"):tokens) -> let (rhs,tokens')=bNt tokens in bTerm'(BinExp (Xor lhs rhs),tokens')
                              ((EQUALS_ _ "EQUALS"):tokens) -> let (rhs,tokens')=bNt tokens in bTerm'(BinExp (Equals lhs rhs),tokens') 
                              otherwise->(lhs,lookaheadtokens)

bNt ::  [PosToken]->(AST,[PosToken])
bNt ([]) = error "Syntax Error: Malformed Expression"
bNt (lookaheadtokens)= case lookaheadtokens of
                       ((NOT_ _ "NOT"):tokens)->let (ast,tokens')=bNt tokens in (UnaryExp (Not ast),tokens')
                       otherwise->expr (lookaheadtokens)

expr :: [PosToken]->(AST,[PosToken])
expr tokens = expr' . factor $ tokens

expr' :: (AST,[PosToken])->(AST,[PosToken])
expr' (lhs,[]) = (lhs,[])
expr' (lhs,lookaheadtokens)= case lookaheadtokens of
                              ((GREATERTHAN_ _ ">"):tokens) -> let (rhs, tokens')=factor tokens in expr'(BinExp(Greaterthan lhs rhs), tokens')
                              ((LESSTHAN_ _ "<"):tokens) -> let (rhs, tokens')=factor tokens in expr'(BinExp(Lessthan lhs rhs), tokens')
                              otherwise -> (lhs,lookaheadtokens)

factor :: [PosToken]->(AST,[PosToken])
factor tokens = factor' . term $ tokens 

factor' :: (AST,[PosToken])->(AST,[PosToken])
factor' (lhs,[])=(lhs,[])
factor' (lhs,lookaheadtokens) = case lookaheadtokens of
                                ((PLUS_ _ "+"):tokens) -> let (rhs, tokens')=term tokens in factor'(BinExp(AddExp lhs rhs), tokens')
                                ((MINUS_ _ "-"):tokens) -> let (rhs, tokens')=term tokens in factor'(BinExp(MinusExp lhs rhs), tokens')
                                otherwise -> (lhs,lookaheadtokens)

term :: [PosToken]->(AST,[PosToken])
term tokens = term' . iden $ tokens

term' :: (AST,[PosToken])->(AST,[PosToken])
term' (lhs,[])=(lhs,[])
term' (lhs,lookaheadtokens)=case lookaheadtokens of
                            ((TIMES_ _ "*"):tokens) -> let (rhs, tokens')=iden tokens in term'(BinExp(MulExp lhs rhs), tokens')
                            otherwise -> (lhs,lookaheadtokens)

iden :: [PosToken]->(AST,[PosToken])
iden ([]) = error "Syntax Error: Malformed Expression"
iden (lookaheadtokens)=case lookaheadtokens of
                       ((NEGATE_ _ "~"):tokens)-> let (ast, tokens') = iden tokens in (UnaryExp(Negate ast), tokens')
                       otherwise -> simple (lookaheadtokens)

simple :: [PosToken] -> (AST, [PosToken])
simple (NUM_ _ a:tokens) = (Number a, tokens)
simple (ID_ _ a:tokens) = (Id a, tokens)
simple (CONST_ _ a:tokens) = (Const a ,tokens)
simple (LPAREN_ _ "(":tokens) =
  let (ast, tokens') = parseStmt tokens
   in case tokens' of
      (RPAREN_ _ ")":rest1)->(ast, drop 1 tokens')
      (x:xs)->error $ "Syntax Error:"++ showPosn x ++"(Missing ')' (<expr>)"
      ([])->error "Syntax Error:(Missing ')' (<expr>)"
simple (x:xs) = error $ "Syntax Error:" ++ showPosn x ++ " (Invalid) <expr>"
  