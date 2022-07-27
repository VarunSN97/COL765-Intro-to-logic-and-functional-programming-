{
module Lexer (
  Token(..),
  PosToken(..),
  scanTokens,
  getTokens,
  showPosn,
  getLineNum,
  getColumnNum,
) where



}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]


token :-

  $white+                       ;

 
  ------------------------------------------------------------------------------
  \+   	                        { \p s -> PLUS_ p s }
  \-     	                    { \p s -> MINUS_ p s }
  \*                            { \p s -> TIMES_ p s }
  \(                            { \p s -> LPAREN_ p s }
  \)                            { \p s -> RPAREN_ p s }
  TRUE							{ \p s -> CONST_ p s }
  FALSE							{ \p s -> CONST_ p s }
  \~							{ \p s -> NEGATE_ p s }
  \==							{ \p s -> EQUALS_ p s }
  \<							{ \p s -> LESSTHAN_ p s }
  \>							{ \p s -> GREATERTHAN_ p s }
  NOT							{ \p s -> NOT_ p s }
  AND							{ \p s -> AND_ p s }
  OR							{ \p s -> OR_ p s }
  XOR							{ \p s -> XOR_ p s }
  IMPLIES						{ \p s -> IMPLIES_ p s }
  \`							{ \p s -> EOF_ p s }
  \:=							{ \p s -> ASSIGNMENT_ p s }
  let							{ \p s -> LET_ p s }
  in							{ \p s -> IN_ p s }
  end							{ \p s -> END_ p s }
  if							{ \p s -> IF_ p s }
  then							{ \p s -> THEN_ p s }
  else							{ \p s -> ELSE_ p s }
  fi							{ \p s -> FI_ p s }
  $alpha [$alpha]* 				{ \p s -> ID_ p s }
  $digit+                       { \p s -> NUM_ p s }
  ------------------------------------------------------------------------------
{

data PosToken
  = CONST_ AlexPosn String
  | ID_ AlexPosn String
  | NUM_ AlexPosn String
  | PLUS_ AlexPosn String
  | MINUS_ AlexPosn String
  | TIMES_ AlexPosn String
  |NEGATE_ AlexPosn String
  | EQUALS_ AlexPosn String
  |LESSTHAN_ AlexPosn String
  |GREATERTHAN_ AlexPosn String
  |NOT_ AlexPosn String
  |AND_ AlexPosn String
  |OR_ AlexPosn String
  |XOR_ AlexPosn String
  |IMPLIES_ AlexPosn String
  |LPAREN_ AlexPosn String
  |RPAREN_ AlexPosn String
  |EOF_ AlexPosn String
  |ASSIGNMENT_ AlexPosn String
  |LET_ AlexPosn String
  |IN_ AlexPosn String
  |END_ AlexPosn String
  |IF_ AlexPosn String
  |THEN_ AlexPosn String
  |ELSE_ AlexPosn String
  |FI_ AlexPosn String
  deriving (Eq,Show)

data Token
  = CONST String
  | ID String
  | NUM String
  | PLUS String
  | MINUS String
  | TIMES String
  | NEGATE String
  | EQUALS String
  |LESSTHAN String
  |GREATERTHAN String
  |NOT String
  |AND String
  |OR String
  |XOR String
  | IMPLIES String
  |LPAREN String
  |RPAREN String
  | EOF String
  |ASSIGNMENT String
  |LET String
  |IN String
  |END String
  |IF String
  |THEN String
  |ELSE String
  |FI String
  deriving (Eq,Show)

scanTokens :: String -> [PosToken]
scanTokens str = go (alexStartPos,'\n',[],str)
  where go inp@(pos,_,_,str) =
          case alexScan inp 0 of
                AlexEOF -> []
                AlexError ((AlexPn _ line column),_,_,str) -> error $ "Unknown token:" ++ (show line) ++ ":" ++ (show column) ++ ":"++str
                AlexSkip  inp' len     -> go inp'
                AlexToken inp' len act -> act pos (take len str) : go inp'


getTokens :: [PosToken]->[Token]
getTokens [] = []
getTokens ls =
	 case ls of
		((CONST_ _ s):xs)->(CONST s):(getTokens xs)
		((ID_ _ s):xs)-> (ID s):(getTokens xs)
		((NUM_ _ s):xs)-> (NUM s):(getTokens xs)
		((PLUS_ _ s):xs)-> (PLUS s):(getTokens xs)
		((MINUS_ _ s):xs)-> (MINUS s):(getTokens xs)
		((TIMES_ _ s):xs)-> (TIMES s):(getTokens xs)
		((NEGATE_ _ s):xs)-> (NEGATE s):(getTokens xs)
		((EQUALS_ _ s):xs)-> (EQUALS s):(getTokens xs)
		((LESSTHAN_ _ s):xs)-> (LESSTHAN s):(getTokens xs)
		((GREATERTHAN_ _ s):xs)-> (GREATERTHAN s):(getTokens xs)
		((NOT_ _ s):xs)-> (NOT s):(getTokens xs)
		((AND_ _ s):xs)-> (AND s):(getTokens xs)
		((OR_ _ s):xs)-> (OR s):(getTokens xs)
		((XOR_ _ s):xs)-> (XOR s):(getTokens xs)
		((IMPLIES_ _ s):xs)-> (IMPLIES s):(getTokens xs)
		((LPAREN_ _ s):xs)-> (LPAREN s):(getTokens xs)
		((RPAREN_ _ s):xs)-> (RPAREN s):(getTokens xs)
		((EOF_ _ s):xs)-> (EOF s):(getTokens xs)
		((ASSIGNMENT_ _ s):xs)-> (ASSIGNMENT s):(getTokens xs)
		((LET_ _ s):xs)-> (LET s):(getTokens xs)
		((IN_ _ s):xs)-> (IN s):(getTokens xs)
		((END_ _ s):xs)-> (END s):(getTokens xs)
		((IF_ _ s):xs)-> (IF s):(getTokens xs)
		((THEN_ _ s):xs)-> (THEN s):(getTokens xs)
		((ELSE_ _ s):xs)-> (ELSE s):(getTokens xs)
		((FI_ _ s):xs)-> (FI s):(getTokens xs)

getLineNum :: AlexPosn -> Int
getLineNum (AlexPn _ lineNum _) = lineNum

getColumnNum :: AlexPosn -> Int
getColumnNum (AlexPn _ _ colNum) = colNum


showPosn :: PosToken -> String
showPosn  t = case t of
             (PLUS_ a b) -> show (getLineNum a) ++ ":" ++ show (getColumnNum a) 
             (MINUS_ a b ) -> show (getLineNum a) ++ ":" ++ show (getColumnNum a) 
             (TIMES_ a b ) -> show (getLineNum a) ++ ":" ++ show (getColumnNum a) 
             (NEGATE_ a b ) -> show (getLineNum a) ++ ":" ++ show (getColumnNum a)
             (EQUALS_ a b ) -> show (getLineNum a) ++ ":" ++ show (getColumnNum a) 
             (LESSTHAN_ a b ) -> show (getLineNum a) ++ ":" ++ show (getColumnNum a) 
             (GREATERTHAN_ a b ) -> show (getLineNum a) ++ ":" ++ show (getColumnNum a) 
             (LPAREN_ a b ) -> show (getLineNum a) ++ ":" ++ show (getColumnNum a) 
             (RPAREN_ a b ) -> show (getLineNum a) ++ ":" ++ show (getColumnNum a) 
             (EOF_ a b ) -> show (getLineNum a) ++ ":" ++ show (getColumnNum a) 
             (NOT_ a b ) -> show (getLineNum a) ++ ":" ++ show (getColumnNum a) 
             (AND_ a b ) -> show (getLineNum a) ++ ":" ++ show (getColumnNum a) 
             (OR_ a b ) -> show (getLineNum a) ++ ":" ++ show (getColumnNum a)
             (XOR_ a b ) -> show (getLineNum a) ++ ":" ++ show (getColumnNum a) 
             (IMPLIES_ a b ) -> show (getLineNum a) ++ ":" ++ show (getColumnNum a) 
             (CONST_ a b ) -> show (getLineNum a) ++ ":" ++ show (getColumnNum a)
             (IF_ a b ) -> show (getLineNum a) ++ ":" ++ show (getColumnNum a) 
             (THEN_ a b ) -> show (getLineNum a) ++ ":" ++ show (getColumnNum a) 
             (ELSE_ a b ) -> show (getLineNum a) ++ ":" ++ show (getColumnNum a) 
             (FI_ a b ) -> show (getLineNum a) ++ ":" ++ show (getColumnNum a)
             (LET_ a b ) -> show (getLineNum a) ++ ":" ++ show (getColumnNum a)
             (IN_ a b ) -> show (getLineNum a) ++ ":" ++ show (getColumnNum a)
             (END_ a b ) -> show (getLineNum a) ++ ":" ++ show (getColumnNum a)
             (ID_ a b ) -> show (getLineNum a) ++ ":" ++ show (getColumnNum a)
             (NUM_ a b ) -> show (getLineNum a) ++ ":" ++ show (getColumnNum a)
             (ASSIGNMENT_ a b ) -> show (getLineNum a) ++ ":" ++ show (getColumnNum a)



              
		
}