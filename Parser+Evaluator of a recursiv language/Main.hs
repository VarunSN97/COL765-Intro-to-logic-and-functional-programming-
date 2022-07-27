import System.Environment
import Lexer
import Parser
import Typing
import Evaluator 
import Ast

main = do
	args <- getArgs
	x<- readFile (head args)
	putStr "Parser:\n" 
	print (parser (scanTokens x))
	let z = (parser (scanTokens x))
	putStr "\n"
	putStr "Evaluator:\n" 
	let a = getType z []
	if (a==Int || a==Boolean) 
        then let
                (val,_) = eval z[]
	     in
		print (val)
        else print(a)
       