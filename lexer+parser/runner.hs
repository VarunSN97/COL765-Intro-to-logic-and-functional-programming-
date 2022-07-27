import Parser
import System.Environment

main :: IO ()
main = do
	args <- getArgs
	x<- readFile (head args)
	print (getTokens (scanTokens x))
	putStr "\n" 
	print (beginning x)
       