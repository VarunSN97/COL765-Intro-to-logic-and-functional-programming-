module Evaluator(eval) where

import Ast
import Environment

data Value = IntVal Integer | BoolVal Bool | FunVal Expr VEnv deriving (Show,Eq)

type VEnv = Env Value

eval (IntConst x) env = ((IntVal x),env)
eval (BoolConst x) env = ((BoolVal x),env)
eval (BinExpr Plus e e') env = case (eval e env) of
				((IntVal x),_) -> case (eval e' env) of
						   ((IntVal x'), _) -> ((IntVal (x+x')),env)

				(_)-> error "Couldn't Evaluate"

eval (BinExpr Minus e e') env = case (eval e env) of
				((IntVal x),_) -> case (eval e' env) of
						   ((IntVal x'),_) -> ((IntVal (x-x')),env)
				_ -> error "Couldn't Evaluate"

eval (BinExpr Times e e') env = case (eval e env) of
				((IntVal x),_) -> case (eval e' env) of
						   ((IntVal x'),_) -> ((IntVal (x*x')),env)
				_ -> error "Couldn't Evaluate"

eval (BinExpr GreaterThan e e') env = case (eval e env) of
			         	((IntVal x),_) -> case (eval e' env) of
				         		   ((IntVal x'),_) -> if x>x'
 								              then ((BoolVal True),env)
								              else ((BoolVal False),env)	
				        _ -> error "Couldn't Evaluate"

eval (BinExpr LessThan e e') env = case (eval e env) of
			         	((IntVal x),_) -> case (eval e' env) of
				         		   ((IntVal x'),_) -> if x<x'
 								              then ((BoolVal True),env)
								              else ((BoolVal False),env)					
				        _ -> error "Couldn't Evaluate"
eval (BinExpr Equals e e') env = case (eval e env) of
				 ((IntVal x),_) -> case (eval e' env) of
				 		    ((IntVal x'),_) -> if (x==x')
				 			               then ((BoolVal True),env)
				 			               else ((BoolVal False),env)
				 _ -> error "Couldn't Evaluate"

eval (BinExpr And e e') env = case (eval e env) of
				((BoolVal x),_) -> case (eval e' env) of
						    ((BoolVal x'),_) -> ((BoolVal (x && x')),env)
				_ -> error "Couldn't Evaluate"

eval (BinExpr Or e e') env = case (eval e env) of
				((BoolVal x),_) -> case (eval e' env) of
						    ((BoolVal x'),_) -> ((BoolVal (x || x')),env)
				_ -> error "Couldn't Evaluate"

eval (BinExpr Xor e e') env = case (eval e env) of
				((BoolVal x),_) -> case (eval e' env) of
						    ((BoolVal x'),_) -> if (x /= x')
								        then ((BoolVal True),env)
								        else ((BoolVal False),env)
				_ -> error "Couldn't Evaluate" 

eval (UnExpr Negate e) env = case (eval e env) of
                               ((IntVal x),_) -> ((IntVal (-1*x)),env)
			       _ -> error "Couldn't Evaluate"

eval (UnExpr Not e) env = case (eval e env) of
			   ((BoolVal x),_) -> if x==True
					      then ((BoolVal False),env)
					      else ((BoolVal True),env)

eval (Ite e e' e'') env = case (eval e env) of
			   ((BoolVal x),_) -> if x==True
					      then eval e' env
					      else eval e'' env

eval (VarExpr e) env = (find e env, env)

eval (Let (Decl e e') e'') env = let
				  (t,env') = eval e' env
				  env'' = insert e t env'
				  (t',env''')= eval e'' env''
				 in 
				  (t',env''')

eval (Fn (VarExpr v) t1 e) env = ((FunVal (Fn (VarExpr v) t1 e) env), env)
eval (FunExp n (VarExpr v) t1 t2 e) env = ((FunVal (FunExp n (VarExpr v) t1 t2 e) env), env) 	

eval (AppExp e e') env = let
			   (t,_) = eval e' env
			 in
			   case e of 
				(VarExpr x) -> let
						 f = find x env
                                               in
                                                 functionalEval f t env
                                _ -> let
					(f,env') = eval e env
				      in
					functionalEval f t env

functionalEval (FunVal (Fn (VarExpr v) t1 e) env') arg env =let
							     env'' = insert v arg env'
							     (t2,env1)= eval e env''
						            in
							     (t2,env1)			    
functionalEval (FunVal (FunExp (VarExpr name) (VarExpr v) t1 t2 e) env') arg env = let
																				(tx,envx) = eval (FunExp (VarExpr name) (VarExpr v) t1 t2 e) env
																				env1 = insert name tx env
																				env2 = insert v arg env1
																				(t3,env3) = eval e env2
																			in
																				(t3,envx)
		    