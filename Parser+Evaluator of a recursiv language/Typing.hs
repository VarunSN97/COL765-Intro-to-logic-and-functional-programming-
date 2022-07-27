module Typing(getType) where

import Ast
import Environment


getType (BoolConst _) env= Boolean
getType (IntConst _ ) env= Int
getType (BinExpr Plus e e') env= let
                                  t = getType e env
                                  t' = getType e' env
                                 in
                                  if (t== Int && t' ==Int)
                                  then Int
                                  else error "Mismatch types in (+)"
getType (BinExpr Minus e e') env= let
                                   t = getType e env
                                   t' = getType e' env
                                  in
                                   if (t== Int && t' ==Int)
                                   then Int
                                   else error "Mismatch types in (-)"
getType (BinExpr Times e e') env= let
                                   t = getType e env
                                   t' = getType e' env
                                  in
                                   if (t== Int && t' ==Int)
                                   then Int
                                   else error "Mismatch types in (*)"
getType (BinExpr LessThan e e') env= let
                                      t = getType e env
                                      t' = getType e' env
                                     in
                                      if (t== Int && t' ==Int)
                                      then Boolean
                                      else error "Mismatch types in (<)"
getType (BinExpr GreaterThan e e') env= let
                                      t = getType e env
                                      t' = getType e' env
                                     in
                                      if (t== Int && t' ==Int)
                                      then Boolean
                                      else error "Mismatch types in (>)"
getType (BinExpr And e e') env= let
                                      t = getType e env
                                      t' = getType e' env
                                     in
                                      if (t== Boolean && t' ==Boolean)
                                      then Boolean
                                      else error "Mismatch types in (AND)"  
getType (BinExpr Or e e') env= let
                                      t = getType e env
                                      t' = getType e' env
                                     in
                                      if (t== Boolean && t' ==Boolean)
                                      then Boolean
                                      else error "Mismatch types in (OR)"
getType (BinExpr Xor e e') env= let
                                      t = getType e env
                                      t' = getType e' env
                                     in
                                      if (t== Boolean && t' ==Boolean)
                                      then Boolean
                                      else error "Mismatch types in (XOR)"

getType (BinExpr Equals e e') env= let
                                      t = getType e env
                                      t' = getType e' env
                                     in
                                      if (t==t')
                                      then Boolean 
                                      else error "Mismatch types in (==)"

getType (UnExpr Negate e) env= case getType e env of
                                Int -> Int
                                _ -> error "Expected Int : Found Boolean for (~)"
getType (UnExpr Not e) env= case getType e env of
                             Boolean -> Boolean
                             _ -> error "Expected Boolean : Found Int (NOT)"

getType (Ite e e' e'') env= case getType e env of
			     Boolean -> let
                                       t'=getType e' env
                                       t''=getType e'' env
                                      in
                                       if(t'==t'')
                                       then t'
                                       else error "If else arguments of different type" 

getType (VarExpr e) env = find e env

getType (FunExp (VarExpr v) (VarExpr e) t (SINGLE s) e') env = let 
                                                                env' = insert e t env
                                                                env''= insert v s env'
			                                       in
                                                                getType e' env''
getType (FunExp (VarExpr v) (VarExpr e) t (ARROW _ s) e') env = let 
                                                                 env' = insert e t env
                                                                 env''= insert v s env'
			                                        in
                                                                 getType e' env''

getType (Fn (VarExpr e) t e') env = let
                                     env' = insert e t env
                                    in
                                     getType e' env'
getType (AppExp e e') env = let
                             t=getType e env
                             t'=getType e' env
                            in
                             if(t==t')
                             then t
                             else error $ "Application Argument mismatch in expression" 

getType (Let (Decl e e') e'') env = let
                                     env' = insert e (getType e' env) env
                                    in
                                     getType e'' env' 
                                    



