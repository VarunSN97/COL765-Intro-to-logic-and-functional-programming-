module Environment where

type Key = String
type Env value = [(Key,value)]


--insert:: Key -> a -> Env a -> Env a
insert k v env = case env of
		    [] -> [(k,v)]
	            ev@((k',v'):env')-> case compare k k' of
					 LT ->(k,v):ev
					 EQ ->(k,v):ev
					 GT ->(k',v'):(insert k v env')
--find:: Key -> Env a -> a
find k env = case env of
                  [] -> error $ "Var"++ k ++"w/o a type"
                  ((k',v'):env') -> case compare k k' of
                                        LT -> error $ "Unbound variable " ++ k
                                        EQ -> v'
                                        GT -> find k env' 