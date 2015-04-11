printTree :: Document -> [String]
printTree doc = case doc of
	Node nodes -> "<<Node>>" : (concatMap (map indent . printTree) nodes)
	Adaptive m -> M.foldWithKey (\k x ks ->
		(k ++ " => ") : map indent (printTree x) ++ ks) [] m
	Leaf text -> ["\"" ++ text ++ "\""]
	where
		indent = (". " ++)
