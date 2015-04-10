import qualified Data.Map as M
import Control.Applicative hiding (many, (<|>), )
import Text.ParserCombinators.Parsec

data Document = Node [Document] 
              | Leaf String 
              | Adaptive (M.Map String Document) deriving (Show)

pDocument = Node <$> many (pAdaptive <|> pLeaf)
	where
		pAdaptive = 
			char '[' *> spaces *> 
			(Adaptive . M.fromList <$> pAdaptiveOpt `sepBy` char '|')
			<* spaces <* char ']'

		pAdaptiveOpt :: Parser (String, Document)
		pAdaptiveOpt = do
			key <- many1 alphaNum
			spaces *> string "=>" <* spaces
			value <- pDocument
			return (key, value)

		pLeaf :: Parser Document
		pLeaf = Leaf <$> (many1 $ noneOf "[|]")

printTree :: Document -> [String]
printTree doc = case doc of
	Node nodes -> "<<Node>>" : (concat $ map (map indent . printTree) nodes)
	Adaptive m -> M.foldWithKey (\k x ks ->
		(k ++ " => ") : map indent (printTree x) ++ ks) [] m
	Leaf text -> ["\"" ++ text ++ "\""]
	where
		indent = ("  " ++)

main = do
	res <- parseFromFile pDocument "test.adapt"
	case res of 
		Right doc -> putStrLn . unlines. printTree $ doc
		Left error -> putStrLn $ show error
