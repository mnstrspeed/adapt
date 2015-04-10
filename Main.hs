import qualified Data.Map as M
import Control.Applicative hiding (many, (<|>), )
import Text.ParserCombinators.Parsec

data Document = Node [Document] 
              | Leaf String 
              | Adaptive (M.Map String Document) deriving (Show)

pDocument = Node <$> many (pAdaptive <|> pLeaf)

pAdaptive = 
	char '[' *> spaces *> 
	(Adaptive <$> M.fromList <$> (pAdaptiveOpt `sepBy` (char '|')))
	<* spaces <* char ']'

pAdaptiveOpt :: Parser (String, Document)
pAdaptiveOpt = do
	key <- many alphaNum
	spaces *> string "=>" <* spaces
	value <- pDocument
	return (key, value)

pLeaf :: Parser Document
pLeaf = Leaf <$> (many1 $ noneOf "[|]")

prettyPrint :: Document -> [String]
prettyPrint doc = case doc of
	Node nodes -> "<<Node>>" : (concat $ map (map indent . prettyPrint) nodes)
	Adaptive m -> M.foldWithKey (\k x ks ->
		((k ++ " => ") : map indent (prettyPrint x) ++ ks)) [] m
	Leaf text -> ["\"" ++ text ++ "\""]
	where
		indent = ((++) "  ")

main = do
	res <- parseFromFile pDocument "test.adapt"
	case res of 
		Right doc -> putStrLn . unlines. prettyPrint $ doc
		-- Right doc -> putStrLn . show $ doc
