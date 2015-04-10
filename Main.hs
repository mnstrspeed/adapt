import qualified Data.Map as M
import Control.Applicative hiding (many, (<|>), )
import Text.ParserCombinators.Parsec
import System.Environment

data Document = Node [Document] 
              | Leaf String 
              | Adaptive (M.Map String Document) deriving (Show)

pDocument = Node <$> many (pAdaptive <|> pLeaf)
	where
		pAdaptive = char '[' *> spaces *> pAdaptiveBody <* spaces <* char ']'
		
		pAdaptiveBody = adaptive <$> pAdaptiveOpt `sepBy` char '|'
			where adaptive = Adaptive . M.fromList

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
	Node nodes -> "<<Node>>" : (concatMap (map indent . printTree) nodes)
	Adaptive m -> M.foldWithKey (\k x ks ->
		(k ++ " => ") : map indent (printTree x) ++ ks) [] m
	Leaf text -> ["\"" ++ text ++ "\""]
	where
		indent = (". " ++)

adapt :: Document -> String
adapt doc = case doc of
	Node nodes -> concatMap adapt nodes
	Leaf text -> text
	Adaptive map -> "adaptive"

main = do
	input <- getArgs >>= \t -> case t of
		[path] -> readFile path
		_ -> getContents	

	case parse pDocument "document" input of 
		Right doc -> putStrLn . unlines . printTree $ doc
		Left error -> putStrLn $ show error
