import Control.Applicative ((<$>), (*>), (<*), empty, )
import Text.ParserCombinators.Parsec
import qualified Data.Map as M
import qualified Data.List as L
import Data.Maybe
import System.Environment

data Document = Node [Document] 
              | Leaf String 
              | Adaptive (M.Map String Document) 
              deriving (Show)

pDocument = Node <$> many (pAdaptive <|> pLeaf)
	where
		pAdaptive = char '[' *> spaces *> pAdaptiveBody <* spaces <* char ']'
		pAdaptiveBody = adaptive <$> pAdaptiveOpt `sepBy` char '|'
			where adaptive = Adaptive . M.fromList
		pAdaptiveOpt = do
			key <- many1 alphaNum
			spaces *> string "=>" <* spaces
			value <- pDocument
			return (key, value)
		pLeaf = Leaf <$> (many1 $ pChar)
		pChar = pEscapedChar <|> noneOf "[|]" 
		pEscapedChar = char '\\' *> anyChar

adapt :: [String] -> Document -> String
adapt context doc = case doc of
	Node nodes -> concatMap (adapt context) nodes
	Leaf text -> text
	Adaptive map -> case L.find (flip elem $ M.keys map) context of
		Just key -> adapt context (fromJust $ M.lookup key map)
		Nothing -> empty

main = do
	(path : tags) <- getArgs
	doc <- parseFromFile pDocument $ path ++ ".adapt"
	case doc of 
		Right doc -> writeFile path $ adapt tags doc
		Left error -> putStrLn $ show error
