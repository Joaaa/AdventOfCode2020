module Day21.Part1 where

import qualified Data.Set as S
import Text.Parsec
import Control.Applicative (liftA2)
import Common.Utils
import Control.Monad
import Common.FileLoading
import qualified Data.Map as M

type Ingredient = String
type Allergen = String

data Food = Food {
    foodIngredients :: S.Set Ingredient,
    foodAllergens :: S.Set Allergen
} deriving Show

parseIngredients :: Parser (S.Set Ingredient)
parseIngredients = S.fromList <$> endBy (many alphaNum) (char ' ')

parseAllergens :: Parser (S.Set Allergen)
parseAllergens = S.fromList <$> (string "(contains " *> sepBy (many alphaNum) (string ", ") <* char ')')

parseFood :: Parser Food
parseFood = liftA2 Food parseIngredients parseAllergens

possibleNamesFor :: Allergen -> [Food] -> S.Set Ingredient
possibleNamesFor allergen foods = foldl1 S.intersection [foodIngredients food | food <- foods, S.member allergen (foodAllergens food)]

allergenNames :: M.Map Allergen Ingredient
allergenNames = M.fromList [("dairy", "lvv"), ("eggs", "xblchx"), ("nuts", "tr"), ("peanuts", "gzvsg"), ("sesame", "jlsqx"), ("shellfish", "fnntr"), ("soy", "pmz"), ("wheat", "csqc")]

solution = do
    foods <- readParsed (Day 21) (sepBy parseFood endOfLine)
    let allergens = S.unions $ map foodAllergens foods
    forM_ allergens $ \allergen -> do
        putStr $ allergen <> ": "
        print $ possibleNamesFor allergen foods
    let allAllergenIngredients = S.fromList $ M.elems allergenNames
    print $ sum $ [S.size (S.difference (foodIngredients food) allAllergenIngredients) | food <- foods]
    print $ M.elems allergenNames