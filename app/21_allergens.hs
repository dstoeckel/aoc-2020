import qualified Data.Map as M
import qualified Data.Set as S
import System.Environment (getArgs)
import Data.List (find, delete, sort, intersperse)
import Data.List.Split (chunksOf, splitOn)
import Data.Tuple (swap)

flatten :: ([a], b) -> [(a, b)]
flatten ([], _) = []
flatten ((x:xs), y) = (x, y):flatten (xs, y)

parseIngredients :: String -> ([String], S.Set String)
parseIngredients s =
    let
        [a, b] = splitOn " (contains " s

    in
        (splitOn ", " . init $ b, S.fromList . words $ a)

removeIngredient :: String -> (String, S.Set String) -> (String, S.Set String)
removeIngredient s (a, xs) = (a, S.delete s xs)

computeAssignments :: [(String, S.Set String)] -> [(String, String)]
computeAssignments [] = []
computeAssignments xs =
    let
        Just (allergen, ingredient) = find ((==1) . S.size . snd) xs
        xs' = delete (allergen, ingredient) xs
        [ingredient'] = S.toList ingredient
    in
        (allergen, ingredient') : computeAssignments (map (removeIngredient ingredient') xs')

main :: IO ()
main = do
    args <- getArgs

    a2i <- map parseIngredients <$> lines <$> readFile (head args)

    let
        reduced = M.toList . M.fromListWith S.intersection . concat . map flatten $ a2i
        assignments = sort . computeAssignments $ reduced
        unsafeIngredients = S.fromList $ map snd assignments

    print $ sum $ map (S.size . (S.\\unsafeIngredients) . snd) a2i
    putStrLn $ concat . intersperse "," . map snd $ assignments
