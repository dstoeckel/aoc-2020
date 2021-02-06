import qualified Data.Array as A
import qualified Data.Map as M
import qualified Data.Set as S
import System.Environment (getArgs)
import Data.List (span, delete, sort, (\\), find)
import Data.Char (isDigit)
import Data.List.Split (chunksOf)
import Data.Maybe (isJust, mapMaybe)
import Debug.Trace (trace)

data Pixel = Dot | Hash deriving (Show, Eq, Ord)
type Tile = A.Array (Int, Int) Pixel
data Direction = DTop | DBottom | DLeft | DRight deriving (Show)

type Tiles = M.Map Int Tile
type Coords = M.Map Int Coord

type Match = (Int, (Direction, Int, Int))
type Coord = (Int, Int)

(!) = (A.!) :: Tile -> (Int, Int) -> Pixel

pixel :: Char -> Pixel
pixel '.' = Dot
pixel '#' = Hash

pToC :: Pixel -> Char
pToC Dot = '.'
pToC Hash = '#'

parseId :: String -> Int
parseId = fst . head . reads . dropWhile (not . isDigit)

tailIf :: [a] -> [a]
tailIf [] = []
tailIf (x:xs) = xs

parseTiles :: [String] -> [(Int, Tile)]
parseTiles [] = []
parseTiles xs =
    let
        id = head xs
        (tile, xs') = span (/="") . tail $ xs
        n = length . head $ tile
    in
        (parseId id, A.listArray ((1,1), (n,n)) . map pixel . concat $ tile) : parseTiles (tailIf xs')

top    n a = [a ! (1, i) | i <- [1..n]]
bottom n a = [a ! (n, i) | i <- [1..n]]
left   n a = [a ! (i, 1) | i <- [1..n]]
right  n a = [a ! (i, n) | i <- [1..n]]

compareTiles' :: Int -> Int -> Int -> Tile -> Tile -> Maybe (Direction, Tile)
compareTiles' n 4 1 a b = Nothing
compareTiles' n 4 0 a b = compareTiles' n 0 1 a (flipTile n b)
compareTiles' n i f a b =
    if (top n a) == (bottom n b) then
        Just (DTop, b)
    else if (bottom n a) == (top n b) then
        Just (DBottom, b)
    else if (left n a) == (right n b) then
        Just (DLeft, b)
    else if (right n a) == (left n b) then
        Just (DRight, b)
    else
        compareTiles' n (i + 1) f a (rotateTile n b)

compareTiles :: Int -> Tiles -> Int -> Int -> Maybe (Direction, (Int, Tile))
compareTiles n mt idA idB =
    case compareTiles' n 0 0 (mt M.! idA) (mt M.! idB) of
        Just (d, t) -> Just (d, (idB, t))
        Nothing -> Nothing

rotateTile :: Int -> Tile -> Tile
rotateTile n a = A.array ((1,1), (n,n)) (zip [(n - i + 1, j) | j <- [1..n], i <- [1..n]] (A.elems a))

flipTile :: Int -> Tile -> Tile
flipTile n a = A.array ((1,1), (n,n)) (zip [(n - i + 1, j) | i <- [1..n], j <- [1..n]] (A.elems a))

coord :: (Int, Int) -> Direction -> (Int, Int)
coord (i, j) DTop    = (i - 1, j)
coord (i, j) DBottom = (i + 1, j)
coord (i, j) DLeft   = (i    , j - 1)
coord (i, j) DRight  = (i    , j + 1)

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (x, y) = (f x, y)

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (x, y) = (x, f y)

updateMap :: (Ord k) => M.Map k a -> [(k, a)] -> M.Map k a
updateMap m xs = foldl (\s (k, a) -> M.insert k a s) m xs

assignCoordinates :: Int -> Tiles -> Coords -> [Int] -> [Int] -> (Tiles, Coords)
assignCoordinates n mt mc [] _ = (mt, mc)
assignCoordinates n mt mc (t:ts) available =
    let
        c = mc M.! t
        neighbours = mapMaybe (compareTiles n mt t) available
        mt' = updateMap mt . map snd $ neighbours
        neighbourCoords = map (mapFst (coord c)) neighbours
        mc' = updateMap mc . map (\(c, (i, t)) -> (i, c)) $ neighbourCoords
        next = map (fst . snd) neighbours
        available' = available \\ next
    in
        assignCoordinates n mt' mc' (next ++ ts) available'

bothInRange :: Int -> ((Int, Int), Pixel) -> Bool
bothInRange n ((i, j), _) = i > 1 && i < n && j > 1 && j < n

extractSingle :: Int -> Coord -> (Int, Tile) -> [((Int, Int), Pixel)]
extractSingle n (x, y) (_, t) =
    let
        pixels = filter (bothInRange n) (A.assocs t)
    in
        map (\((i, j), p) -> ((i - 1 + x*(n - 2), j - 1 + y*(n - 2)), p)) pixels


extract :: Int -> Int -> Coords -> [(Int, Tile)] -> Tile
extract n m mc tiles =
    let
        tileCoords = concat $ map (\(i, t) -> extractSingle n (mc M.! i) (i, t)) tiles
    in
        A.array ((1,1), (m*(n - 2), m*(n - 2))) tileCoords


scanForMonster :: Int -> Tile -> [(Int, Int)] -> (Int, Int) -> S.Set (Int, Int)
scanForMonster n t m (i, j) =
    let
        indices = [(i + x - 1, j + y - 1) | (x,y) <- m]
    in
        if all ((==Hash) . (t!)) indices
            then S.fromList indices else S.empty

scanForMonsters :: [(Int, Int)] -> Tile -> S.Set (Int, Int)
scanForMonsters m t =
    let
        n = fst . snd . A.bounds $ t
        (mx, my) = (3, 20)
    in
       S.unions . map (scanForMonster n t m) $ [(i, j) | i <- [1..n - mx + 1], j <- [1 .. n - my + 1]]

main :: IO ()
main = do
    args <- getArgs

    tileList <- parseTiles <$> lines <$> readFile (head args)

    let
        n = 10
        m = round . sqrt . fromIntegral . length $ tileList
        n' = m * (n - 2)
        (id1, t1) = head tileList
        (tiles, coords) = assignCoordinates n (M.fromList tileList) (M.singleton id1 (0, 0)) [id1] (map fst (tail tileList))
        mx = M.foldl (\s (x, _) -> min s x) maxBound coords
        my = M.foldl (\s (_, y) -> min s y) maxBound coords
        coords' = M.map (\(x, y) -> (x - mx, y - my)) coords
        assembled = extract n m coords' . M.toList $ tiles

        monsterRaw = "                  # #    ##    ##    ### #  #  #  #  #  #   "
        monster = map snd $ filter ((=='#') . fst) $ zip monsterRaw [(i,j) | i <- [1..3], j <- [1..20]]

        possible = take 4 ( iterate (rotateTile n') assembled) ++ take 4 (iterate (rotateTile n') (flipTile n' assembled))
        Just monsterPixel = find (not . S.null) . map (scanForMonsters monster) $ possible

    -- Part 1
    print $ product . map fst . filter (\(_, c) -> c == (0,0) || c == (0, m - 1) || c == (m - 1, 0) || c == (m - 1, m - 1)) . M.toList $ coords'

    -- Part 2
    print $ (length . filter ((==Hash)) . A.elems $ assembled) - S.size monsterPixel

