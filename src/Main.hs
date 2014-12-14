module Main where
import Data.List (nub, foldl')
import Control.Applicative ((<$>), (<*>), pure)
import qualified Data.Map.Strict as M
import qualified Data.Set        as S


main :: IO ()
main = do let b = M.fromList [((1,1), White)]
          print $ negamaxScore 2 White b


type Board = M.Map (Int, Int) Space


data Space = Empty | White | Black deriving (Eq)


instance Show Space where
    show Empty = "."
    show White = "W"
    show Black = "B"


data RotateDirection = Clockwise | Counter deriving (Eq, Show)


negamaxScore :: Int -> Space -> Board -> Int
negamaxScore depth color = abPrune depth color (-(8^8)) (8^8)
    where abPrune depth color a b board
              | depth == 0 = let s = scoreBoard board in if color == White then s else negate s
              | otherwise = (\(a,_,_) -> a) $ foldl' f (-(8^8), a, b) (nub $ possibleMoves color board)
              where f :: (Int, Int, Int) -> Board -> (Int, Int, Int)
                    f x@(bestVal, a, b) board = if a >= b then x
                                                else let val = abPrune (depth-1) (otherColor color) (negate b) (negate a) board
                                                         bestVal' = max bestVal val
                                                         a' = max a val
                                                     in (bestVal', a', b)

otherColor :: Space -> Space
otherColor Empty = Empty
otherColor White = Black
otherColor Black = White


showBoard :: Board -> String
showBoard b = concat [showRow y | y <- [1..6]]
              where showRow y = concat [show (b ! (x, y)) | x <- [1..3]] ++ " " ++ concat [show (b ! (x, y)) | x <- [4..6]] ++ "\n"


possibleMoves :: Space -> Board -> [Board]
possibleMoves s b = move <$>
                    (S.toList $ S.difference allBoardCoordinates (M.keysSet b)) <*>
                    pure s <*>
                    quadrantDeltas <*>
                    [Clockwise, Counter] <*>
                    pure b

move :: (Int, Int) -> Space -> (Int, Int) -> RotateDirection -> Board -> Board
move p s d r b = rotateQuadrant d r (M.insert p s b)


allBoardCoordinates :: S.Set (Int, Int)
allBoardCoordinates = S.fromList [(x, y) | x <- [1..6], y <- [1..6]]


quadrantDeltas :: [(Int, Int)]
quadrantDeltas = [
    (0, 0),
    (3, 0),
    (3, 3),
    (0, 3)]


rotateCoordinates :: [((Int, Int), (Int, Int))]
rotateCoordinates = [
    ((2, 1), (3, 2)),
    ((3, 2), (2, 3)),
    ((2, 3), (1, 2)),
    ((1, 2), (2, 1)),
    ((1, 1), (3, 1)),
    ((3, 1), (3, 3)),
    ((3, 3), (1, 3)),
    ((1, 3), (1, 1))]


rotateQuadrant :: (Int, Int) -> RotateDirection -> Board -> Board
rotateQuadrant (dx, dy) direction board = foldr (swapSpace direction) board rotateCoordinates
                                          where swapSpace :: RotateDirection -> ((Int, Int), (Int, Int)) -> Board -> Board
                                                swapSpace Clockwise ((sx, sy), (tx, ty)) = offsetInsert sx sy tx ty
                                                swapSpace Counter   ((tx, ty), (sx, sy)) = offsetInsert sx sy tx ty
                                                offsetInsert :: Int -> Int -> Int -> Int -> Board -> Board
                                                offsetInsert sx sy tx ty = case board ! (sx+dx, sy+dy) of
                                                                                Empty -> M.delete (tx+dx, ty+dy)
                                                                                a     -> M.insert (tx+dx, ty+dy) a


(!) :: Board -> (Int, Int) -> Space
(!) = flip (M.findWithDefault Empty)


scoreBoard :: Board -> Int
scoreBoard b = sum $ map (scoreLine . map (b !)) (rowsFromGrid (6, 6) 5)
               where scoreLine :: [Space] -> Int
                     scoreLine xs = case countSpaces xs (0, 0)
                                    of (0, 0) -> 0
                                       (n, 0) -> 8 ^ n
                                       (0, n) -> negate $ 8 ^ n
                                       _      -> 0
                     countSpaces :: [Space] -> (Int, Int) -> (Int, Int)
                     countSpaces (Empty:xs) (w, b) = countSpaces xs (w, b)
                     countSpaces (White:xs) (w, b) = countSpaces xs (w+1, b)
                     countSpaces (Black:xs) (w, b) = countSpaces xs (w, b+1)
                     countSpaces [] t = t


rowsFromGrid :: (Int, Int) -> Int -> [[(Int, Int)]]
rowsFromGrid (x,y) n = map (take n . iterate (\(a, b) -> (a+1, b)))   ((,) <$> [1..x-n+1] <*> [1..y])     ++
                       map (take n . iterate (\(a, b) -> (a,   b+1))) ((,) <$> [1..x]     <*> [1..y-n+1]) ++
                       map (take n . iterate (\(a, b) -> (a+1, b+1))) ((,) <$> [1..x-n+1] <*> [1..y-n+1]) ++
                       map (take n . iterate (\(a, b) -> (a-1, b+1))) ((,) <$> [n..x]     <*> [1..y-n+1])
