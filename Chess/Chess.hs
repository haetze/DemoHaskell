module Chess where

import qualified Data.Map as Map
import Data.List
import Data.Char

data Color = Black | White
           deriving(Show, Read, Eq)

data PieceType = King | Queen | Rook | Knight | Bishop | Pawn
               deriving(Show, Read, Eq)

type XPosition = Int 
              
type YPosition = Int
        
type Position = (XPosition, YPosition)

type Piece = (PieceType, Color)



startingBoard:: Map.Map Position Piece
startingBoard = Map.fromList $ [((1, 1), (Rook,   Black))
                               ,((2, 1), (Knight, Black))
                               ,((3, 1), (Bishop, Black))
                               ,((4, 1), (Queen,  Black))
                               ,((5, 1), (King,   Black ))
                               ,((6, 1), (Bishop, Black))
                               ,((7, 1), (Knight, Black))
                               ,((8, 1), (Rook,   Black))
                               ,((1, 2), (Pawn, Black))
                               ,((2, 2), (Pawn, Black))
                               ,((3, 2), (Pawn, Black))
                               ,((4, 2), (Pawn, Black))
                               ,((5, 2), (Pawn, Black))
                               ,((6, 2), (Pawn, Black))
                               ,((7, 2), (Pawn, Black))
                               ,((8, 2), (Pawn, Black))
                                -- White Pieces
                               ,((1, 7), (Pawn, White))
                               ,((2, 7), (Pawn, White))
                               ,((3, 7), (Pawn, White))
                               ,((4, 7), (Pawn, White))
                               ,((5, 7), (Pawn, White))
                               ,((6, 7), (Pawn, White))
                               ,((7, 7), (Pawn, White))
                               ,((8, 7), (Pawn, White))              
                               ,((1, 8), (Rook, White))
                               ,((2, 8), (Knight, White))
                               ,((3, 8), (Bishop, White))
                               ,((4, 8), (King, White))
                               ,((5, 8), (Queen, White))
                               ,((6, 8), (Bishop, White))
                               ,((7, 8), (Knight, White))
                               ,((8, 8), (Rook, White))]

showBoard:: Map.Map Position Piece -> IO ()
showBoard = loop 1 1
  where
    loop _ 9 _ = do putStrLn "|"
    loop 9 y b = do putStrLn "|"; loop 1 (y+1) b
    loop x y b = do case Map.lookup (x,y) b of
                      Nothing -> do putStr "|_"; loop (x+1) y b
                      Just p  -> do putStr ("|" ++ toString p); loop (x+1) y b


toString::Piece -> String
toString (t, Black) = return $ toLower $ head $ show t
toString (t, _)     = return $ head $ show t


filterInBounds:: [Position] -> [Position]
filterInBounds = filter f
  where
    f (x,y) | x > 0 && x < 9 && y > 0 && y < 9 = True
            | otherwise                        = False  
 
possibleMoves:: Position -> Piece -> [Position]

possibleMoves (x,2) (Pawn,Black) = filterInBounds [(x,3),(x,4),(x+1,3),(x-1,3)]
possibleMoves (x,y) (Pawn,Black) = filterInBounds [(x,y+1),(x+1,y+1),(x-1,y+1)]

possibleMoves (x,7) (Pawn,White) = filterInBounds [(x,6),(x,5),(x+1,6),(x-1,6)]
possibleMoves (x,y) (Pawn,White) = filterInBounds [(x,y-1),(x+1,y-1),(x-1,y-1)]

possibleMoves (x,y) (Rook,_)     = filterInBounds [(x',y') | x' <- [1..8], y' <- [1..8] , x == x' || y == y']

possibleMoves (x,y) (Bishop,_)   = filterInBounds $
  [(x-n,y-n)| n <- [1..8]] ++
  [(x-n,y+n)| n <- [1..8]] ++
  [(x+n,y-n)| n <- [1..8]] ++
  [(x+n,y+n) | n <- [1..8]]

possibleMoves (x,y) (Knight,_) = filterInBounds $ [(x+2,y+1)
                                                  ,(x+2,y-1)
                                                  ,(x-2,y+1)
                                                  ,(x-2,y-1)
                                                  ,(x+1,y+2)
                                                  ,(x-1,y+2)
                                                  ,(x+1,y-2)
                                                  ,(x-1,y-2)]
  
possibleMoves (x,y) (Queen,_) = possibleMoves (x,y) (Rook,Black) ++ possibleMoves (x,y) (Bishop, Black)

possibleMoves (x,y) (King,_)  = filterInBounds $ [(x+1,y+1)
                                                 ,(x+1,y-1)
                                                 ,(x-1,y-1)
                                                 ,(x-1,y+1)
                                                 ,(x+1,y)
                                                 ,(x-1,y)
                                                 ,(x,y+1)
                                                 ,(x,y-1)]


possibleOnBoard:: Position -> Piece -> Map.Map Position Piece -> [Position]
possibleOnBoard pos p@(piece, color) b = filter (pathwise pos b) [p' | p' <- possibleMoves pos p
                                              ,check pos p' p b]

check:: Position -> Position -> Piece -> Map.Map Position Piece -> Bool
check _ _  (Knight,_) b = True

check (x,y) (x',y') (Pawn, c)   b | x /= x' = case Map.lookup (x',y') b of
                                      Nothing -> False
                                      Just (piece, color) -> color /= c
                                  | otherwise = and [Map.lookup pos b == Nothing
                                                     || f (Map.lookup pos b) c | pos <- findPath (x,y) (x',y') ]
  where
    f (Just (_, color)) c = c /= color

check p p' (_, color)   b = and [f (Map.lookup pos b) color | pos <- findPath p p']
  where
    f Nothing           _ = True
    f (Just (_, color)) c = c /= color


findPath:: Position -> Position -> [Position]
findPath p p' | p == p'                    = []
findPath (x,y) (x',y') | y' == y && x < x' = (x',y') : findPath (x,y) (x'-1,y')
                       | y' == y && x > x' = (x',y') : findPath (x,y) (x'+1,y')
                       | x' == x && y < y' = (x',y') : findPath (x,y) (x',y'-1)
                       | x' == x && y > y' = (x',y') : findPath (x,y) (x',y'+1)
                       | x < x'  && y < y' = (x',y') : findPath (x,y) (x'-1,y'-1)
                       | x > x'  && y > y' = (x',y') : findPath (x,y) (x'+1,y'+1)
                       | x > x'  && y < y' = (x',y') : findPath (x,y) (x'+1,y'-1)
                       | x < x'  && y > y' = (x',y') : findPath (x,y) (x'-1,y'+1)

remove x = filter (\y -> y /= x)

pathwise:: Position -> Map.Map Position Piece -> Position -> Bool
pathwise p b p' = and [Map.lookup pos b == Nothing | pos <- remove p' $ remove p $ findPath p p']


testBoard::Map.Map Position Piece
testBoard = Map.fromList $ [((2,2),(Pawn, White))
                            ,((3,3),(Pawn,White))]
