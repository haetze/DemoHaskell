module Chess where

import qualified Data.Map as Map
import Data.List
import Data.Char

data Color = Black | White
           deriving(Show, Read)

data PieceType = King | Queen | Rook | Knight | Bishop | Pawn
               deriving(Show, Read)

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
 
