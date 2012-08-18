module Chess where

import Data.Char
import Control.Monad
import Test.HUnit
type Board = [[Square]]

note :: a -> Maybe b -> Either a b
note a m = case m of
    Nothing -> Left  a
    Just b  -> Right b



initialBoardStr ::  String
initialBoardStr = unlines ["rnbqkbnr"
                          ,"pppppppp"
                          ,"........"
                          ,"........"
                          ,"........"
                          ,"........"
                          ,"PPPPPPPP"
                          ,"RNBQKBNR"
                          ]

readBoard ::  String -> Either String Board
readBoard = (mapM . mapM) readSquare . lines

showBoard :: Board -> String
showBoard = unlines . (map . map) showSquare

type Square = Maybe Piece

-- | Show a square using FEN notation or ' ' for an empty square.
showSquare :: Square -> Char
showSquare = maybe '.' showPiece

-- | Read a square using FEN notation or ' ' for an empty square.
readSquare :: Char -> Either String Square
readSquare '.' = return Nothing
readSquare  c  = note errormsg $ fmap return (readPiece c)
  where errormsg = "Error reading square: " ++ show c ++ " is not a valid square"

data Piece = Piece PColor PType deriving (Show)
data PColor = White | Black deriving (Show)
data PType = Pawn | Knight | Bishop | Rook | Queen | King deriving (Show)

-- | Shows a piece using FEN notation.
--
-- * White pieces are "PNBRQG"
-- * Black pieces are "pnbrqg"
showPiece :: Piece -> Char
showPiece (Piece White Pawn)   = 'P'
showPiece (Piece White Knight) = 'N'
showPiece (Piece White Bishop) = 'B'
showPiece (Piece White Rook)   = 'R'
showPiece (Piece White Queen)  = 'Q'
showPiece (Piece White King)   = 'K'
showPiece (Piece Black Pawn)   = 'p'
showPiece (Piece Black Knight) = 'n'
showPiece (Piece Black Bishop) = 'b'
showPiece (Piece Black Rook)   = 'r'
showPiece (Piece Black Queen)  = 'q'
showPiece (Piece Black King)   = 'k'

typeList :: [(Char, PType)]
typeList =  [('p', Pawn)
            ,('n', Knight)
            ,('b', Bishop)
            ,('r', Rook)
            ,('q', Queen)
            ,('k', King)
            ]

-- | Reads a piece using FEN notation.
readPiece :: Char -> Maybe Piece
readPiece c = fmap makePiece lookupType
  where color      = if isUpper c then White else Black
        lookupType = lookup (toLower c) typeList
        makePiece  = Piece color

movePiece :: Piece -> Square -> Maybe Square
