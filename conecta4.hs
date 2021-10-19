import Control.Monad
import Data.Array
import Data.List
import Data.Map (Map)
import System.IO

type Tablero = [[Char]]

data Marca = X | O
  deriving (Show, Eq)

tableroInicial :: [[a]]
tableroInicial = replicate 7 []

push :: Show p => Int -> p -> [[Char]] -> [[Char]]
push columna marca tablero
  | columna < 1 || columna > 7 = tablero
  | otherwise = y ++ x
  where
    (i, x) = splitAt columna tablero
    y = init i ++ [last i ++ show marca]

listToLines :: Tablero -> [Char]
listToLines tablero =
  let x = tablero
      accumulate acc el
        | null el || head el == 'F' = acc ++ "□"
        | head el == 'X' || head el == 'O' = acc ++ [head el]
      x1 = foldl' accumulate "" x
      x2 = foldl' accumulate "" $ map rex x
      x3 = foldl' accumulate "" $ map (rex . rex) x
      x4 = foldl' accumulate "" $ map (rex . rex . rex) x
      x5 = foldl' accumulate "" $ map (rex . rex . rex . rex) x
      x6 = foldl' accumulate "" $ map (rex . rex . rex . rex . rex) x
      j s = "[" ++ s ++ "]"
   in intercalate "\n" $ map (j . intersperse ',') [x6, x5, x4, x3, x2, x1]

rex :: [a] -> [a]
rex col
  | null col = col
  | otherwise = tail col

muestraTablero :: [[Char]] -> [Char]
muestraTablero tablero =
  "Numero de columna\n"
    ++ "{"
    ++ intercalate "," (map show [1 .. 7])
    ++ "}\n"
    ++ getSeparator
    ++ listToLines tablero
    ++ "\n"
    ++ getSeparator

getSeparator :: [Char]
getSeparator =
  do
    concat (replicate 20 "-") ++ "\n"

next :: Marca -> Marca
next marca
  | marca == X = O
  | marca == O = X

checkFour :: [Char] -> Bool
checkFour (x : y : z : t : rest)
  | not (null [x, y, z, t]) && (x == y && y == z && z == t) && (x /= 'F') = True
  | otherwise = checkFour (y : z : t : rest)
checkFour _ = False

checkWin :: Tablero -> Bool
checkWin tab
  | any checkFour $ tab ++ transpose tab ++ diagonals (map filler tab) ++ diagonals (rotl (map filler tab)) = True
  | otherwise = False

muestraGanador :: Marca -> String
muestraGanador marca
  | marca == X = "Gano X"
  | marca == O = "Gano O"

diagonals :: [[a]] -> [[a]]
diagonals [] = []
diagonals (xs : xss) =
  takeWhile (not . null) $
    zipWith
      (++)
      (map (: []) xs ++ repeat [])
      ([] : diagonals xss)

rotl :: [[x]] -> [[x]]
rotl = transpose . map reverse

filler :: [Char] -> [Char]
filler col
  | length col == 6 = col
  | otherwise = filler $ col ++ "F"

main :: IO ()
main =
  do
    hSetBuffering stdout NoBuffering
    putStrLn "Conecta 4 | 2 jugadores"
    play X 0 tableroInicial
  where
    play marca col tablero = do
      putStrLn (muestraTablero tablero)
      if checkWin tablero
        then putStrLn (muestraGanador (next marca))
        else do
          putStrLn "Movimiento jugador:"
          print marca
          l <- getLine
          case filter (\(_, s) -> s == "") (reads l :: [(Int, String)]) of
            [] -> do
              putStrLn "Movimiento invalido\n"
              play marca col tablero
            (xAsInt, _) : _ ->
              play (next marca) (read l :: Int) $ push (read l :: Int) marca tablero
