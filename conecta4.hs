import Control.Monad
import Data.Array
import Data.List
import System.IO

type Posicion = Int

type Posiciones = [Posicion]

data Marcador = Tab Posiciones Posiciones
  deriving (Show)

data Marca = X | O
  deriving (Show, Eq)

muestraPosicion :: Marcador -> Posicion -> String
muestraPosicion (Tab xs os) p
  | p `elem` xs = "X"
  | p `elem` os = "O"
  | otherwise = show p

marcadorInicial :: Marcador
marcadorInicial = Tab [] []

tableroInicial :: [[a]]
tableroInicial = replicate 7 []

push :: Show p => Int -> p -> [[Char]] -> [[Char]]
push columna marca tablero
  | columna < 1 || columna > 7 = tablero
  | otherwise = y ++ x
  where
    (i, x) = splitAt columna tablero
    y = init i ++ [last i ++ show marca]

test =
  push 2 X $ push 2 O tableroInicial

listToLines l =
  []

-- tableroMod =
-- map listToLines tableroPrueba

-- >>> tableroInicial
-- [[],[],[],[],[],[],[]]
-- >>> listToLines tableroInicial
-- []

listToLines :: [[Char]] -> [Char]
listToLines tablero =
  let x = tablero
      accumulate acc el
        | null el = acc ++ "□"
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

-- >>> muestraTablero marcadorInicial
-- "Numero de columna\n{1,2,3,4,5,6,7}\n--------------------\n[0,0,0,0,0,0,0]\n[0,0,0,0,0,0,0]\n[0,0,0,0,0,0,0]\n[0,0,0,0,0,0,0]\n[0,0,0,0,0,0,0]\n[0,0,0,0,0,0,0]\n--------------------\n"

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

main :: IO b
main =
  do
    hSetBuffering stdout NoBuffering
    putStrLn "Conecta 4 | 2 jugadores"
    play X 0 tableroInicial
  where
    play marca col tablero = do
      putStrLn (muestraTablero tablero)
      putStrLn "Movimiento jugador:"
      print marca
    l <- getLine
      play (next marca) (read l :: Int) $ push (read l :: Int) marca tablero
