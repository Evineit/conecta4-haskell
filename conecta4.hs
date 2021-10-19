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

tableroPrueba = tableroInicial

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

getRows :: Marcador -> Posiciones
getRows (Tab row columns) = row

muestraLinea :: Marcador -> [Posicion] -> String
muestraLinea t =
  intercalate " | " . map (muestraPosicion t)

-- >>> getRows marcadorInicial
-- []

-- >>> muestraTablero marcadorInicial
-- "Numero de columna\n{1,2,3,4,5,6,7}\n--------------------\n[0,0,0,0,0,0,0]\n[0,0,0,0,0,0,0]\n[0,0,0,0,0,0,0]\n[0,0,0,0,0,0,0]\n[0,0,0,0,0,0,0]\n[0,0,0,0,0,0,0]\n--------------------\n"

muestraTablero :: Marcador -> String
muestraTablero marcador =
  "Numero de columna\n"
    ++ "{"
    ++ intercalate "," (map show [1 .. 7])
    ++ "}\n"
    ++ getSeparator
    ++ intercalate "\n" (replicate 6 (show (replicate 7 0)))
    ++ "\n"
    ++ getSeparator

getSeparator :: [Char]
getSeparator =
  do
    concat (replicate 20 "-") ++ "\n"

play :: IO ()
play =
  do
    putStrLn "Juego"

main :: IO ()
main =
  do
    hSetBuffering stdout NoBuffering
    putStrLn "Conecta 4 | 2 jugadores"
    putStrLn (muestraTablero marcadorInicial)
    putStrLn "Comienza el juego ? (s/n)"
    l <- getLine
    play
