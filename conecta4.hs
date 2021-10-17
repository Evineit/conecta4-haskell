import Control.Monad
import Data.List
import System.IO

type Posicion = Int

type Posiciones = [Posicion]

data Marcador = Tab Posiciones Posiciones
  deriving (Show)

muestraPosicion :: Marcador -> Posicion -> String
muestraPosicion (Tab xs os) p
  | p `elem` xs = "X"
  | p `elem` os = "O"
  | otherwise = show p

marcadorInicial :: Marcador
marcadorInicial = Tab [] []

getRows :: Marcador -> Posiciones
getRows (Tab row columns) = row

muestraLinea :: Marcador -> [Posicion] -> String
muestraLinea t =
  intercalate " | " . map (muestraPosicion t)

-- >>> getRows marcadorInicial
-- Tab [] []

-- >>> muestraTablero marcadorInicial
-- "{1,2,3,4,5,6,71 | 2 | 3\n -+-+-\n4 | 5 | 6\n -+-+-\n7 | 8 | 9"

muestraTablero :: Marcador -> String
muestraTablero marcador =
  "Numero de columna\n" ++
  "{" ++ intercalate "," (map show [1 .. 7]) ++ "}\n"
    ++ getSeparator
    ++ intercalate "\n" (replicate 6 (show (replicate 7 0))) ++ "\n"
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
