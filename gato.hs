import Data.List
import System.IO

profundidadDeBusqueda :: Int
profundidadDeBusqueda = 6

type Posicion = Int
type Posiciones = [Posicion]

data Tablero = Tab Posiciones Posiciones
    deriving Show

tableroInicial :: Tablero
tableroInicial = Tab [][]

turnoDeX :: Tablero -> Bool
turnoDeX (Tab xs os) =
    length xs == length os

pone :: Tablero -> Posicion -> Tablero
pone (Tab xs os) p | turnoDeX (Tab xs os) = Tab (p:xs) os
    | otherwise = Tab xs (p:os)

completo :: Tablero -> Bool
completo (Tab xs os) = length xs + length os == 9

subconjunto :: Posiciones -> Posiciones -> Bool
subconjunto s1 s2 = all (`elem` s2) s1

tieneLinea :: Posiciones -> Bool 
tieneLinea ps =
    subconjunto [1,2,3] ps || subconjunto [4,5,6] ps ||
    subconjunto [7,8,9] ps || subconjunto [1,4,7] ps ||
    subconjunto [2,5,8] ps || subconjunto [3,6,9] ps ||
    subconjunto [1,5,9] ps || subconjunto [3,5,7] ps

tieneGanador :: Tablero -> Bool
tieneGanador (Tab xs os ) = tieneLinea xs || tieneLinea os

data Arbol a = Nodo a [Arbol a]

muestraArbol :: Show a => Arbol a -> [Char]
muestraArbol (Nodo x xs ) =
    show x ++ '\n' : (unlines . map ("  "++) . concatMap (lines . show)) xs

instance Show a => Show (Arbol a) where
    show = muestraArbol

posicionesLibres :: Tablero -> Posiciones
posicionesLibres (Tab xs os) = [1..9] \\ (xs++os)

siguientesTableros :: Tablero -> [Tablero]
siguientesTableros t
    | tieneGanador t = []
    | otherwise = map (pone t) (posicionesLibres t)

construyeArbol :: Tablero -> Arbol Tablero
construyeArbol t =
    Nodo t (map construyeArbol (siguientesTableros t))

type Valor = Int

valores :: [Arbol (Valor, Tablero)] -> [Valor]
valores vts = [v | Nodo (v,_) _ <- vts]

maximiza :: Arbol Tablero -> Arbol (Valor, Tablero)
maximiza (Nodo t []) | tieneGanador t = Nodo (-1, t) [] 
    | otherwise = Nodo (0, t) []
maximiza (Nodo t ts) = Nodo (maximum (valores vts), t) vts
    where vts = map minimiza ts

minimiza :: Arbol Tablero -> Arbol (Valor, Tablero)
minimiza (Nodo t []) | tieneGanador t = Nodo (1,t) []
    | otherwise = Nodo (0, t) []
minimiza (Nodo t ts) = Nodo (minimum (valores vts), t) vts
    where vts = map maximiza ts

poda :: Int -> Arbol a -> Arbol a
poda n (Nodo x as) | n == 0 = Nodo x []
    | otherwise = Nodo x (map (poda (n-1)) as)

selecciona :: Arbol (Valor, Tablero) -> Tablero
selecciona (Nodo (v, _) ts) =
    head [t | Nodo (v', t) _ <- ts, v' == v]

mejorMovimiento :: Tablero -> Tablero
mejorMovimiento = selecciona . maximiza . poda profundidadDeBusqueda . construyeArbol

muestraPosicion :: Tablero -> Posicion -> String
muestraPosicion (Tab xs os) p
    | p `elem` xs = "X"
    | p `elem` os = "O"
    | otherwise = show p

muestraLinea :: Tablero -> [Posicion] -> String
muestraLinea t =
    concat . intersperse " | " . map (muestraPosicion t)

muestraTablero :: Tablero -> String 
muestraTablero t = 
    muestraLinea t [1..3] ++ "\n -+-+-\n" ++
    muestraLinea t [4..6] ++ "\n -+-+-\n" ++
    muestraLinea t [7..9]

main :: IO ()
main = 
  do
    hSetBuffering stdout NoBuffering 
    putStrLn "Tres en raya"
    putStrLn (muestraTablero tableroInicial)
    putStrLn "Comienza el juego ? (s/n)"
    l <- getLine
    if head l `elem` "sS"
        then humano tableroInicial
        else computadora tableroInicial

humano :: Tablero -> IO ()
humano t =
  do
    putStr "\n Indica el lugar donde colocar la ficha: "
    l <- getLine
    let t' = pone t (read l :: Posicion)
    putStrLn (muestraTablero t')
    if tieneGanador t'
      then putStrLn "Has ganado"
      else
        if completo t'
          then putStrLn "Empate"
          else computadora t'

computadora :: Tablero -> IO ()
computadora t =
  do
    putStrLn "\n Mi jugada: "
    let t' = mejorMovimiento t
    putStrLn (muestraTablero t')
    if tieneGanador t'
      then putStrLn "He ganado."
      else if completo t'
        then putStrLn "Empate" 
        else humano t' 
        
