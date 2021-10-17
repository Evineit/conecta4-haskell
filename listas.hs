digs :: Integral x => x -> [x]
digs 0 = []
digs x = digs (div x 10) ++ [mod x 10]

longitud m = length (show m)

pre n
  | n == 0 = "Cero"
  | n == 100 = "Cien"
  | n == 1000 = "Mil"
  | n <= 20 = base !! (n - 1)
  | longitud n == 2 = casodos (digs n)
  | longitud n == 3 = casotres (digs n)
  | otherwise = "Mayor de 1000"

casotres k
  | (k !! 1 == 0) && (k !! 2 == 0) = loscienes k
  | k !! 1 == 1 = cen k ++ dieces k
  | k !! 1 == 0 = cen k ++ uniplus k
  | k !! 2 == 0 = cen k ++ decplus2 k
  | otherwise = cen k ++ decplus k ++ uniplus k

casodos d
  | d !! 1 == 0 = decplusesp d
  | otherwise = dec d ++ uni d

loscienes :: [Int] -> String
loscienes arr = base3esp !! ((arr !! 0) - 2)

dieces :: [Int] -> String
dieces arr = base !! ((arr !! 2) + 9)

uni :: [Int] -> String
uni arr = base !! ((arr !! 1) - 1)

dec :: [Int] -> String
dec arr = base2 !! ((arr !! 0) - 2)

cen :: [Int] -> String
cen arr = base3 !! ((arr !! 0) - 1)

uniplus :: [Int] -> String
uniplus arr = base !! ((arr !! 2) - 1)

decplus :: [Int] -> String
decplus arr = base2 !! ((arr !! 1) - 2)

decplus2 :: [Int] -> String
decplus2 arr = base2esp !! ((arr !! 1) - 2)

decplusesp :: [Int] -> String
decplusesp arr = base2esp !! ((arr !! 0) - 2)

censolo :: [Int] -> String
censolo arr = base3 !! ((arr !! 0) - 1)

base = ["Uno", "Dos", "Tres", "Cuatro", "Cinco", "Seis", "Siete", "Ocho", "Nueve", "Diez", "Once", "Doce", "Trece", "Catorce", "Quince", "Dieciseis", "Diecisiete", "Dieciocho", "Diecinueve", "Veinte"]

base2 = ["Veinti", "Treinta y ", "Cuarenta y", "Cincuenta y ", "Sesenta y ", "Setenta y ", "Ochenta y ", "Noventa y "]

base3 = ["Ciento ", "Doscientos ", "Trescientos ", "Cuatrocientos ", "Quinientos ", "Seiscientos ", "Setecientos ", "Ochocientos ", "Novecientos"]

base2esp = ["Veinte", "Treinta", "Cuarenta", "Cincuenta", "Sesenta", "Setenta", "Ochenta", "Noventa"]

base3esp = ["Doscientos ", "Trescientos ", "Cuatrocientos ", "Quinientos ", "Seiscientos ", "Setecientos ", "Ochocientos ", "Novecientos "]
