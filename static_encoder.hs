import System.Environment
import Data.Char
data Expresion = Or [Expresion] | And [Expresion] | Variable Int Int Int Bool | Verdadero | Falso
               deriving (Show, Read)

-- AND de dos expresiones.
(<&&>) :: Expresion -> Expresion -> Expresion
(And x) <&&> (And y) = And (x++y)
x@(Variable _ _ _ _) <&&> Verdadero = And [x]
x <&&> Verdadero = x
x@(Variable _ _ _ _) <&&> (And y) = And (x:y)

-- OR de dos expresiones
(<||>) :: Expresion -> Expresion -> Expresion
x <||> y = Or [x, y]

-- Retorna los valores que hay que chequear para conservar la unicidad
-- en el subcuadro
cuadro :: Int -> Int -> [(Int, Int)]
cuadro i j = let modI = mod i 3
                 modJ = mod j 3
                 vals x = case x of
                   0 -> [1, 2]
                   1 -> [-1, 1]
                   2 -> [-1, -2]
             in [(i+i', j+j') | i' <- vals modI, j' <- vals modJ ]

-- Computa las reglas que se tienen que seguir para resolver un sudoku.
-- Estas son:
-- * reglasIniciales -> los valores iniciales que toma el tablero al iniciar
-- * unicidadCasillas -> cada casilla puede tener un solo valor asignado
-- * unicidadFilas -> en cada fila solo puede haber un valor k
-- * unicidadColumna -> en cada columna solo puede haber un valor k
-- * unicidadSubC -> en cada subcuadrado solo puede haber un valor k
-- * casillaConValor -> cada casilla tiene que tener un valor
reglas :: [(Int, Int, Int)] -> Expresion
reglas inicial = reglasIniciales-- <&&> casillasConValor <&&> unicidadCasillas <&&> unicidadFilas <&&> unicidadCols <&&> unicidadSubC
  where reglasIniciales = foldr (<&&>) (Verdadero) (map transformar inicial)
        transformar (i, j, k) = Variable i j k True
        --Calcula el resto de las reglas
        --Para agilizar el calculo, las precauculamos en el acrihvo 'common'
        --unicidadCasillas = And [(Variable i j k False) <||> (Variable i j k' False) | i <- [0..8], j<- [0..8], k <- [1..9], k' <- [1..9], k' /= k]
        --unicidadFilas = And [(Variable i j k False) <||> (Variable i j' k False) | i <- [0..8], k <- [1..9], j <- [0..8], j' <- [0..8], j /= j']
        --unicidadCols = And [(Variable i j k False) <||> (Variable i' j k False) | j <- [0..8], k <- [1..9], i <- [0..8], i' <- [0..8], i /= i']
        --unicidadSubC = And [(Variable i j k False) <||> (Variable i' j' k False) | i <- [0..8], j <- [0..8], k <- [1..9], (i', j') <- cuadro i j]
        --casillasConValor = And [Or [Variable i j k True | k <- [1..9]] | i <- [0..8], j <- [0..8]]

nv :: Expresion -> Int
nv (Variable i j k _) = (k-1)*81+9*i+j+1

-- Transformamos nuestra expresion a formato dimacs para poder leerlo
-- con nuestro SAT solver
toDimacs :: String -> Expresion -> String
toDimacs common (And xs) = unlines (encabezado:common:clausulas)
  where encabezado = "p cnf 729 " ++ show (20498+(length xs))
        clausulas = map ((\x -> x ++ "0") . stringify) xs
        stringify x@(Variable _ _ _ b) = show (if b then  (nv x) else - (nv x)) ++ " "
        stringify (Or xs) = concat $ map stringify xs



trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

-- Transformamos nuestra instancia a una serie de variables iniciales
-- que ya tienen valor.
variablesIniciales :: Int -> String -> [(Int, Int, Int)]
variablesIniciales i "" = []
variablesIniciales i line = res
  where res = calc i (take 9 line) ++ variablesIniciales (i+1) (drop 9 line)
        calc _ "" = []
        calc i ('.':xs) = calc i xs
        calc i (x:xs) = (i, 8-length(xs), digitToInt x):(calc i xs)

main :: IO ()
main = do
  args <- getArgs
  reglasComunes <- readFile "common"
  putStrLn $ toDimacs (init reglasComunes) $ reglas $ variablesIniciales 0 (trim (args !! 0))
