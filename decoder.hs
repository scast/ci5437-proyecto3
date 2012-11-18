import Data.List
import Data.Char
import System.Environment
sat :: String -> Bool
sat status = status == "s SATISFIABLE"

-- Split de un string con un booleano
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
  "" -> []
  s' -> w : wordsWhen p s''
        where (w, s'') = break p s'

-- Dada una serie de variables que leemos de stdin lo vamos a
-- transformar de su forma x_n a su forma (i, j, k) para luego sacar
-- solamente el valor k
tablero :: String -> [String]
tablero variables = tablero' valores
  where variables' = tail (wordsWhen (==' ') variables)
        valores = map (\(_, _, x) -> x) $ sort $ map f (filter (>0) $ map (\x -> read x::Int) variables')
        f x = [(i, j, k) | i <- [0..8], j <- [0..8], k <- [1..9], (k-1)*81+9*i+j+1 == x ]!! 0
        tablero' [] = []
        tablero' xs =  (map intToDigit (take 9 xs) : tablero' (drop 9 xs))

-- Imprimamos una version leible por un humano
separadorHorizontal :: String
separadorHorizontal =  "+---+---+---+"

ponerSeparador :: [String] -> [String]
ponerSeparador [x, y, z] = [vertical x, vertical y, vertical z, separadorHorizontal]
  where vertical xs = "|" ++ (take 3 xs) ++ "|" ++ (take 3 (drop 3 xs)) ++ "|" ++ (drop 6 xs) ++ "|"

imprimirTablero t = let pprint = putStr . unlines . ponerSeparador
                    in do
                      putStrLn separadorHorizontal
                      pprint $ take 3 t
                      pprint $ take 3 $ drop 3 t
                      pprint $ drop 6 t

main :: IO ()
main = do
  args <- getArgs -- Si pasamos algun argumento, se imprimira para un
                  -- humano. Si no se imprimira la instancia resuelta
                  -- como fue pasada
  status <- getLine
  if (sat status) then
    do
      variables <- getLine
      tableroR <- return $ tablero variables
      if (length args) > 0 then
        do imprimirTablero tableroR
      else
        do
          putStrLn $ concat tableroR

  else
    putStrLn $ "No hay solucion"