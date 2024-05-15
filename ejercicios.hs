import System.Win32.Automation (xBUTTON1)
siguiente :: Integer -> Integer
siguiente nro = nro + 1

doble :: Integer -> Integer
doble nro = nro * 2

calcular :: Integer -> Integer
calcular nro | even nro = siguiente nro
             | otherwise = doble nro

calcular' :: (Integer, Integer) -> (Integer, Integer)
calcular'  (nro, otroNum) = (primeroPar nro, segundoImpar otroNum)

primeroPar :: Integer -> Integer
primeroPar nro | even nro = doble nro
               | otherwise = nro

segundoImpar :: Integer -> Integer
segundoImpar nro | odd nro = siguiente nro
                 | otherwise = nro


and' :: Bool -> Bool -> Bool
and' valor otroValor | valor = otroValor
                     | otherwise = False

and'' :: Bool -> Bool -> Bool
and'' True otroValor = otroValor
and'' _ _ = False

or' :: Bool -> Bool -> Bool
or' False False = False
or' _ _ = True

or'' :: Bool -> Bool -> Bool
or'' unValor otroValor | unValor = True
                       | otherwise = otroValor

or''' :: Bool -> Bool -> Bool
or''' False otroValor = otroValor
or'''  _ _ = True


type Nota = Integer
type Alumno = (String, Nota, Nota, Nota)

notaMaxima :: Alumno -> Nota
notaMaxima (_, nota, nota2, nota3) = nota `max` (nota2 `max` nota3)

notaMaxima' :: Alumno -> Nota
notaMaxima' (_, nota, nota2, nota3) = max nota (max nota2 nota3)

cuadruple nro = doble (doble nro)

esMayorA :: Integer -> Bool
esMayorA nro =  doble (siguiente (nro+2) ) > 10

maximoSe :: Ord a => (t -> a) -> [t] -> t
maximoSe _ [x] = x
maximoSe f (x:xs) | f x > f (maximoSe f xs) = x
                  | otherwise = maximoSe f xs


algo lista = map (promedio. filter (>=6)) lista

promedio notas = (sum notas) `div` (length notas) 

sinApla listaAlumnos = map (promedio . filter (>= 6)) listaAlumnos