
cantidadDeElementos lista = foldl (\sem _ -> sem + 1)  0 lista

cantidadDeElementos' lista = foldr (\_ sem -> sem + 1) 0  lista

masGastador :: [(String, Integer)] -> (String, Integer)
masGastador   (cab:cola)   = foldl mayorGasto  cab cola

mayorGasto :: (String, Integer) -> (String, Integer) -> (String, Integer)
mayorGasto emple otroEmple | snd emple > snd otroEmple = emple
                           | otherwise = otroEmple
                             

masGastador' ::[(String, Integer)] -> (String, Integer)
masGastador'  (cab:cola) = foldr mayorGasto cab cola

monto :: [(String, Integer)]-> Integer
monto empleados = foldl  (\sem (_, gasto)-> sem + gasto)  0  empleados

monto' :: [(String, Integer)] -> Integer
monto' empleados = foldr (\(_, gasto) sem -> gasto + sem)  0  empleados



-- foldl (\sem fun -> fun sem ) 2 [(3+), (*2), (5+)]

-- foldr (\fun sem -> fun sem)  2 [(3+), (*2), (5+)]

type Nombre  = String
type InversionInicial = Int
type Profesionales = [String]

data  Proyecto = Proy {nombre:: Nombre, inversionInicial::  InversionInicial, profesionales:: Profesionales} deriving Show

proyectos :: [Proyecto]
proyectos = [Proy "red social de arte"  20000 ["ing. en sistemas", "contador"], Proy "restaurante" 5000 ["cocinero", "adm. de empresas", "contador"], Proy "ventaChurros" 1000 ["cocinero"] ]

maximoProySegun :: ( Proyecto -> Int) -> [Proyecto] -> Proyecto 
maximoProySegun f (proyecto : proyectos) = foldl (maximoSegun f)  proyecto  proyectos


maximoSegun :: ( Proyecto -> Int) -> Proyecto -> Proyecto -> Proyecto
maximoSegun f unProyecto otroProyecto | f unProyecto > f otroProyecto = unProyecto
                                      | otherwise = otroProyecto

{- a
ghci> maximoProySegun inversionInicial proyectos
Proy {nombre = "red social de arte", inversionInicial = 20000, profesionales = ["ing. en sistemas","contador"]}

-}

{- b
ghci> maximoProySegun (length.profesionales) proyectos
Proy {nombre = "restaurante", inversionInicial = 5000, profesionales = ["cocinero","adm. de empresas","contador"]}
-}

{- c
ghci> maximoProySegun (length.words.nombre) proyectos
Proy {nombre = "red social de arte", inversionInicial = 20000, profesionales = ["ing. en sistemas","contador"]}
-}

maximoProySegun' :: ( Proyecto -> Int) -> [Proyecto] -> Proyecto 
maximoProySegun' f (proyecto : proyectos) = foldr (maximoSegun f)  proyecto  proyectos

maximoProySegun'' :: ( Proyecto -> Int) -> [Proyecto] -> Proyecto 
maximoProySegun'' f proyectos = foldl1 (maximoSegun f) proyectos

