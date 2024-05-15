cantidadDeElementos ::  [(Integer, Integer)] -> Integer
cantidadDeElementos lista = foldl (\sem _ -> sem + 1)  0  lista

cantidadDeElementos' lista = foldr (\_ sem -> sem + 1) 0 lista


masGastador :: [(String, Integer)] -> (String, Integer)
masGastador (cab:lista) = foldl mayorGasto cab  lista


mayorGasto :: (String, Integer) -> (String, Integer) -> (String, Integer)
mayorGasto per otraPer | snd per > snd otraPer = per
                        | otherwise = otraPer


masGastadorFoldr (cab:gasto)  = foldr mayorGasto cab gasto

--ghci> masGastadorFoldr [("juan", 44), ("pedro", 55)]
--("pedro",55)

monto:: [(String, Integer)] -> Integer
monto empleados = foldl (\sem emple -> sem + snd emple) 0 empleados


montoFoldr:: [(String, Integer)] -> Integer
montoFoldr empleados = foldr (\(_, monto) sem -> sem + monto) 0 empleados

{-
    ghci> foldl (\sem fun -> fun sem) 2 [(3+), (*2), (5+)] 
    15
-}

{-
ghci> foldr (\fun sem -> fun sem) 2 [(3+), (2*), (5+)]
17
ghci> foldr ($) 2 [(3+), (2*), (5+)]                  
17
-}

type Nombre  = String
type InversionInicial = Int
type Profesionales = [String]

data  Proyecto = Proy {nombre:: Nombre, inversionInicial::  InversionInicial, profesionales:: Profesionales} deriving Show

proyectos = [Proy "red social de arte"  20000 ["ing. en sistemas", "contador"], Proy "restaurante" 5000 ["cocinero", "adm. de empresas", "contador"],Proy "ventaChurros" 1000 ["cocinero"] ]

maximoProyectoSegun :: [Proyecto] -> (Proyecto -> Int) -> Proyecto
maximoProyectoSegun (proyecto:proyectos) f = foldl (maximoSegun f)   proyecto  proyectos


maximoSegun :: (Proyecto -> Int) -> Proyecto -> Proyecto -> Proyecto
maximoSegun f unProyecto otroProyecto | f unProyecto > f otroProyecto = unProyecto
                                      | otherwise = otroProyecto

{-  a)
ghci> maximoProyectoSegun proyectos inversionInicial
Proy {nombre = "red social de arte", inversionInicial = 20000, 
profesionales = ["ing. en sistemas","contador"]}
-}
 {- b)
 ghci> maximoProyectoSegun proyectos (length.profesionales)
Proy {nombre = "restaurante", inversionInicial = 5000, profesionales = ["cocinero","adm. de empresas","contador"]}
-}

{- c)
ghci> maximoProyectoSegun proyectos (length.words.nombre) 
Proy {nombre = "red social de arte", inversionInicial = 20000, 
profesionales = ["ing. en sistemas","contador"]}
ghci>
-}


maximoProyectoSegunFoldr :: [Proyecto] -> (Proyecto -> Int) -> Proyecto
maximoProyectoSegunFoldr (proyecto:proyectos) f = foldr (maximoSegun f)   proyecto  proyectos

