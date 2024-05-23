import Data.Array (listArray)
import Language.Haskell.TH (AnnLookup)

data Animal= Raton {nombre :: String, edad :: Double, peso :: Double, enfermedades :: [String]} deriving Show
-- Ejemplo de raton
cerebro = Raton "Cerebro" 9.0 0.2 ["brucelosis", "sarampión", "tuberculosis"]
-- Estos son las enfermedades infecciosas
enfermedadesInfecciosas = [ "brucelosis", "tuberculosis"]


modificarNombre :: (String -> String) -> Animal -> Animal
modificarNombre f unAnimal = unAnimal { nombre = (f.nombre)unAnimal}

{-
ghci> modificarNombre (++ " el genio") cerebro
Raton {nombre = "Cerebro el genio", edad = 
9.0, peso = 0.2, enfermedades = ["brucelosis","sarampi\243n","tuberculosis"]}
-}

modificarEdad :: (Double -> Double) -> Animal -> Animal
modificarEdad f unAnimal = unAnimal {edad = (f.edad)unAnimal}

{-
ghci> modificarEdad (2*) cerebro
Raton {nombre = "Cerebro", edad = 18.0, peso = 0.2, enfermedades = ["brucelosis","sarampi\243n","tuberculosis"]}
-}

modificarPeso :: (Double -> Double) -> Animal -> Animal
modificarPeso f animal = animal {peso = (f.peso)animal}

{-
    ghci> modificarPeso (2*) cerebro
Raton {nombre = "Cerebro", edad = 9.0, peso = 0.4, enfermedades = ["brucelosis","sarampi\243n","tuberculosis"]}
-}

modificarEnfermedad :: ([String] ->[String]  ) -> Animal -> Animal
modificarEnfermedad f unAnimal = unAnimal { enfermedades = (f.enfermedades) unAnimal}


{-
ghci> modificarEnfermedad tail cerebro
Raton {nombre = "Cerebro", edad = 9.0, peso = 0.2, enfermedades = ["sarampi\243n","tuberculosis"]}
-}

hierbaBuena :: Animal -> Animal
hierbaBuena raton = modificarEdad sqrt raton

hierbaVerde :: String -> Animal -> Animal
hierbaVerde enfermedad raton = modificarEnfermedad (filter (/= enfermedad))  raton


alcachofa :: Animal -> Animal
alcachofa raton = modificarPeso pierdePeso raton

pierdePeso :: Double -> Double
pierdePeso peso | peso > 2 = peso * 0.9
                | otherwise = peso * 0.95


hierbaMagica :: Animal -> Animal
hierbaMagica unAnimal = (modificarEdad (*0). modificarEnfermedad (const [])) unAnimal


medicamento :: [(Animal -> Animal)] -> Animal -> Animal
medicamento lista animal = foldl  (\unAni unaHierba -> unaHierba unAni)  animal  lista

medicamento' lista animal = foldl (flip ($)) animal lista
{-
    ghci> medicamento [hierbaVerde "tuberculosis", alcachofa, hierbaMagica] cerebro
-}

antiAge :: Animal -> Animal
antiAge  = medicamento(replicate 3 hierbaBuena ++ [alcachofa]) 


reduceFatFast :: Int -> Animal -> Animal
reduceFatFast unaPotencia unAnimal = medicamento ([hierbaVerde "obesidad"] ++ replicate unaPotencia alcachofa) unAnimal


{- ghci> reduceFatFast 3 cerebro 
Raton {nombre = "Cerebro", edad = 9.0, peso = 0.171475, enfermedades = ["brucelosis","sarampi\243n","tuberculosis"]}
-}
{-
ghci> antiAge cerebro
Raton {nombre = "Cerebro", edad = 1.3160740129524924, peso = 0.19, enfermedades = ["brucelosis","sarampi\243n","tuberculosis"]}
-}

hierbaMilagrosa :: Animal -> Animal
hierbaMilagrosa unAnimal = medicamento (map hierbaVerde enfermedadesInfecciosas) unAnimal


cantidadIdeal :: ( Int -> Bool)  -> Int
cantidadIdeal condicion = (head. filter condicion) [1..]


estanMejoresQueNunca :: [Animal]  -> (Animal -> Animal) -> Bool
estanMejoresQueNunca ratones medicamento = all ((<1).peso.medicamento) ratones

{-ghci> estanMejoresQueNunca [cerebro] antiAge 
True-}


experimento :: [Animal] -> Int
experimento animales = cantidadIdeal(estanMejoresQueNunca animales.reduceFatFast)