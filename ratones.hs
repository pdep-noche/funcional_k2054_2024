
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

