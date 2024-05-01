pares numeros = [num | num <- numeros , even num]
divisiblePor n numeros = [num | num <- numeros , esDivisiblePor n num]
mayoresA n numeros = [num | num <-numeros , (> n) num]

esDivisiblePor n m =((==0).(` mod ` n))m

find' :: (a -> Bool) -> [a] -> a
find' f lista = (head . filter f) lista


data Politico = Politico {proyectosPresentados :: [String], sueldo :: Integer,  edad :: Int } deriving Show 
politicos = [ Politico ["ser libres", "libre estacionamiento coches politicos", "ley no fumar", "ley 19182"] 20000 81, Politico ["tratar de reconquistar luchas sociales"] 10000 63, Politico ["tolerancia 100 para delitos"] 15500 49 ]


{- ghci> find' ((<50).edad) politicos 
Politico {proyectosPresentados = ["tolerancia 100 para delitos"], sueldo = 15500, edad = 49}

ghci> find' ((>3).length.proyectosPresentados) politicos

ghci>  find' (any((>3).length.words) .proyectosPresentados) politicos

Politico {proyectosPresentados = ["ser libres","libre estacionamiento coches politicos","ley no fumar","ley 19182"], sueldo = 20000, edad = 81}

-}

type Nombre = String
type Notas = [Int]
data Persona = Alumno {nombre :: Nombre, notas :: Notas}

promedioAlumnos :: [Persona] -> [(Nombre, Int)]
promedioAlumnos listaAlumnos = map (\(Alumno nombre notas) -> (nombre, promedio notas))listaAlumnos

promedio :: Notas -> Int
promedio notas = (sum notas) `div` (length notas)

{-ghci> promedioAlumnos [(Alumno "juan" [8,6]), Alumno "maria" [7, 9,4]]
[("juan",7),("maria",6)]
-}

promediosSinAplazos :: [Notas] -> [Int]
promediosSinAplazos listaNotas = map (promedio.filter(>=6)) listaNotas


aprobo :: Persona -> Bool
aprobo alumno = (all(>=6).notas) alumno

{-ghci> aprobo (Alumno "manuel" [8,6,9,7])     
True
-}

aprobaron :: [Persona] -> [String]
aprobaron alumnos = (map nombre . filter aprobo) alumnos

productos :: [String] -> [Int] -> [(String, Int)]
productos nombres precios = zip nombres precios


productos' :: [String] -> [Int] -> [(String, Int)]
productos' nombres precios = zipWith (\nom pre -> (nom, pre))  nombres precios