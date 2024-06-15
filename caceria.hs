import Text.Show.Functions


data Personaje = Personaje {experiencia :: Float, fuerzaBasica :: Float, elemento :: Elemento} deriving Show

type Elemento = Float -> Float
espadaOxidada:: Elemento
espadaOxidada = (1.2*)
katanaFilosa :: Elemento
katanaFilosa = (10+).(0.9*)
sableLambdico :: Float -> Elemento
sableLambdico cm = ((1+cm/100)*)
redParadigmatica = sqrt
baculoDuplicador x= x* 2
espadaMaldita = espadaOxidada.sableLambdico 89


--nivel :: Personaje -> Float
nivel (Personaje experiencia _ _) = ceiling(experiencia^2/(experiencia + 1))

capacidad :: Personaje -> Float
capacidad (Personaje _ fuerza elemento) = elemento fuerza

julia :: Personaje
julia = Personaje 20 90 baculoDuplicador

type Alquimista = Personaje -> Personaje

aprendiz :: Alquimista
aprendiz personaje =alterarElemento (2*) personaje

alterarElemento :: Elemento -> Alquimista
alterarElemento f personaje = personaje {elemento = f.elemento personaje}


maestroAlquimista :: Int -> Alquimista
maestroAlquimista años personaje = alterarElemento (extraPorAntiquedad años). aprendiz $ personaje

--extraPorAntiquedad  :: Int -> Float
extraPorAntiquedad 0 = id
extraPorAntiquedad años = (*1.1). extraPorAntiquedad (años -1)

estafador :: Alquimista
estafador personaje = personaje { elemento = id}


inventado :: Alquimista
inventado personaje = personaje { elemento = (\nro -> (nro*). experiencia $ personaje)}


capacidadSuperiorA :: Float -> Personaje -> [Alquimista] -> [Alquimista]
capacidadSuperiorA valor personaje alquimistas = filter (tieneCapacidasSuperiorA valor personaje ) alquimistas

tieneCapacidasSuperiorA :: Float-> Personaje -> Alquimista -> Bool
tieneCapacidasSuperiorA valor personaje alquimista = (valor <). capacidad. alquimista $ personaje 

todosConvienen :: Personaje -> [Alquimista] -> Bool
todosConvienen personaje alquimistas = all (tieneCapacidasSuperiorA (capacidad personaje) personaje)   alquimistas


{-
ghci> todosConvienen julia [aprendiz,  inventado]
True
-}


data Monstruo = Monstruo {especie :: String, resistencia :: Float,  habilidades :: [Habilidad]}

type Habilidad = (String, String)

tipo = snd

esAgresivo :: Monstruo -> Bool
esAgresivo monstruo  = (not.especieInofensiva.especie) monstruo && (tieneMasHabilidadesOfensivas.habilidades) monstruo && ((>0).resistencia) monstruo  

especieInofensiva:: String -> Bool
especieInofensiva especie = elem especie ["animal", "chocobo"]

tieneMasHabilidadesOfensivas :: [Habilidad] -> Bool
tieneMasHabilidadesOfensivas habilidades = (length.filter(esOfensiva.tipo)) habilidades > div (length habilidades) 2


esOfensiva:: String -> Bool
esOfensiva "magica" = True
esOfensiva "fisica" = True
esOfensiva _ = False


modificarExperiencia :: Float -> Personaje -> Personaje
modificarExperiencia valor personaje = personaje { experiencia= experiencia personaje  + valor}

leGana :: Personaje -> Monstruo -> Bool
leGana personaje monstruo = capacidad personaje > resistencia monstruo

estadoPersonaje :: Personaje -> [Monstruo] -> Personaje
estadoPersonaje personaje monstruos = foldl pelear personaje (filter esAgresivo monstruos)

pelear :: Personaje -> Monstruo -> Personaje
pelear personaje monstruo | leGana personaje monstruo = modificarExperiencia 100 personaje
                          | otherwise =  (alterarElemento (*0.9) . modificarExperiencia (-50)) personaje

hayMonstruoInvencible :: Personaje -> Alquimista -> [Monstruo] -> Bool
hayMonstruoInvencible personaje alquimista monstruos = any ( not.leGana (alquimista personaje)) monstruos 

