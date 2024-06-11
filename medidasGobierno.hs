
type Bien = (String,Float)
type Ciudad = [Ciudadano]
data Ciudadano = UnCiudadano {profesion :: String, sueldo :: Float, 
cantidadDeHijos :: Float, bienes :: [Bien] } deriving Show

homero = UnCiudadano "SeguridadNuclear" 9000 3 [("casa",50000), ("deuda",-70000)]
frink = UnCiudadano "Profesor" 12000 1 []
krabappel = UnCiudadano "Profesor" 12000 0 [("casa",35000)]
burns = UnCiudadano "Empresario" 300000 1 [("empresa",1000000),("empresa",500000),("auto",200000)]

springfield = [homero, burns, frink, krabappel] 


diferenciaDePatrimonio :: Ciudad -> Float
diferenciaDePatrimonio ciudad =  (patrimonio.ciudadanoSegun maximoPatrimonio) ciudad - (patrimonio.ciudadanoSegun minimoPatrimonio) ciudad

patrimonio:: Ciudadano  -> Float
patrimonio (UnCiudadano _ sueldo _ bienes) = foldl  (\sem (_, valor)-> sem + valor)  sueldo bienes

ciudadanoSegun :: (  Ciudadano -> Ciudadano -> Ciudadano ) -> Ciudad  -> Ciudadano
ciudadanoSegun f (unCiudadano: ciudadanos ) =  foldl f unCiudadano ciudadanos



maximoPatrimonio:: Ciudadano -> Ciudadano -> Ciudadano
maximoPatrimonio unCiu otroCiu | patrimonio unCiu > patrimonio otroCiu = unCiu
                               | otherwise = otroCiu



minimoPatrimonio :: Ciudadano -> Ciudadano -> Ciudadano
minimoPatrimonio unCiu otroCiu | patrimonio unCiu < patrimonio otroCiu = unCiu
                               | otherwise = otroCiu


{- ghci> diferenciaDePatrimonio springfield
2011000.0
-}


tieneAutoAltaGama :: Ciudadano  -> Bool
tieneAutoAltaGama ciudadano = any autoAltaGama . bienes $ ciudadano


autoAltaGama :: Bien -> Bool
autoAltaGama ("auto", monto) = monto > 100000
autoAltaGama _ = False

type Medida = Ciudadano -> Ciudadano

auh :: Medida
auh ciudadano = aplicarMedidaSegun (patrimonio ciudadano < 0) (modificarSueldo ((incremento.cantidadDeHijos) ciudadano))  ciudadano

incremento :: Float -> Float
incremento hijos = hijos *1000


modificarSueldo :: Float -> Ciudadano -> Ciudadano
modificarSueldo cantidad ciudadano = ciudadano { sueldo = sueldo ciudadano + cantidad }

aplicarMedidaSegun :: Bool -> ( Ciudadano   -> Ciudadano ) -> Ciudadano  -> Ciudadano
aplicarMedidaSegun condicion f ciudadano | condicion = f ciudadano
                                         | otherwise = ciudadano


{-ghci> auh homero
UnCiudadano {profesion = "SeguridadNuclear", sueldo = 12000.0, cantidadDeHijos = 3.0, bienes = [("casa",50000.0),("deuda",-70000.0)]}
-}

impuestoGanancias :: Float -> Medida
impuestoGanancias minimo ciudadano = aplicarMedidaSegun (sueldo ciudadano > minimo) (modificarSueldo (diferencia minimo (sueldo ciudadano)))  ciudadano


diferencia :: Float -> Float -> Float
diferencia minimo sueldo = (minimo - sueldo) * 0.3

{-
ghci> impuestoGanancias 100000 burns
UnCiudadano {profesion = "Empresario", sueldo = 240000.0, cantidadDeHijos = 1.0, bienes = [("empresa",1000000.0),("empresa",500000.0),("auto",200000.0)]}
-}


impuestoAltaGama :: Medida
impuestoAltaGama ciudadano = aplicarMedidaSegun (tieneAutoAltaGama ciudadano) (modificarSueldo ((impuesto.bienes)ciudadano)) ciudadano

impuesto :: [Bien] ->Float
impuesto bienes = (*(-0.1)).snd.head.filter autoAltaGama $ bienes

{-ghci> impuestoAltaGama burns
UnCiudadano {profesion = "Empresario", sueldo = 280000.0, cantidadDeHio 
s = 1.0, bienes = [("empresa",1000000.0),("empresa",500000.0),("auto",200000.0)]}
-}

negociarSueldoProfesion :: String  -> Float ->  Medida
negociarSueldoProfesion unaProfesion porcentaje ciudadano = aplicarMedidaSegun ((== unaProfesion).profesion $ ciudadano) (modificarSueldo (aumento porcentaje (sueldo ciudadano))) ciudadano

aumento :: Float -> Float -> Float
aumento porcentaje sueldo = (sueldo * porcentaje)/100


{- ghci> negociarSueldoProfesion "Profesor" 30 frink
UnCiudadano {profesion = "Profesor", sueldo = 15600.0, cantidadDeHijos = 1.0, bienes = []}
-}

data Gobierno = UnGobierno {años :: [Float], medidas :: [Ciudadano->Ciudadano ]}

gobiernoA :: Gobierno
gobiernoA = UnGobierno [1999..2003] [impuestoGanancias 30000, negociarSueldoProfesion "Profesor" 10, negociarSueldoProfesion "Empresario" 40, impuestoAltaGama, auh]

gobiernoB :: Gobierno
gobiernoB = UnGobierno [2004..2008] [impuestoGanancias 40000, negociarSueldoProfesion "Profesor" 30, negociarSueldoProfesion "Camionero" 40 ]


gobernarUnAño :: Gobierno -> Ciudad -> Ciudad
gobernarUnAño gobierno ciudad = map (aplicarMedidas gobierno)  ciudad


aplicarMedidas :: Gobierno -> Ciudadano -> Ciudadano
aplicarMedidas gobierno unCiudadano = foldl (flip ($))  unCiudadano (medidas gobierno)


gobernarPeriodoCompleto :: Gobierno -> Ciudad -> Ciudad
gobernarPeriodoCompleto unGobierno ciudad  = foldl (\unaCiudad _ -> gobernarUnAño unGobierno unaCiudad) ciudad.  años $ unGobierno

distribuyoRiqueza :: Gobierno -> Ciudad -> Bool
distribuyoRiqueza gobierno ciudad = diferenciaDePatrimonio ciudad > (diferenciaDePatrimonio.gobernarPeriodoCompleto gobierno) ciudad

{-ghci> distribuyoRiqueza gobiernoA springfield
True
-}

kane :: Ciudadano
kane = UnCiudadano "Empresario" 100000 0 infinitosTrineos 

infinitosTrineos :: [(String, Float)]
infinitosTrineos = [("Rosebud", valor)| valor <- [5,10..] ]