module Library where
import PdePreludat

--Primera parte
{-
Los enanos de Nidavellir nos han pedido modelar los guanteletes que ellos producen en su herrería.
 Un guantelete está hecho de un material (“hierro”, “uru”, etc.) y sabemos las gemas que posee. 

 También se sabe de los personajes que tienen una edad, una energía, una serie de habilidades 
 (como por ejemplo “usar espada”, “controlar la mente”, etc), su nombre y en qué planeta viven. 

 Los fabricantes determinaron que cuando un guantelete está completo -es decir, tiene las 6 gemas posibles- 
 y su material es “uru”, se tiene la posibilidad de chasquear un universo que contiene a todos sus habitantes 
 y reducir a la mitad la cantidad de dichos personajes. Por ejemplo si tenemos un universo en el cual existen ironMan,
  drStrange, groot y wolverine, solo quedan los dos primeros que son ironMan y drStrange. Si además de los 4 personajes
   estuviera viudaNegra, quedarían también ironMan y drStrange porque se considera la división entera.
-}
-- PUNto 1

data Personaje = UnPersonaje{
    edad :: Number
    ,nombre :: String
    ,habilidades :: [String]
    ,energia :: Energia
    ,planeta :: String
} deriving (Show, Eq)

data Guantelete = Guantelete{ 
    material :: String
    ,gemas ::Number
} deriving (Show,Eq)

type Universo = [Personaje]


{-
    Punto 1: (2 puntos)
Modelar Personaje, Guantelete y Universo como tipos de dato e implementar el chasquido de un universo.
-}

chasquidoDeUniverso :: Universo -> Universo
chasquidoDeUniverso universo = take (nuevoCantPoblacion universo) universo

nuevoCantPoblacion :: Universo->Number
nuevoCantPoblacion  =  (flip div 2 .length)



-- DECLARANDO PERSONAJES

ironMan = UnPersonaje {
    edad = 35
    ,nombre = "tony stark"
    ,habilidades = ["superTrake"]
    ,energia = 26
    ,planeta = "Tierra"
 } 

thor = UnPersonaje {
     edad = 35
    ,nombre = "Thor"
    ,habilidades = ["superTrake"]
    ,energia = 55
    ,planeta = "Asgard"
 } 

blackWidow = UnPersonaje {
     edad = 55
    ,nombre = "blackWidow"
    ,habilidades = ["superTrake"]
    ,energia = 3
    ,planeta = "Tierra"
 } 

{-
Punto 2: (3 puntos) Resolver utilizando únicamente orden superior.
Saber si un universo es apto para péndex, que ocurre si alguno de los personajes que lo integran 
tienen menos de 45 años.
Saber la energía total de un universo que es la sumatoria de todas las energías de sus
 integrantes que tienen más de una habilidad.
-}
type Criterio = Personaje->Bool

-- a
esAptoParaPendex :: Universo->Bool
esAptoParaPendex universo = any esPendex universo -- pasamos esPendex como paramtero (Orden Superior)

esPendex :: Criterio
esPendex = (< 45).edad 
-- esPendex unPersonaje = (< 45).edad $ unPersonaje  FORMA DE VILLITA

-- b 
type Energia = Number

energiaTotal :: Universo->Energia
energiaTotal = sum.map energia.integrantesHabilidosos 

integrantesHabilidosos :: Universo->Universo
integrantesHabilidosos = filter esHabilidoso

esHabilidoso :: Criterio
esHabilidoso = (> 1).length.habilidades

--Saber la energía total de un universo que es la sumatoria de todas las 
--energías de sus integrantes que tienen más de una habilidad.


{-
Segunda parte
A su vez, aunque el guantelete no se encuentre completo con las 6 gemas, 
el poseedor puede utilizar el poder del mismo contra un enemigo,
 es decir que puede aplicar el poder de cada gema sobre el enemigo. 
 Las gemas del infinito fueron originalmente parte de la entidad primordial llamada Némesis,
  un ser todopoderoso del universo anterior quién prefirió terminar su existencia en lugar de vivir
   como la única conciencia en el universo. Al morir, dio paso al universo actual, y el núcleo de 
   su ser reencarnó en las seis gemas: 
-}


