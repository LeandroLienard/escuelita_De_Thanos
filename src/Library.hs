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
    ,gemas ::[Gema]
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
    ,habilidades = ["superTrake","rayos laser"]
    ,energia = 26
    ,planeta = "Tierra"
 } 

thor = UnPersonaje {
     edad = 35
    ,nombre = "Thor"
    ,habilidades = ["superTraje","martillo fuerte"]
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

{-Segunda parte
A su vez, aunque el guantelete no se encuentre completo con las 6 gemas, 
el poseedor puede utilizar el poder del mismo contra un enemigo,
 es decir que puede aplicar el poder de cada gema sobre el enemigo. 
 
-}

type Gema = Personaje->Personaje


--La mente que tiene la habilidad de debilitar la energía de un usuario en un valor dado.
mente ::Number->Gema
mente n  =  debilitarEnergia n 

debilitarEnergia :: Number->Personaje->Personaje
debilitarEnergia n aPersonaje = aPersonaje{energia = energia aPersonaje - n }   

--El alma puede controlar el alma de nuestro oponente permitiéndole eliminar una 
--habilidad en particular si es que la posee. Además le quita 10 puntos de energía.

alma :: String->Gema
alma habilidad = debilitarEnergia 10 . controlarAlma habilidad

controlarAlma :: String -> Personaje -> Personaje
controlarAlma habilidad aPersonaje = aPersonaje {habilidades = eliminarHabilidad habilidad aPersonaje} 
--controlarAlma hab aPersonaje = aPersonaje {habilidades = filter (/= hab) (habilidades aPersonaje)} 

eliminarHabilidad :: String -> Personaje -> [String]
eliminarHabilidad hab   = filter (/= hab) . habilidades  

-- c: EL espacio
espacio :: Universo->Gema
espacio 


{- 

El espacio que permite transportar al rival al planeta x (el que usted decida) y
 resta 20 puntos de energía.

El poder deja sin energía al rival y si tiene 2 habilidades o menos se las quita
 (en caso contrario no le saca ninguna habilidad).

El tiempo que reduce a la mitad la edad de su oponente pero como no está 
permitido pelear con menores, no puede dejar la edad del oponente con menos de 18 años. Considerar la mitad entera, por ej: si el oponente tiene 50 años, le quedarán 25. Si tiene 45, le quedarán 22 (por división entera). Si tiene 30 años, le deben quedar 18 en lugar de 15. También resta 50 puntos de energía.

La gema loca que permite manipular el poder de una gema y la ejecuta 2
 veces contra un rival.
-}
