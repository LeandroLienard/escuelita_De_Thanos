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
    ,planeta :: Planeta
} deriving (Show, Eq)

data Guantelete = UnGuantelete{ 
    material :: String
    ,gemas :: [Gema]
} deriving (Show,Eq)

type Universo = [Personaje]

{-Punto 1: (2 puntos)
Modelar Personaje, Guantelete y Universo como tipos de dato e implementar el chasquido de un universo.-}

--COmo lo hciimos con villitas (sin leer enunciado)
    
chasquidoDeUniverso :: Universo -> Universo
chasquidoDeUniverso universo = take (nuevoCantPoblacion universo) universo

--COmo habia q hacerlo
chasquear :: Guantelete -> Universo -> Universo
chasquear guantelete universo 
    | puedeUsarse guantelete = take (nuevoCantPoblacion universo) universo
    | otherwise = universo 


nuevoCantPoblacion :: Universo->Number
nuevoCantPoblacion  =  flip div 2 .length

puedeUsarse:: Guantelete -> Bool
puedeUsarse guantelete = ((==6).length.gemas) guantelete && ((=="uru").material) guantelete
-- DECLARANDO PERSONAJES

ironMan = UnPersonaje {
    edad = 35
    ,nombre = "tony stark"
    ,habilidades = ["superTrake","rayos laser"]
    ,energia = 26
    ,planeta = "Tierra"} 

thor = UnPersonaje {
     edad = 35
    ,nombre = "Thor"
    ,habilidades = ["superTraje","martillo fuerte"]
    ,energia = 55
    ,planeta = "Asgard"} 

blackWidow = UnPersonaje {
     edad = 1000
    ,nombre = "blackWidow"
    ,habilidades = ["electrocuta","salta ", "seduce"]
    ,energia = 3
    ,planeta = "Tierra"} 

{-Punto 2: (3 puntos) Resolver utilizando únicamente orden superior.
Saber si un universo es apto para péndex, que ocurre si alguno de los personajes que lo integran tienen menos de 45 años.
Saber la energía total de un universo que es la sumatoria de todas las energías de sus integrantes que tienen más de una habilidad.-}
type Criterio = Personaje->Bool
-- a
esAptoParaPendex :: Universo->Bool
esAptoParaPendex  = any esPendex  -- pasamos esPendex como paramtero (Orden Superior)

esPendex :: Criterio
esPendex = (< 45).edad 
-- esPendex unPersonaje = (< 45).edad $ unPersonaje  FORMA DE VILLITA 
-- b 
type Energia = Number

energiaTotal :: Universo->Energia
energiaTotal = sum . map energia . integrantesHabilidosos 

integrantesHabilidosos :: Universo->Universo
integrantesHabilidosos = filter (tieneMasDeNHabilidades 1)

tieneMasDeNHabilidades :: Number->Criterio
tieneMasDeNHabilidades n  =  (> n).length.habilidades

--esHabilidoso :: Criterio
--esHabilidoso = (> 1).length.habilidades

--Saber la energía total de un universo que es la sumatoria de todas las energías de sus integrantes que tienen más de una habilidad.

{-Segunda parteA su vez, aunque el guantelete no se encuentre completo con las 6 gemas, 
el poseedor puede utilizar el poder del mismo contra un enemigo,
 es decir que puede aplicar el poder de cada gema sobre el enemigo. -}

type Gema = Personaje->Personaje
aplicarPoder :: Personaje->Gema->Personaje
aplicarPoder aPersonaje gema = gema aPersonaje

--La mente que tiene la habilidad de debilitar la energía de un usuario en un valor dado.
mente :: Number->Gema
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
eliminarHabilidad hab  = filter (/= hab) . habilidades  



type Planeta = String

espacio :: Planeta->Gema
espacio  nuevoPlaneta   = debilitarEnergia 20 . transportarAPlaneta nuevoPlaneta      --SE ESTA APLICANDO PARCIALMENTE APERSONAJE
 --(debilitarEnergia 20 . transportarAPlaneta nuevoPlaneta) aPersonaje == debilitarEnergia 20 . transportarAPlaneta nuevoPlaneta)

transportarAPlaneta :: String->Personaje->Personaje
transportarAPlaneta nuevoPlaneta aPersonaje = aPersonaje {planeta = nuevoPlaneta}

--El poder deja sin energía al rival y si tiene 2 habilidades o menos se las quita (en caso contrario no le saca ninguna habilidad
poder :: Gema
poder = dejarSinEnergia . eliminarHabilidadSiEsMenorA2

dejarSinEnergia :: Personaje->Personaje
dejarSinEnergia aPersonaje = aPersonaje {energia = 0}

eliminarHabilidadSiEsMenorA2 :: Personaje->Personaje
eliminarHabilidadSiEsMenorA2 aPersonaje
    |tieneMasDeNHabilidades 3 aPersonaje  = aPersonaje -- tiene mas 3 habs
    |otherwise = dejarSinHabilidades aPersonaje -- tiene 2 habs o menos 

dejarSinHabilidades :: Personaje ->Personaje
dejarSinHabilidades aPersonaje = aPersonaje {habilidades = []} 

{-El tiempo que reduce a la mitad la edad de su oponente pero como no está 
permitido pelear con menores, no puede dejar la edad del oponente con menos de 18 años. 
Considerar la mitad entera,  También resta 50 puntos de energía.-}
tiempo :: Gema
tiempo = debilitarEnergia 50 .  reducirEdadAMitad  

reducirEdadAMitad :: Personaje->Personaje
reducirEdadAMitad aPersonaje = aPersonaje{edad = max 18 (div (edad aPersonaje) 2)}

--type Indice = Personaje->Number

{-La gema loca que permite manipular el poder de una gema y la ejecuta 2 veces contra un rival. -}
gemaLoca :: Gema->Gema
gemaLoca gema = aplicarDosVecesPoder gema 

aplicarDosVecesPoder :: Gema->Personaje->Personaje
aplicarDosVecesPoder gema = flip aplicarPoder gema . flip aplicarPoder gema

{-Punto 4: (1 punto) Dar un ejemplo de un guantelete de goma con las gemas tiempo, 
alma que quita la habilidad de “usar Mjolnir” y la gema loca que manipula el poder del alma 
tratando de eliminar la “programación en Haskell”.
-}-- Puntp 4 
guanteleteDeGoma = UnGuantelete{
    material = "guanteleteDeGoma"
    ,gemas = [tiempo , alma "usar Mjolnir" ,gemaLoca (alma "programación en Haskell")]
}

{- Punto 5: (2 puntos). No se puede utilizar recursividad.
 Generar la función utilizar
  que dado una lista de gemas y un enemigo ejecuta el poder de cada una de las gemas que
   lo componen contra el personaje dado. Indicar cómo se produce el “efecto de lado” sobre la víctima.-}

type Enemigo = Personaje

utilizar :: [Gema]->Enemigo->Enemigo
utilizar listaGemas enemigo = foldl aplicarPoder enemigo  listaGemas

{-Punto 6: (2 puntos). Resolver utilizando recursividad. Definir la función gemaMasPoderosa 
que dado un guantelete y una persona obtiene la gema del infinito que produce la pérdida más grande de
 energía sobre la víctima. -}

gemaMasPoderosa :: Personaje->Guantelete->Gema
gemaMasPoderosa aPersonaje guantelete = foldl1 (compararSegunEnergia aPersonaje) (gemas guantelete)

--fuenteGanadora :: Ord a=> Persona->(Persona->a)->[Fuente]->Fuente
--fuenteGanadora aPersonaje criterio  = foldl1 (compararFuentesSegun criterio aPersona)   --aplicacion parcial

compararSegunEnergia :: Personaje->Gema->Gema->Gema
compararSegunEnergia aPersonaje aGema bGema 
   | energia (aplicarPoder aPersonaje aGema) < energia (aplicarPoder aPersonaje bGema) = aGema
   | otherwise = bGema

--UTilizando recursividadd

--OPCION A :SEGUN rtas
gemaMasPoderosa2 :: Personaje -> Guantelete -> Gema
gemaMasPoderosa2 personaje guantelte = gemaMasPoderosaDe personaje $ gemas guantelte

gemaMasPoderosaDe :: Personaje -> [Gema] -> Gema
gemaMasPoderosaDe _ [gema] = gema
gemaMasPoderosaDe personaje (gema1:gema2:gemas) 
    | (energia.gema1) personaje < (energia.gema2) personaje = gemaMasPoderosaDe personaje (gema1:gemas)
    | otherwise = gemaMasPoderosaDe personaje (gema2:gemas)

-- OPCION B:SEgun RUSo

gemaMasPoderosa3 :: Personaje->Guantelete->Gema
gemaMasPoderosa3 aPersonaje guantelete = gemaDeMayorPoder aPersonaje  (gemas guantelete)

gemaDeMayorPoder :: Personaje->[Gema]->Gema
gemaDeMayorPoder _ [gema] = gema

gemaDeMayorPoder aPersonaje (gema1:gema2:gemas) 
   | (energia.aplicarPoder aPersonaje) gema1  > (energia.aplicarPoder aPersonaje) gema2 = gemaDeMayorPoder aPersonaje (gema2:gemas)
   | otherwise = gemaDeMayorPoder aPersonaje (gema1:gemas) 



{-Punto 7: (1 punto) Dada la función generadora de gemas y un guantelete de locos:
-}  
infinitasGemas :: Gema -> [Gema]
infinitasGemas gema = gema:(infinitasGemas gema)

guanteleteDeLocos :: Guantelete
guanteleteDeLocos = UnGuantelete "vesconite" (infinitasGemas tiempo) -- guantelete con infinitasGemas de tiempo

usoLasTresPrimerasGemas :: Guantelete -> Personaje -> Personaje
usoLasTresPrimerasGemas guantelete = (utilizar . take 3. gemas) guantelete

{-
Justifique si se puede ejecutar, relacionándolo con conceptos vistos en la cursada:

gemaMasPoderosa punisher guanteleteDeLocos        -- NO puede JAMAS TERMINA DE EJECUTAR

usoLasTresPrimerasGemas guanteleteDeLocos punisher     SI PUEDE , UTILIZA LAS 3 PRIMERAS

-}