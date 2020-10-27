module Practica1
--Logica Computacional 2020-1.
--Practica 01
--Pineda Lopez Milton Geovany
where





--Ejercicio 1 ---------------------------------------------------------
--Elige alguna de las siguientes implementaciones para variable en 
--haskell, tambien puedes crear tu propia implementacion.

--Variables representadas como cadenas

--Variables representadas como enteros
type Variable   = String

--Variables definidas recursivamente




--Ejercicio 2 ---------------------------------------------------------
--Elige alguna de las siguientes implementaciones para valuaciones o 
--modelos en haskell, tambien puedes crear tu propia implementacion.

--Definicion de valuaciones como funciones

--Definicion de valuaciones como conjuntos

--Definicion de valuaciones como listas
type Modelo= [Variable]





--Ejercicio 3 ---------------------------------------------------------
--Implementa una funcion que reciba una formula y regrese el conjunto
--o lista, de las variables usadas en la formula.
toList :: PL -> [Variable]
toList phi = case phi of --Segu la estructura de phi
           -- Caso base
           Bot -> []
           Top -> []
           Var x -> [x]
           -- Casos recursivos
           Imp alpha beta -> []
           Dis alpha beta -> []
           Con alpha beta -> []

--Ejercicio 4 ---------------------------------------------------------
--Utilizando el siguiente tipo de datos para formulas de PL

data PL = Bot           --Constructor para bottom
        | Top           --Constructor para top
        | Var Variable  --Constructor de variables
        | Imp PL PL     --Constructor de implicaciones
        | Dis PL PL     --Constructor de disyunciones
        | Con PL PL     --Constructor de conjunciones
        | Neg PL        --Constructor de negaciones
        deriving (Eq,Show)


--Elige implementar alguna de estas 2 funciones:

--Una funcion que devuelva el numero de apariciones de conjunciones 
--en la formula

numConj :: PL -> Int
numConj phi = case phi of -- Segun la estructura de phi:
            Bot             -> 0 --Si phi=Bot
            Top             -> 0 --Si phi=Top
            Var _           -> 0 --Si phi es una variable 
            Imp alpha beta  -> (numConj alpha)+ numConj(beta)     --Si phi=alpha -> beta
            Dis alpha beta  -> (numConj alpha)+ numConj(beta)     --Si phi=alpha | beta
            Con alpha beta  -> (numConj alpha)+ numConj(beta) + 1 --Si phi=alpha & beta
            Neg alpha       -> numConj alpha  

--numConj (((Var "q") `Con` (Var "p")) `Con` (Var "p"))

--Una funciones que reciba un operador de la logica de PL (ya sea 
--conjunciones,disyunciones) y una formula y regrese el numero de 
--apariciones del operador en la formula

--Crea una instancia de la clase show para el tipo de datos PL




--Ejercicio 5 ---------------------------------------------------------

--Elige implementar alguna de estas 3 funciones:

--1. Implementa el algoritmo quitaImp, que recibe una formula de Logica
--Proposicional y regresa una formula de Logica proposicional sin 
--apariciones del operador implicacion

quitaImp :: PL -> PL
quitaImp phi = case phi of -- Segun la estructura de phi:
            -- Casos base:
            Bot             -> Bot   --Si phi=Bot
            Top             -> Top   --Si phi=Top
            Var a           -> Var a --Si phi es una variable
            -- Casos recursivos:
            Imp alpha beta  -> (Neg(quitaImp alpha)) `Dis` (quitaImp(beta))   --Si phi=alpha -> beta
            Dis alpha beta  -> (quitaImp alpha) `Dis` (quitaImp(beta))        --Si phi=alpha | beta
            Con alpha beta  -> (quitaImp alpha) `Con` quitaImp(beta)          --Si phi=alpha & beta
            Neg alpha       -> (Neg(quitaImp alpha))                          --Si phi= -alpha

 -- quitaImp ((Var "q") `Imp` (Var "p"))

--2. Implementa el algoritmo quitaAnd, que recibe una formula de Logica
--Proposicional y regresa una formula de Logica proposicional sin apari-
--ciones del operador and.

--3. Implementa el algoritmo quitaOr, que recibe una formula de Logica 
--Proposicional y regresa una formula de Logica proposicional sin 
--apariciones del operador or.




--Ejercicio 6 ---------------------------------------------------------

--Elegir alguna de las siguientes implementaciones.

--1. Implementa el algoritmo lNand que recibe una formula de Logica 
--proposicional y regresa una formula en donde solo aparece el 
--operador nand

--2. Implementa el algoritmo lNor que recibe una formula de Logica 
--proposicional y regresa una formula en donde solo aparece el 
--operador nor.





--Ejercicio 7 ---------------------------------------------------------

--Define la funcion mSatisface :: Modelo-> PL -> Bool, que reciba un 
--modelo y una formula de PL y regrese si el modelo satisface o no a 
--la formula.

mSatisface :: Modelo-> PL -> Bool
-- mSatisface m phi implementa "m |= phi".
mSatisface m phi= case phi of -- Segun la estructura de phi:
        -- Casos base:
        Bot             -> False        --m no satisface bottom
        Top             -> True         --m satisface top 
        Var x           -> x `elem` m   --m satisface x sii x pertenece a m
        -- Casos recursivos:
        Imp alpha beta  ->     not(mSatisface m alpha)  -- m no satisface a alpha
                            || (mSatisface m beta)      -- o m satisface a beta
        Dis alpha beta  ->     (mSatisface m alpha)     -- m satisface a alpha
                            || (mSatisface m beta)      -- o m satisface a beta
        Con alpha beta  ->     (mSatisface m alpha)     -- m satisface a alpha
                            && (mSatisface m beta)      -- y m satisface a beta
        Neg alpha       ->  not(mSatisface m alpha)     -- m no satisface a alpha