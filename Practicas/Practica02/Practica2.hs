module Practica2
-- Logica Computacional 2020-1.
-- Practica 02
-- Pineda Lopez Milton Geovany
-- Garcia Zarraga Angelica Lizbeth 
where

type Variable   = Int

type Modelo= [Variable]

data PL = Bot           --Constructor para bottom
        | Top           --Constructor para top
        | Var Variable  --Constructor de variables
        | Imp PL PL     --Constructor de implicaciones
        | Dis PL PL     --Constructor de disyunciones
        | Con PL PL     --Constructor de conjunciones
        | Neg PL        --Constructor de negaciones
        deriving (Eq,Show)

--Formulas
f1:: PL
f1= ((Var 1) `Con` (Var 2))      

f2:: PL
f2= ((Var 1) `Dis` (Var 2)) 

f3:: PL
f3= ((Neg $ Var 3) `Con` (Var 4)) 

f4:: PL
f4= ((Neg $ Var 3) `Dis` (Neg $ Var 4)) 

f5:: PL
f5= (f1 `Dis` f3)

f6:: PL
f6= (f2 `Dis` f4)

f7:: PL
f7= Dis f2 (Neg $ Var 1)     


-- Ejercicio 1
--
-- Usando el siguiente codigo de ejemplo que decide si una formula que esta en 
-- CNF pertenece a VAL, realiza una funcion que decida si una formula en DNF
-- esta en SAT.

-- Si phi en PL es una disyuncion de literales,
-- entonces disLit2ListLit transforma phi en una lista de literales.
-- Ejemplos: (x3 | !x4) --> [x3,!x4], Bot ---> []
disLit2ListLit :: PL -> [PL] 
disLit2ListLit phi = case phi of
    Bot         -> [] 
    Var x       -> [Var x]       
    Neg (Var x) -> [Neg (Var x)] 
    (Dis alpha beta)    -> (disLit2ListLit alpha) ++ (disLit2ListLit beta) 
    _                   -> error $ 
            "disLit2ListLit: phi no es una disyuncion de literales, phi="++(show phi)


-- Dado un literal l en PL, litComp calcula el literal complementario de l.
litComp :: PL -> PL
litComp phi= case phi of
                Var x         -> Neg (Var x)
                Neg (Var x)   -> Var x
                _ -> error $ "litComp: phi no es literal, phi="++(show phi)    


-- Dada una clausula de PL, representada por una lista de literales ll,
-- clausulaVal determina si ll es una clausula valida.
-- ll es valida sii ll tiene al menos dos literales complementarias.
clausulaVal :: [PL] -> Bool
clausulaVal ll= case ll of
                    []      -> False
                    (l:ls)  -> (litComp l) `elem` ll || clausulaVal ls


-- Dada phi en PL, cnf2LListLit transforma phi a una formula phi' en CNF,
-- donde phi' esta representada como una lista de lista de literales.
-- Ejemplos: (x1 | x2) & (x3 | !x4) ---> [[x1,x2], [x3,!x4]], Top ---> []
cnf2LListLit :: PL -> [[PL]] 
cnf2LListLit phi = case phi of
    Top              -> [] 
    Var x       -> [[Var x]]                    
    Neg (Var x) -> [[Neg (Var x)]]              
    (Dis _ _)   -> [disLit2ListLit phi]        
    (Con alpha beta) -> (cnf2LListLit alpha) ++ (cnf2LListLit beta)
    _                -> error $ "cnf2LListLit: phi no esta en CNF, phi="++(show phi)


-- Dada phi en CNF, representada como una lista de listas de literales lc,
-- clauListTrue determina si todas las clausulas de lc son validas.
-- Es decir clauListTrue determina si todos los elementos de lc son clausulas validas.
clauListVal :: [[PL]] -> Bool
clauListVal lc = case lc of
                    []      -> True
                    (c:cs)  -> clausulaVal c && clauListVal cs


-- Dada phi en PL, decide si phi pertenece, o no, a Val:={phi in PL | forall m: m | = phi}.
-- Esto se hace transformando primero phi a una formula en CNF representada mediante una lista
-- y luego aplicando clauListVal a dicha lista.
decideCNFenVAL :: PL -> Bool
decideCNFenVAL phi = clauListVal (cnf2LListLit phi) 


-- Tests:
-- decideCNFenVAL f1
-- decideCNFenVAL f2
-- decideCNFenVAL f3
-- decideCNFenVAL f7


-- Funcion que decide si una formula en DNF esta en SAT.


-- Si phi en PL es una conjuncion de literales,
-- entonces conLit2ListLit transforma phi en una lista de literales.
-- Ejemplos: (x3 | !x4) --> [x3,!x4], Bot ---> []  
conLit2ListLit :: PL -> [PL] 
conLit2ListLit phi = case phi of
    Bot         -> [] 
    Var x       -> [Var x]       
    Neg (Var x) -> [Neg (Var x)] 
    (Con alpha beta)    -> (conLit2ListLit alpha) ++ (conLit2ListLit beta) 
    _                   -> error $ 
            "conLit2ListLit: phi no es una conjuncion de literales, phi="++(show phi)


-- Dada una clausula de PL, representada por una lista de literales ll,
-- clausulaValDNF determina si ll es una clausula valida.
-- ll es valida sii ll no tiene dos literales complementarios.
clausulaValDNF :: [PL] -> Bool
clausulaValDNF ll= case ll of
                    []      -> True
                    (l:ls)  -> (litComp l) `notElem` ll && clausulaValDNF ls


-- Dada phi en PL, dnf2LListLit transforma phi a una formula phi' en DNF,
-- donde phi' esta representada como una lista de lista de literales.
-- Ejemplos: (x1 | x2) & (x3 | !x4) ---> [[x1,x2], [x3,!x4]], Top ---> []
dnf2LListLit :: PL -> [[PL]] 
dnf2LListLit phi = case phi of
    Top              -> [] 
    Var x       -> [[Var x]]                    
    Neg (Var x) -> [[Neg (Var x)]]             
    (Con _ _)   -> [conLit2ListLit phi]         
    (Dis alpha beta) -> (dnf2LListLit alpha) ++ (dnf2LListLit beta)
    _                -> error $ "dnf2LListLit: phi no esta en DNF, phi="++(show phi)


-- Dada phi en DNF, representada como una lista de listas de literales lc,
-- clauListValDNF determina si todas las clausulas de lc son validas.
-- Es decir clauListValDNF determina si todos los elementos de lc son clausulas validas.
clauListValDNF :: [[PL]] -> Bool
clauListValDNF lc = case lc of
                    []      -> False
                    (c:cs)  -> clausulaValDNF c || clauListValDNF cs


decideDNFenSAT :: PL -> Bool
decideDNFenSAT phi = clauListValDNF (dnf2LListLit phi)


-- Tests:
-- decideDNFenSAT f1
-- decideDNFenSAT f2
-- decideDNFenSAT f3
-- decideDNFenSAT f4
-- decideDNFenSAT f5
-- decideDNFenSAT f6
