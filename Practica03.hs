module Practica03 where

-- Práctica 03: Resolución Binaria

-- 3.1 Lógica Proposicional

-- Tipo de dato Prop
data Prop = 
    Var String |
    Cons Bool |
    Not Prop |
    And Prop Prop |
    Or Prop Prop |
    Impl Prop Prop |
    Syss Prop Prop 
    deriving (Eq)

-- Imprimir el tipo de dato Prop
instance Show Prop where
    show (Cons True) = "Verdadero"
    show (Cons False) = "Falso"
    show (Var p) = p
    show (Not p) = "¬" ++ show p 
    show (Or p q) = "(" ++ show p ++ " ∨ " ++ show q ++ ")"
    show (And p q) = "(" ++ show p ++ " ∧ " ++ show q ++ ")"
    show (Impl p q) = "(" ++ show p ++ " → " ++ show q ++ ")"
    show (Syss p q) = "(" ++ show p ++ " ↔ " ++ show q ++ ")"


-- 3.2 Formas Normales

distribuir :: Prop -> Prop
distribuir (Or (And p q) r) = distribuir (And (Or p r) (Or q r))
distribuir (Or p (And q r)) = distribuir (And (Or p q) (Or p r))
distribuir (Or p q) = Or (distribuir p) (distribuir q)
distribuir (And p q) = And (distribuir p) (distribuir q)
distribuir p = p

negar :: Prop -> Prop
negar (Var p) = Not (Var p)
negar (Cons f) = (Cons (not f))
negar (Not f) = f
negar (And f1 f2) = (Or (negar f1) (negar f2))
negar (Or f1 f2) = (And (negar f1) (negar f2))
negar (Impl f1 f2) = (And f1 (negar f2))
negar (Syss f1 f2) = negar(And (Impl f1 f2) (Impl f2 f1))
-- Ejercicio 1
fnn :: Prop -> Prop
fnn (Var p) = Var p
fnn (Cons f) = Cons f
fnn (Not f) = negar(fnn(f))
fnn (And f1 f2) = (And (fnn f1) (fnn f2))
fnn (Or f1 f2) = (Or (fnn f1) (fnn f2))
fnn (Impl f1 f2) = (Or (negar(fnn f1)) (fnn f2))
fnn (Syss f1 f2) = (And (fnn (Impl f1 f2)) (fnn (Impl f2 f1)))

-- Ejercicio 2
fnc :: Prop -> Prop
fnc (Var p) = fnn (Var p)
fnc (Cons f) = fnn (Cons f)
fnc (Not f) = fnn (Not f)
fnc (And f1 f2) = And (fnc f1) (fnc f2)
fnc (Or f1 f2) = distribuir (Or (fnc f1) (fnc f2))
fnc (Impl f1 f2) = distribuir(fnn (Impl f1 f2))
fnc (Syss f1 f2) = distribuir(fnn (Syss f1 f2))


-- 3.3 Resolución Binaria

-- Sinónimo Literal
type Literal = Prop

-- Sinónimo Cláusula
type Clausula = [Literal]


-- Ejercicios

-- Ejercicio 1
clausulas :: Prop -> [Clausula]
clausulas (And p q) = clausulas p ++ clausulas q
clausular (Or p q) = [extraerLiterales (Or p q)]
clausulas p = [[p]]

-- Funcion auxiliar para extraer literales de la disyuncion
extraerLiterales :: Prop -> Clausula
extraerLiterales (Or p q) = extraerLiterales p ++ extraerLiterales q
extraerLiterales p = [p]

-- Ejercicio 2
-- resolucion binaria para dos clausulas
resolucion :: Clausula -> Clausula -> Clausula
resolucion c1 c2 = [l | l <- c1, not (elem (negarLiteral l) c2 || notElem l c2] ++ <-c2, not (elem (negarLiteral l) c1)]

-- Funcion auxiliar para negar un literal
negarLiteral :: Literal -> Literal
negarLiteral (Not p) = p
negarLiteral p = Not p

-- 3.4 Algoritmo de saturación

-- Ejercicios

-- Ejercicio 1
hayResolvente :: Clausula -> Clausula -> Bool
hayResolvente = undefined

-- Ejercicio 2
saturacion :: Prop -> Bool
saturacion = undefined