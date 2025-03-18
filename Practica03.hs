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


-- Ejercicio 1
fnn :: Prop -> Prop
fnn = undefined

-- Ejercicio 2
fnc :: Prop -> Prop
fnc = undefined


-- 3.3 Resolución Binaria

-- Sinónimo Literal
type Literal = Prop

-- Sinónimo Cláusula
type Clausula = [Literal]


-- Ejercicios

-- Ejercicio 1
clausulas :: Prop -> [Clausula]
clausulas = undefined

-- Ejercicio 2
resolucion :: Clausula -> Clausula -> Clausula
resolucion = undefined


-- 3.4 Algoritmo de saturación

-- Ejercicios

-- Ejercicio 1
hayResolvente :: Clausula -> Clausula -> Bool
hayResolvente = undefined

-- Ejercicio 2
saturacion :: Prop -> Bool
saturacion = undefined