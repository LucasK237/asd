module Library where
import PdePreludat

---PUNTO 1---

type Requisito = Persona -> Bool

data Criatura = Criatura{
    peligrosidad :: Number,
    requisito :: Requisito
}deriving (Show,Eq)

data Persona = Persona{
    edad :: Number,
    items :: [String],
    experiencia :: Number
}deriving(Show,Eq)

juan :: Persona
juan = Persona 15 ["soplador de hojas"] 20

siempreDetras :: Criatura
siempreDetras = Criatura 0 imposibleDeDeshacer

gnomos :: Number -> Criatura
gnomos cantidadDeGnomos = Criatura (2 ^ cantidadDeGnomos) personaPoseeSoplador

fantasma :: Number -> Requisito -> Criatura
fantasma categoria condicion = Criatura (categoria * 20) condicion

imposibleDeDeshacer :: Persona -> Bool
imposibleDeDeshacer _ = False

personaPoseeSoplador :: Persona -> Bool
personaPoseeSoplador unaPersona = elem "soplador de hojas" . items $ unaPersona

---PUNTO 2----

cambiarExperiencia :: (Number -> Number) -> Persona -> Persona
cambiarExperiencia funcion unaPersona = unaPersona{experiencia = funcion . experiencia $ unaPersona}

seDeshace :: Persona -> Criatura -> Bool
seDeshace unaPersona unaCriatura = requisito unaCriatura $ unaPersona

enfrentar :: Persona -> Criatura -> Persona
enfrentar unaPersona unaCriatura | seDeshace unaPersona unaCriatura = cambiarExperiencia (+ (peligrosidad $ unaCriatura)) unaPersona
                                 | otherwise = cambiarExperiencia (+1) unaPersona


---PUNTO 3---

grupoDeCriaturas :: [Criatura]
grupoDeCriaturas = [siempreDetras, gnomos 10]

enfrentarGrupoDeCriaturas :: Persona -> [Criatura] -> Number
enfrentarGrupoDeCriaturas unaPersona criaturas = experiencia (foldl enfrentar unaPersona criaturas)

sinRepetidos [] = []
sinRepetidos (x:xs) | elem x xs = sinRepetidos xs
                    | otherwise = x : sinRepetidos xs

sumarLista [] = 0
sumarLista (x:xs) = x + sumarLista xs

infinitosUnos = 1 : infinitosUnos

longitud [] = 0
longitud (x:xs) = 1 + longitud xs

fibonacci 0 = 0
fibonacci numero | numero == 1 = 1
                 | numero == 2 = 1
                 | otherwise = fibonacci (numero - 1) + fibonacci (numero - 2)

pertenece [] _ = False
pertenece (x:xs) numero | x == numero = True
                        | otherwise = pertenece xs numero
                        
interseccion [] [] = []                        
interseccion (x:xs) (y:ys) | x == y = x : interseccion x ys
                           | 