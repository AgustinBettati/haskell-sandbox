 Para correr abrir ghci
 :l ejercicios.hs
 y ahi podes llamar a las funciones
 
 -- Funciones 
doubleMe x = x * x

mayor x y = if x > y then x else y

[1,2,3] ++ [4,5,6]

[1..5]  

['a'..'d']  te da "abcd"


take 10 ( cycle [1,2,3] )  se tiene una lista infinita 
                            pero solo se toma lo que necesita


[2 * x | x <- [1..10]] te da [2,4,6,8,10,12,14,16,18,20]

[(valor a obtener) | x <- [1,2..], x <= 10]

[x * 2 | x <- list, x < 5]

map (\x -> x * 2) (filter (\x -> x > 5) list)

Qsort (x:xs) = Qsort[ z | z <- xs, z <= x] ++ [x] ++ Qsort[ z | z <- xs, z > x]

qs :: (Ord a) => [a] -> [a]
qs (x:xs) = qs [z | z <- xs, z < x] ++ [x] ++ qs [z | z <- xs, z >= x]
qs [] = []



funcion zip
zip [1,2,3] [4,5,6,7]  te da   [(1,4),(2,5),(3,6)]


sort :: [Int] -> [Int]

get :: [Int] -> Int -> Int  toma un arreglo y un int, y devuelve un int

get :: [a] -> Int -> a  igual a lo anterior pero generica

Tipos de datos empiezan con mayuscula (Bool, Char, Int)
Se pueden definir tipos de datos propios


--Una clase define funcionalidad que despues implementa el tipo --
No existen objectos

Eq define dos funciones  (==) (!=)
    pero la clase autodefine (!=) = not (==)
Todos los tipos que implementan Eq lo tiene que implementar unicamente 
la funcion == 

Or  define < > <= >= y  hereda de Eq


max :: (Ord a) => [a] -> a
necesita que el tipo a sea de la clase ordenable para poder comparar

para usar [a..z] los elementos tiene que ser de la clase Enumerable


Como se definen las funciones en Haskell?

mayor :: (Ord a) => a -> a -> a  //no es necesario definir esto, pero te aseguras de no tener errores
y una implementacion puede ser
mayor x y = if x > y then x else y

fact :: (Integral a) => a -> a   Integral son numeros positivos enteros

fact x = if x == 0 then 1 else x * fact(x - 1)

--con Pattern Matching se puede definir de esta forma--

factorial 0 = 1
factorial a = a * factorial(a - 1)

evalua los patrones en el orden que los escribo

get :: [a] -> Int -> a

get [] _ = error "No encontrado"
get (x:xs) i = if i == 1 then x else get xs (i - 1)


--Guards--

una funcion que recibe un entero y devuelve un Char

f :: Int -> Char

f x
    | x <= 0 = 'a' 
    | x <= 10 = 'b'
    | x <= 20 = 'c'
    | otherwise = 'd'

Ojo que necesita indentacion y el simbolo |

bmitell :: (RealFloat a) => a -> a -> String
bmitell w h
    | bmi <= 18.5 = "bajo peso"
    | bmi <= 25 = "en peso"
    | bmi <= 30 = "sobrepeso"
    | otherwise = "obeso"
    where bmi = w/h

el where se corre solo una vez y se usa para condicionales


cylinder :: (RealFloat a) => a -> a -> a
cylinder r h = 
    let sideArea = 2 * pi * r * h
        topArea = pi * r^2
    in sideArea + (2 * topArea)

let permite guardar un simbolo con informacion (solo presente dentro de funciones)
los simbolos no se ejecutan hasta que realmente se use (lazy). Con el in efectivamente
calcula los let

--Tuplas--
t = (x, y)

x = fst t
y = snd t

-- Funciones de listas --

init devuelve todo menos el ultimo
last devuelve el ultimo elemento


-- == SEGUNDA CLASE == --

-- Funciones de orden superior (funcion que retorna otra funcion) --

mayor :: (Ord a) => a -> a -> Bool

esta implicito que recibe un unico argumento a -> ( a -> Bool)

mayor :: (Ord a) => a -> a -> Bool

mayor x y = x > y

menorQueCinco :: (Num a, Ord a) => a -> Bool

menorQueCinco = mayor 5 


-- Funcion map --

map :: (a -> b) -> [a] -> [b]

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]

filter :: (a -> Bool) -> [a] -> [a]

filter f l = [a | z <- l, f z]

--usando lambdas para pasarle la funcion directamente
filter \x -> x > 5 [1,2,3,4]

/x y -> x + y   le podes pasar mas argumentos a un lambda

equivalente a reduce
foldr :: (b -> a -> b) -> b -> [a] -> b

foldr (/prev c -> prev + c) 0 [1,2,3]   esto devuelve 6 (la suma de todo)

implementacion seria
foldr _ y [] = y
foldr f y [x] = f y x
foldr f y (x:xs) = foldr f (f y x) xs

implementacion de map
map :: (a -> b) -> [a] -> [b]

map _ [] = []
map f (x:xs) = [f x] ++ map f xs

foldr (map f l) es igual a foldr $ map f l


-- Existen Tipos de datos --

data Bool = true | false
yo puedo definir data Bit = Uno | cuatroEsMayor

un type es un alias a un tipo de dato existente

data Tree a = Leaf a | Branch a Tree Tree | Nil
Leaf y Branch son simplemente palabras reservadas
a es una variable de tipo (es contiene los valores)

Branch 10 (Branch 8 Leaf 7 Nil) (Leaf 6)

data crea un dato no existente

type Graph = [(a,[a])]
es para definir un tipo de dato que usas datos ya existentes (lo que seria a)

data Person = Person { firstName :: String, lastNmae :: String }

data Maybe a = Nothing | Just a

divide :: (Num a) => a -> a -> Maybe a
divide _ 0 = Nothing
divide a b = Just a/b

-- Clases --

data Person = Person { DNI :: Int, firstNmae :: String } deriving (Eq)
deriving te pide que todos los parametros implementen Eq y los aplica

otra manera de implementar una clase
instance Eq Person where
    (==) x y = x:DNI = y:DNI


en el caso del Tree
instance Eq Tree (Eq a) => a where
    (==) Branch x Lt Rt  Branch y Ls Rs
        | x == y = Lt == Ls && Rt == Rs
        | otherwise = false
    
    (==) Leaf x  Branch _ _ _ = false
    (==) 


-- == TERCERA CLASE == --

-- Funtor --

El f hay que verlo como un Maybe, que tiene una variable de tipo (a,b)

class Funtor f where 
    fmap :: (a -> b) -> f a -> f b

instance Functor [] where
    fmap = map


instance Functor Maybe where
    fmap f (Just x) = Just (f x)
    fmap f Nothing = Nothing

map.fmap (\x -> x + 1) [Just 1, Just 2, Nothing, Just 3]
devuelve [Just 2, Just 3, Nothing, Just 4]

map ( fmap(\x -> x + 1) ) [Just 1, Just 2, Nothing, Just 3]

f.g x = f(g x)


map (+) [1,2,3]
[(+1), (+2), (+3)]
es un arreglo de funciones pacialmente evaluadas

map (\f -> f 3) $ map (+) [1,2,3]
esto devuelve [4,5,6]


-- == CUARTA CLASE == --

-- Application Functor --

class (functor f) => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
    -- el () me permite pasarle los argumentos en los extremos al llamar el metodo
    
    
-- implementacion para Maybe de la clase Applicative

instance Applicative Maybe where
    pure element = Just element
    Nothing <*> _ = Nothing
    (Just f) <*> something = fmap f something

    --something es el argumento f a

z :: Maybe(a -> b) -> Maybe a -> Maybe b
-- la funcion z es el ejemplo de caso <*> en el que hay una funcion presente


-- class Monoid (Aquellos tipos que son asociativas) --

class Monoid m where
    mempty :: m
    mappend :: m -> m -> m
    mconcat :: [m] -> m
    mconcat foldr mappend mempty


-- class Monad el parametro m el contexto, como podria ser Maybe--

class Monad m where
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b
    (>>) :: m a -> m b -> m b
    x >> y = x >>= \_ -> y
    fail :: String -> m a
    fail msg = error msg


instance Monad Maybe where
    return x = Just x
    Nothing >>= f = Nothing
    Just x >>= f = f x
    fail _ = Nothing


-- Modulos --

para importar un modulo ya existente puede pasar que dos modulos tengan definidas las mismas funciones
import Data.List es un modulo que tiene todos los metodos correspondientes a list
import Data.List (sort)
import Data.List hiding (sort)  esto importa todo menos sort
import Data.List qualified as L implica que las funciones de Data.List se tiene que expresar directamente, sino va usar otra

-- a b c son solo el nombre de la funcion
module test (
    a, 
    b,
    c
) where
    a :: 
    b ::
    c ::
    --aca se declaran y se implementan

-- PARA EL FINAL APRENDER EL USO DE IO, EN EL CUAL SE USAN LOS MONADS --

--implementando huffman--

qty :: (Eq a) => [a] -> [(a, Int)]

qty l = nub ( map \x -> (x, length (filter (== x) l)) l ) --nuv elimina elementos repetidos
                                                        --el (== x) es una funcion parcialmente evaluada
lo sorteo de mayor a menor frecuencia

foldl createTree Nil [sorted]

-- es una variacion de huffman en la que el arbol tiene nodos con un value y otro branch
createTree :: Tree a -> a -> Tree a
Nil b = Leaf b
t b = Branch Leaf a t







































































