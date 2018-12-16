main :: IO ()
main = print(2)

invertir :: [Int] -> [Int]
invertir [] = []
invertir (x:xs) = invertir xs ++ [x]


sumarTodo :: [Int] -> Int
sumarTodo [] = 0
sumarTodo (x:xs) = x + sumarTodo(xs)

mayorElem :: [Int] -> Int
mayorElem [x] = x 
mayorElem (x:xs) = if(x > head xs) then mayorElem (x : tail xs) else mayorElem(head xs: tail xs)


fibo :: Int -> Int
fibo 0 = 0;
fibo 1 = 1;
fibo x = fibo(x-1) + fibo(x-2)


--invertir de a pares--

permutar :: [Int] -> [Int]
permutar [] = []
permutar [x] = [x]
permutar(x:xs) = [head xs, x] ++ permutar (tail xs)

--Ordenar Tuplas--

ordenarTuplas :: (Ord x) => [(x,y)] -> [(x,y)]
ordenarTuplas [] = []
ordenarTuplas (x:xs) = ordenarTuplas[ z | z <- xs, fst z <= fst x] ++ [x] ++ ordenarTuplas[ z | z <- xs, fst z > fst x]


--Lista es capicua--

capicua :: (Eq a) => [a] -> Bool

capicua (x:xs) = if(x == last xs) then capicua (init xs) else False
capicua [] = True

-- Implementar el insert --

insert :: [a] -> a -> Int -> [a]

insert [] _ _ = []
insert lista y i = take i lista ++ [y] ++ drop i lista 

-- Length de una lista --

largo :: [a] -> Int

largo [] = 0
largo (x:xs) = 1 + largo xs

-- numero de veces que aparece algo --

amt :: (Eq a) => [a] -> a -> Int

amt [] _ = 0
amt (x:xs) y = if x == y then 1 + amt xs y else amt xs y


-- SEGUNDA CLASE --

-- Implementar suma binaria

data Bit = Zero | One
instance Show Bit where
    show (Zero) = "Zero"
    show (One) = "One"

type Binary = [Bit]

binarySum :: Binary -> Binary -> Binary

binarySum [] [] = []
binarySum [] y = y
binarySum x [] = x
binarySum x y =  (binarySum [fst sum] (binarySum (init x) (init y))) ++ [snd sum] 
                    where sum = bitSum (last x) (last y)

bitSum :: Bit -> Bit -> (Bit, Bit)
bitSum One One = (One, Zero)
bitSum Zero Zero = (Zero, Zero)
bitSum _ _ = (Zero, One)

-- Definir tipo arbol y definir una funcion de construccion
data Tree a = Leaf a | Branch a (Tree a) (Tree a) | Nil
instance (Show a) => Show (Tree a)  where
    show (Nil) = ""
    show (Leaf a) = show(a) 
    show (Branch a x y) = show(a) ++ " -> { (" ++ show(x) ++ ") | (" ++ show(y) ++ ") }"

crearTree :: (Ord a) => [a] -> Tree a
crearTree [] = Nil
crearTree [x] = Leaf x
crearTree (x:xs) = Branch x (crearTree [z | z <- xs, z < x]) (crearTree [z | z <- xs, z >= x])

-- Dado el tipo Graph [(nodo, [nodos vecinos])], determinar si existen ciclos.
-- Dado el tipo Graph de 3 determinar si existe camino entre dos nodos
-- Definir una funcion que retorne los primeros N numeros primos.
getPrimes ::  Int -> [Int]
getPrimes n = take n $ filter isPrime [1..] 

isPrime :: Int -> Bool
isPrime x = length (filter (\n -> (x `mod` n) == 0) [2..(x-1)]) == 0 
-- Resolver el problema de las 8 reinas.
-- Implementar el algoritmo de compresion de Huffman

-- Dado el tipo tree del punto 2, determinar la profundidad del arbol

-- profundidad :: Tree a -> Int

-- Implementar una lista de contactos (Contact {Nmae::String, Phone::Int, Email::String}) funciones add, find por name y email

data Contact = Contact { email :: String, phone :: Int } deriving Show

addContact :: [Contact] -> Contact -> [Contact]
addContact [] x = [x]
addContact l x = l ++ [x]

findByEmail :: [Contact] -> String -> Maybe Contact
findByEmail [] _ = Nothing
findByEmail (x:xs) name = if (email x) == name then Just x else findByEmail xs name

-- Dada una matriz obtener la traspuesta
traspuesta :: [[Int]] -> [[Int]]
traspuesta m = foldr (\cList mat -> insertRow cList mat) [] m 

insertRow :: [Int] -> [[Int]] -> [[Int]]
insertRow l [] = map (\x -> [x]) l
insertRow l m = zipWith (\num list ->  [num] ++ list) l m





















