
-- Definir el tipo arbol e implementar la clase Show y Eq

data Tree a = Leaf a | Branch a (Tree a) (Tree a) | Nil

-- implementacion de show para un tree
instance (Show a) => Show (Tree a) where
    show (Nil) = " * "
    show (Leaf a) = show(a) 
    show (Branch a x y) = show(a) ++ " -> { (" ++ show(x) ++ ") | (" ++ show(y) ++ ") }"

-- implementacion de show para un tree
instance (Eq a) => Eq (Tree a) where
    (==) (Branch x lt rt) (Branch y ls rs) 
        | x == y = lt == ls && rt == rs
        | otherwise = False
    (==) (Nil) (Nil) = True
    (==) (Leaf x) (Leaf y) = x == y
    (==) _ _ = False


-- data -> Crear simbolos nuevos
-- type -> Es un alias a un tipo de dato existente
-- Implementar una definicion de grafo y determinar si existen ciclos

type Graph = [(Int, [Int])]

connected :: Graph -> Int -> Int -> Bool
connected [] _ _ = False
connected g start finish = searchWithVisited g start finish []

cycleExists :: Graph -> Bool
cycleExists [] = False
cycleExists graph = foldr (||) False [searchWithVisited graph (fst z) (fst z) [] | z <- graph]


searchWithVisited :: Graph -> Int -> Int -> [Int] -> Bool
searchWithVisited graph start finish visited
-- si algun neigbor de start es finish devuelvo true
    | foldr (||) False [z == finish | z <- startNeighbors] = True
-- si dentro de los visited tengo a start 
    | foldr (||) False [z == start | z <- visited] = False
-- llamo a todos los vecinos con el mismo metodo viendo si alguno llega
    | otherwise = foldr (||) False [searchWithVisited graph z finish (visited ++ [start]) | z <- startNeighbors]
  where startNeighbors = getNeighbors graph start

getNeighbors ::  Graph -> Int -> [Int]
getNeighbors [] _  = []
getNeighbors (x:xs) pos = if( (fst x) == pos) then snd x else getNeighbors xs pos 


--Pasar decimal a binario

data Bit = One | Zero

instance Show Bit where
    show (One) = "One"
    show (Zero) = "Zero"

type Bits = [Bit]

decimalToBits :: Int -> Bits

decimalToBits 0 = [Zero]
decimalToBits 1 = [One]
decimalToBits number 
    | (rem number 2) == 0 = decimalToBits(division) ++ [Zero] 
    | otherwise = decimalToBits(division) ++ [One]
    where division = quot number 2


biggestElem ::  (Ord a) => Tree a -> Maybe a
biggestElem (Branch a lt rt) = maximum [Just(a), biggestElem lt, biggestElem rt]
biggestElem (Leaf a) = Just(a)
biggestElem (Nil) = Nothing


-- encoding table

type CodeTable = [(Char, String)]

encode :: CodeTable -> String -> String
encode table a = foldr (\cur acum -> cur ++ acum) "" (map (\x -> findCode table x) a)

findCode :: CodeTable -> Char -> String 
findCode [] _ = ""
findCode (x:xs) char 
    | (fst x) == char = snd x 
    | otherwise = findCode xs char
   

-- ['a','p','u'] => ["fun","cio","no"] => "funciono"

getFirstJust :: [Maybe a] -> Maybe a

getFirstJust [] = Nothing
getFirstJust ((Just a):xs) = Just a
getFirstJust (Nothing:xs) = getFirstJust xs 

-- getFirstJust [Nothing, Just [1,2,3], Nothing, Just [2,4,5]]
-- Just [1,2,3]



















