import Data.List

data Tree = Node Int Char | Connection Int (Tree) (Tree) deriving Show

value :: Tree -> Int
value (Node a _)  = a
value (Connection a _ _ ) = a

instance Eq Tree where
    a == b =  (value a) == (value b)

instance Ord Tree where
    compare a b = compare (value a) (value b) 

countWords :: String -> [Tree]
countWords [] = []
countWords (x:xs) = 
    let array = filter (/=x) xs in
    [Node (length (x:xs) - length array) x] ++ (countWords array)

createTree :: [Tree] -> Tree
createTree [] = error ":c"
createTree [x] = x
createTree (x:y:xs) =  createTree (addNode (Connection ((value x) + (value y)) x y) xs)

addNode :: Tree -> [Tree] -> [Tree]
addNode a [] = [a]
addNode a (x:xs) = 
    if (value a) < (value x) then [a] ++ (x:xs)
        else [x] ++ addNode a xs

generateCodes :: Tree -> String -> [(Char, String)]
generateCodes (Node freq char) code = [(char, code)]
generateCodes (Connection a l r) s = (generateCodes (l) (s ++ "0")) ++ (generateCodes (r) (s ++ "1"))

encode :: [(Char, String)] -> Char -> String
encode [] c = error "Your fucked up"
encode (x:xs) c = 
    if (fst x) == c then snd x
        else encode xs c

huffman :: String -> String
huffman text =  foldr (++) [] (map (encode (generateCodes (createTree (sort (countWords text))) "")) text)

