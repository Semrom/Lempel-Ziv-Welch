import Test.QuickCheck
type Code = Int

class Table a where
    empty :: a
    insert  :: a -> String  -> a
    codeOf  :: a -> String  -> Maybe  Code
    stringOf  :: a -> Code  -> Maybe  String
    isIn :: a -> String  -> Bool
    split :: a -> String  -> (String ,Maybe Code ,String)


-- Fonction utile (gestion du "Just")

maybeOf :: Maybe String -> String
maybeOf Nothing = []
maybeOf (Just x) = x



-- QUESTION 2 : Implémentation des listes associatives --

data ListAsso = ListAsso [(String,Code)] 
instance Table ListAsso where

    empty = ListAsso []

    insert (ListAsso table) word
                        | table == [] = ListAsso[(word, 1)]
                        | otherwise = ListAsso(table ++ [(word, code + 1)])
                        where
                            (_, code) = last table

    codeOf (ListAsso table) word
                        | table == [] = Nothing
                        | item == word = Just code
                        | otherwise = codeOf (ListAsso(tail table)) word
                        where 
                            (item, code) = head table

    stringOf (ListAsso table) code
                        | table == [] = Nothing
                        | codeItem == code = Just item
                        | otherwise = stringOf (ListAsso(tail table)) code
                        where
                            (item, codeItem) = head table

    isIn (ListAsso table) word
                        | codeOf (ListAsso table) word == Nothing = False
                        | otherwise = True
    
    split (ListAsso table) word
                        | table == [] = ("", Nothing, word)
                        | isIn (ListAsso table) word == True = (word, codeOf (ListAsso table) word, "")
                        | otherwise = (prefix, code, suffix ++ [last word])
                        where
                            (prefix, code, suffix) = split (ListAsso table) (take (length word - 1) word)


-- QUESTION 1 : Fonction d'encodage et de décodage--

lzwEncode :: Table a => a -> String -> [Code]
lzwEncode table [] = []
lzwEncode table message =
            case split table message of
                            (_, Nothing, _) -> []
                            (prefix, Just code, []) -> code:[]
                            (prefix, Just code, (first:others)) -> code : lzwEncode (insert table (prefix ++ [first])) (first:others)




lzw_Decode :: Table a => a -> String -> [Code] -> String
lzw_Decode table lastCode (first:others)
                                | others == [] = stringOfFirst
                                | lastCode == [] = stringOfFirst ++ (lzw_Decode table (stringOfFirst) others)
                                | otherwise = 
                                        case stringOfFirst2 of
                                                Nothing -> newWord ++ (lzw_Decode (insert table newWord) newWord others)
                                                Just word -> word ++ (lzw_Decode (insert table (lastCode ++ [head word])) word others)
                                where
                                    stringOfFirst = maybeOf (stringOf table first)
                                    stringOfFirst2 = stringOf table first
                                    newWord = lastCode ++ [head lastCode]


lzwDecode :: Table a => a -> [Code] -> String
lzwDecode table [] = []
lzwDecode table codes = lzw_Decode table "" codes



-- QUESTION 3 : QUICKCHECK --


-- Création d'un type "Message" ([Char]) afin de l'instancier à la classe "Arbitrary"
data Message = Message [Char] deriving (Show)
instance Arbitrary Message where
    arbitrary = fmap Message (listOf1 (choose ('a','z')))


-- Création de la table de test (insertion de chaque élément de l'intervalle ['a'..'z'] dans la table)
tableTest = foldl (insert) (ListAsso []) (map(\a -> [a]) ['a'..'z'])


-- Création de la fonction de test où "msg" sera générée aléatoirement 
lzwEncodeDecode :: Message -> Bool
lzwEncodeDecode (Message msg) = (lzwDecode tableTest (lzwEncode tableTest msg)) == msg


-- Fonction qui teste 20 fois "lzwEncodeDecode"
checkLzw = quickCheckWith stdArgs { maxSuccess = 20 } lzwEncodeDecode


-- main par défaut
main = print "Test LZW"
                        
