-- Define a type for objects in the category
data Object = Object String deriving (Eq, Show)

-- Define a type for morphisms in the category
data Morphism = Morphism String Object Object deriving (Eq, Show)

-- Define a simple category type
data Category = Category [Object] [Morphism]

-- Example of creating a category
simpleCat :: Category
simpleCat = Category [Object "A", Object "B", Object "C"]
                    [ Morphism "f" (Object "A") (Object "B")
                    , Morphism "g" (Object "B") (Object "C")]

-- Function to compose morphisms
compose :: Category -> Morphism -> Morphism -> Maybe Morphism
compose (Category _ morphisms) (Morphism _ _ b) (Morphism _ a _) =
    case filter (\(Morphism _ x y) -> x == a && y == b) morphisms of
        [m] -> Just m
        _   -> Nothing

-- Example usage
main :: IO ()
main = do
    let comp = compose simpleCat (Morphism "f" (Object "A") (Object "B")) (Morphism "g" (Object "B") (Object "C"))
    case comp of
        Just (Morphism name _ _) -> putStrLn $ "Composition: " ++ name
        Nothing                  -> putStrLn "Morphisms cannot be composed"
