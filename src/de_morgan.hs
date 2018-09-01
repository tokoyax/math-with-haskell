main :: IO ()
main = do
    putStrLn headerA
    putStrLn devider
    mapM_ (putStrLn . (\ x -> row x expressionA expressionB)) input
    putStrLn ""
    putStrLn headerB
    putStrLn devider
    mapM_ (putStrLn . (\ x -> row x expressionA' expressionB')) input
    return ()

type Pair = (Bool, Bool)
type Expression = Pair -> Bool

headerA :: String
headerA = "   A   |   B   | (¬ A) ∨ (¬ B) | ¬ (A ∧ B) "

headerB :: String
headerB = "   A   |   B   | (¬ A) ∧ (¬ B) | ¬ (A ∨ B) "

devider :: String
devider = "============================================"

row :: Pair -> Expression -> Expression -> String
row (f, s) a b = boolToString f ++ "|" ++
                 boolToString s ++ "|" ++
                 boolToString (a (f, s)) ++ "         |" ++
                 boolToString (b (f, s))

boolToString :: Bool -> String
boolToString True  = " True  "
boolToString False = " False "

expressionA :: Pair -> Bool
expressionA (a, b) = not a || not b

expressionB :: Pair -> Bool
expressionB (a, b) = not (a && b)

expressionA' :: Pair -> Bool
expressionA' (a, b) = not a && not b

expressionB' :: Pair -> Bool
expressionB' (a, b) = not (a || b)

input :: [(Bool, Bool)]
input = [ (True,  True)
        , (True,  False)
        , (False, True)
        , (False, False)
        ]
