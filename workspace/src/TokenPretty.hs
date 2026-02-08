module TokenPretty (printTokens) where

import Token

printTokens :: [Token] -> IO ()
printTokens = mapM_ printToken

printToken :: Token -> IO ()
printToken tok =
    putStrLn $
        padRight 18 (posStr tok) ++ tokenStr tok

posStr :: Token -> String
posStr t =
    let Position l c = tokenPos t
    in "line " ++ show l ++ ", col " ++ show c

tokenStr :: Token -> String
tokenStr (Identifier _ s)   = "Identifier \"" ++ s ++ "\""
tokenStr (ValueInt _ i)     = "ValueInt " ++ show i
tokenStr (ValueFloat _ f)   = "ValueFloat " ++ show f
tokenStr (ValueString _ s)  = "ValueString " ++ show s
tokenStr (ValueBool _ b)    = "ValueBool " ++ show b
tokenStr t                  = takeWhile (/= ' ') (show t)

padRight :: Int -> String -> String
padRight n s =
    s ++ replicate (max 0 (n - length s)) ' '
