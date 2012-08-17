module Main where

import Types
import Data.List

formulas = 
  [ Impl (Var "x3") (Not (Var "xT") ) 
  , Impl (Var "x4") (Not (And (Var "x3") (Var "x2")) ) 
  , Impl (Var "x5") (Or  (And (Var "xT") (Var "x4")) (And (Not $ Var "xT") (Not $ Var "x4")))
  , Impl (Var "x6") (And (Var "xT") (Var "x2")) 
  , Impl (Var "x7") (Not $ Var "x5")
  , Impl (Var "xB") (Not (And (Var "x6") (Var "x7")) )
  ]
 

main = 
  case filter (\x -> (eval' x)) input of
      [] -> putStrLn "can't find sutable solution"
      xs -> 
        --mapM_ (\x -> putStrLn $ show $ zip vars x) xs
        let anses = map (zip vars) xs
            xB = map (lookup "xB") anses
            (ans1,ans2,ans3) = foldl foldf (False,False,False) xB
            foldf :: (Bool,Bool,Bool) -> Maybe LogicVal -> (Bool,Bool,Bool)
            foldf (_,b,c) (Just Good) = (True,b,c)
            foldf (a,_,c) (Just HZ  ) = (a,True,c)
            foldf (a,b,_) (Just Bad)  = (a,b,True) in
        do 
          putStrLn $ "vars = " ++ (show vars)
          putStrLn $ "variants count = " ++ (show $ length input)
          putStrLn $ "solutions count = " ++ (show $ length xs)
          putStrLn ("Testing Good: " ++ (show ans1))
          putStrLn ("Testing HZ:   " ++ (show ans2))
          putStrLn ("Testing Bad : " ++ (show ans3))
          mapM_ (\x -> putStrLn $ show x) anses
  where 
    eval' variant = all ((==)Good) $ map (\x -> eval x find') formulas
      where 
        blah = zip vars variant
        find' s =
          case lookup s blah of
            Just x -> x
            Nothing -> error "Impossible"
    vars = Data.List.nub $ concatMap getVariables formulas
    input = f (length vars)
    f :: Int -> [[LogicVal]]
    f 0 = []
    f 1 = map (\x -> [x]) [Good,Bad]
    f n = concatMap (\x -> [Good:x,Bad:x]) prev
      where prev = f (n-1)