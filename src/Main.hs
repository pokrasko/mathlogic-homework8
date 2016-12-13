module Main where

import           CNF
import           Ordinal

inputFile  = "task8.in"
outputFile = "task8.out"

main = do
    input <- readFile inputFile
    let (OrdComp o1 o2) = read input
    if ordinalToCNF o1 == ordinalToCNF o2
        then writeFile outputFile $ "Равны"
             ++ "\nКНФ первого выражения: " ++ show (ordinalToCNF o1)
             ++ "\nКНФ второго выражения: " ++ show (ordinalToCNF o2)
        else writeFile outputFile $ "Не равны"
             ++ "\nКНФ первого выражения: " ++ show (ordinalToCNF o1)
             ++ "\nКНФ второго выражения: " ++ show (ordinalToCNF o2)