module Main where

import Control.Monad.Trans.Cont

import ContBahug

printWComment :: (Show a) => String -> a -> IO ()
printWComment s f = putStrLn $ s ++ ": " ++ show f

section :: String -> IO ()
section s = putStrLn "" >> putStrLn s >> putStrLn (map (const '-') s)

main :: IO ()
main = do
    section "Suspension Examples"
    printWComment "doSomethingToR areaCircle" $ doSomethingToR areaCircle
    printWComment "areaCircle 1.0 £ id" $ areaCircle 1.0 £ id
    printWComment "doSomethingToR volumeCylinder 2.0" $
                   doSomethingToR volumeCylinder 2.0

    section "suspendVolume Examples"
    print $ "map (== suspendVolume 1 2 id) [suspendVolume', " ++
                                      "suspendVolume'', suspendVolumeCont]"
    print $ map ((== suspendVolume 1 2 id) . (\f -> f 1.0 2.0 id) )
                [ suspendVolume'  ,
                  suspendVolume'' ,
                  \x y f -> runCont (suspendVolumeCont x y) f ]

    section "Using k different numbers of times"
    printWComment "oops" oops
    printWComment "dbl" dbl

    section "calc Examples"
    printWComment "calc1" calc1
    printWComment "calc2" calc2

    section "prod Examples"
    printWComment "prod [1, 2, 3, 4]" $ prod [1, 2, 3, 4]
    printWComment "prod [1, 0, 3, 4]" $ prod [1, 0, 3, 4]


    section "Set Example using insert"

    let s = Set [3, 4]
    printWComment "Initial Set" s

    let s'  = insert s  1
        s'' = insert s' 2
    printWComment "After inserting [1, 2] with insert" s''

    let s''' = insert s'' 4
    printWComment "After inserting 4 with insert" s'''


    section "Set Example using insert"

    let t = Set [3, 4]
    printWComment "Initial Set" t

    let t'  = insert' t  1
        t'' = insert' t' 2
    printWComment "After inserting [1, 2] with insert'" t''

    let t''' = insert' t'' 4
    printWComment "After inserting 4 with insert'" t'''
