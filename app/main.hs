module Main where

import HaskellGrad

main :: IO ()
main = do
  let a = var 6.0 "a"
  let b = var 2.0 "b"
  
  let d = (a - b) / b
  
  let grads = backward d
  
  putStrLn $ "d = " ++ show (val d)
  putStrLn $ "d/da = " ++ show (getGrad a grads)
  putStrLn $ "d/db = " ++ show (getGrad b grads)