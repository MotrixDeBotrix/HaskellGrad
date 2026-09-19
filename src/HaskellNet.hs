module HaskellNet where

import HaskellGrad

data Neuron = Neuron { weights :: [Value], bias :: Value }
  deriving (Show)


newNeuron :: [Float] -> Float -> String -> Neuron
newNeuron weights' bias' tag = Neuron
  { weights = [ var w (tag ++ "_w" ++ show i) | (i, w) <- zip [1..] weights'],
    bias    = var bias' (tag ++ "_b") }

callNeuron :: Neuron -> [Value] -> Value
callNeuron neuron inputs = tanh' $ weightedSum + bias neuron
  where weightedSum = sum [ w * x | (w, x) <- zip (weights neuron) inputs ]


main :: IO ()
main = do
  -- Create a neuron with 2 inputs (initial weights = [2.0, -3.0], bias = 1.0)
  let neuron = newNeuron [2.0, -3.0] 1.0 "n1"
  
  -- Define inputs x1 = 1.0, x2 = 2.0
  let x1 = var 1.0 "x1"
  let x2 = var 2.0 "x2"
  
  -- Forward pass: output = tanh((2.0 * 1.0) + (-3.0 * 2.0) + 1.0) = tanh(-3.0)
  let out = callNeuron neuron [x1, x2]
  
  -- Backward pass
  let grads = backward out
  
  putStrLn $ "Neuron output: " ++ show (val out)
  putStrLn "Gradients for neuron parameters:"
  mapM_ (\w -> putStrLn $ "  d/d" ++ show (label w) ++ " = " ++ show (getGrad w grads)) (weights neuron)
  putStrLn $ "  d/d" ++ show (label (bias neuron)) ++ " = " ++ show (getGrad (bias neuron) grads)