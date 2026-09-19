module HaskellNet where

import HaskellGrad

data Neuron = Neuron { weights :: [Value], bias :: Value }
  deriving (Show)

getNeuronParams :: Neuron -> [Value]
getNeuronParams n = weights n ++ [bias n]

newNeuron :: [Float] -> Float -> String -> Neuron
newNeuron weights' bias' tag = Neuron
  { weights = [ var w (tag ++ "_w" ++ show i) | (i, w) <- zip [1..] weights'],
    bias    = var bias' (tag ++ "_b") }

callNeuron :: Neuron -> [Value] -> Value
callNeuron neuron inputs = tanh' $ weightedSum + bias neuron
  where weightedSum = sum [ w * x | (w, x) <- zip (weights neuron) inputs ]


newtype Layer = Layer { neurons :: [Neuron] }
  deriving (Show)

getLayerParams :: Layer -> [Value]
getLayerParams (Layer neurons) = concatMap getNeuronParams neurons

newLayer :: [[Float]] -> [Float] -> String -> Layer
newLayer weightsMatrix biases tag = Layer
  [ newNeuron w b (tag ++ "_n" ++ show i)
  | (i, (w, b)) <- zip [1..] (zip weightsMatrix biases) ]

callLayer :: Layer -> [Value] -> [Value]
callLayer (Layer neurons) inputs = [ callNeuron n inputs | n <- neurons ]


newtype MLP = MLP { layers :: [Layer] }
  deriving (Show)

getMLPParams :: MLP -> [Value]
getMLPParams (MLP layers) = concatMap getLayerParams layers

callMLP :: MLP -> [Value] -> [Value]
callMLP (MLP layers) inputs = foldl' (\input layer -> callLayer layer input) inputs layers



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