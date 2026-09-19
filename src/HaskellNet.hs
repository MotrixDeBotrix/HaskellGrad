module HaskellNet where

import Data.List (foldl')
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


meanSquaredLoss :: [Value] -> [Float] -> Value
meanSquaredLoss predictions targets =
  let squaredErrors = [ (predVal - constant target) * (predVal - constant target)
                      | (predVal, target) <- zip predictions targets ]
      totalError = sum squaredErrors
      numSamples = fromIntegral $ length predictions
  in totalError / numSamples

main :: IO ()
main = do
  -- let dataSetInputs = [ [2.0,  3.0, -1.0],
  --                       [3.0, -1.0,  0.5],
  --                       [0.5,  1.0,  1.0],
  --                       [1.0,  1.0, -1.0] ]

  -- let dataSetTargets = [1.0, -1.0, -1.0, 1.0]

  let x = [constant 1.0, constant 2.0]

  let layer1 = Layer [ newNeuron [0.5, -0.5] 0.1 "h1",
                       newNeuron [0.2,  0.8] 0.0 "h2" ]
  let layer2 = Layer [ newNeuron [0.4, -0.1] 0.2 "out"]
  let model = MLP [layer1, layer2]

  let preds = callMLP model x
  let loss  = meanSquaredLoss preds [1.0]

  let grads = backprop loss

  putStrLn $ "Prediction: " ++ show (map val preds)
  putStrLn $ "Loss:       " ++ show (val loss)
  case neurons layer1 of
    (n1:_) -> putStrLn $ "Grad (h1_b): " ++ show (getGrad (bias n1) grads)
    []     -> putStrLn "No neurons in layer1"