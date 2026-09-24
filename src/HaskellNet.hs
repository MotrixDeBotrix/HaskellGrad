module HaskellNet
  ( Neuron (..),
    Layer (..),
    MLP (..),
    newNeuron,
    newLayer,
    callNeuron,
    callLayer,
    callMLP,
    getNeuronParams,
    getLayerParams,
    getMLPParams,
    meanSquaredLoss,
    updateVal,
    updateNeuron,
    updateLayer,
    updateMLP,
  )
where

import Data.List (foldl')
import HaskellGrad

--------------------------------------------------------------------------------
-- Data types
--------------------------------------------------------------------------------

data Neuron = Neuron {weights :: [Value], bias :: Value}
  deriving (Show)

newtype Layer = Layer {neurons :: [Neuron]}
  deriving (Show)

newtype MLP = MLP {layers :: [Layer]}
  deriving (Show)

--------------------------------------------------------------------------------
-- Constructors and initialization
--------------------------------------------------------------------------------

newNeuron :: [Float] -> Float -> String -> Neuron
newNeuron weights' bias' tag =
  Neuron
    { weights = [var w (tag ++ "_w" ++ show i) | (i, w) <- zip [1 ..] weights'],
      bias = var bias' (tag ++ "_b")
    }

newLayer :: [[Float]] -> [Float] -> String -> Layer
newLayer weightsMatrix biases tag =
  Layer
    [ newNeuron w b (tag ++ "_n" ++ show i)
    | (i, (w, b)) <- zip [1 ..] (zip weightsMatrix biases)
    ]

--------------------------------------------------------------------------------
-- Calling (forward pass)
--------------------------------------------------------------------------------

callNeuron :: Neuron -> [Value] -> Value
callNeuron neuron inputs = tanh' $ weightedSum + bias neuron
  where
    weightedSum = sum [w * x | (w, x) <- zip (weights neuron) inputs]

callLayer :: Layer -> [Value] -> [Value]
callLayer (Layer neurons) inputs = [callNeuron n inputs | n <- neurons]

callMLP :: MLP -> [Value] -> [Value]
callMLP (MLP layers) inputs = foldl' (\input layer -> callLayer layer input) inputs layers

--------------------------------------------------------------------------------
-- Parameter extraction helpers
--------------------------------------------------------------------------------

getNeuronParams :: Neuron -> [Value]
getNeuronParams n = weights n ++ [bias n]

getLayerParams :: Layer -> [Value]
getLayerParams (Layer neurons) = concatMap getNeuronParams neurons

getMLPParams :: MLP -> [Value]
getMLPParams (MLP layers) = concatMap getLayerParams layers

--------------------------------------------------------------------------------
-- | _
--------------------------------------------------------------------------------

meanSquaredLoss :: [Value] -> [Float] -> Value
meanSquaredLoss predictions targets =
  let squaredErrors =
        [ (predVal - constant target) * (predVal - constant target)
        | (predVal, target) <- zip predictions targets
        ]
      totalError = sum squaredErrors
      numSamples = fromIntegral $ length predictions
   in totalError / numSamples

--------------------------------------------------------------------------------
-- Gradient descent
--------------------------------------------------------------------------------

updateVal :: Float -> GradMap -> Value -> Value
updateVal learn grads v = v {val = val v - learn * getGrad v grads}

updateNeuron :: Float -> GradMap -> Neuron -> Neuron
updateNeuron learn grads n =
  n
    { weights = map (updateVal learn grads) (weights n),
      bias = updateVal learn grads (bias n)
    }

updateLayer :: Float -> GradMap -> Layer -> Layer
updateLayer learn grads (Layer neurons) = Layer [updateNeuron learn grads n | n <- neurons]

updateMLP :: Float -> GradMap -> MLP -> MLP
updateMLP learn grads (MLP layers) = MLP [updateLayer learn grads l | l <- layers]
