module Main where

import HaskellGrad
import HaskellNet
import Model

main :: IO ()
main = do
  -- Get dataset and targets

  -- Specify model

  -- Configure stuff
  let maxEpochs = 10000
  let learningRate = 0.05

  -- Train model on part of dataset

  -- Validate model performance on unseen part of dataset




  -- putStrLn "| Loading dataset..."
  -- -- Arbitrary testing dataset
  -- let rawInputs = [ [2.0,  3.0, -1.0],
  --                   [3.0, -1.0,  0.5],
  --                   [0.5,  1.0,  1.0],
  --                   [1.0,  1.0, -1.0] ]

  -- let targets = [1.0, -1.0, -1.0, 1.0]

  -- let inputs = [ map constant row | row <- rawInputs ]

  -- putStrLn "\n| Building MLP Model..."
  -- -- 3 Inputs, Hidden Layer (4 neurons), 1 output
  -- let layer1 = newLayer [ [ 0.2, -0.5,  0.1]
  --                       , [ 0.1,  0.3, -0.2]
  --                       , [-0.3,  0.4,  0.5]
  --                       , [ 0.5, -0.1,  0.2] 
  --                       ] [0.0, 0.0, 0.0, 0.0] "h1"

  -- let layer2 = newLayer [ [ 0.1, -0.2, 0.3, -0.4] ] [0.0] "out"
  
  -- let initialModel = MLP [layer1, layer2]


  -- putStrLn "\n| Untrained model performance:"
  -- let initPreds = predictDataset initialModel inputs
  -- let initLoss  = meanSquaredLoss initPreds targets
  -- putStrLn $ "Initial Predictions : " ++ show (map val initPreds)
  -- putStrLn $ "Initial Loss        : " ++ show (val initLoss)

  -- putStrLn $ "\n| Training Loop (" ++ show maxEpochs ++ "Epochs, Learning Rate = " ++ show learningRate ++")"
  -- trainedModel <- train 1 maxEpochs initialModel inputs targets 0.05

  -- putStrLn "\n| Evaluation After Training"
  -- let finalPreds = predictDataset trainedModel inputs
  -- let finalLoss  = meanSquaredLoss finalPreds targets
  -- putStrLn $ "Targets          : " ++ show targets
  -- putStrLn $ "Final Predictions: " ++ show (map val finalPreds)
  -- putStrLn $ "Final Loss       : " ++ show (val finalLoss)
