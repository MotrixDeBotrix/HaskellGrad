module Main where

import Grad.HaskellGrad
import Net.HaskellNet
import Net.Model

main :: IO ()
main = do
  -- Get dataset and targets
  
  -- putStrLn "| Loading dataset..."
  -- -- Arbitrary testing dataset
  -- let rawInputs = [ [2.0,  3.0, -1.0],
  --                   [3.0, -1.0,  0.5],
  --                   [0.5,  1.0,  1.0],
  --                   [1.0,  1.0, -1.0] ]

  -- let targets = [1.0, -1.0, -1.0, 1.0]

  -- let inputs = [ map constant row | row <- rawInputs ]



  -- Specify model
  let initialWeights = repeat 0.1
  let myHaskellNet = newHaskellNet 3 [4, 4] 1 initialWeights



  -- Configure training
  let maxEpochs = 10000
  let learningRate = 0.05

  -- Train model on part of dataset

  -- Validate model performance on unseen part of dataset









  print "Test"




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
