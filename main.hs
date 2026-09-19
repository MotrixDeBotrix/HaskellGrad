module Main where

import HaskellGrad
import HaskellNet


predictDataset :: MLP -> [[Value]] -> [Value]
predictDataset model inputs = map getSingleOutput inputs
  where
    getSingleOutput x = case callMLP model x of
      (out:_) -> out
      []      -> error "MLP returned no outputs!"

-- Training step
epoch :: MLP -> [[Value]] -> [Float] -> Float -> (MLP, Value)
epoch model inputs targets learn =
  let predictions = predictDataset model inputs
      lossVal     = meanSquaredLoss predictions targets
      grads       = backprop lossVal
      updated     = updateMLP learn grads model
  in (updated, lossVal)

-- Actual training loop
train :: Int -> Int -> MLP -> [[Value]] -> [Float] -> Float -> IO MLP
train currentEpoch maxEpochs model inputs targets learningRate
  | currentEpoch > maxEpochs = return model
  | otherwise                = do
      let (updatedModel, lossVal) = epoch model inputs targets learningRate

      if currentEpoch == 1 || currentEpoch `mod` 10 == 0
        then putStrLn $ "Epoch " ++ show currentEpoch ++ ", Loss: " ++ show (val lossVal)
        else return ()
      train (currentEpoch + 1) maxEpochs updatedModel inputs targets learningRate


main :: IO ()
main = do 
  let maxEpochs = 100000
  let learningRate = 0.05

  putStrLn "| Loading dataset..."
  -- Arbitrary testing dataset
  let rawInputs = [ [2.0,  3.0, -1.0],
                    [3.0, -1.0,  0.5],
                    [0.5,  1.0,  1.0],
                    [1.0,  1.0, -1.0] ]

  let targets = [1.0, -1.0, -1.0, 1.0]

  let inputs = [ map constant row | row <- rawInputs ]

  putStrLn "\n| Building MLP Model..."
  -- 3 Inputs, Hidden Layer (4 neurons), 1 output
  let layer1 = newLayer [ [ 0.2, -0.5,  0.1]
                        , [ 0.1,  0.3, -0.2]
                        , [-0.3,  0.4,  0.5]
                        , [ 0.5, -0.1,  0.2] 
                        ] [0.0, 0.0, 0.0, 0.0] "h1"

  let layer2 = newLayer [ [ 0.1, -0.2, 0.3, -0.4] ] [0.0] "out"
  
  let initialModel = MLP [layer1, layer2]


  putStrLn "\n| Untrained model performance:"
  let initPreds = predictDataset initialModel inputs
  let initLoss  = meanSquaredLoss initPreds targets
  putStrLn $ "Initial Predictions : " ++ show (map val initPreds)
  putStrLn $ "Initial Loss        : " ++ show (val initLoss)

  putStrLn $ "\n| Training Loop (" ++ show maxEpochs ++ "Epochs, Learning Rate = " ++ show learningRate ++")"
  trainedModel <- train 1 maxEpochs initialModel inputs targets 0.05

  putStrLn "\n| Evaluation After Training"
  let finalPreds = predictDataset trainedModel inputs
  let finalLoss  = meanSquaredLoss finalPreds targets
  putStrLn $ "Targets          : " ++ show targets
  putStrLn $ "Final Predictions: " ++ show (map val finalPreds)
  putStrLn $ "Final Loss       : " ++ show (val finalLoss)