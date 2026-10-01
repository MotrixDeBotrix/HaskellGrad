module Model
  ( train
  , predictDataset
  )
where

import HaskellGrad
import HaskellNet

-- Training step
epoch :: MLP -> [[Value]] -> [Float] -> Float -> (MLP, Value)
epoch model inputs targets learn =
  let predictions = predictDataset model inputs
      lossVal     = meanSquaredLoss predictions targets
      grads       = backprop lossVal
      updated     = updateMLP learn grads model
  in (updated, lossVal)

-- Training loop
train :: Int -> Int -> MLP -> [[Value]] -> [Float] -> Float -> IO MLP
train currentEpoch maxEpochs model inputs targets learningRate
  | currentEpoch > maxEpochs = return model
  | otherwise                = do
      let (updatedModel, lossVal) = epoch model inputs targets learningRate

      if currentEpoch == 1 || currentEpoch `mod` 10 == 0
        then putStrLn $ "Epoch " ++ show currentEpoch ++ ", Loss: " ++ show (val lossVal)
        else return ()
      train (currentEpoch + 1) maxEpochs updatedModel inputs targets learningRate


-- Call Model
predictDataset :: MLP -> [[Value]] -> [Value]
predictDataset model inputs = map getSingleOutput inputs
  where
    getSingleOutput x = case callMLP model x of
      (out:_) -> out
      []      -> error "MLP returned no outputs!"