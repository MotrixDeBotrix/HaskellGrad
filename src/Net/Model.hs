module Net.Model
  ( train
  , predictDataset
  , newHaskellNet
  )
where

import Grad.HaskellGrad
import Net.HaskellNet

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



layerDimensions :: Int -> [Int] -> Int -> [(Int, Int)]
layerDimensions inputSize hiddenSizes outputSize =
  let sizes = inputSize : hiddenSizes ++ [outputSize]
  in zip sizes (drop 1 sizes)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- Build a new model
-- Input size -> [layer size] -> output size -> MLP
newHaskellNet :: Int -> [Int] -> Int -> [Float] -> MLP
newHaskellNet inputSize hiddenLayerSizes outputSize floatStream =
  let dims = layerDimensions inputSize hiddenLayerSizes outputSize
      (layersList, _) = foldl' buildLayer ([], floatStream) (zip [1 ..] dims)
  in MLP (reverse layersList)
  where
    buildLayer (accLayers, stream) (layerIdx, (nIn, nOut)) =
      let layerTag = "l" ++ show layerIdx
          
          -- Extract weights: nOut neurons x nIn weights each
          (wChunk, stream1) = splitAt (nIn * nOut) stream
          -- Extract biases: 1 per neuron
          (bChunk, stream2) = splitAt nOut stream1
          
          -- Group flat weight list into a matrix (nOut rows of nIn weights)
          weightsMatrix = chunksOf nIn wChunk
          
          layer = newLayer weightsMatrix bChunk layerTag
      in (layer : accLayers, stream2)


-- Call Model
predictDataset :: MLP -> [[Value]] -> [Value]
predictDataset model inputs = map getSingleOutput inputs
  where
    getSingleOutput x = case callMLP model x of
      (out:_) -> out
      []      -> error "MLP returned no outputs!"