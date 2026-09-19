module HaskellGrad
  ( Value(..)
  , Op(..)
  , GradMap
  , var
  , constant
  , node
  , pow
  , tanh'
  , getChildren
  , setLabel
  , getGrad
  , topoSort
  , propagateValue
  , backward
  ) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.List (foldl')


type GradMap = Map Value Float

data Op = Add | Mul | Tanh | Pow Float
  deriving (Eq, Ord, Show)

data Value 
  = Leaf { val :: Float, label :: Maybe String }
  | Node { val :: Float, children :: [Value], op :: Op, label :: Maybe String }
  deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------
-- Constructors and helpers
--------------------------------------------------------------------------------

var :: Float -> String -> Value
var x name = Leaf { val = x, label = Just name }

constant :: Float -> Value
constant x = Leaf { val = x, label = Nothing }

node :: Float -> [Value] -> Op -> Value
node v ch o = Node { val = v, children = ch, op = o, label = Nothing }


getChildren :: Value -> [Value]
getChildren (Node { children = ch }) = ch
getChildren (Leaf {})                = []

setLabel :: String -> Value -> Value
setLabel lbl v = v { label = Just lbl }

pow :: Value -> Float -> Value
pow x k = node (val x ** k) [x] (Pow k)

tanh' :: Value -> Value
tanh' v = node (tanh (val v)) [v] Tanh

--------------------------------------------------------------------------------
-- Typeclass instancing
--------------------------------------------------------------------------------

instance Num Value where
  (+) a b = node (val a + val b) [a, b] Add
  (*) a b = node (val a * val b) [a, b] Mul
  fromInteger n = constant (fromInteger n)
  negate a = a * (-1)
  abs _    = error "abs not implemented"
  signum _ = error "signum not implemented"

instance Fractional Value where
  recip x = pow x (-1.0)
  fromRational r = constant (fromRational r)

--------------------------------------------------------------------------------
-- Gradient stuff
--------------------------------------------------------------------------------

getGrad :: Value -> GradMap -> Float
getGrad v grads = Map.findWithDefault 0.0 v grads

propagateValue :: Value -> GradMap -> GradMap
propagateValue v grads =
  let g = getGrad v grads
      addGrad child delta gMap = Map.insertWith (+) child delta gMap
  in case v of
    Node { op = Add, children = [a, b] } ->
      addGrad a (g * 1.0) . addGrad b (g * 1.0) $ grads

    Node { op = Mul, children = [a, b] } ->
      addGrad a (g * val b) . addGrad b (g * val a) $ grads

    Node { op = Pow k, children = [a] } ->
      let localDeriv = k * (val a ** (k - 1.0))
      in addGrad a (g * localDeriv) grads

    Node { op = Tanh, children = [a] } ->
      let y = val v
      in addGrad a (g * (1.0 - y * y)) grads

    _ -> grads


topoSort :: Value -> [Value]
topoSort root = go [] root
  where
    go visited v 
      | v `elem` visited = visited
      | otherwise        = v : foldl' go visited (getChildren v)


backward :: Value -> GradMap
backward root =
  let topo = topoSort root
      initialGrads = Map.singleton root 1.0
  in foldl' (\grads v -> propagateValue v grads) initialGrads topo