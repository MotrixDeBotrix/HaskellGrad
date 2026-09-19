import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.List (foldl')

type GradMap = Map Value Float

-- Datatype for operations
data Op = Add | Mul | Pow Float | Tanh
  deriving (Eq, Ord, Show)

-- Datatype for expression graph nodes (added Ord deriving)
data Value 
  = Leaf { val :: Float, label :: Maybe String }
  | Node { val :: Float, children :: [Value], op :: Op, label :: Maybe String }
  deriving (Eq, Ord, Show)

-- Constructors
var :: Float -> String -> Value
var x name = Leaf { val = x, label = Just name }

constant :: Float -> Value
constant x = Leaf { val = x, label = Nothing }

node :: Float -> [Value] -> Op -> Value
node v ch o = Node { val = v, children = ch, op = o, label = Nothing }

-- Safe helper to extract children (returns [] for Leaf)
getChildren :: Value -> [Value]
getChildren (Node { children = ch }) = ch
getChildren (Leaf {})                = []

setLabel :: String -> Value -> Value
setLabel lbl v = v { label = Just lbl }

pow :: Value -> Float -> Value
pow x k = node (val x ** k) [x] (Pow k)

tanh' :: Value -> Value
tanh' v = node (tanh (val v)) [v] Tanh


instance Num Value where
  (+) a b = node (val a + val b) [a, b] Add
  (*) a b = node (val a * val b) [a, b] Mul
  fromInteger n = constant (fromInteger n)

  negate a = a * (-1)
  abs _    = error "abs not implemented"
  signum _ = error "signum not implemented"

instance Fractional Value where
  recip x        = pow x (-1.0)
  fromRational r = constant (fromRational r)


-- Gradients
getGrad :: Value -> GradMap -> Float
getGrad v grads = Map.findWithDefault 0.0 v grads

addGrad :: Value -> Float -> GradMap -> GradMap
addGrad child delta gradMap = Map.insertWith (+) child delta gradMap

-- Safely inspect operation and children
propagateValue :: Value -> GradMap -> GradMap
propagateValue v grads =
  case v of
    Node { op = Add, children = [a, b] } ->
      addGrad a (g * 1.0) . addGrad b (g * 1.0) $ grads

    Node { op = Mul, children = [a, b] } ->
      addGrad a (g * val b) . addGrad b (g * val a) $ grads

    Node { op = Tanh, children = [a] } ->
      addGrad a (g * (1.0 - (val v)**2)) grads

    Node { op = Pow k, children = [a] } ->
      let localDeriv = k * (val a ** (k - 1.0))
      in addGrad a (g * localDeriv) grads

    _ -> grads
  where
    g = getGrad v grads

-- Topological sort
topoSort :: Value -> [Value]
topoSort root = go [] root
  where
    go visited v 
      | v `elem` visited = visited
      | otherwise        = v : foldl' go visited (getChildren v)


backprop :: Value -> GradMap
backprop root =
  let topo = topoSort root
      initialGrads = Map.singleton root 1.0
  in foldl' (\grads v -> propagateValue v grads) initialGrads topo


main :: IO ()
main = do
  let a = var 6.0 "a"
  let b = var 2.0 "b"
  
  -- d = a / b = 6.0 / 2.0 = 3.0
  let d = a / b
  
  let grads = backprop d
  
  putStrLn $ "d = " ++ show (val d)
  putStrLn $ "d/da = " ++ show (getGrad a grads)
  putStrLn $ "d/db = " ++ show (getGrad b grads)