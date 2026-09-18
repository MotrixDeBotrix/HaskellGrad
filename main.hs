-- Custom datatype to represent operations
data Op 
  = Add 
  | Mul 
  | Abs 
  | Signum 
  | Negate 
  | None
  deriving (Eq, Show)

-- Expression graph node definition
data Value 
  = Leaf Float
  | Node Float [Value] Op
  deriving (Eq, Show)

-- Extract scalar Float value from any node
val :: Value -> Float
val (Leaf x)     = x
val (Node x _ _) = x

instance Num Value where
  (+) a b = Node (val a + val b) [a, b] Add
  (*) a b = Node (val a * val b) [a, b] Mul

  abs a        = Node (abs $ val a) [a] Abs
  signum a     = Node (signum $ val a) [a] Signum
  negate a     = Node (negate $ val a) [a] Negate
  fromInteger n = Leaf (fromInteger n)

main :: IO ()
main = do
  let a = Leaf 1.0
  let b = Leaf 2.0
  let c = a + b
  
  print c