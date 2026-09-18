-- Datatype for operations on expression graph nodes
data Op = Add | Mul | Tanh
  deriving (Eq, Show)

-- Datatype for expression graph nodes
data Value 
  = Leaf { val :: Float, grad :: Float, label :: Maybe String }
  | Node { val :: Float, grad :: Float, children :: [Value], op :: Op, label :: Maybe String }
  deriving (Eq, Show)

tanh' :: Value -> Value
tanh' v = node (tanh (val v)) [v] Tanh


-- Constructors
-- Default constructor for leaves/variables
var :: Float -> String -> Value
var x name = Leaf { val = x, grad = 0.0, label = Just name }

-- Default constructor for literal values
constant :: Float -> Value
constant x = Leaf { val = x, grad = 0.0, label = Nothing }

-- Default constructor for intermediate computation nodes
node :: Float -> [Value] -> Op -> Value
node v ch o = Node { val = v, grad = 0.0, children = ch, op = o, label = Nothing }


-- Helper to tag intermediate nodes
setLabel :: String -> Value -> Value
setLabel lbl v = v { label = Just lbl }


instance Num Value where
  (+) a b = node (val a + val b) [a, b] Add
  (*) a b = node (val a * val b) [a, b] Mul
  fromInteger n = constant (fromInteger n)

  negate a = a * (-1)
  abs _    = error "abs not implemented"
  signum _ = error "signum not implemented"



main :: IO ()
main = do
  let a = var 1.0 "a"
  let b = var 2.0 "b"
  let c = setLabel "c" (a + b)
  
  print $ tanh' c