data Func
  = X -- f(x) = x
  | Sin Func -- f(x) = sen(g(x))
  | Cos Func -- f(x) = cos(g(x))
  | Exp Func -- f(x) = e^g(x)
  | Ln Func -- f(x) = ln(g(x))
  | Power Func Double -- f(x) = g(x)^n
  | Const Double -- f(x) = k
  | Sum Func Func -- h(x)= f(x) + g(x)
  | Mult Func Func -- h(x) = f(x) * g(x)
  | Div Func Func -- h(x) = (f(x) / g(x))
  deriving (Show)

instance Read Func where
  readsPrec _ input = [(Const (read input :: Double), "")]

ddx :: Func -> Double -> Double
ddx X _ = 1
ddx (Sin f) x = cos (eval f x) * ddx f x
ddx (Cos f) x = -sin (eval f x) * ddx f x
ddx (Exp f) x = exp (eval f x) * ddx f x
ddx (Ln f) x = ddx f x / eval f x
ddx (Power f n) x = n * eval f x ** (n - 1) * ddx f x
ddx (Sum f g) x = ddx f x + ddx g x
ddx (Mult (Const k) f) x = k * ddx f x
ddx (Mult f g) x = ddx f x * eval g x + eval f x * ddx g x
ddx (Div f g) x = (ddx f x * eval g x - eval f x * ddx g x) / (eval g x ** 2)
ddx (Const _) _ = 0

eval :: Func -> Double -> Double
eval X x = x
eval (Sin f) x = sin (eval f x)
eval (Cos f) x = cos (eval f x)
eval (Exp f) x = exp (eval f x)
eval (Ln f) x = log (eval f x)
eval (Power f n) x = eval f x ** n
eval (Const k) _ = k
eval (Sum f g) x = eval f x + eval g x
eval (Mult f g) x = eval f x * eval g x
eval (Div f g) x = eval f x / eval g x

main :: IO ()
main = putStrLn "Olá, usuário! Vamos calcular?"

-- ddx ( Power ( Mult (Const 2) X ) 3) 1 r: 2x^3" em relação a x, avaliada em x = 1

-- ddx (Sum (Const 1 ) X ) 5 r: "1+x" em relação a x, avaliada em x = 5

-- ddx (Sum (Const 1 ) X ) 0 r: "1+x" em relação a x, avaliada em x = 0.

-- ddx (Div (Sum (Sum (Power X 2) (Mult (Const 2) X)) (Const 1)) (Sum X (Const (-1)))) 2 r: "(x^2 + 2x + 1)/(x-1)" em relação a x, avaliada em x = 2.

-- ddx (Power (Sum X (Const 1)) 2) 5 r: "(x+1)^2" em relação a x, avaliada em x = 5.

-- ddx (Power (Sum X (Const 1)) 2) 0 r:"(x+1)^2" em relação a x, avaliada em x = 0.