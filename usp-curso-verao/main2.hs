import Text.XHtml (base)
import Graphics.Win32 (iDI_HAND)

efeDeX :: Int -> Int
efeDeX x = x + 2


geDex :: Int -> Int
geDex x = x * x


somaDe5 :: Int -> Int -> Int -> Int -> Int -> Int
somaDe5 a b c d e = a + b + c + d + e


funcaoAplica :: a -> (a -> b) -> b
funcaoAplica x funcao = funcao x 


somaUm :: Int -> Int
somaUm x = x + 1



data Pessoa = Fisica{nome::String, dinheiro::Float} | Juridica{nome::String,dinheiro::Float} deriving Show


pagamento :: Float -> Pessoa -> Pessoa
pagamento pagamento (Fisica nome valor ) = Fisica nome pagamento
pagamento pagamento (Juridica nome valor ) = Juridica nome pagamento


pessoas = [Fisica "Fulano" 0,Juridica "Beltrano" 0, Fisica "Cicrano" 0]


ehFisica :: Pessoa -> Bool
ehFisica (Fisica _ _) = True
ehFisica _  = False

-- Sintaxe de funcao 

data TipoPessoa = Crianca | Adolescente | Adulto 
    | AdolescenteOuCrianca
    deriving Show

{-}
verificaIdade :: Int -> TipoPessoa
verificaIdade idade 
    | ehCrianca idade = Crianca
    | ehAdolescente = Adolescente
    | ehAdolescente || ehCrianca idade =            AdolescenteOuCrianca
    | otherwise   = Adulto
    where 
        ehCrianca i = i <= 12
        ehAdolescente = idade <= 17
-}


reverter :: String -> [Char]
reverter [] = [] -- condicao de parada
reverter (x:xs) = reverter xs ++ [x]



fib :: Int -> Int
fib 1 = 1
fib 2 = 1
fib valor = fib(valor-1) + fib(valor-2)