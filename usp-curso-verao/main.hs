
somar :: Int -> Int -> Int
somar 1 2 = 4
somar x y = x + y


saudacao :: String -> String
saudacao "Palmeiras" = "Nao tem Mundial"
saudacao nome = "Ola: " ++ nome

foo :: (Int,Int) -> Int
foo (1,x) = 1
foo (x,1) = 2
foo (x,y) = x + y

patternLista :: [Int] -> Int
patternLista [] = 0
patternLista [x] = x
patternLista [x,y] = y
patternLista [_,_,z] = z
patternLista (x:xs) = last xs

{-
data Fruta = Maca Float String | Banana Float String deriving Show
-}


getPreco :: Fruta -> Float
getPreco (Maca p _) = p
getPreco (Banana p _) = p

setPreco :: Float -> Fruta -> Fruta
setPreco novoPreco (Maca _ c ) = Maca novoPreco c
setPreco novoPreco (Banana p c ) = Banana novoPreco c

data TipoTela = LCD{origemDescricao:: String} | LED | CVT | Plasma deriving Show

data Peca = Tela{tamanho::Int,tipo::TipoTela} | Bateria deriving Show

data Celular = Celular{tela::Peca,bateria::Peca} deriving Show


data Sacola = Plastico Fruta | Metal [Fruta]

data Fruta = Maca{peso :: Float, cor :: String} | Banana{peso::Float,cor::String} deriving Show


data Booleano = Verdadeiro | Falso deriving (Show,Eq)


numeroPar :: Int-> Booleano
numeroPar x 
    | even x    = Verdadeiro
    | otherwise = Falso


conversaoDeBooleanoParaBool :: Booleano -> Bool
conversaoDeBooleanoParaBool valor
    | valor == Verdadeiro = True




frutas = [Banana 5.5 "Verde", Maca 2.0 "Vermelho", Maca 3.0 "Azul" ]


--numeroPar x = if even x then Verdadeiro else Falso






-- newtype Foo = Foo 




data Pessoa = Fisica{nome::String, dinheiro::Float} | Juridica{nome::String,dinheiro::Float} deriving Show


pessoas = [Fisica "Fulano",Juridica "Beltrano", Fisica "Cicrano" ]






{-
terceiro :: (a,b,c) -> c
terceiro (x,y,z) = z
-}



