import Distribution.Fields.LexerMonad (LexState)

data Lista a = Nulo | a :>: Lista a deriving Show

listaX :: Lista Int     
listaX = 10 :>: (20 :>: ( 30 :>: Nulo))


verificaElemento :: Int -> Lista Int -> Bool
verificaElemento _ Nulo = False
verificaElemento elementoD (elementoL :>: restoLista)
    | elementoL == elementoD = True
    | otherwise = verificaElemento elementoD restoLista

inserirElemento :: Int -> Lista Int -> Lista Int
inserirElemento :: int -> Lista Int -> Lista Int 
inserirElemento novoElemento Nulo = novoElemento :>:



class SimNao a where   
    simnao :: a -> Bool