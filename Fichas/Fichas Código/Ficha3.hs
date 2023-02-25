module Ficha3 where
import Ficha1.hs

data Hora = H Int Int
        deriving Show

type Etapa = (Hora,Hora)
type Viagem = [Etapa]

valEta :: Etapa -> Bool
valEta (H x y) | (isValidHora x) && (isValidHora y) && (isHora x y) = True
               | otherwise = False

valVia :: Viagem -> Bool
valVia ((H a b):(H c d):xs) | valEta(H a b) && (isHora b c) = valVia ((H c d):xs)
                            | otherwise = False

--Tempo Total
tempoPercorrido :: Viagem -> Hora
tempoPercorrido [] = (H 0 0)
tempoPercorrido (etapa:[]) = duraVia [etapa]
tempoPercorrido (etapa1:etapa2:etapas) = 
    let dura = duraVia (etapa1:etapa2:etapas)
        (H h1 m1) = snd etapa1
        (H h2 m2) = fst etapa2
        tempoEntreEtapas = (H (h2 - h1) (m2 - m1))
    in (H (h dura + h tempoEntreEtapas) (m dura + m tempoEntreEtapas))


--Efetiva
duraVia :: Viagem -> Hora
duraVia [] = (H 0 0) 
duraVia etapas = foldl (\(H h m) ((H a b), (H c d)) -> (H (h + c - a) (m + d - b))) (H 0 0) etapas



