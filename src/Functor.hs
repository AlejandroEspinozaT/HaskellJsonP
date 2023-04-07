module Functor where
    
import Control.Applicative (liftA2)
import Control.Applicative (Alternative, empty, (<|>))

suma :: Int -> Int -> Int
suma x y = x + y

a :: Maybe Int
a = Just 2
b :: Maybe Int
b = Just 3
c :: Maybe Int
c = Just 2
d :: Maybe Int
d = Nothing

duplicar :: Int -> Int
duplicar x = x * 2

numeros :: [Int]
numeros = [1, 2, 3, 4, 5]

sumaEncapsulada :: Maybe (Int -> Int -> Int)
sumaEncapsulada = pure suma

resultadoAlternative :: Maybe Int
resultadoAlternative = a <|> b

resultadoApplicative :: Maybe Int
resultadoApplicative = sumaEncapsulada <*> a <*> b

resultadoFunctor :: [Int]
resultadoFunctor = fmap duplicar numeros
{-Research
    En Haskell los tipos de datos Functor, Applicative y Alternative son 
    typeclasses que proporcionan una abstracción sobre diferentes tipos de datos 
    que pueden ser combinados de manera genérica.

    -La clase Functor permite aplicar una función a los valores encapsulados 
     en un tipo de datos, usando la función fmap. (fmap :: Functor f => (a -> b) -> f a -> f b)
    -La clase Applicative extiende Functor y permite combinar valores encapsulados 
     en varios tipos de datos aplicando funciones también encapsuladas en esos tipos
     usando la función pure para introducir valores en el contexto y (<*>) para 
     combinar valores aplicando funciones encapsuladas.
    -La clase Alternative extiende Applicative y proporciona una manera de elegir 
     entre diferentes opciones encapsuladas, usando las funciones empty para 
     representar una opción vacía y (<|>) para elegir entre dos opciones.

    Estas clases de tipos proporcionan una abstracción muy poderosa para 
    trabajar con diferentes tipos de datos, como listas, Maybe o Either 
    permitiendo escribir código genérico y modular que es fácil de entender y mantener.
-}


