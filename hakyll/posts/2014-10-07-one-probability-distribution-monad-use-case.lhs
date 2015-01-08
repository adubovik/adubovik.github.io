---
title: MergeSort and probability distribution monad
description: MergeSort and probability distribution monad
date: Oct 7, 2014
---

> {-# language
>   ScopedTypeVariables
> , TupleSections
> , RankNTypes
> , FlexibleInstances
> , GADTs
> #-}

> module Main where

> import qualified System.Random as Random
> import Control.Arrow
> import Control.Monad
> import Control.Applicative
> import qualified Data.Map as Map

Недавно [\@MAH3IOK][manzyuk_twitter] поделился со мной интересным функциональным
алгоритмом генерации случайной перестановки списка.
Алгоритм примечательным образом схож с алгоритмом сортировки
слиянием (на котором, кстати, основан `sort` из [Data.List][data.list])
и заключается в следующем:

1. исходный список превращается в список одноэлементных списков (`map (:[])`)
2. смежные списки в получившемся списке сливаются (функция `merge`) -
   получается список списков в два раза меньшей длины
3. и так до тех пор, пока не остается один список (функция `tillSingletonM`) - он и будет случайной перестановкой.

> randomShuffle :: forall m a. (Functor m, Random m) => [a] -> m [a]
> randomShuffle = tillSingletonM step . map (:[])
>  where
>    step :: [[a]] -> m [[a]]
>    step [] = return []
>    step [x] = return [x]
>    step (x:y:rest) =
>      liftM2 (:) (merge x y) (step rest)

Два списка сливаются так, чтобы в результате получился список,
являющийся случайным перемежением элементов исходных списков.
Относительный порядок между элементами исходных списков сохраняется.

>    merge :: [a] -> [a] -> m [a]
>    merge x y = runMerge (length x) (length y) x y

Голова результирующего списка выбирается случайным образом из голов
исходных списков с вероятностью, определяемой длинами списков.
Чем длиннее один список относительно другого, тем вероятнее, что его
голова станет головой объединенного списка.

>    runMerge :: Int -> Int -> [a] -> [a] -> m [a]
>    runMerge nx ny x y
>      | nx == 0 = return y
>      | ny == 0 = return x
>      | xh:xt <- x
>      , yh:yt <- y
>      = do
>        rnd <- random (1, nx+ny)
>        if rnd <= nx
>        then (xh:) <$> runMerge (nx-1) ny xt y
>        else (yh:) <$> runMerge  nx (ny-1) x yt

> tillSingletonM :: Monad m => ([a] -> m [a]) -> [a] -> m a
> tillSingletonM f ls
>   | [x] <- ls = return x
>   | otherwise = (f >=> tillSingletonM f) ls

Сложность в точности как у merge sort - _O(n\*log(n))_.
Функция работает в монаде, поддерживающей интерфейс работы со случайными числами:

> class Monad m => Random m where
>   random :: (Int,Int) -> m Int

Для IO монады она определяется через `randomRIO` из `System.Random`:

> instance Random IO where
>   random = Random.randomRIO

Посмотрим, что же генерирует алгоритм:

~~~
*Main> replicateM_ 5 $ randomShuffle [1..5] >>= print
[2,5,4,3,1]
[4,3,2,1,5]
[1,2,5,4,3]
[4,5,2,1,3]
[5,1,4,2,3]
~~~

Что ж, весьма похоже на случайные перестановки!

Но как убедиться в том, что каждая из _n!_ перестановок имеет
одну и ту же вероятность выпасть, равную _1/n!_ ?
Из алгоритма это не сразу очевидно и доказательство требует некоторых
умственных усилий, которых нам, конечно, не хотелось бы прилагать.

Можно сгенерировать много перестановок подряд и посмотреть, сколько
раз выпала каждая перестановка.
По закону больших чисел при всё большем количестве сгенерированных
перестановок, мы должны всё ближе приближаться к теоретической вероятности.

> calcProb :: Int -> Int -> IO ()
> calcProb m n = do
>   permutations <- replicateM n (randomShuffle [1..m])
>   let hist = Map.toList
>            . Map.unionsWith (+)
>            . map (flip Map.singleton 1)
>            $ permutations
>       toProb x = fromIntegral x / fromIntegral n
>   forM_ hist $ \(p,r) ->
>     putStrLn $ show p ++ " " ++ show (toProb r)

~~~
*Main> calcProb 4 10^6
[1,2,3,4] 4.1701e-2
[1,2,4,3] 4.1917e-2
[1,3,2,4] 4.1909e-2
[1,3,4,2] 4.1402e-2
[1,4,2,3] 4.1642e-2
[1,4,3,2] 4.1845e-2
[2,1,3,4] 4.1471e-2
[2,1,4,3] 4.1743e-2
[2,3,1,4] 4.1639e-2
[2,3,4,1] 4.1831e-2
[2,4,1,3] 4.1157e-2
[2,4,3,1] 4.1859e-2
[3,1,2,4] 4.141e-2
[3,1,4,2] 4.1428e-2
[3,2,1,4] 4.1654e-2
[3,2,4,1] 4.1821e-2
[3,4,1,2] 4.1729e-2
[3,4,2,1] 4.1764e-2
[4,1,2,3] 4.1572e-2
[4,1,3,2] 4.1393e-2
[4,2,1,3] 4.1909e-2
[4,2,3,1] 4.1654e-2
[4,3,1,2] 4.1812e-2
[4,3,2,1] 4.1738e-2
*Main> 1/24.0
4.1666666666666664e-2
~~~

Выглядит неплохо.
Но сравните с ожидаемой вероятностью - погрешность всё еще во втором знаке
после запятой, и это с миллионом перестановок всего лишь четырехэлементного списка!
Неприемлимо.

Ок... Как насчет того, чтобы в моменте генерации случайного числа разделиться
на две вселенные, в одной из которых выпала решка, а в другой - орел?
При этом мы постраемся следить за накапливаемой вероятностью, т.е. вероятность
выпадения решки будет домножена на вероятности последующих событий в этой
вселенной (в точности по формуле условной вероятности).
Тогда мы сможем получить не просто одну случайную перестановку, а распределение
случайной перестановки, т.е. список возможных перестановок с вероятностью их генерации.

И именно так работает вероятностная монада
(см. например [Functional Pearls: Probabilistic Functional Programming in Haskell][erwig])!

Определим тип объекта, обозначающего распределение:

> newtype DistBase a = DistBase { runDistBase :: Map.Map a Rational }

> instance Show a => Show (DistBase a) where
>   show (DistBase m) =
>     unlines $
>     map (\(x,y) -> show x ++ ": " ++ show y) $
>     Map.toList m

Попытаемся определить реализацию `Monad` и `Random` для `DistBase`, ведь нам
хотелось бы оставить функцию `randomShuffle` без изменений и получить
распределение случайной перестановки, просто вычислив функцию в монаде `DistBase`.

> returnDistBase :: a -> DistBase a
> returnDistBase a = DistBase $ Map.singleton a 1

> bindDistBase :: Ord b => DistBase a -> (a -> DistBase b) -> DistBase b
> bindDistBase d f =
>     DistBase $
>       Map.unionsWith (+) $
>         map (scale . first f) $
>           Map.toList $
>            runDistBase d
>     where
>       scale (dist,factor) = Map.map (*factor) $
>                             runDistBase dist

~~~
instance Monad DistBase where
  return = returnDistBase
  (>>=) = bindDistBase
~~~

Но не тут-то было, это невозможно!
На возвращаемое в монаде значение должно быть наложено ограничение
`Ord b` - `Data.Map` требует его для постраения дерева поиска.
В свою очередь, класс типов `Monad` не поддерживает никаких ораничений,
переменные типа `a` и `b` в операторе (`>>= :: m a -> (a -> m b) -> m b`)
должны быть полиморфными, ни больше ни меньше.
Подозреваю, что отчасти поэтому эта замечательная монада малоизвестна
(как минимум, я не видел её ни в одном _Haskell monad tutorial_).

Это известная в Haskell мире проблема называется *constrained monad problem*
или *restricted monad*.
Решена она может быть многими способами (которые легко гуглятся по
этим ключевым словам), но наиболее простым и понятным мне показался подход,
использованный в пакете [set-monad]
[set-monad]:

> data Dist a where
>   Prim :: DistBase a -> Dist a
>   Return :: a -> Dist a
>   Bind :: Dist a -> (a -> Dist b) -> Dist b

> instance Monad Dist where
>   return = Return
>   (>>=) = Bind

> instance Functor Dist where
>   fmap f x = x >>= (return . f)

> instance Applicative Dist where
>   pure = return
>   (<*>) = ap

> runDist :: Ord a => Dist a -> DistBase a
> runDist (Prim d) = d
> runDist (Return a) = returnDistBase a
> runDist (Bind (Prim d) f) = bindDistBase d (runDist . f)
> runDist (Bind (Return a) f) = runDist $ f a
> runDist (Bind (Bind d g) f) = runDist $ Bind d $ \a -> Bind (g a) f

`random` в монаде `Dist` выдает все числа в заданном диапазоне с
равной вероятностью:

> instance Random Dist where
>   random (lo,hi) =
>     let rangeSz = fromIntegral $ hi-lo+1 in
>     Prim $
>       DistBase $
>         Map.fromList $
>           map (,1/rangeSz) $
>             [lo..hi]

Посмотрим на распределение случайной перестановки списка длины 4:

~~~
*Main> print $ runDist (randomShuffle [1..4])
[1,2,3,4]: 1 % 24
[1,2,4,3]: 1 % 24
[1,3,2,4]: 1 % 24
[1,3,4,2]: 1 % 24
[1,4,2,3]: 1 % 24
[1,4,3,2]: 1 % 24
[2,1,3,4]: 1 % 24
[2,1,4,3]: 1 % 24
[2,3,1,4]: 1 % 24
[2,3,4,1]: 1 % 24
[2,4,1,3]: 1 % 24
[2,4,3,1]: 1 % 24
[3,1,2,4]: 1 % 24
[3,1,4,2]: 1 % 24
[3,2,1,4]: 1 % 24
[3,2,4,1]: 1 % 24
[3,4,1,2]: 1 % 24
[3,4,2,1]: 1 % 24
[4,1,2,3]: 1 % 24
[4,1,3,2]: 1 % 24
[4,2,1,3]: 1 % 24
[4,2,3,1]: 1 % 24
[4,3,1,2]: 1 % 24
[4,3,2,1]: 1 % 24
~~~

Да! Это именно то, что мы и ожидали. Алгоритм выдает равновероятную
случайную перестановку из 24 (4!) возможных.
И хотя мы можем быть уверенными в равномерности распределения только
для ограниченного множества списков (_n!_ растет черезчур быстро),
этого достаточно, чтобы убедить меня в корректности алгоритма.

> main :: IO ()
> main = do
>   let ls = [1..4]
>       n = 5
>
>   putStrLn "Random permutations:"
>   replicateM_ n $
>     randomShuffle ls >>= print
>
>   putStrLn "Distribution of the random permutation:"
>   print $ runDist (randomShuffle ls)

[manzyuk_twitter]: https://twitter.com/mah3iok
[data.list]: http://hackage.haskell.org/package/base-4.7.0.1/docs/src/Data-List.html#sort
[erwig]: http://web.engr.oregonstate.edu/~erwig/papers/PFP_JFP06.pdf
[set-monad]: http://hackage.haskell.org/package/set-monad-0.1.0.0/docs/src/Data-Set-Monad.html#Set
