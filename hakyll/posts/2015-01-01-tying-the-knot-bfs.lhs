---
title: Tying the knot BFS
description: Tying the knot BFS
date: Jan 1 2015
---

+++

> {-# language
>    RankNTypes
>  #-}

> module Main where

> import Control.Arrow
> import Debug.Trace

> tracesFirst :: Eq a => a -> a
> tracesFirst x = (x==x) `seq` x

+++

Традиционно алгоритм обхода графа в ширину определяется с помощь очереди.
Сначала в очередь добавляется корневая вершина. Потом из очереди извлекается
вершина, в конец очереди добавляются смежные ей вершины и так пока очередь не пуста.
Извлекаемые из очереди вершины будут как раз идти в порядке обхода в ширину.

Этот алгоритм можно прямолинейно реализовать в Haskell.
Для простоты я ограничился деревьями (для графов нужно поддерживать
список посещенных вершин).

> type Path a = [a]
>
> data Tree a = Tree a [Tree a]
>   deriving (Show, Eq)
>
> get :: Tree a -> a
> get (Tree a _) = a
>
> ch :: Tree a -> [Tree a]
> ch (Tree _ ts) = ts

> levelOrderQueue :: Tree a -> [Tree a]
> levelOrderQueue t = go (enqueue t emptyQueue)
>   where
>     -- Функции для работы с очередью.
>     isEmpty            = null
>     emptyQueue         = []
>     dequeue            = head &&& tail
>     enqueue x          = (++ [x])
>     enqueueMany []     = id
>     enqueueMany (x:xs) = enqueueMany xs . enqueue x
>
>     go q
>       | isEmpty q = []
>       | otherwise
>       = let (t,q') = dequeue q
>         in  t : go (enqueueMany (ch t) q')

`levelOrderQueue` выдает список поддеревьев в порядке обхода в ширину т.е. поуровнево.

Собственно поиск в ширину параметризуется функцией, которая перечисляет
поддеревья в подрядке обхода в ширину (например `levelOrderQueue`), и
предикатом поиска `pred`:

> bfs :: (forall a. Tree a -> [Tree a])
>     -> (Tree (Path a) -> Bool)
>     -> Tree a
>     -> [Path a]
> bfs levelOrder pred = map get
>                     . filter pred
>                     . levelOrder
>                     . pathsTree
>   where
>     pathsTree :: Tree a -> Tree [a]
>     pathsTree = descend []
>       where
>         descend path (Tree a ts) =
>           let path' = a:path
>           in  Tree path' (map (descend path') ts)

Как таковой `bfs` нам не сильно интересен в отличие от
различных вариантов реализации `levelOrder`.

`levelOrderQueue` использует наиболее простую реализацию очереди
через список, которая имеет _O(n)_ сложность для операции вставки
в конец - `enqueue`.

Можно было бы реализовать очередь через два стека - операции
вставки и извлечения из очереди занимали бы амортизированное _O(1)_.
Что хорошо, но хотелось бы настоящей константы.

Добавим в алгоритм отладочную печать очереди:

> levelOrderQueueTrace :: Show a => Tree a -> [Tree a]
> levelOrderQueueTrace t = go (enqueue t emptyQueue)
>   where
>     traceQ s q = trace (s ++ " " ++ (show . map get $ q)) q
>
>     isEmpty            = null
>     emptyQueue         = []
>     enqueue x          = traceQ "enqueue" . (++ [x])
>     dequeue            = head &&& (traceQ "dequeue" . tail)
>     enqueueMany []     = id
>     enqueueMany (x:xs) = enqueueMany xs . enqueue x
>
>     go q
>       | isEmpty q = []
>       | otherwise
>       = let (t,q') = dequeue q
>         in  t : go (enqueueMany (ch t) q')

и рассмотрим, какие значения принимает очередь в ходе выполнения
алгоритма на полном бинарном дереве высоты 3:

~~~
*Main> print $ tracesFirst $ map get $ levelOrderQueueTrace $ binTree 3
enqueue [1]
dequeue []
enqueue [2]
enqueue [2,3]
dequeue [3]
enqueue [3,4]
enqueue [3,4,5]
dequeue [4,5]
enqueue [4,5,6]
enqueue [4,5,6,7]
dequeue [5,6,7]
dequeue [6,7]
dequeue [7]
dequeue []
[1,2,3,4,5,6,7]
~~~

После выравнивания становится очевидно, что очередь принимает
значения некоторого подотрезка списка деревьев в breadth-first порядке.
Начало и конец отрезка смещаются вправо при каждом вызове `dequeue`
и `enqueue` соответственно.

~~~
enqueue [1]
dequeue  []
enqueue   [2]
enqueue   [2,3]
dequeue     [3]
enqueue     [3,4]
enqueue     [3,4,5]
dequeue       [4,5]
enqueue       [4,5,6]
enqueue       [4,5,6,7]
dequeue         [5,6,7]
dequeue           [6,7]
dequeue             [7]
dequeue              []
        [1,2,3,4,5,6,7]
~~~

Так почему бы не совместить результирующий список деревьев и очередь
в один список?

> levelOrderCorecQueue :: Tree a -> [Tree a]
> levelOrderCorecQueue t = queue
>   where
>     queue = t : go queue
>     go [] = []
>     go (Tree _ ts : rest) = ts ++ go rest

Указатели на начало и конец очереди здесь поддерживаются неявно в функции `go`.
В ней аргумент обозначает начало очереди, а результат - конец.
Сопоставление с образцом `(:)` - аналог `dequeue`, `(t:)` - `enqueue`,
`(ts++)` - `enqueueMany ts`.

Что интересно, значение аргумента функции `go` зависит от результата
этой функции за счет _value recursion_ в `queue`.

`levelOrderCorecQueue` корректно выдаст первые `n` поддеревьев,
где `n` - количество вершин в дереве, но после этого ghc просто зависнет.
Сопоставление по образцу `[]` в `go` спровоцирует очередной вызов `go`
и так до бесконечности.

Можно брать ровно столько поддеревьев, сколько их в дереве:

> levelOrderCorecQueue' :: Tree a -> [Tree a]
> levelOrderCorecQueue' t = take (size t)
>                         $ levelOrderCorecQueue t
>
> size :: Tree a -> Int
> size (Tree _ ts) = 1 + sum (map size ts)

Однако, это не будет работать на бесконечных деревьях.
Вычисление их размера займет бесконечно много времени.

Что ж поддержим размер очереди явно:

> levelOrderCorecQueueCount :: Tree a -> [Tree a]
> levelOrderCorecQueueCount t = queue
>   where

Вначале очередь содержит всего один элемент - корневое дерево.

>     queue = t : go 1 queue

Если очередь пуста, алгоритм завершается.
 
>     go 0 _ = []

Иначе из очереди извлекается один элемент _(-1)_ и добавляется _(length ts)_ новых
к _(n)_ уже имеющимся.

>     go n (Tree a ts : rest) = ts ++ go (n - 1 + length ts) rest

Такая очередь называется корекурсивной очередью.
Она описана в отличном туториале [Lloyd Allison’s Corecursive Queues: Why Continuations Matter][why-cont-matter], где также предложен способ оформить
такую очередь в typeclass `MonadQueue` с функциями `enqueue` и `dequeue`.

+++

> sampleTree :: Tree Int
> sampleTree =
>   Tree 1
>     [ Tree 2
>       [ Tree 5 [] ]
>     , Tree 3 []
>     , Tree 4
>       [ Tree 6 [] ]
>     ]

> binTree :: Int -> Tree Int
> binTree d = go d 1
>   where
>     go :: Int -> Int -> Tree Int
>     go 1 n = Tree n []
>     go d n = Tree n [ go (d-1) (2*n)
>                     , go (d-1) (2*n+1)
>                     ]

> infBinTree :: Tree Int
> infBinTree = binTree 0

> main :: IO ()
> main = do
>   let printResult = print . tracesFirst . map get
>
>       finTest msg alg = do
>         putStrLn $ "Finite binary tree: " ++ msg
>         printResult $ alg sampleTree
>
>       infTest msg alg = do
>         putStrLn $ "Infinite binary tree: " ++ msg
>         printResult $ take 15 $ alg infBinTree
>
>   finTest "levelOrderQueue" levelOrderQueue
>   infTest "levelOrderQueue" levelOrderQueue

>   finTest "levelOrderQueueTrace" levelOrderQueueTrace
>   infTest "levelOrderQueueTrace" levelOrderQueueTrace

>   -- finTest "levelOrderCorecQueue" levelOrderCorecQueue  -- hangs
>   infTest "levelOrderCorecQueue" levelOrderCorecQueue

>   finTest "levelOrderCorecQueue'" levelOrderCorecQueue'
>   -- infTest "levelOrderCorecQueue'" levelOrderCorecQueue'  -- hangs

>   finTest "levelOrderCorecQueueCount" levelOrderCorecQueueCount
>   infTest "levelOrderCorecQueueCount" levelOrderCorecQueueCount

+++

[why-cont-matter]: http://www.melding-monads.com/files/CorecQueues.pdf
