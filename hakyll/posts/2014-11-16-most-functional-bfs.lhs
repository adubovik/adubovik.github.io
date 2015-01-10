---
title: Most functional BFS
description: Most functional BFS
date: Nov 16, 2014
---

+++

> {-# language
>    RankNTypes
>  , RecordWildCards
>  , DeriveFunctor
>  #-}
> 
> import Data.List
> import Control.Monad

+++

Недавно мы с коллегами в ожидании митинга обсуждали интересную
реализацию алгоритма обхода графа в ширину, которая была описана в блогпосте [Purely Functional: Graph Search Algorithms][purely-functional-graph-search-algos].

Самым интересным в описанном алгоритме было то, что он непохож
на стандартные реализации bfs, которые используют очередь,
калькируя bfs в императивном сеттинге.

Граф представляется следующим типом:

> data Graph a = Node a [Graph a] deriving (Functor, Show)
> type Path a = [a]

Каждая вершина уникально идентифицируется значением типа `a`.
Путь в графе обозначается списком таких значений.
Из графа предварительно удаляются все циклы, так что в последующих
рассуждениях мы будет рассматривать только ацикличные графы. 

Алгоритм из блогпоста возвращает последовательность путей от
корневой вершины дерева до каждой вершины графа в порядке обхода в ширину:

> bfsOrig :: Graph a -> [Path a]
> bfsOrig (Node x xs) =
   
Сперва вычисляется рекурсивно множество путей для детей данной
вершины `kidsPaths :: [[Path a]]`,

>   let kidsPaths = map bfsOrig xs in

затем списки путей в `kidsPaths` объединяются таким образом,
что пути следуют в порядке неубывания длины пути.

>   ([x]:) . map (x:) $ foldr mergeRanked [] kidsPaths

С помощью `mergeRanked` можно объединить два списка путей,
упорядоченных в порядке неубывания длины, в список с таким же свойством.
`mergeRanked` проверяет длины путей, стоящих в голове списков,
и ставит вперед путь с меньшей длиной.

>   where      
>     mergeRanked :: [Path a] -> [Path a] -> [Path a]
>     mergeRanked [] ys = ys
>     mergeRanked xs [] = xs
>     mergeRanked (x:xs) (y:ys) =
>       if length x < length y
>       then x : mergeRanked xs (y:ys)
>       else y : mergeRanked (x:xs) ys

Во время обсуждения алгоритма мы с коллегами пришли в выводу,
что этот алгоритм чересчур строг, за счет того, что использует `length`
для вычисления длины пути, а это значит, что в дереве, отличном от
линейной последовательности вершин, нам придётся вычислить как минимум
глубину left-most листа дерева.

Рассуждения эти оказались неверны - алгоритм лениво вычисляет
bfs пути даже в бесконечном дереве.

Определим полное бинарное дерево высоты n:

> binTree :: Int -> Graph Int
> binTree height = go 1 1
>   where
>     go num depth =
>       Node num [ go (2*num+add) (depth+1)
>                | add <- [0,1]
>                , depth < height || height==0
>                ]
> 
> binTreeInf :: Graph Int
> binTreeInf = binTree 0

Проверим, что бесконечное дерево обрабатывается корректно:

~~~
*Main> take 3 $ bfsOrig $ binTreeInf
[[1],[1,3],[1,2]]
~~~

Indeed so!

Какая жалость, ведь я начинал писать этот текст, будучи уверенным,
что алгоритм не работает на бесконечных деревьях.
Тем не менее, моему алгоритму, который был придуман как более
ленивая версия оригинального, есть чем вас позабавить.

Я избавился от того, что мне изначально не понравилось в оригинальном
алгоритме - вычисление (и последующее неоднократное перевычисление) `length`.
Конечно, можно было бы кешировать длину и увеличивать на единицу
каждый раз как путь удлинялся, но это только усложнило бы решение.

> bfsPaths :: Graph a -> [Path a]
> bfsPaths = concat . go []
>   where

Вместо этого, новый алгоритм оперирует последовательностью путей,
разбитых на классы эквивалентности по длине пути.
`go` принимает путь от корня дерева до текущей вершины `x`, сам граф
и возвращает список типа `[[Path a]]`:

~~~
[<множество путей из корня, проходящих через 'x', длины (length path + 1)>
[<множество путей из корня, проходящих через 'x', длины (length path + 2)>
[<множество путей из корня, проходящих через 'x', длины (length path + 3)>
...]
~~~

Т.е. теперь длина пути кодируется позицией в списке, и поэтому нет нужды
явно вычислять длину.

>     go :: Path a -> Graph a -> [[Path a]]
>     go path (Node x xs) =
>       let path' = x:path

Чтобы посчитать пути до вершин, достижимых из `x`, достаточно рекурсивно
вычислить пути для детей `kidsPaths`,

>           kidsPaths = map (go path') xs

затем поэлементно объединить их через `concat` и добавить путь до `x`:

>       in  [path'] : zipWithN concat kidsPaths
> 
>     zipWithN :: ([a] -> b) -> [Path a] -> [b]
>     zipWithN f = map f . transpose

Этот алгоритм обладает тремя замечательными качествами:

1. эффективность в плане потребления памяти
2. ленивость
3. простота реализации и Haskell идиоматичность
    - не используется очередь
    - алгоритм рекурсивен
    - никаких `length`

Память
------

Сколько памяти займет результирующий список путей, вычисленный `bfsPaths`?

Каждый вызов функции `go` аллоцирует константное количество конструкторов списка:

1. один `(:)` в (`x:path`)
2. два `(:)` и один `[]` в (`[path'] :`)

`map f` аллоцирует новых конструкторов столько же, сколько будет
деаллоцированно сборщиком мусора, при условии, что список, к которому
применяется `map f`, больше никому не потребуется.
С `transpose` баланс может сместиться немного в сторону
аллоцированных конструкторов, но он позже восстанавливается `concat`.

Получается, что множество всех путей будет занимать в памяти
всего _O(|V|)_ памяти, где _|V|_ - количество вершин в графе!

Это немного контринтуитивно, ведь если сложить длины всех _|V|_ путей
получится _O(|V|\*log2|V|)_. А ведь это только оценка для полного бинарного
дерева (доказательство оставляю читателю).

Для вырожденого линейного графа оценка будет и вовсе _O(|V|^2)_!

> pathsSizeBin :: Int -> Int
> pathsSizeBin n = sum . map length . bfsPaths $ binTree n
> 
> linTree :: Int -> Graph Int
> linTree n = foldr (\i -> Node i . (:[])) (Node n []) [1..n-1]
> 
> pathsSizeLin :: Int -> Int
> pathsSizeLin n = sum . map length . bfsPaths $ linTree n

Выигрыш по памяти достигается за счет sharing. Например два пути:

~~~
[4,3,2,1]
[7,6,2,1]
~~~

будут представлены в памяти не 8 `(:)` ячейками, а только 6-ю,
потому что часть `[2,1]` у обоих путей общая.
Как раз для достижения такого обобществления суффиксов путей было
выбрано представление пути от вершины до корня дерева,
а не наоборот (как в алгоритм по ссылке).

Ленивость
---------

Простейший способ узнать насколько строга/ленива функция,
обрабатывающая какое-нибудь дерево - это запустить её на бесконечном
дереве и зафорсить небольшую часть того, что вычислила функция.
В случаем, если функция чересчур строга, ghc просто зависнет,
пытаясь обработать бесконечное дерево.

Даже если функция никогда не будет вызываться на бесконечных деревьях,
она будет форсить дерево до самых листьев, что на больших деревьях
может быть неприемлимо - алгоритм завершит свою работу, но за большее
время, чем его более ленивый аналог.

~~~
*Main> take 3 $ bfsPaths $ binTreeInf
[[1],[2,1],[3,1]]
~~~

Больше статических гарантий
---------------------------

Пожертвовав ясностью кода, с помощью системы типов Haskell можно
гаранировать, что в последовательности путей, которую вычисляет
`bfsPaths`, пути будут следовать в порядке неубывания длины пути.

Можно достичь этого с помощью _GADTs_ или _nested datatype_.
Я рассмотрю второй вариант.

> data List f a as = Cons (f (a,as)) (List f a (a,as)) | Nil
> type PathSet a = List [] a ()

Тип `PathSet a` изоморфен `[[Path a]]` с одной оговоркой - системой
типов гаранитруется, что первый элемент этого списка содержит все
пути длины 1 (ни больше, ни меньше), второй элемент - все пути
длины 2 и т.д. Такого типа значение возвращает `bfs`.

> newtype LL a = LL { unLL :: [[a]] }

Увы, пришлось ввести `LL` обертку, потому что нельзя уточнить
тип `List f a as` типом `List [[]] a as`, приходится работать с `List LL a as`.

> onUnderlying :: (forall x. f x -> g x) -> List f a as -> List g a as
> onUnderlying t (Cons fa rest) = Cons (t fa) (onUnderlying t rest)
> onUnderlying _ Nil = Nil
> 
> bfsPaths' :: Graph a -> [Path a]
> bfsPaths' = concat . lower . go ()
>   where
>     go :: as -> Graph a -> List [] a as
>     go path (Node x xs) =
>       let path' = (x,path)
>           kidsPaths = map (go path') xs
>       in  Cons [path'] (zipAndConcat kidsPaths)
> 
>     zipAndConcat :: [List [] a as] -> List [] a as
>     zipAndConcat = onUnderlying (concat . unLL) . transpose'

Определение `transpose'` повторяет [transpose][transpose] из _Data.List_ с очевидными измениями.

>     transpose' :: [List [] a as] -> List LL a as
>     transpose' [] = Nil
>     transpose' (Nil : xss) = transpose' xss
>     transpose' (Cons x xs : xss) =
>       let h = LL $ x : [h | Cons h _ <- xss]
>           t = transpose' (xs : [ t | Cons _ t <- xss])
>       in  Cons h t
>
> lower :: forall f a. Functor f => List f a () -> [f (Path a)]
> lower = go (\a () -> a:[])
>   where
>     go :: Functor f => (a -> as -> [a]) -> List f a as -> [f (Path a)]
>     go _ Nil = []
>     go toList (Cons ls rest) =
>       let toList' a = (a:) . uncurry toList
>       in  fmap (uncurry toList) ls : go toList' rest

~~~
*Main> take 3 $ bfsPaths' $ binTreeInf
[[1],[2,1],[3,1]]
~~~

Яху, работает!

UPD 28 Dec 2014
---------------

Разделил задачи построения пути от корня дерева до вершины и собственно
обход дерева в порядке поиска в ширину на две функции:

> pathsGraph :: Graph a -> Graph (Path a)
> pathsGraph = descend []
>   where
>     descend path (Node a gs) =
>       let path' = a:path
>       in  Node path' (map (descend path') gs)

и

> levelOrder :: Graph a -> [Graph a]
> levelOrder g = go [g]
>   where
>     go [] = []
>     go gs = gs ++ go (concatMap children gs)

соответственно.

> get :: Graph a -> a
> get (Node a _) = a
>
> children :: Graph a -> [Graph a]
> children (Node _ gs) = gs

`bfsPaths''` получается их композицией и извлечением корневого
пути из каждого дерева:

> bfsPaths'' :: Graph a -> [Path a]
> bfsPaths'' = map get . levelOrder . pathsGraph

~~~
*Main> take 3 $ bfsPaths'' $ binTreeInf
[[1],[2,1],[3,1]]
~~~

+++

> main :: IO ()
> main = do
>   let algos = [bfsOrig, bfsPaths, bfsPaths', bfsPaths'']
>   forM_ algos $ \algo -> 
>     print . take 3 . algo $ binTreeInf

+++

[purely-functional-graph-search-algos]: http://monadmadness.wordpress.com/2014/11/10/purely-functional-graph-search-algorithms/
[transpose]: http://hackage.haskell.org/package/base-4.7.0.2/docs/src/Data-List.html#transpose
