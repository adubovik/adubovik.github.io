---
title: Least-strict reverse and reverse state monad
description: Least-strict reverse and reverse state monad
date: Jan 29 2015
---

+++

> {-# language
>    DeriveFunctor
>  , RankNTypes
>  , ImpredicativeTypes
>  , FlexibleInstances
>  , MultiParamTypeClasses
>  #-}

> import Data.Functor
> import Control.Arrow
> import Control.Monad
> import Control.Applicative
> import Control.Exception.Base
> import Control.Monad.State.Class
> import Control.Monad.State
> import Test.QuickCheck
> import System.Timeout

+++

Least-strictness
----------------

Недавно прочитал статью [StrictCheck: a Tool for Testing Whether a
Function is Unnecessarily Strict][strict-check-paper], которая
интересна тем, что вводит и формально определяет свойство
_least-strictness_ для функций.

Неформально функция _least-strict_, если она настолько ленива,
насколько это возможно.

В статье, в частности, утверждается, что знакомая всем функция
`reverse` из [Data.List][data-list-reverse] не является _least-strict_.

Действительно, следующее выражение расходится для стандартной
реализации `reverse`:

~~~
*Main> print . show . length . take 3 . reverse $ [1..]
Hungs...Zzzz...
~~~

Завершается ошибкой и более реалистичное выражение:

~~~
*Main> print . show . length . take 3 . reverse $ (1:2:3:4:error "Ooops!")
"*** Exception: Ooops!
~~~

Дело в том, что `reverse` - это _tail-recursive_ функция:

~~~
-- | 'reverse' @xs@ returns the elements of @xs@ in reverse order.
-- @xs@ must be finite.
reverse :: [a] -> [a]
reverse l =  rev l []
  where
    rev []     a = a
    rev (x:xs) a = rev xs (x:a)
~~~

и пока функция `rev` не достигнет конца списка, невозможно будет
сопоставить с образцом `(:)` результат `reverse`. Т.е. приведение
развернутого списка к WHNF форсит все конструкторы списка в оригинальном.

Но стандартный `reverse` и не претендует на _least-strictness_.
На это указывает и его спецификация в документации.

Давайте всё-таки попробуем сделать функцию обращения более ленивой.
Хотя это и выглядит бесполезным, но, возможно, чему-нибудь научит.

Blueprint reverse
-----------------

Как насчет того, чтобы взять структуру списка (_spine_) оригинального
списка и вложить в него значения из обернутого списка?
Тогда для того чтобы зафорсить первый `(:)` конструктор в развернутом
списке потребуется только зафорсить первый `(:)` конструктор в
оригинальном списке, для второго - второй и т.д.

Ок, зипнем оригинальный список и развернутый и оставим елементы
только развернутого.

> reverseZip :: [a] -> [a]
> reverseZip xs = zipWith (\_x rx -> rx) xs (reverse xs)

Однако, это не сработает, потому что `zipWith` слишком строг:

~~~
zipWith :: (a->b->c) -> [a]->[b]->[c]
zipWith f (a:as) (b:bs) = f a b : zipWith f as bs
zipWith _ _      _      = []
~~~

В первом уравнении сопоставляется с `(:)` оба списка.
Т.е. лучше не стало - мы по-прежнему смотрим на конструктор
развернутого списка.

`zipWith` так написан, чтобы обеспечить логику отсечения хвоста
того списка, который длиннее другого.

Но если положить, что длина первого списка не больше длины второго,
то можно сделать `zipWith` ленивым по конструкторам второго списка:

> zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
> zipWith' f (a:as) ~(b:bs) = f a b : zipWith' f as bs
> zipWith' _ []      _      = []

Тогда `reverseZip'` будет действительно _least-strict_:

> reverseZip' :: [a] -> [a]
> reverseZip' xs = zipWith' (\_x rx -> rx) xs (reverse xs)

Такой приём подмены структуры (_spine_) значения более ленивой
структурой называется [blueprint technique][blue-print] и может
использоваться для вполне практичных целей типа получения **максимального**
элемента списка за _O(n)_ с помощью функции быстрой сортировки.

Tying the knot reverse
----------------------

Можно модифицировать стандартный `reverse` так, что помимо аккумулятора
(который накапливает развернутый список) и остатка оригинального списка
он будет принимать и сам развернутый список. Из этого списка мы достаём
значения и складываем в структуру списока, которую уже можно лениво
рассматривать - `first (rx:) <rest of the computation>`.

> reverseKnot :: [a] -> [a]
> reverseKnot ls = res'
>   where
>     (res',res) = go ls res []
>
>     go []     ~[]       acc = ([],acc)
>     go (x:xs) ~(rx:rxs) acc = 
>       first (rx:) $ go xs rxs (x:acc)

Reverse state monad reverse
---------------------------

Монада _Reverse state_ позволяет скрыть за монадическим интерфейсом
классический пример _tying the knot_, когда функция принимает как
аргумент то, что сама и вычисляет.

Обычно, это реализуется с помощью оператора неподвижной точки:

> fix :: (a -> a) -> a
> fix f = let x = f x in x

~~~
*Main> take 10 $ fix (\fibs -> 1 : 1 : zipWith (+) fibs (tail fibs))
[1,1,2,3,5,8,13,21,34,55]
~~~

В _Reverse state monad_ варианте `x` становится состоянием этой монады:

> fix' :: (a -> a) -> a
> fix' f = flip execRState undefined $ do
>   x <- get
>   put (f x)

~~~
*Main> take 10 $ fix' (\fibs -> 1 : 1 : zipWith (+) fibs (tail fibs))
[1,1,2,3,5,8,13,21,34,55]
~~~

где `execRState :: RState s a -> s -> s` запускает монаду и вычисляет
состояние по аналогии с обыкновенным [execState][exec-state].

Отличие от стандартной _State monad_ состоит в том, что все операции,
связанные с состоянием, такие как `get`, `put`, `gets`, `modify`,
выполняются в обратном порядке. Т.е. не сверху вниз, если смотреть по
коду, а снизу вверх.

Получается, что сначала в состояние помещается `f x` с помощью `put`,
а потом оно извлекается и связывается с переменной `x` с помощью `get`,
что и эквивалентно уравнению

~~~
x = f x
~~~

В `execRState` передаётся `undefined` как начальное значение состояния,
потому что неважно какое оно будет, всё равно потом перезатрётся командой
`put (f x)`.

<p class="bg-info">
См. также [более изощренный способ вычисления чисел Фибоначчи][lukepalmer-mindfuck]
и реализацию обратной монады состояния в пакете [Tardis][tardis].
</p>

`(m >>= f)` для _Reverse state monad_ завязывает
рекурсивный узел для того, чтобы протянуть состояние от `f` к `m`, а не
наоборот, как это делается в _State monad_.

> newtype RState s a = RState { runRState :: s -> (a,s) }
>   deriving Functor
>
> instance Monad (RState s) where
>   return a = RState $ \s -> (a,s)
>   m >>= f  = RState $ \s -> 
>     let (a,s'') = runRState m     s'
>         (b,s' ) = runRState (f a) s
>     in  (b,s'')

+++

> execRState :: RState s a -> s -> s
> execRState s i = snd $ runRState s i
> 
> evalRState :: RState s a -> s -> a
> evalRState s i = fst $ runRState s i

+++

Кстати, этот паттерн обратного применения эффектов в монаде (сначала `f`,
потом `m`) можно реализовать в трансформере [ReverseT][reverseT] так,
что `ReverseT (State s)` будет эквивалентен `RState s`.

+++

> instance Applicative (RState s) where
>   pure = return
>   (<*>) = liftM2 ($)

> instance MonadState s (RState s) where
>   state = RState

+++

Вернёмся к обращению списка.

Обращение списка можно реализовать и с помощью стандартной монады
состояния, но оно будет по-прежнему _tail-recursive_ и потому
недостаточно ленивым:

> reverseState :: [a] -> [a]
> reverseState xs = evalState (go []) xs
>   where
>     go acc = do
>       ls <- get
>       case ls of
>         [] -> return acc
>         _  -> do
>           h <- gets head
>           modify tail
>           go (h:acc)

Проследим изменение аккумулятора и состояния для
`reverseState [1,2,3,4]`:

~~~
 state: [1,2,3,4] [2,3,4] [3,4]     [4]        []
   acc:        []     [1] [2,1] [3,2,1] [4,3,2,1]
result:                                 [4,3,2,1]
~~~

Если обратить эту последовательность изменений состояний с помощью
_Reverse state monad_, то можно возвращать очередной элемент
обращенной последовательности вне рекуривного вызова `go`, сделав
его тем самым более ленивым:

> reverseRState :: [a] -> [a]
> reverseRState xs = evalRState (go xs) undefined
>   where
>     go [] = do
>       put xs
>       return []
>     go (_:ys) = do
>       modify tail
>       h <- gets head
>       (h:) <$> go ys

что и требовалось получить.

+++

> type Reverse = forall a. [a] -> [a]

> propRev1 :: Reverse -> [Int] -> Bool
> propRev1 f xs = xs == f (f xs)

> propRev2 :: Reverse -> [Int] -> Bool
> propRev2 f xs = reverse xs == f xs

> checkProps :: Reverse -> String -> IO ()
> checkProps rev msg = do
>   putStrLn $ "Quick checking `" ++ msg ++ "`..."
>   quickCheck $ propRev1 rev
>   quickCheck $ propRev2 rev

> checkLazynessInf :: Reverse -> String -> IO ()
> checkLazynessInf f msg = do
>   putStrLn $ "Check lazyness infinity `" ++ msg ++ "`..."
>   let waitASecond = timeout 100000
>   len <- waitASecond . evaluate . length . take 3 . f $ [(1::Int)..]
>   case len of
>     Just _  -> putStrLn "PASSED"
>     Nothing -> putStrLn "FAILED"

> checkLazynessErr :: Reverse -> String -> IO ()
> checkLazynessErr f msg = do
>   putStrLn $ "Check lazyness error `" ++ msg ++ "`..."
>   len <- try . evaluate .
>          length . take 3 . f $
>          ((1::Int):2:3:4:error "Ooops!")
>   case len of
>     Right{}              -> putStrLn "PASSED"
>     Left SomeException{} -> putStrLn "FAILED"

> main :: IO ()
> main = do
>   let revs :: [(Reverse, String)]
>       revs = [ (reverse      , "Data.List reverse")
>              , (reverseZip   , "Naive blueprint zip reverse")
>              , (reverseZip'  , "Proper blueprint zip reverse")
>              , (reverseKnot  , "Tying the knot reverse")
>              , (reverseRState, "Reverse state monad reverse")
>              ]
> 
>   forM_ revs $ uncurry checkProps
>   
>   -- NOTE: be careful with multiple runs of this
>   -- lazyness check into the ghci.
>   -- For some reason ghci doesn't dispose memory
>   -- which was allocated by `reverse` and `reverseZip`
>   -- calls. Thus you may end up with "out of memory" error
>   -- after a dozen of `main` runs in ghci.
>   forM_ revs $ uncurry checkLazynessInf
>   forM_ revs $ uncurry checkLazynessErr

+++

[strict-check-paper]: http://www.cs.kent.ac.uk/pubs/2011/3134/content.pdf
[data-list-reverse]: http://hackage.haskell.org/package/base-4.7.0.2/docs/src/GHC-List.html#reverse
[blue-print]: http://www.reddit.com/r/haskell/comments/2tfapq/the_blueprint_technique/
[exec-state]: http://hackage.haskell.org/package/transformers-0.4.2.0/docs/src/Control-Monad-Trans-State-Lazy.html#execState
[lukepalmer-mindfuck]: https://lukepalmer.wordpress.com/2008/08/10/mindfuck-the-reverse-state-monad/
[reverseT]: http://martijn.van.steenbergen.nl/journal/2010/11/19/the-reverset-monad-transformer/
[tardis]: https://hackage.haskell.org/package/tardis-0.3.0.0
