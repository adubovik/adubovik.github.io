---
title: Lie to me
description: Решение интересной логической задачи
date: Oct 16, 2014
---

> {-# language RecordWildCards #-}

> module LieToMe where

> import Control.Monad
> import Data.List
> import Text.Printf

Недавно услышал такую итересную логическую задачу:

  Есть три компьютера - A, B и С. Компьютеру можно задавать вопросы,
  на которые он отвечает "да" или "нет". Известно, что
  1. Компьютер A всегда говорит правду. Т.е. если спросить у
     него "Верно ли, что 2*2 равно 4?", от ответит "да".
  2. Компьютер B всегда лжет. Т.е. на тот же вопрос он ответит "нет".
  3. Компьютер C может сказать как ложь, так и правду. Он отвечает
     случайным образом, независимо от заданного вопроса, а значит,
     невозможно заранее предсказать, что он ответит.

  Перед вами стоят три компьютера - A, B и C. Вам неизвестно, кто из них кто.
  Единственное, чем они отличаются - это номера на корпусе 1, 2 и 3.
  Нужно выбрать из этих компьютеров либо А, либо B, задав одному
  компьютеру всего один вопрос.
  При этом компьютеры осведомлены о том, кто есть кто среди них троих.

  Какой вопрос нужно задать и как выбрать компьютер после этого?

Откиньтесь на спинку кресла и немного поразмышляйте над задачей.

...

Сдаётесь? Ок, мне она тоже не далась.

На первый взгляд, это одна их тех логических задач, для решения
которой требуется инсайт, некоторая догадка, которую можно очень
долго искать, но так и не найти. Кому-то она придёт в голову почти
сразу, кто-то так никогда и не додумается.

Но неизменно, когда ответ озвучен, те, кто не решил задачу, делают
круглые глаза и задаются вопросом "Как вообще к этому решению можно было прийти?".

Однако, для этой задачи достаточно перебрать всё множество вопросов
(а оно конечно) вместе со стратегией выбора компьютера и отфильтровать
те решения, которые позволяют гарантированно выбрать компьютер А или В.
Полученные механических путем решения будет ничем не хуже данных в озарении.

С чего же начать поиск решения?

Во-первых, кому его задать? Без потери общности положим, что вопрос
всегда задаётся первому компьютеру.

> data ComputerKind = A | B | C deriving (Show,Eq)
> data ComputerId = I1 | I2 | I3 deriving (Enum,Bounded)

> instance Show ComputerId where
>   show I1 = "1"
>   show I2 = "2"
>   show I3 = "3"

Во-вторых, какие вопросы имеет смысл задавать?
Очевидно, что обойтись вопросом, ответ на который мы знаем сами
(например, "2*2 равно 4?") не удастся.
Значит нужно задавать более предметные вопросы, вопросы о состоянии мира,
в котором находятся компьютеры. Всё, что они знают - это кто есть кто среди них.
Поэтому имеет смысл задавать вопросы типа
  "Верно ли, что 1ый компьютер - это А, 2ой - С, 3ий - В?".
Кроме того, можно комбинировать в вопросе возможные конфигурации
компьютеров через "или": "Верно ли, что 1-А, 2-С, 3-В или 1-В, 2-С, 3-А?".
Теперь, если мы переберем все возможные подмножества (их 2^(3!))
множества возможных конфигураций (их 3!), то получим все возможные
вопросы, которые можно задать компьютерам.

Итак, вопрос - есть дизъюнкция (Or) конъюнкций (And), где
конъюнкты (Is) утверждают, что i-ый компьютер есть на самом деле A, B или C.

> data Question = Or  Question Question
>               | And Question Question
>               | Is ComputerId ComputerKind

> instance Show Question where
>   showsPrec _ (Is cid kind) =
>     showsPrec 0 cid .
>     showString "==" .
>     showsPrec 0 kind
>   showsPrec _ (And q1 q2) =
>     showsPrec 3 q1 .
>     showString "&" .
>     showsPrec 3 q2
>   showsPrec p (Or q1 q2) = showParen (p > 2) $
>     showsPrec 2 q1 .
>     showString " | " .
>     showsPrec 2 q2

В-третьих, как выбрать компьютер?
В зависимости от данного ответа - "да" или "нет", выбрать один
из трех компьютеров. Всего таких стратегий 3*3=9.

> data Strategy = Strategy { ifTrue  :: ComputerId
>                          , ifFalse :: ComputerId
>                          }

> instance Show Strategy where
>   show Strategy{..} = printf
>                         "Yes ? %s : %s"
>                         (show ifTrue)
>                         (show ifFalse)

Чтобы перебрать все возможные вопросы, нам понадобится тип,
обозначающий конкретную конфигурацию компьютеров.

> data World = World { i1 :: ComputerKind
>                    , i2 :: ComputerKind
>                    , i3 :: ComputerKind
>                    }
>   deriving Show

> worlds :: [World]
> worlds = map mkWorld $ permutations [A,B,C]
>   where
>     mkWorld [i1,i2,i3] = World{..}
>     mkWorld _ = error "Should not happen."

Тогда из каждого подмножества 'worlds' можно сформировать один вопрос,
объединяющий конфигурации компьютеров через "или".

> questions :: [Question]
> questions =
>   map orQuestion $
>   filter (not . null) $
>   subsequences worldQuestions
>   where
>     orQuestion :: [Question] -> Question
>     orQuestion = foldr1 Or
>     
>     worldQuestions :: [Question]
>     worldQuestions = map worldToQuestion worlds
>     
>     worldToQuestion :: World -> Question
>     worldToQuestion world =
>       foldr1 And
>       [ Is cid (getKind cid world)
>       | cid <- [minBound..maxBound]
>       ]      

> getKind :: ComputerId -> World -> ComputerKind
> getKind I1 = i1
> getKind I2 = i2
> getKind I3 = i3

Все стратегии выбора перебираются тривиально.

> strategies :: [Strategy]
> strategies = do
>   t <- [minBound..maxBound]
>   f <- [minBound..maxBound]
>   return $ Strategy t f

'evalQuestion' интерпретирует вопрос относительно заданной
конфигурации компьютеров.

> evalQuestion :: Question -> World -> Bool
> evalQuestion (Is cid kind) world = getKind cid world == kind
> evalQuestion (Or  q1 q2) world = evalQuestion q1 world ||
>                                  evalQuestion q2 world
> evalQuestion (And q1 q2) world = evalQuestion q1 world &&
>                                  evalQuestion q2 world

'applyStrategy' выбирает компьютер, руководствуясь полученным ответом.

> applyStrategy :: Strategy -> Bool -> ComputerId
> applyStrategy Strategy{..} answer | answer    = ifTrue
>                                   | otherwise = ifFalse

'check' проверяет, что вопрос и стратегия позволяют всегда
выбрать компьютер A или B.

> check :: Question -> Strategy -> Bool
> check question strategy =

Для этого нужно убедиться, что для всех возможных конфигураций ('worlds')
алгоритм позволит выбрать подходящий компьютер.

>   all (all isOkComputer . selectComputer) worlds
>   where
>     isOkComputer :: ComputerKind -> Bool
>     isOkComputer k = k == A || k == B

Для заданной конфигурации 'world' мы вычисляем множество ответов 'compAnswers',
которые может выдать опрашиваемый компьютер.
Затем применяем стратегию выбора и получаем компьютеры, которые будут нами выбраны.

>     selectComputer :: World -> [ComputerKind]
>     selectComputer world = map selectComp compAnswers
>       where
>         selectComp ans = getKind (applyStrategy strategy ans) world

>         answer  = evalQuestion question world
>         compAnswers = case i1 world of
>           A -> [ answer ]
>           B -> [ not answer ]
>           C -> [ True, False ]

Проверяем все возможные пары вопрос-стратегия.

> solutions :: [(Question,Strategy)]
> solutions = do
>   q <- questions
>   s <- strategies
>   guard (check q s)
>   return (q,s)

> main :: IO ()
> main = do
>   let showSolution (question,strategy) =
>         show strategy ++ " <== " ++ show question ++ " ?"
>   mapM_ (putStrLn . showSolution) solutions

Вот и все решения задачи:

< ghci> main
< Yes ? 2 : 3 <== 1==A&2==B&3==C | 1==B&2==C&3==A ?
< Yes ? 2 : 3 <== 1==A&2==B&3==C | 1==C&2==B&3==A | 1==B&2==C&3==A ?
< Yes ? 2 : 3 <== 1==A&2==B&3==C | 1==B&2==C&3==A | 1==C&2==A&3==B ?
< Yes ? 2 : 3 <== 1==A&2==B&3==C | 1==C&2==B&3==A | 1==B&2==C&3==A | 1==C&2==A&3==B ?
< Yes ? 3 : 2 <== 1==B&2==A&3==C | 1==A&2==C&3==B ?
< Yes ? 3 : 2 <== 1==B&2==A&3==C | 1==C&2==B&3==A | 1==A&2==C&3==B ?
< Yes ? 3 : 2 <== 1==B&2==A&3==C | 1==C&2==A&3==B | 1==A&2==C&3==B ?
< Yes ? 3 : 2 <== 1==B&2==A&3==C | 1==C&2==B&3==A | 1==C&2==A&3==B | 1==A&2==C&3==B ?

Заметьте, что ни в одном решении не выбирается опрашиваемый компьютер (1ый).

Рассмотрим одно из решений (все они верны, но только оно и симметричное
ему дают в итоге решение, которое красиво формулируется на естественном языке):

  Yes ? 3 : 2 <== 1==B&2==A&3==C | 1==C&2==A&3==B | 1==A&2==C&3==B ?

Вопрос можно упростить, опустив в каждом дизъюнкте любой из конъюнктов,
потому что он однозначно восстанавливается по оставшимся двум.
Удобнее всего будет исключить упоминания первого компьютера:

  2==A&3==C | 2==A&3==B | 2==C&3==B ?

Дизъюнкты можно обобщить в один, если воспользоваться очевидными утверждениями:
1. Компьютер А отвечает правдиво чаще, чем компьютер С
2. Компьютер С отвечает правдиво чаще, чем компьютер B
3. Компьютер A отвечает правдиво чаще, чем компьютер B

Финальная лаконичная формулировка вопроса:

  Верно ли, что 2ой компьютер отвечает правдиво чаще, чем 3ий?

Если 1ый компьютер отчает "да", то выбирается 3ий компьютер, иначе 2ой.
