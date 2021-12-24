{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fplugin=RecordDotPreprocessor -fplugin=Data.Record.Plugin #-}

import qualified Data.Record.Plugin as Nau (LargeRecord (..))

{-# ANN type A Nau.LargeRecord #-}
data A a = A {a :: a, b :: B a}
  deriving stock (Show, Eq, Ord)

{-# ANN type B Nau.LargeRecord #-}
data B a = B {a :: a, b :: Int}
  deriving stock (Show, Eq, Ord)

-- test for lr
_ = (vectorToA, vectorToB)

exampleA = A {a = 1, b = B {a = (1 :: Int), b = 2}}

transformA A {a, b} = A {a = 100, b = id b}

main = do
  print exampleA
  print (transformA exampleA)
  print (exampleA == transformA exampleA)
  print (exampleA == exampleA)
  print (exampleA < exampleA)
  print (exampleA < transformA exampleA)
  print (exampleA.a, exampleA.b, exampleA.b.a, exampleA.b.b)
  print exampleA{a = 0}
