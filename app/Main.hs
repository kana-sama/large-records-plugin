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
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fplugin=RecordDotPreprocessor -fplugin=Data.Record.Plugin #-}

{-# ANN type A "large-record" #-}
data A a = A {a :: a, b :: Int}
  deriving stock (Show, Eq, Ord)

exampleX = A {a = 1, b = 2}

transformX A {a} = A {a = id a, b = 190}

qwe = exampleX.b

main = do
  print exampleX
  print (transformX exampleX)
  print (exampleX == transformX exampleX)
  print (exampleX == exampleX)
  print (exampleX < exampleX)
  print (exampleX < transformX exampleX)
  print (exampleX.a, exampleX.b)

-- main = print (transformX exampleX)
