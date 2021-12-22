{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- {-# OPTIONS_GHC -fplugin-opt=Data.Record.Plugin:args=[defaultPureScript] #-}
-- {-# OPTIONS_GHC -fplugin-opt=Data.Record.Plugin:via=Data.Record.TH.largeRecord #-}
-- {-# OPTIONS_GHC -fplugin-opt=Data.Record.Plugin:via=Q.qwe #-}
{-# OPTIONS_GHC -fplugin=Data.Record.Plugin #-}

import Data.Record.TH
import qualified Q

data X = X {a :: Int, b :: Int}
  deriving stock (Show)
{-# ANN type X "large-record" #-}

data Y = Y {a :: Int, b :: Int}
  deriving stock (Show)

exampleX = X {a = 10, b = 20}

exampleY = Y {a = 10, b = 20}

transformX X {a} = X {a = a + 10, b = 20}

transformY Y {a} = Y {a = a + 10, b = 20}

main = do
  print (transformX exampleX)
  print (transformY exampleY)
