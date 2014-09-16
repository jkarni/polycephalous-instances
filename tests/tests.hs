{-# LANGUAGE TemplateHaskell  #-}
module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Language.Haskell.TH

import Language.Haskell.TH.Alpha

import qualified Polycephaly.TH as TH.Tests

main = defaultMain tests

tests :: TestTree
tests = testGroup "Unit tests" TH.Tests.units
