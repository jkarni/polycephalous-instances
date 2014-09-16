{-# LANGUAGE TemplateHaskell  #-}
module Polycephaly.TH (
                      units
                      ) where

import Test.Tasty
import Test.Tasty.HUnit
import Language.Haskell.TH
import Language.Haskell.TH.Alpha ( areExpAEq )

import Language.Haskell.Polycephaly.TH

units = units_mkFlagDC


units_mkFlagDC =
    [ testCase "mkFlagDC adds flag var to class declaration" $
      do
          (clsD:_) <- runQ [d| class Print a where { print :: a -> IO ()}|]
          let ClassD [] clsName tyVarBndrs [] decs = mkFlagDC defaultRules clsD
          assertBool "Incorrect class name!" (show clsName == "Print'")
          -- TODO: where clause check (fix th-alpha)
    ]

