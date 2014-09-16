{-# LANGUAGE TemplateHaskell
 #-}
{-|
Module      : Language.Polycephaly.Utils
Description : Polycephalous-instances code generation
Copyright   : (c) Julian K. Arni, 2014
License     : BSD3
Maintainer  : jkarni@gmail.com
Stability   : experimental
-}

module Language.Haskell.Polycephaly.Utils where



notCDec :: String -> a
notCDec fnName = error $ fnName ++ " called on something other than a class dec!"
