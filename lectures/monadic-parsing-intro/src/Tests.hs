module Tests where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

import Control.Applicative ( Alternative( (<|>), empty, many ) )

import MatchParser

unitTests :: TestTree
unitTests = testGroup "Unit Tests"
  [ testCase "The reason char returns Char" $
      (parse (many $ chars "ab") "abab") @?=
        (Just ("abab", ""))
  ]

properties :: TestTree
properties = testGroup "QuickCheck"
  [ QC.testProperty "Fancy property" $
      \(s, v) -> parse (chars s) v ==
        parse (foldl ((<|>)) empty (map char s)) v
  ]

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

main :: IO ()
main = defaultMain tests
