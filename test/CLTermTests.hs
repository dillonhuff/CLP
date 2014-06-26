module CLTermTests(
  allCLTermTests) where

import CLTerm
import ErrorHandling
import TestUtils

allCLTermTests = do
  assignTypeTests
  
assignTypeTests =
  testFunction (assignType baseCombinators) assignTypeCases
  
assignTypeCases =
  [(val $ int 4, Succeeded $ base "INT"),
   (com "I", Succeeded $ func [var "a", var "a"]),
   (ap (com "I") (val $ char 'a'), Succeeded $ base "CHAR"),
   (ap (com "K") (val $ float 12.2), Succeeded $ func [var "a", base "CHAR"]),
   (ap (com "K") (com "I"), Succeeded $ func [var "a", func [var "b", var "b"]]),
   (ap (ap (com "K") (com "I")) (val $ float (-1)), Succeeded $ func [var "a", var "a"]),
   (ap (ap (com "S") (com "K")) (com "K"), Succeeded $ func [var "a", var "a"]),
   (ap (ap (com "S") (ap (com "K") (com "S"))) (com "K"),
    Succeeded $ func [func [var "a", var "b"], func [func [var "c", var "a"], func [var "c", var "b"]]])]