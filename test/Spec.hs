{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import YANG.Internal
import qualified Text.Parsec as P

main :: IO ()
main = hspec $ 
  describe "YANG" $ do 
    it "can parse namespace" $ 
      P.parse namespace "ns" "namespace \"http://tail-f.com/pkg/tailf-etsi-rel2-nfvo\";" 
         `shouldBe` Right "http://tail-f.com/pkg/tailf-etsi-rel2-nfvo"
    it "can parse quoted string" $
      P.parse quotedString "str" "\"the string\"" 
        `shouldBe` Right "the string"
    it "can parse non quoted string" $
      P.parse nonQuotedString "str" "the-string;" 
        `shouldBe` Right "the-string"
    it "can parse non quoted string" $
      P.parse nonQuotedString "str" "the-string " 
        `shouldBe` Right "the-string"
    it "can parse prefix" $
      P.parse prefix "pfx" "prefix nfvo-rel2;" 
        `shouldBe` Right "nfvo-rel2"
    it "can parse quoted prefix" $
      P.parse prefix "pfx" "prefix \"nfvo-rel2\";" 
        `shouldBe` Right "nfvo-rel2"
    it "can parse a revisionDate" $
      P.parse revisionDate "yang" "revision-date \"2017-01-20\";"
        `shouldBe` Right "2017-01-20"
    it "can parse an include" $
      P.parse include "yang" "include tailf-etsi-rel2-nfvo-vnf {\n revision-date \"2017-01-20\";\n }"
        `shouldBe` Right ("tailf-etsi-rel2-nfvo-vnf", "2017-01-20")
