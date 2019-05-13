module ContractApplicationSpec (
    spec
) where

import Test.Hspec
import ContractApplication

spec :: Spec 
spec = do 
    describe "契約の合計料金を取得する" $ do 
        it "1GBプランを選んだら1000円" $ do 
            (totalPayment One [] dataPlanList optionList) `shouldBe` 1000
        it "3GBプランを選んだら2000円" $ do 
            (totalPayment Three [] dataPlanList optionList) `shouldBe` 2000
        it "30GBプランを選んだら6000円" $ do 
            (totalPayment Thirty [] dataPlanList optionList) `shouldBe` 6000
        it "3GBにエンタメフリープランを付けたら、3200円" $ do 
            (totalPayment Three [EntertainmentFree] dataPlanList optionList) `shouldBe` 3200
        it "30GBにエンタメフリープランを付けたら、7200円" $ do 
            (totalPayment Thirty [EntertainmentFree] dataPlanList optionList) `shouldBe` 7200
        it "1GBにエンタメフリープランを付けたら、1000円（オプション付けれず）" $ do 
            (totalPayment One [EntertainmentFree] dataPlanList optionList) `shouldBe` 1000
    