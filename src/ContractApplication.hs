module ContractApplication where

import qualified Data.Map.Strict as M
import Data.Maybe
-- データ容量
data TraficData = One | Three | Thirty deriving (Show, Eq, Ord)

-- オプション
data Option = EntertainmentFree deriving  (Show, Eq, Ord)
type Options = [Option]
type PurchasableRule = [TraficData]

-- 型シノニムでクラス表現っぽいことできそう？
type Plan = (TraficData, Options)
type MonthryFee = Int
type PlanList =  M.Map TraficData MonthryFee
type OptionList = M.Map Option (MonthryFee, PurchasableRule)

-- プランリスト
-- これがインメモリじゃなくて、データ層になる？
dataPlanList :: PlanList
dataPlanList =
    M.insert One 1000
    . M.insert Three 2000
    . M.insert Thirty 6000
    $ M.empty

optionList :: OptionList
optionList = 
    M.insert EntertainmentFree (1200,[Three,Thirty])
    $ M.empty

-- オプションが選択可能かどうか
isPurchasable :: Option -> TraficData -> OptionList -> Bool 
isPurchasable option trafic list = elem trafic rule
    where 
        rule = snd $ fromJust $ M.lookup option list

-- オプションのリストから月額料金を抜き出し、合算したものを返したい
totalOptionPayment :: Options -> OptionList ->  Int 
totalOptionPayment [] _ = 0
totalOptionPayment (op:ops) optionList =
    (fst $ fromJust $ M.lookup op optionList) + (totalOptionPayment ops optionList)

-- 流れ的には、
-- 受け取った容量プランの金額算出
-- 受け取ったオプションが購入可能かどうか
-- 購入可能なものだけ金額算出
totalPayment :: TraficData -> Options -> PlanList -> OptionList -> Int
totalPayment trafic [] planlist optionList = fromJust $ M.lookup trafic planlist
totalPayment trafic (op:ops) planlist optionList
    | isPurchasable op trafic optionList = (fst $ fromJust $ M.lookup op optionList) + (totalPayment trafic ops planlist optionList )
    | otherwise = 0 + (totalPayment trafic ops planlist optionList )