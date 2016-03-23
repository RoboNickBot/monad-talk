module ShoppingB where

----------------------------------------------------------------------

import Prelude hiding (lookup, empty)

import Control.Applicative
import Control.Monad

import Text.Printf (printf)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.List as L
import Data.Foldable

----------------------------------------------------------------------


-- | An item in a store's product lineup
data Item = Item { itemName :: String
                 , itemPrice :: Double
                 , itemStock :: Int
                 , itemRequiresID :: Bool
                 , itemLocation :: String }

-- | Product catalog
type Catalog = Map String Item

newItem :: Catalog -> Item -> Catalog
newItem m i = M.insert (itemName i) i m

inventory :: Catalog -> [Item]
inventory = M.elems

superBuy :: Catalog
superBuy = 
  foldl' newItem M.empty [Item "pen"             1.99 2000 False "SuperBuy"
                         ,Item "computer"      399.99   20 False "SuperBuy"
                         ,Item "vodka"          29.99   40  True "SuperBuy"
                         ,Item "hotdog"          2.59   50 False "SuperBuy"
                         ,Item "boeing747"  700000.01    1  True "SuperBuy"
                         ,Item "sandals"        11.99    0 False "SuperBuy"
                         ,Item "boots"          44.99    0 False "SuperBuy"
                         ,Item "keyboard"       23.99    3 False "SuperBuy"]

-- | Tries to get an item's data from the catalog
fetchItem :: Catalog -> String -> Either String Item
fetchItem m n = case M.lookup n m of
                  Just i -> Right i
                  Nothing -> Left (n ++ " is not in the catalog")


----------------------------------------------------------------------
-- FUNCTOR

-- | What should this be?
mapEither :: (a -> b) -> Either e a -> Either e b
mapEither f (Right r) = Right (f r)
mapEither _ (Left e) = Left e

-- | Check the catalog for the price of an item
lookPrice :: Either String Item -> Either String Double
lookPrice = mapEither itemPrice
-- lookPrice mi = case mi of
--                  Right i -> Right (itemPrice i)
--                  Left e -> Left e

modSale :: Double -> Either String Double -> Either String Double
modSale d = mapEither (* d)
-- modSale d (Right p) = Right (p * d)
-- modSale _ (Left e) = Left e

-- | for convenience
fetchPrice :: Catalog -> String -> Either String Double
fetchPrice m = lookPrice . fetchItem m

-- | Prompt the customer for an item and tell them what they owe.
checkout :: Catalog -> IO ()
checkout m = do name <- prompt "What do you need today?"
                printPrice ( (fetchPrice m name))


-- | All items are %10 off today.
simpleSale :: Catalog -> IO ()
simpleSale m = do name <- prompt "Everything is %10 off! Buy something!"
                  printPrice (modSale 0.8 (fetchPrice m name))

----------------------------------------------------------------------
-- APPLICATIVE

-- | More powerful than 'fmap'...
applyEither :: Either String (a -> b) -> Either String a -> Either String b
applyEither f a = f <*> a

-- | Now we need to operate over two contextual values at once?
doublesale :: Catalog -> IO ()
doublesale m = 
  do putStrLn "Buy 2 things, get 25% off!"
     name1 <- prompt "Thing 1:"
     name2 <- prompt "Thing 2:"
     let p = applyEither (applyEither (Right (applySale 0.25)) (fetchPrice m name1)) (fetchPrice m name2)
         p' = applySale 0.25 <$> fetchPrice m name1 <*> fetchPrice m name2
     printPrice p'
     -- case (fetchPrice m name1, fetchPrice m name2) of
     --   (Right p1, Right p2) -> 
     --     printPrice $ Right (applySale 0.75 p1 p2) -- ((p1 + p2) * 0.75)
     --   (Left e,_) -> printPrice (Left e)
     --   (_,Left e) -> printPrice (Left e)

-- | for clarity
applySale :: Double -> Double -> Double -> Double
applySale per item1 item2 = per * (item1 + item2)


-- | operate over unlimited Either values!
bulkBuy :: Catalog -> IO ()
bulkBuy m = do names <- promptMany "List the things you want to buy:"
               printPrice (getManyPrices m names)

-- | this is the tricky bit
getManyPrices :: Catalog -> [String] -> Either String Double
getManyPrices m ns = foldr f (Right 0) ns
  where f :: String -> Either String Double -> Either String Double
        f n d = (+) <$> (fetchPrice m n) <*> d
-- getManyPrices m (n:ns) = case fetchPrice m n of
--                            Right p -> case getManyPrices m ns of
--                                         Right ps -> Right (p + ps)
--                                         Left e -> Left e
--                            Left e -> Left e
-- getManyPrices _ [] = Right 0



----------------------------------------------------------------------
-- MONAD

-- | At last, the primary Monad function
eitherChain :: (a -> Either String b) -> Either String a -> Either String b
eitherChain f (Right a) = f a
eitherChain _ (Left e) = Left e

(=<<*) = eitherChain

infixr 1 =<<*

-- | Errors

moneyEr :: Double -> String
moneyEr d = "that costs $" 
            ++ printf "%.2f" d 
            ++ ", which is out of your range"

legalEr :: String
legalEr = "you are not authorized"

stockEr :: String -> String
stockEr s = s ++ " is out of stock"


inStock :: Item -> Bool
inStock i = itemStock i > 0

smartCheckout :: Catalog -> IO ()
smartCheckout m = 
  do hasID <- read <$> prompt "Do you have a valid license?" :: IO Bool
     money <- read <$> prompt "How much are you willing to spend?" :: IO Double
     name <- prompt "What do you need today?"

     (printPrice) (priceRange money . itemPrice =<<* legal hasID =<<* stock =<<* fetchItem m name)



     -- let foo = (fmap . fmap) (legal hasID) (stock <$> fetchItem m name)


     -- printPrice (case (fmap . fmap . fmap) itemPrice foo of
     --               Right (Right (Right i)) -> Right i
     --               _ -> Left "whatever")
     -- printPrice (case fetchItem m name of
     --               Right i -> if inStock i
     --                             then if itemRequiresID i
     --                                     then if hasID
     --                                             then if itemPrice i > money
     --                                                     then Left (moneyEr (itemPrice i))
     --                                                     else Right (itemPrice i)
     --                                             else Left legalEr
     --                                     else if itemPrice i > money
     --                                             then Left (moneyEr (itemPrice i))
     --                                             else Right (itemPrice i)
     --                             else Left (stockEr (itemName i))
     --               Left e -> Left e)



stock :: Item -> Either String Item
stock i = if inStock i
               then Right i
               else (Left . stockEr . itemName) i


legal :: Bool -> Item -> Either String Item
legal hasId i = if itemRequiresID i
                   then if hasId
                           then Right i
                           else Left legalEr
                   else Right i

priceRange :: Double -> Double -> Either String Double
priceRange r p = if p > r
                    then Left (moneyEr p)
                    else Right p

ultraCheckout :: Catalog -> IO ()
ultraCheckout m = do hasID <- read <$> prompt "ID?" :: IO Bool
                     money <- read <$> prompt "price range?" :: IO Double
                     names <- promptMany "List items:"
                     printPrice (getManyPrices' m (hasID, money) names) 

checkAll :: Catalog -> String -> Bool -> Double -> Either String Double
checkAll m name hasID money = priceRange money . itemPrice 
                              =<<* legal hasID 
                              =<<* stock 
                              =<<* fetchItem m name

getManyPrices' m (hasID, money) ns = priceRange money =<< foldr f (Right 0) ns
  where f :: String -> Either String Double -> Either String Double
        f n d = (+) <$> (checkAll m n hasID money) <*> d


----------------------------------------------------------------------

-- LISTS

----------------------------------------------------------------------

eithers2List :: [Either a b] -> [b]
eithers2List = foldr (\a as -> case a of
                                 Right r -> r:as
                                 Left e -> as) []

ezBay :: Catalog
ezBay = 
  foldl' newItem M.empty [Item "pen"             2.99 2000 False "EzBay"
                         ,Item "computer"      199.99    1 False "EzBay"
                         ,Item "sandals"        11.99    3 False "EzBay"
                         ,Item "boots"          44.99   70 False "EzBay"
                         ,Item "keyboard"       23.99    0 False "EzBay"
                         ,Item "banana"          0.89   80 False "EzBay"]

markIns :: Catalog
markIns = 
  foldl' newItem M.empty [Item "pen"             2.99    0 False "MarkIns"
                         ,Item "anchor"         99.99    0 False "MarkIns"
                         ,Item "motherboard"   233.99   54 False "MarkIns"
                         ,Item "bear"            1.00    1 True  "MarkIns"
                         ,Item "computer"      194.99    1 False "MarkIns"
                         ,Item "chips"           3.99   50 False "MarkIns"
                         ,Item "sandals"        15.99    6 False "MarkIns"
                         ,Item "boots"          40.99   70 False "MarkIns"
                         ,Item "keyboard"       29.99    0 False "MarkIns"
                         ,Item "banana"          1.89   80 False "MarkIns"]                         

collected :: [Catalog]
collected = [superBuy, ezBay, markIns]


query :: [Catalog] -> String -> [Item]
query cats n = eithers2List $ map (\c -> fetchItem c n >>= stock) cats


----------------------------------------------------------------------
-- FUNCTOR

-- | hmmm
listMap :: (a -> b) -> [a] -> [b]
listMap = undefined


onlineShop :: [Catalog] -> IO ()
onlineShop cats = do name <- prompt "What are you looking for?"
                     printOptions (getOptions (query cats name))


getOptions :: [Item] -> [(String, Double)]
getOptions [] = []
getOptions (a:as) = (itemLocation a, itemPrice a) : getOptions as


----------------------------------------------------------------------
-- APPLICATIVE

-- | collects three items and prints them, summing their price
cart :: Item -> Item -> Item -> (String, Double)
cart i1 i2 i3 = ((L.concat . L.intersperse ", ") [sh i1, sh i2, sh i3] ++ " -> $" ++ pmoney s, s)
  where s = sum (map itemPrice [i1,i2,i3])
        sh i = itemName i ++ ":" ++ itemLocation i ++ ":$" ++ pmoney (itemPrice i)

itemSum :: [Item] -> Double
itemSum = sum . map itemPrice

-- | running cart on values that may not exist
normalCart :: Catalog -> IO ()
normalCart c = do putStrLn "List three things you are looking for"
                  item1 <- fetchItem c <$> prompt "thing 1:"
                  item2 <- fetchItem c <$> prompt "thing 2:"
                  item3 <- fetchItem c <$> prompt "thing 3:"
                  let rs = cart <$> item1 <*> item2 <*> item3
                  eitherCart rs

eitherCart :: Either String (String, Double) -> IO ()
eitherCart (Right (s,_)) = putStrLn s
eitherCart (Left e) = putStrLn ("Whoops, " ++ e)

-- | running cart on non-deterministic values!
onlineCart :: [Catalog] -> IO ()
onlineCart cats = do putStrLn "List three things you are looking for"
                     items1 <- query cats <$> prompt "thing 1:"
                     items2 <- query cats <$> prompt "thing 2:"
                     items3 <- query cats <$> prompt "thing 3:"
                     let ops = cart <$> items1 <*> items2 <*> items3
                     listCart ops

listCart :: [(String, Double)] -> IO ()
listCart [] = putStrLn"Hmm, not all your requests could be found"
listCart as = do putStrLn "Here are your options:"
                 sequence_ (map (putStrLn . ("- " ++)) (map fst as))
                 putStrLn ("The best price you can get is " ++ mn as)
  where mn = ("$" ++ ) . pmoney . minimum . (map snd)


----------------------------------------------------------------------
-- MONAD

buyEverything :: [Catalog] -> IO ()
buyEverything cats = 
  do prompt "Are you sure you want to buy everything?"
     (printPrice . Right . sum) (itemPrice <$> (cats >>= inventory))

----------------------------------------------------------------------

prompt :: String -> IO String
prompt s = putStrLn s >> getLine

promptMany :: String -> IO [String]
promptMany s = putStrLn s 
               >> putStrLn "(separate with spaces)"
               >> words <$> getLine

printPrice :: Either String Double -> IO ()
printPrice (Right p) = 
  putStrLn $ "Your total is [$" ++ printf "%.2f" p ++ "].  Please swipe card."
printPrice (Left e) = 
  putStrLn $ "I'm sorry, " ++ e ++ " :("

printOptions :: [(String, Double)] -> IO ()
printOptions [] = putStrLn "Sorry, that item is not available anywhere!"
printOptions as = do putStrLn "That item is available in these locations:"
                     sequence_ (map print as)

pmoney = printf "%.2f"
