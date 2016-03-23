module Shopping where

----------------------------------------------------------------------

import Prelude hiding (lookup)

import Control.Applicative
import Control.Monad

import Data.Map (Map, insert, lookup)

----------------------------------------------------------------------

data Item = Item { itemName :: String
                 , itemPrice :: Double
                 , itemStock :: Int
                 , itemRequiresID :: Bool }

type Catalog = Map String Item

newItem :: Catalog -> Item -> Catalog
newItem m i = insert (itemName i) i m



----------------------------------------------------------------------
-- FUNCTORS

getPrice1 :: Map String Item -> String -> Maybe Double
getPrice1 m n = case lookup n m of
                  Just i -> Just (itemPrice i)
                  Nothing -> Nothing

checkStock :: Map String Item -> String -> Maybe Bool
checkStock = undefined

maybeFun :: (a -> b) -> Maybe a -> Maybe b
maybeFun = undefined

----------------------------------------------------------------------
-- APPLICATIVE

