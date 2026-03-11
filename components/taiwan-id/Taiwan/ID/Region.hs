{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Taiwan.ID.Region
  ( Region
  , fromChar
  , fromLetter
  , toChar
  , toLetter
  , toText
  , generate
  ) where

import Control.Monad.Random.Class
  ( MonadRandom (..) )
import Data.Finitary
  ( Finitary )
import Data.Text
  ( Text )
import GHC.Generics
  ( Generic )
import Taiwan.ID.Language
  ( Language (..) )
import Taiwan.ID.Letter
  ( Letter (..) )
import Taiwan.ID.Utilities
  ( randomFinitary )
import Text.Read
  ( Lexeme (Ident, Symbol), Read (readPrec), lexP, parens )

import qualified Taiwan.ID.Letter as Letter

-- |
-- $setup
-- >>> import qualified Taiwan.ID.Region as Region

-- | Represents a geographical region.
--
-- == Region codes
--
-- Every region has a unique letter code:
--
-- +------+---------+-------------------+
-- | Code | Chinese | English           |
-- +======+=========+===================+
-- | @A@  | 臺北市     | Taipei City       |
-- +------+---------+-------------------+
-- | @B@  | 臺中市     | Taichung City     |
-- +------+---------+-------------------+
-- | @C@  | 基隆市     | Keelung City      |
-- +------+---------+-------------------+
-- | @D@  | 臺南市     | Tainan City       |
-- +------+---------+-------------------+
-- | @E@  | 高雄市     | Kaohsiung City    |
-- +------+---------+-------------------+
-- | @F@  | 新北市     | New Taipei City   |
-- +------+---------+-------------------+
-- | @G@  | 宜蘭縣     | Yilan County      |
-- +------+---------+-------------------+
-- | @H@  | 桃園市     | Taoyuan City      |
-- +------+---------+-------------------+
-- | @I@  | 嘉義市     | Chiayi City       |
-- +------+---------+-------------------+
-- | @J@  | 新竹縣     | Hsinchu County    |
-- +------+---------+-------------------+
-- | @K@  | 苗栗縣     | Miaoli County     |
-- +------+---------+-------------------+
-- | @L@  | 臺中縣     | Taichung County   |
-- +------+---------+-------------------+
-- | @M@  | 南投縣     | Nantou County     |
-- +------+---------+-------------------+
-- | @N@  | 彰化縣     | Changhua County   |
-- +------+---------+-------------------+
-- | @O@  | 新竹市     | Hsinchu City      |
-- +------+---------+-------------------+
-- | @P@  | 雲林縣     | Yunlin County     |
-- +------+---------+-------------------+
-- | @Q@  | 嘉義縣     | Chiayi County     |
-- +------+---------+-------------------+
-- | @R@  | 臺南縣     | Tainan County     |
-- +------+---------+-------------------+
-- | @S@  | 高雄縣     | Kaohsiung County  |
-- +------+---------+-------------------+
-- | @T@  | 屏東縣     | Pingtung County   |
-- +------+---------+-------------------+
-- | @U@  | 花蓮縣     | Hualien County    |
-- +------+---------+-------------------+
-- | @V@  | 臺東縣     | Taitung County    |
-- +------+---------+-------------------+
-- | @W@  | 金門縣     | Kinmen County     |
-- +------+---------+-------------------+
-- | @X@  | 澎湖縣     | Penghu County     |
-- +------+---------+-------------------+
-- | @Y@  | 陽明山     | Yangmingshan      |
-- +------+---------+-------------------+
-- | @Z@  | 連江縣     | Lienchiang County |
-- +------+---------+-------------------+
--
-- == Usage
--
-- To construct a 'Region' from its letter code, use the 'fromLetter'
-- function.
--
-- To print the full name of a 'Region', use the 'toText' function.
--
-- To generate a random 'Region', use the 'generate' function.
--
newtype Region = Region Letter
  deriving stock (Eq, Generic, Ord)
  deriving newtype (Bounded, Enum)
  deriving anyclass Finitary

instance Read Region where
  readPrec = parens $ do
    Ident "Region"     <- lexP
    Symbol "."         <- lexP
    Ident "fromLetter" <- lexP
    fromLetter <$> readPrec

instance Show Region where
  showsPrec _ s =
    showString "Region.fromLetter " . shows (toLetter s)

-- | Attempts to construct a 'Region' from its corresponding letter code as a
-- 'Char'.
--
-- >>> Region.fromChar 'A'
-- Just (Region.fromLetter A)
--
-- >>> Region.fromChar '?'
-- Nothing
--
fromChar :: Char -> Maybe Region
fromChar = fmap fromLetter . Letter.fromChar

-- | Constructs a 'Region' from its corresponding letter code.
--
fromLetter :: Letter -> Region
fromLetter = Region

-- | Converts a 'Region' to its corresponding letter code as a 'Char'.
--
toChar :: Region -> Char
toChar = Letter.toChar . toLetter

-- | Converts a 'Region' to its corresponding letter code.
--
toLetter :: Region -> Letter
toLetter (Region letter) = letter

-- | Prints the specified 'Region'.
toText :: Language -> Region -> Text
toText = \case
  English -> toTextEnglish
  Chinese -> toTextChinese

toTextChinese :: Region -> Text
toTextChinese (Region letter) = case letter of
  A -> "臺北市"
  B -> "臺中市"
  C -> "基隆市"
  D -> "臺南市"
  E -> "高雄市"
  F -> "新北市"
  G -> "宜蘭縣"
  H -> "桃園市"
  I -> "嘉義市"
  J -> "新竹縣"
  K -> "苗栗縣"
  L -> "臺中縣"
  M -> "南投縣"
  N -> "彰化縣"
  O -> "新竹市"
  P -> "雲林縣"
  Q -> "嘉義縣"
  R -> "臺南縣"
  S -> "高雄縣"
  T -> "屏東縣"
  U -> "花蓮縣"
  V -> "臺東縣"
  W -> "金門縣"
  X -> "澎湖縣"
  Y -> "陽明山"
  Z -> "連江縣"

toTextEnglish :: Region -> Text
toTextEnglish (Region letter) = case letter of
  A -> "Taipei City"
  B -> "Taichung City"
  C -> "Keelung City"
  D -> "Tainan City"
  E -> "Kaohsiung City"
  F -> "New Taipei City"
  G -> "Yilan County"
  H -> "Taoyuan City"
  I -> "Chiayi City"
  J -> "Hsinchu County"
  K -> "Miaoli County"
  L -> "Taichung County"
  M -> "Nantou County"
  N -> "Changhua County"
  O -> "Hsinchu City"
  P -> "Yunlin County"
  Q -> "Chiayi County"
  R -> "Tainan County"
  S -> "Kaohsiung County"
  T -> "Pingtung County"
  U -> "Hualien County"
  V -> "Taitung County"
  W -> "Kinmen County"
  X -> "Penghu County"
  Y -> "Yangmingshan"
  Z -> "Lienchiang County"

-- | Generates a random 'Region'.
--
generate :: MonadRandom m => m Region
generate = randomFinitary
