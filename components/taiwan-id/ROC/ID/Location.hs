{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module ROC.ID.Location
  ( Location
  , fromLetter
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
import ROC.ID.Language
  ( Language (..) )
import ROC.ID.Letter
  ( Letter (..) )
import ROC.ID.Utilities
  ( randomFinitary )
import Text.Read
  ( Lexeme (Ident, Symbol), Read (readPrec), lexP, parens )

-- | Represents a location, encodable within an ROC identification number.
--
-- == Location codes
--
-- Every location that can be represented in an ROC identification number has a
-- unique code:
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
-- To construct a 'Location' from its letter code, use the 'fromLetter'
-- function.
--
-- To print the full name of a 'Location', use the 'toText' function.
--
-- To generate a random 'Location', use the 'generate' function.
--
newtype Location = Location Letter
  deriving stock (Eq, Generic, Ord)
  deriving newtype (Bounded, Enum)
  deriving anyclass Finitary

instance Read Location where
  readPrec = parens $ do
    Ident "Location"   <- lexP
    Symbol "."         <- lexP
    Ident "fromLetter" <- lexP
    fromLetter <$> readPrec

instance Show Location where
  showsPrec _ s =
    showString "Location.fromLetter " . shows (toLetter s)

-- | Constructs a 'Location' from its corresponding letter code.
--
fromLetter :: Letter -> Location
fromLetter = Location

-- | Converts a 'Location' to its corresponding letter code.
--
toLetter :: Location -> Letter
toLetter (Location letter) = letter

-- | Prints the specified 'Location'.
toText :: Language -> Location -> Text
toText = \case
  English -> toTextEnglish
  Chinese -> toTextChinese

toTextChinese :: Location -> Text
toTextChinese (Location letter) = case letter of
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

toTextEnglish :: Location -> Text
toTextEnglish (Location letter) = case letter of
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

-- | Generates a random 'Location'.
--
generate :: MonadRandom m => m Location
generate = randomFinitary
