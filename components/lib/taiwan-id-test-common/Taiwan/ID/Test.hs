{-# LANGUAGE NamedFieldPuns #-}

module Taiwan.ID.Test where

import Control.Monad
  ( replicateM
  )
import Data.Function
  ( (&)
  )
import Data.Text
  ( Text
  )
import GHC.Stack
  ( HasCallStack
  )
import Taiwan.ID
  ( ID (ID, c0, c1, c2, c3, c4, c5, c6, c7, c8)
  )
import Taiwan.ID.Digit
  ( Digit
  )
import Taiwan.ID.Digit1289
  ( Digit1289
  )
import Taiwan.ID.Gender
  ( Gender
  )
import Taiwan.ID.Issuer
  ( Issuer
  )
import Taiwan.ID.Language
  ( Language
  )
import Taiwan.ID.Letter
  ( Letter
  )
import Taiwan.ID.Region
  ( Region
  )
import Test.QuickCheck
  ( Gen
  , arbitraryBoundedEnum
  , choose
  , elements
  , oneof
  , shrinkBoundedEnum
  )

import qualified Data.Text as Text
import qualified Taiwan.ID as ID
import qualified Taiwan.ID.Digit as Digit

--------------------------------------------------------------------------------
-- General-purpose generators and shrinkers
--------------------------------------------------------------------------------

genDigit :: Gen Digit
genDigit = arbitraryBoundedEnum

shrinkDigit :: Digit -> [Digit]
shrinkDigit = shrinkBoundedEnum

genDigit1289 :: Gen Digit1289
genDigit1289 = arbitraryBoundedEnum

shrinkDigit1289 :: Digit1289 -> [Digit1289]
shrinkDigit1289 = shrinkBoundedEnum

genGender :: Gen Gender
genGender = arbitraryBoundedEnum

shrinkGender :: Gender -> [Gender]
shrinkGender = shrinkBoundedEnum

genID :: Gen ID
genID =
  ID
    <$> genLetter
    <*> genDigit1289
    <*> genDigit
    <*> genDigit
    <*> genDigit
    <*> genDigit
    <*> genDigit
    <*> genDigit
    <*> genDigit

shrinkID :: ID -> [ID]
shrinkID i@ID {c0, c1, c2, c3, c4, c5, c6, c7, c8} =
  mconcat
    [ [i {c0 = c0'} | c0' <- c0 & shrinkLetter]
    , [i {c1 = c1'} | c1' <- c1 & shrinkDigit1289]
    , [i {c2 = c2'} | c2' <- c2 & shrinkDigit]
    , [i {c3 = c3'} | c3' <- c3 & shrinkDigit]
    , [i {c4 = c4'} | c4' <- c4 & shrinkDigit]
    , [i {c5 = c5'} | c5' <- c5 & shrinkDigit]
    , [i {c6 = c6'} | c6' <- c6 & shrinkDigit]
    , [i {c7 = c7'} | c7' <- c7 & shrinkDigit]
    , [i {c8 = c8'} | c8' <- c8 & shrinkDigit]
    ]

genIssuer :: Gen Issuer
genIssuer = arbitraryBoundedEnum

shrinkIssuer :: Issuer -> [Issuer]
shrinkIssuer = shrinkBoundedEnum

genLanguage :: Gen Language
genLanguage = arbitraryBoundedEnum

shrinkLanguage :: Language -> [Language]
shrinkLanguage = shrinkBoundedEnum

genLetter :: Gen Letter
genLetter = arbitraryBoundedEnum

shrinkLetter :: Letter -> [Letter]
shrinkLetter = shrinkBoundedEnum

genRegion :: Gen Region
genRegion = arbitraryBoundedEnum

shrinkRegion :: Region -> [Region]
shrinkRegion = shrinkBoundedEnum

--------------------------------------------------------------------------------
-- Specialised generators
--------------------------------------------------------------------------------

genInvalidChar :: Gen Char
genInvalidChar = elements "+!@#$%^&*()"

genIDText :: Gen Text
genIDText =
  oneof
    [ genIDTextValid
    , genIDTextInvalid
    ]

genIDTextValid :: Gen Text
genIDTextValid = ID.toText <$> genID

genIDTextInvalid :: Gen Text
genIDTextInvalid =
  oneof
    [ genIDTextInvalidChar
    , genIDTextInvalidChecksum
    , genIDTextInvalidLength
    ]

genIDTextInvalidChar :: Gen Text
genIDTextInvalidChar =
  genIDTextInvalidCharAtIndex =<< choose (0, 9)

genIDTextInvalidCharAtIndex :: Int -> Gen Text
genIDTextInvalidCharAtIndex index =
  unsafePokeChar index <$> genIDTextValid <*> genInvalidChar

genIDTextInvalidChecksum :: Gen Text
genIDTextInvalidChecksum = do
  idTextValid <- genIDTextValid
  let charOld = unsafePeekChar checksumCharIndex idTextValid
  let charNew = Digit.toChar (unsafeDigitFromChar charOld + 1)
  pure $ unsafePokeChar checksumCharIndex idTextValid charNew
  where
    checksumCharIndex = 9

genIDTextInvalidLength :: Gen Text
genIDTextInvalidLength =
  oneof
    [ genIDTextInvalidLengthTooLong
    , genIDTextInvalidLengthTooShort
    ]

genIDTextInvalidLengthTooShort :: Gen Text
genIDTextInvalidLengthTooShort = do
  idTextValid <- genIDTextValid
  invalidLength <- choose (0, 9)
  pure (Text.take invalidLength idTextValid)

genIDTextInvalidLengthTooLong :: Gen Text
genIDTextInvalidLengthTooLong = do
  idTextValid <- genIDTextValid
  extraCharCount <- choose (1, 4)
  extraChars <- Text.pack <$> replicateM extraCharCount genChar
  pure (idTextValid <> extraChars)
  where
    genChar = choose ('0', '9')

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

unsafeDigitFromChar :: HasCallStack => Char -> Digit
unsafeDigitFromChar c = case Digit.fromChar c of
  Nothing -> error "unsafeDigitFromChar"
  Just d -> d

unsafePeekChar :: HasCallStack => Int -> Text -> Char
unsafePeekChar i t
  | i < indexMin = outOfBoundsError
  | i > indexMax = outOfBoundsError
  | otherwise = Text.index t i
  where
    indexMin = 0
    indexMax = Text.length t - 1
    outOfBoundsError = error "unsafePeekChar: index out of bounds"

unsafePokeChar :: HasCallStack => Int -> Text -> Char -> Text
unsafePokeChar i t c
  | i < indexMin = outOfBoundsError
  | i > indexMax = outOfBoundsError
  | otherwise = Text.take i t <> Text.singleton c <> Text.drop (i + 1) t
  where
    indexMin = 0
    indexMax = Text.length t - 1
    outOfBoundsError = error "unsafePokeChar: index out of bounds"
