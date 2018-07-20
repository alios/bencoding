{-# LANGUAGE OverloadedStrings #-}


module Data.Bencoding.Parser
  ( parseBValue, parseBDict, parseBList, parseBInt, parseBString
  ) where

import           Control.Lens.Fold
import           Control.Lens.Operators
import           Control.Monad
import           Data.Attoparsec.ByteString.Lazy as P
import           Data.Bencoding.Types
import qualified Data.Map                        as Map
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Data.Text.Encoding              as T
import           Text.Read

parseBDict :: Parser BValue
parseBDict = do
  skip (P.inClass "d")
  r <- (bvalue #) . Map.fromList <$> many' parseBDictValue
  skip (P.inClass "e")
  return r

parseBList :: Parser BValue
parseBList = do
  skip (P.inClass "l")
  r <- (bvalue #) <$> many' parseBValue
  skip (P.inClass "e")
  return r


parseBInt :: Parser BValue
parseBInt = do
  skip (P.inClass "i")
  lbs <- T.unpack . T.decodeUtf8 <$> takeWhile1 (notInClass "e")
  skip (const True)
  case (readMaybe lbs :: Maybe Integer) of
    Nothing -> fail $ "unable to read BInteger from" ++ lbs
    Just i  -> pure (_BInt # i)

parseBString :: Parser BValue
parseBString = do
  lbs <- T.unpack . T.decodeUtf8 <$> takeWhile1 (notInClass ":")
  void anyWord8
  case readMaybe lbs of
    Nothing -> fail $ "unable to read BString length from " ++ lbs
    Just l  -> (bvalue #) . T.decodeUtf8 <$> P.take l




parseBDictValue :: Parser (Text, BValue)
parseBDictValue = do
  k' <- parseBString
  case preview bvalue k' of
    Nothing -> fail $ "unable to read dict key from: " ++ show k'
    Just k  -> (,) <$> pure k <*> parseBValue



parseBValue :: Parser BValue
parseBValue = P.choice [parseBDict, parseBList, parseBInt, parseBString]
