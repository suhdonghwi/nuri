module Nuri.Parse.Util where

import Nuri.Parse
  ( Parser,
    hangulSyllable,
    lexeme,
    reserved,
  )
import Text.Megaparsec ((<?>))
import Control.Monad.Combinators.NonEmpty (some)
import qualified Data.Text as T
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P

keywords :: [Text]
keywords =
  [ "함수",
    "동사",
    "형용사",
    "구조체",
    "없음",
    "참",
    "거짓",
    "만약",
    "이라면",
    "아니라면",
    "순서대로",
    "그리고",
    "또는"
  ]

parseKeyword :: Parser ()
parseKeyword = P.choice $ reserved . T.unpack <$> keywords

parseJosa :: Parser Text
parseJosa =
  ( do
      P.notFollowedBy parseKeyword
      josa <- toText <$> P.some hangulSyllable
      return
        ( case josa of
            "으로" -> "로"
            "과" -> "와"
            "를" -> "을"
            "는" -> "은"
            "가" -> "이"
            j -> j
        )
  )
    <?> "조사"

parseFuncIdentifier :: Parser Text
parseFuncIdentifier = lexeme funcIdentifier

funcIdentifier :: Parser Text
funcIdentifier = P.notFollowedBy parseKeyword *> (P.try hangulWords <|> lastWord)
  where
    char = (hangulSyllable <|> P.letterChar) <?> "한글 음절 또는 영문"
    word = P.try $ do
      raw <- lastWord
      if T.last raw == '고' 
         then fail "이 자리에는 '고'가 올 수 없습니다." 
         else return raw

    lastWord = do 
      t <- toText <$> P.some char
      if t `elem` keywords
         then fail "여기에는 키워드가 사용될 수 없습니다." 
         else return t

    hangulWords = do
      xs <- T.intercalate " " <$> word `P.sepBy1` (P.char ' ')
      x <- P.optional (P.char ' ' >> lastWord)
      case x of
        Nothing -> return xs
        Just x' -> return $ xs <> " " <> x'

parseStructIdentifier :: Parser Text
parseStructIdentifier = lexeme structIdentifier

structIdentifier :: Parser Text
structIdentifier = (toText . toList) <$> some (hangulSyllable <|> P.letterChar)
