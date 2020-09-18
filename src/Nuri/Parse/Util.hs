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
funcIdentifier = P.notFollowedBy parseKeyword *> hangulWords "" 
  where
    char = (hangulSyllable <|> P.letterChar) <?> "한글 음절 또는 영문"

    word = do 
      t <- toText <$> P.some char
      if t `elem` keywords
         then fail "여기에는 키워드가 사용될 수 없습니다." 
         else return t

    hangulWords s = do
      w <- P.optional word
      case w of
        Just w' -> do
          let con = if T.null s then w' else s <> " " <> w'
          if T.last w' == '고'
             then return con
             else P.try (P.char ' ' >> hangulWords con) <|> return con
        Nothing -> if T.null s then fail "" else return s

parseStructIdentifier :: Parser Text
parseStructIdentifier = lexeme structIdentifier

structIdentifier :: Parser Text
structIdentifier = (toText . toList) <$> some (hangulSyllable <|> P.letterChar)
