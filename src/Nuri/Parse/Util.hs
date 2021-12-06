module Nuri.Parse.Util where

import Control.Monad.Combinators.NonEmpty (some)
import qualified Data.Text as T
import Nuri.Parse
  ( MonadParser,
    hangulSyllable,
    lexeme,
    reserved,
  )
import Text.Megaparsec ((<?>))
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

parseKeyword :: (MonadParser m) => m ()
parseKeyword = P.choice $ reserved . toString <$> keywords

parseJosa :: (MonadParser m) => m Text
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

parseFuncIdentifier :: (MonadParser m) => m Text
parseFuncIdentifier = lexeme funcIdentifier

funcIdentifier :: (MonadParser m) => m Text
funcIdentifier = P.notFollowedBy parseKeyword *> hangulWords ""
  where
    char = (hangulSyllable <|> P.letterChar) <?> "한글 음절 또는 영문"

    word = do
      t <- toText <$> P.some char
      if t `elem` keywords
        then fail "여기에는 키워드가 사용될 수 없습니다."
        else return t

    hangulWords s = do
      w <- word
      let con = if T.null s then w else s <> " " <> w
      if T.last w == '고'
        then return con
        else P.try (P.char ' ' >> hangulWords con) <|> return con

parseStructIdentifier :: (MonadParser m) => m Text
parseStructIdentifier = lexeme structIdentifier

structIdentifier :: (MonadParser m) => m Text
structIdentifier = toText . toList <$> some (hangulSyllable <|> P.letterChar)
