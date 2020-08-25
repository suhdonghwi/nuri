module Nuri.Parse.Util where

import Nuri.Parse
  ( Parser,
    hangulSyllable,
    lexeme,
    reserved,
  )
import Text.Megaparsec ((<?>))
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P

parseKeyword :: Parser ()
parseKeyword = P.choice $ reserved <$> keywords
  where
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

parseJosa :: Parser Text
parseJosa =
  ( do
      P.notFollowedBy parseKeyword
      josa <- parseFuncIdentifier
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
funcIdentifier = P.notFollowedBy parseKeyword *> hangulWord
  where
    hangulWord = toText <$> P.some (hangulSyllable <|> P.char '_')
