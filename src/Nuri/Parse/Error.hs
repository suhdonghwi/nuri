{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Nuri.Parse.Error (errorBundlePretty) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import Text.Megaparsec (PosState, pstateSourcePos)
import Text.Megaparsec.Error
  ( ErrorFancy (..),
    ErrorItem (..),
    ParseError (..),
    ParseErrorBundle (..),
    ShowErrorComponent (..),
    errorOffset,
  )
import Text.Megaparsec.Pos (SourcePos (sourceColumn, sourceLine, sourceName), unPos)
import Text.Megaparsec.Stream (Stream (Token, reachOffset))

hasJongseong :: String -> Bool
hasJongseong text =
  let lastChar = last `viaNonEmpty` text
      headChar = init `viaNonEmpty` text
   in case lastChar of
        Nothing -> False
        Just '\'' ->
          case headChar of
            Nothing -> False
            Just str -> hasJongseong str
        Just ch -> hasJongseongChar ch
  where
    hasJongseongChar ch
      | ch `elem` ['1', '3', '6', '7', '8', '0', 'l', 'L', 'm', 'M', 'n', 'N', 'r', 'R'] = True
      | ('0' <= ch && ch <= '9') || ('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z') = False
      | otherwise = (ord ch - 0xAC00) `mod` 28 > 0

byJongseongSubject :: String -> String
byJongseongSubject str = str <> if hasJongseong str then "이" else "가"

isWide :: Char -> Bool
isWide c = '가' <= c && c <= '힣'

errorBundlePretty ::
  forall e.
  ( ShowErrorComponent e
  ) =>
  -- | Parse error bundle to display
  ParseErrorBundle Text e ->
  -- | Textual rendition of the bundle
  String
errorBundlePretty ParseErrorBundle {..} =
  let (r, _) = foldl' f (id, bundlePosState) bundleErrors
   in drop 1 (r "")
  where
    f ::
      (String -> String, PosState Text) ->
      ParseError Text e ->
      (String -> String, PosState Text)
    f (o, !pst) e = (o . (outChunk ++), pst')
      where
        (sline, pst') = reachOffset (errorOffset e) pst
        epos = pstateSourcePos pst'
        outChunk =
          "\n" <> "파일 '" <> sourceName epos <> "', " <> lineNumber <> "번째 줄:\n"
            <> padding
            <> "|\n"
            <> lineNumber
            <> " | "
            <> sline
            <> "\n"
            <> padding
            <> "| "
            <> rpadding
            <> pointer
            <> "\n"
            <> parseErrorTextPretty e
        lineNumber = (show . unPos . sourceLine) epos
        padding = replicate (length lineNumber + 1) ' '
        rpadding = [if isWide c then '　' else ' ' | c <- take rpshift sline]
        rpshift = unPos (sourceColumn epos) - 1
        pointer = replicate pointerLen '^'
        pointerLen =
          if rpshift + elen > slineLen
            then slineLen - rpshift + 1
            else elen
        slineLen = length sline
        elen =
          case e of
            TrivialError _ Nothing _ -> 1
            TrivialError _ (Just x) _ -> errorItemLength x
            FancyError _ xs ->
              S.foldl' (\a b -> max a (errorFancyLength b)) 1 xs

errorItemLength :: ErrorItem (Token Text) -> Int
errorItemLength = \case
  Tokens ts -> NE.length ts
  _ -> 1

errorFancyLength :: ShowErrorComponent e => ErrorFancy e -> Int
errorFancyLength = \case
  ErrorCustom a -> errorComponentLen a
  _ -> 1

parseErrorTextPretty ::
  forall e.
  (ShowErrorComponent e) =>
  -- | Parse error to render
  ParseError Text e ->
  -- | Result of rendering
  String
parseErrorTextPretty (TrivialError _ us ps) =
  if isNothing us && S.null ps
    then "알 수 없는 파싱 에러입니다.\n"
    else
      let gotMessage = messageItemsPretty (showErrorItem `S.map` maybe S.empty S.singleton us)
          expectedMessage = messageItemsPretty (showErrorItem `S.map` ps)
       in if null expectedMessage
            then byJongseongSubject gotMessage <> " 올 자리가 아닙니다."
            else byJongseongSubject gotMessage <> " 아닌 " ++ byJongseongSubject expectedMessage ++ " 올 자리입니다."
parseErrorTextPretty (FancyError _ xs) =
  if S.null xs
    then "알 수 없는 파싱 에러입니다.\n"
    else toString $ unlines (toText <$> (showErrorFancy <$> S.toAscList xs))

showErrorItem :: ErrorItem Char -> String
showErrorItem = \case
  Tokens ts -> showTokens ts
  Label label -> NE.toList label
  EndOfInput -> "입력의 끝"

showErrorFancy :: ShowErrorComponent e => ErrorFancy e -> String
showErrorFancy = \case
  ErrorFail msg -> msg
  ErrorIndentation order ref actual ->
    "올바르지 않은 들여쓰기입니다. ("
      <> show (unPos ref)
      <> " "
      <> p
      <> " 단계여야 하는데 "
      <> show (unPos actual)
      <> " 단계가 주어짐)"
    where
      p = case order of
        LT -> "보다 작은"
        EQ -> ""
        GT -> "보다 큰"
  ErrorCustom a -> showErrorComponent a

messageItemsPretty ::
  -- | Collection of messages
  Set String ->
  -- | Prefix to prepend
  String
messageItemsPretty ts
  | S.null ts = ""
  | otherwise =
    (orList . NE.fromList . S.toAscList) ts

orList :: NonEmpty String -> String
orList (x :| []) = x
orList (x :| [y]) = x <> " 또는 " <> y
orList xs = intercalate ", " (toList xs) <> " 중 하나"

showTokens :: NonEmpty Char -> String
showTokens = stringPretty

stringPretty :: NonEmpty Char -> String
stringPretty (x :| []) = charPretty x
stringPretty ('\r' :| "\n") = "줄 바꿈"
stringPretty xs = "\"" <> concatMap f (NE.toList xs) <> "\""
  where
    f ch =
      case charPretty' ch of
        Nothing -> [ch]
        Just pretty -> "<" <> pretty <> ">"

-- | @charPretty ch@ returns user-friendly string representation of given
-- character @ch@, suitable for using in error messages.
charPretty :: Char -> String
charPretty ' ' = "공백"
charPretty ch = fromMaybe ("'" <> [ch] <> "'") (charPretty' ch)

-- | If the given character has a pretty representation, return that,
-- otherwise 'Nothing'. This is an internal helper.
charPretty' :: Char -> Maybe String
charPretty' = \case
  '\NUL' -> Just "널 문자"
  '\SOH' -> Just "헤딩의 시작"
  '\STX' -> Just "텍스트의 시작"
  '\ETX' -> Just "텍스트의 끝"
  '\EOT' -> Just "전송의 끝"
  '\ENQ' -> Just "조사 문자"
  '\ACK' -> Just "인정 문자"
  '\BEL' -> Just "벨 문자"
  '\BS' -> Just "백스페이스"
  '\t' -> Just "탭"
  '\n' -> Just "줄 바꿈"
  '\v' -> Just "세로 탭"
  '\f' -> Just "폼 피드"
  '\r' -> Just "캐리지 리턴 문자"
  '\SO' -> Just "쉬프트 아웃"
  '\SI' -> Just "쉬프트 인"
  '\DLE' -> Just "데이터 링크 이스케이프"
  '\DC1' -> Just "기기 제어 1"
  '\DC2' -> Just "기기 제어 2"
  '\DC3' -> Just "기기 제어 3"
  '\DC4' -> Just "기기 제어 4"
  '\NAK' -> Just "불인정"
  '\SYN' -> Just "동기화 대기"
  '\ETB' -> Just "전송 블럭의 끝"
  '\CAN' -> Just "취소"
  '\EM' -> Just "미디어의 끝"
  '\SUB' -> Just "대체"
  '\ESC' -> Just "ESC"
  '\FS' -> Just "파일 구분자"
  '\GS' -> Just "그룹 구분자"
  '\RS' -> Just "레코드 구분자"
  '\US' -> Just "단위 구분자"
  '\DEL' -> Just "삭제"
  '\160' -> Just "줄 바꿈 없는 공백"
  _ -> Nothing