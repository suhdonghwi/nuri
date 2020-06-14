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
import Data.Text.ICU.Char (EastAsianWidth (..), EastAsianWidth_ (..), property)
import Text.Megaparsec (PosState, pstateSourcePos)
import Text.Megaparsec.Error hiding (errorBundlePretty, parseErrorTextPretty)
import Text.Megaparsec.Pos
import Text.Megaparsec.Stream

isWide :: Char -> Bool
isWide c = property EastAsianWidth c `elem` [EAFull, EAWide]

errorBundlePretty ::
  forall s e.
  ( Stream s,
    ShowErrorComponent e
  ) =>
  -- | Parse error bundle to display
  ParseErrorBundle s e ->
  -- | Textual rendition of the bundle
  String
errorBundlePretty ParseErrorBundle {..} =
  let (r, _) = foldl' f (id, bundlePosState) bundleErrors
   in drop 1 (r "")
  where
    f ::
      (String -> String, PosState s) ->
      ParseError s e ->
      (String -> String, PosState s)
    f (o, !pst) e = (o . (outChunk ++), pst')
      where
        (sline, pst') = reachOffset (errorOffset e) pst
        epos = pstateSourcePos pst'
        outChunk =
          "\n" <> sourcePosPretty epos <> ":\n"
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
        pxy = Proxy :: Proxy s
        elen =
          case e of
            TrivialError _ Nothing _ -> 1
            TrivialError _ (Just x) _ -> errorItemLength pxy x
            FancyError _ xs ->
              S.foldl' (\a b -> max a (errorFancyLength b)) 1 xs

errorItemLength :: Stream s => Proxy s -> ErrorItem (Token s) -> Int
errorItemLength pxy = \case
  Tokens ts -> tokensLength pxy ts
  _ -> 1

errorFancyLength :: ShowErrorComponent e => ErrorFancy e -> Int
errorFancyLength = \case
  ErrorCustom a -> errorComponentLen a
  _ -> 1

parseErrorTextPretty ::
  forall s e.
  (Stream s, ShowErrorComponent e) =>
  -- | Parse error to render
  ParseError s e ->
  -- | Result of rendering
  String
parseErrorTextPretty (TrivialError _ us ps) =
  if isNothing us && S.null ps
    then "알 수 없는 파싱 에러입니다.\n"
    else
      messageItemsPretty (showErrorItem pxy `S.map` maybe S.empty S.singleton us) "이(가) 아닌"
        <> messageItemsPretty (showErrorItem pxy `S.map` ps) "이(가) 올 자리입니다."
  where
    pxy = Proxy :: Proxy s
parseErrorTextPretty (FancyError _ xs) =
  if S.null xs
    then "알 수 없는 파싱 에러입니다.\n"
    else toString $ unlines (toText <$> (showErrorFancy <$> S.toAscList xs))

showErrorItem :: Stream s => Proxy s -> ErrorItem (Token s) -> String
showErrorItem pxy = \case
  Tokens ts -> showTokens pxy ts
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
      <> " 단계여야 하는데"
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
  String ->
  -- | Result of rendering
  String
messageItemsPretty ts postfix
  | S.null ts = ""
  | otherwise =
    (orList . NE.fromList . S.toAscList) ts <> postfix <> "\n"

orList :: NonEmpty String -> String
orList (x :| []) = x
orList (x :| [y]) = x <> " 또는 " <> y
orList xs = intercalate ", " (toList xs) <> " 중 하나"