{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Nuri.Parse.Error (errorBundlePretty) where

import qualified Data.Set as S
import Data.Text.ICU.Char (EastAsianWidth (..), EastAsianWidth_ (..), property)
import Text.Megaparsec (PosState, pstateSourcePos)
import Text.Megaparsec.Error hiding (errorBundlePretty)
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