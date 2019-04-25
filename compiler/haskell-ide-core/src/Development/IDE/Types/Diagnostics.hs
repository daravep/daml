-- Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE BlockArguments #-}
module Development.IDE.Types.Diagnostics (
  LSP.Diagnostic(..),
  FileDiagnostics,
  Location(..),
  Range(..),
  LSP.DiagnosticSeverity(..),
  Position(..),
  DiagnosticStore,
  StoreItem(..),
  Uri(..),
  noLocation,
  noRange,
  ideErrorText,
  ideErrorPretty,
  errorDiag,
  ideTryIOException,
  prettyFileDiagnostics,
  prettyDiagnostic,
  prettyDiagnosticStore,
  defDiagnostic,
  addDiagnostics,
  filterSeriousErrors,
  addLocation,
  addFilePath,
  getDiagnosticsFromStore
  ) where

import Control.Exception
import qualified Control.Lens as L
import Data.Either.Combinators
import Data.Maybe as Maybe
import Data.Foldable
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc.Syntax
import Data.String (IsString(..))
import qualified Text.PrettyPrint.Annotated.HughesPJClass as Pretty
import           Language.Haskell.LSP.Types as LSP (
    DiagnosticSeverity(..)
  , Diagnostic(..)
  , filePathToUri
  , uriToFilePath
  , List(..)
  , DiagnosticRelatedInformation(..)
  , Uri(..)
  )
import Language.Haskell.LSP.Diagnostics

import Development.IDE.Types.Location

ideErrorText :: FilePath -> T.Text -> LSP.Diagnostic
ideErrorText fp = errorDiag fp "Ide Error"

ideErrorPretty :: Pretty.Pretty e => FilePath -> e -> LSP.Diagnostic
ideErrorPretty fp = ideErrorText fp . T.pack . Pretty.prettyShow

errorDiag :: FilePath -> T.Text -> T.Text -> LSP.Diagnostic
errorDiag fp src =
  addFilePath fp . diagnostic noRange LSP.DsError src

-- | This is for compatibility with our old diagnostic type
diagnostic :: Range
           -> LSP.DiagnosticSeverity
           -> T.Text -- ^ source
           -> T.Text -- ^ message
           -> LSP.Diagnostic
diagnostic rng sev src msg
    = LSP.Diagnostic {
          _range = rng,
          _severity = Just sev,
          _code = Nothing,
          _source = Just src,
          _message = msg,
          _relatedInformation = Nothing
          }

-- | Any optional field is instantiated to Nothing
defDiagnostic ::
  Range ->
  T.Text -> -- ^ error message
  LSP.Diagnostic
defDiagnostic _range _message = LSP.Diagnostic {
    _range
  , _message
  , _severity = Nothing
  , _code = Nothing
  , _source = Nothing
  , _relatedInformation = Nothing
  }

-- | addLocation but with no range information
addFilePath ::
  FilePath ->
  LSP.Diagnostic ->
  LSP.Diagnostic
addFilePath fp =
  addLocation $ Location (filePathToUri fp) noRange

-- | This adds location information to the diagnostics but this is only used in
--   the case of serious errors to give some context to what went wrong
addLocation ::
  Location ->
  LSP.Diagnostic ->
  LSP.Diagnostic
addLocation loc d =
  d {
    LSP._relatedInformation =
        Just $
        maybe
        (LSP.List [rel loc])
        (L.over lspList (rel loc:)) $
        _relatedInformation d
    } where
      rel loc = DiagnosticRelatedInformation loc ""

lspList :: L.Iso (LSP.List a) (LSP.List b) [a] [b]
lspList = L.coerced

filterSeriousErrors ::
    FilePath ->
    [LSP.Diagnostic] ->
    [LSP.Diagnostic]
filterSeriousErrors fp =
    filter (maybe False hasSeriousErrors . LSP._relatedInformation)
    where
        hasSeriousErrors :: List DiagnosticRelatedInformation -> Bool
        hasSeriousErrors (List a) = any ((/=) uri . _uri . _location) a
        uri = LSP.filePathToUri fp

addDiagnostics ::
  FilePath ->
  [LSP.Diagnostic] ->
  DiagnosticStore -> DiagnosticStore
addDiagnostics fp diags ds =
    updateDiagnostics
    ds
    (LSP.filePathToUri fp)
    Nothing $
    partitionBySource diags

ideTryIOException :: FilePath -> IO a -> IO (Either LSP.Diagnostic a)
ideTryIOException fp act =
  mapLeft (\(e :: IOException) -> ideErrorText fp $ T.pack $ show e) <$> try act

-- | Human readable diagnostics for a specific file.
--
--   This type packages a pretty printed, human readable error message
--   along with the related source location so that we can display the error
--   on either the console or in the IDE at the right source location.
--
type FileDiagnostics = (Uri, StoreItem)

prettyRange :: Range -> Doc SyntaxClass
prettyRange Range{..} =
  label_ "Range" $ vcat
  [ label_ "Start:" $ prettyPosition _start
  , label_ "End:  " $ prettyPosition _end
  ]

prettyPosition :: Position -> Doc SyntaxClass
prettyPosition Position{..} = label_ "Position" $ vcat
   [ label_ "Line:" $ pretty _line
   , label_ "Character:" $ pretty _character
   ]

stringParagraphs :: T.Text -> Doc a
stringParagraphs = vcat . map (fillSep . map pretty . T.words) . T.lines

prettyDiagnostic :: LSP.Diagnostic -> Doc SyntaxClass
prettyDiagnostic LSP.Diagnostic{..} =
    vcat
        [label_ "Range:   "
            $ prettyRange _range
        , label_ "Source:  " $ pretty _source
        , label_ "Severity:" $ pretty $ show sev
        , label_ "Message: "
            $ case sev of
              LSP.DsError -> annotate ErrorSC
              LSP.DsWarning -> annotate WarningSC
              LSP.DsInfo -> annotate InfoSC
              LSP.DsHint -> annotate HintSC
            $ stringParagraphs _message
        , label_ "Code:" $ pretty _code
        ]
    where
        sev = fromMaybe LSP.DsError _severity

prettyDiagnosticStore :: DiagnosticStore -> Doc SyntaxClass
prettyDiagnosticStore ds =
    vcat $ map prettyFileDiagnostics $ Map.assocs ds


prettyFileDiagnostics :: FileDiagnostics -> Doc SyntaxClass
prettyFileDiagnostics (uri, StoreItem _ diags) =
    label_ "Compiler error in" $ vcat
        [ label_ "File:" $ pretty filePath
        , label_ "Errors:" $ prettyFileDiags storeContents
        ] where

    prettyFileDiags :: (FilePath, [(T.Text, [LSP.Diagnostic])]) -> Doc SyntaxClass
    prettyFileDiags (fp,stages) =
        label_ ("File: "<>fp) $ vcat $ map prettyStage stages

    prettyStage :: (T.Text, [LSP.Diagnostic]) -> Doc SyntaxClass
    prettyStage (stage,diags) =
        label_ ("Stage: "<>T.unpack stage) $ vcat $ map prettyDiagnostic diags

    filePath :: FilePath
    filePath = fromMaybe dontKnow $ uriToFilePath uri

    storeContents ::
        (FilePath, [(T.Text, [LSP.Diagnostic])])
        -- ^ Source File, Stage Source, Diags
    storeContents = (fromMaybe dontKnow $ uriToFilePath uri, getDiags diags)

    dontKnow :: IsString s => s
    dontKnow = "<unknown>"

    getDiags :: DiagnosticsBySource -> [(T.Text, [LSP.Diagnostic])]
    getDiags = map (\(ds, diag) -> (fromMaybe dontKnow ds, toList diag)) . Map.assocs

getDiagnosticsFromStore :: StoreItem -> [Diagnostic]
getDiagnosticsFromStore (StoreItem _ diags) =
    toList =<< Map.elems diags
