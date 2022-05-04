--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints -O2 #-}
--------------------------------------------------------------------------------

module Compilers
    ( compileToPandocAST
    , renderPandocASTtoLaTeX
    , renderPandocASTtoPDF
    , renderPandocASTtoHTML
    , buildLaTeX
    , metadataCompiler
    ) where

import           Hakyll
import           Hakyll.Core.Compiler.Internal
import           XMLWalker ()

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString.Lazy as LB
import qualified Data.Map as M
import           Data.Char                     (isDigit, isSpace, isAlpha)
import           Data.Maybe                    (fromMaybe, mapMaybe)
import           Data.List                     (isPrefixOf, stripPrefix, sort, intersperse, zipWith4)
import           Data.Fixed                    (Fixed)
import           Data.Functor                  ((<&>))
import           Data.Traversable
import           Data.Foldable
import qualified Data.Set as S
import qualified Data.Char as C
import qualified Data.Binary as B

import           GHC.IO                        (unsafePerformIO)

import           System.Process                (system, readProcessWithExitCode)
import           System.Exit                   (ExitCode(..))
import           System.Directory              (createDirectory, setCurrentDirectory, listDirectory,
                                                renameFile, copyFile, doesFileExist)
import           System.FilePath               (replaceExtension, takeDirectory,
                                                dropExtensions, takeFileName, takeBaseName,
                                                replaceBaseName, takeExtension, (</>), (<.>), (-<.>),
                                                replaceDirectory, dropExtension, stripExtension)

import           Control.Monad
import           Control.Applicative
import           Control.Monad.State.Lazy
import qualified Control.Monad.Reader as R
import           Control.Monad.Except


import           Text.Pandoc
import           Text.Pandoc.Walk
import           Text.Pandoc.Shared            (filteredFilesFromArchive)
import           Text.Pandoc.Builder as B
import           Text.Pandoc.Parsing           (runF, defaultParserState, extractIdClass)
import           Text.Pandoc.Readers.Markdown  (yamlToMeta)
import           Text.Pandoc.CrossRef
import           Text.Blaze.Html5.Attributes   (xmlns, item)
import qualified Text.XML as X

buildLaTeX :: Item String -> Compiler (Item TmpFile)
buildLaTeX item = do
    latexFile@(TmpFile latexPath) <- newTmpFile "lualatex.tex"
    unsafeCompiler $ writeFile latexPath $ itemBody item
    runLuaLaTeX latexPath >>= makeItem . TmpFile

getConfig :: Compiler Configuration
getConfig = compilerConfig <$> compilerAsk

runLuaLaTeX :: FilePath  -> Compiler FilePath
runLuaLaTeX latexPath = do
    exitCode <- unsafeCompiler $ system $ unwords
        [ "latexmk", "-lualatex", "-outdir=" ++ takeDirectory latexPath
        , latexPath, ">/dev/null", "2>&1"]
    id <- getUnderlying
    conf <- getConfig
    let idPath = toFilePath id
        logDir = providerDirectory conf </> storeDirectory conf 
            </> "texlog" </> takeDirectory idPath
        logDestinationPath = case identifierVersion id of
            Nothing -> logDir </> takeBaseName idPath <.> "log"
            Just v -> logDir </> (takeBaseName idPath ++ "#" ++ v) <.> "log"
        logSourcePath = latexPath -<.> "log"
    unsafeCompiler $ do
        exists <- doesFileExist logSourcePath
        when exists $ do
                makeDirectories logDestinationPath
                copyFile logSourcePath logDestinationPath
    case exitCode of
        ExitSuccess ->
            return $ latexPath -<.> "pdf"
        ExitFailure err ->
            throwError ["LaTeX compiler: failed while processing item " ++
                show id ++ " exit code " ++ show err ++ "."]

pdfToSVGs :: FilePath -> Compiler [FilePath]
pdfToSVGs pdfPath = do
    exitCode <- unsafeCompiler $ system $ unwords
        ["pdf2svg", pdfPath, svgPath, "all"]
    case exitCode of
        ExitSuccess ->
            map snd . sort . mapMaybe processSVGFileNo <$>
                unsafeCompiler (listDirectory (takeDirectory pdfPath))
        ExitFailure err -> do
            id <- getUnderlying
            throwError ["PDFtoSVG compiler: failed while processing item " ++
                show id ++ " exit code " ++ show err ++ "."]
    where
        svgPath :: FilePath
        svgPath =
            replaceBaseName
                (pdfPath -<.> "svg")
                (takeBaseName pdfPath ++ "-eqn-%i")

        processSVGFileNo :: FilePath -> Maybe (Integer, FilePath)
        processSVGFileNo fp = do
            bare <- stripExtension "svg" fp
            num <- stripPrefix (takeBaseName pdfPath ++ "-eqn-") bare
            guard $ not (null num) && all isDigit num
            return (read num, takeDirectory pdfPath </> fp)

cleanImageSVG :: Integer -> T.Text -> Compiler T.Text
cleanImageSVG num svg = do
    (exitCode, svgout, _) <- unsafeCompiler $
        readProcessWithExitCode "svgcleaner" ["--remove-nonsvg-attributes=no","-c", "-"] (T.unpack svg)
    case exitCode of
        ExitSuccess -> return $ T.pack svgout
        ExitFailure err -> do
            id <- getUnderlying
            throwError ["SVGCleaner: failed while processing image " ++ show num ++
                " from item " ++ show id ++ " exit code " ++ show err ++ "."]

metadataCompiler :: Compiler (Item Meta)
metadataCompiler =
    getResourceLBS >>= withItemBody (
        unsafeCompiler . runIOorExplode . yamlToMeta domsDefaultReaderOptions Nothing)

--------------------------------------------------------------------------------

domsDefaultReaderOptions :: ReaderOptions
domsDefaultReaderOptions = defaultHakyllReaderOptions

domsDefaultHTMLWriterOptions :: WriterOptions
domsDefaultHTMLWriterOptions = defaultHakyllWriterOptions

domsDefaultLaTeXWriterOptions :: WriterOptions
domsDefaultLaTeXWriterOptions = defaultHakyllWriterOptions

{-# NOINLINE domsDefaultStandaloneLaTeXWriterOptions #-}
domsDefaultStandaloneLaTeXWriterOptions :: WriterOptions
domsDefaultStandaloneLaTeXWriterOptions = unsafePerformIO $ do
    templ <- runIO (compileDefaultTemplate "latex") >>= handleError
    return $ def
        {   writerTemplate = Just templ
        }

domsDefaultXMLRenderSettings :: X.RenderSettings
domsDefaultXMLRenderSettings = X.def { X.rsXMLDeclaration = False }

--------------------------------------------------------------------------------

writePandocTyped :: (Pandoc -> PandocPure T.Text) -> Pandoc -> String
writePandocTyped writer doc =
    case runPure $ writer doc of
        Left err    -> error $ "Compiler.writePandocTyped: " ++ show err
        Right doc' -> T.unpack doc'

writePandocToLaTeX :: Pandoc -> String
writePandocToLaTeX = writePandocTyped $ writeLaTeX domsDefaultLaTeXWriterOptions

writePandocToStandaloneLaTeX :: Pandoc -> String
writePandocToStandaloneLaTeX =
    writePandocTyped $ writeLaTeX domsDefaultStandaloneLaTeXWriterOptions

writePandocToHTML :: Pandoc -> String
writePandocToHTML = writePandocTyped $ writeHtml5String domsDefaultHTMLWriterOptions

--------------------------------------------------------------------------------

compileToPandocAST :: Compiler (Item Pandoc)
compileToPandocAST = do
    getResourceString >>= readPandocWith domsDefaultReaderOptions

-- Apply citeproc and crossref filters
applyPandocFilters :: Meta -> Maybe Format -> Item Pandoc -> Compiler (Item Pandoc)
applyPandocFilters opts fmt item = do
    bibs <- loadAll "pandoc/*.bib"
    csl <- load "pandoc/elsevier.csl"
    processPandocBiblios csl bibs $
        fmap (runCrossRef (getMeta (itemBody item) <> opts)
            fmt defaultCrossRefAction) item

-- Generate LaTeX body only, no pandoc filters applied
renderPandocASTtoLaTeX :: Item Pandoc -> Compiler (Item String)
renderPandocASTtoLaTeX =
    return . fmap writePandocToLaTeX

-- Apply standard pandoc LaTeX template and compile 
renderPandocASTtoPDF :: Item Pandoc -> Compiler (Item TmpFile)
renderPandocASTtoPDF doc = do
    latexOpts <- load "pandoc/latexOptions.yaml"
    pdfOpts <- load "pandoc/pdfGenOptions.yaml"
    crossOpts <- load "pandoc/crossrefOptions.yaml"
    applyPandocFilters (itemBody latexOpts <> itemBody pdfOpts <> itemBody crossOpts)
        (Just "latex") doc >>= buildLaTeX . fmap writePandocToStandaloneLaTeX

-------------------------------------------------------------------------------
-- Standalone Binary instances for pandoc types

deriving instance B.Binary Pandoc
deriving instance B.Binary CitationMode
deriving instance B.Binary Citation
deriving instance B.Binary Inline
deriving instance B.Binary Block
deriving instance B.Binary QuoteType
deriving instance B.Binary MathType
deriving instance B.Binary Format
deriving instance B.Binary ListNumberStyle
deriving instance B.Binary ListNumberDelim
deriving instance B.Binary Caption
deriving instance B.Binary Alignment
deriving instance B.Binary ColWidth
deriving instance B.Binary TableHead
deriving instance B.Binary TableBody
deriving instance B.Binary TableFoot
deriving instance B.Binary Row
deriving instance B.Binary Cell
deriving instance B.Binary RowSpan
deriving instance B.Binary ColSpan
deriving instance B.Binary RowHeadColumns
deriving instance B.Binary Meta
deriving instance B.Binary MetaValue

instance Writable Meta where
    -- Shouldn't be written.
    write _ _ = return ()

instance Writable Pandoc where
    -- Shouldn't be written.
    write _ _ = return ()

-------------------------------------------------------------------------------
-- Very simple backtracking parser monad.

type ParserMonad = StateT String Maybe

stripSpaces :: ParserMonad ()
stripSpaces = modify $ dropWhile isSpace

word :: (Char -> Bool) -> ParserMonad String
word p = do
    s0 <- get
    let (x, s1) = span p s0
    put s1
    return x

token :: String -> ParserMonad String
token t = do
    s0 <- get
    let (s,s1) = splitAt (length t) s0
    guard (s == t)
    put s1
    return s

number :: ParserMonad Int
number = read <$> word isDigit

-------------------------------------------------------------------------------
-- Parser to read the image dimensions recorded in a LaTeX log file.

-- Dimensions in em, for conversion from LaTeX pts we assume a 17 point fontsize here.

type Dimen = Fixed 1000000

fontSize :: Fixed 1000000
fontSize = 17

data ImageInfo = ImageInfo
    { depth :: Dimen
    , height :: Dimen
    , width :: Dimen
    } deriving (Show)

getEqnDimens :: FilePath -> Compiler [ImageInfo]
getEqnDimens fp = unsafeCompiler $
        mapMaybe (evalStateT parseImageDimens) . lines <$> readFile fp
    where
        parseDimen :: ParserMonad Dimen
        parseDimen = do
            n1 <- word isDigit
            n2 <- (token "." >> word isDigit) <|> return "0"
            token "pt"
            return $ read (n1 ++ "." ++ n2) / fontSize

        parseImageDimens :: ParserMonad ImageInfo
        parseImageDimens = do
            token "Preview: eqn" >> stripSpaces >> token "("
            e <- number
            stripSpaces >> token ")" >> stripSpaces >> token "dims" >> stripSpaces
            d1 <- parseDimen
            token ","
            d2 <- parseDimen
            token ","
            ImageInfo d1 d2 <$> parseDimen  -- depth, height, width

-------------------------------------------------------------------------------
-- Build an HTML file with embedded SVG sections from an input containing 
-- LaTeX equations.

renderPandocASTtoHTML :: Item Pandoc -> Compiler (Item String)
renderPandocASTtoHTML doc = do
    svgs <- makeEquationSVGs doc
    htmlOpts <- load "pandoc/htmlOptions.yaml"
    crossOpts <- load "pandoc/crossrefOptions.yaml"
    fmap (writePandocToHTML . embedEquationImages svgs) <$>
        applyPandocFilters (itemBody htmlOpts <> itemBody crossOpts) (Just "html5") doc

embedEquationImages :: [T.Text] -> Pandoc -> Pandoc
embedEquationImages imgs (Pandoc meta body) =
    Pandoc meta (evalState (walkM transformEquation body) imgs)
    where
        transformEquation :: Inline -> State [T.Text] Inline
        transformEquation (Math typ body) = do
            imgs <- get
            case imgs of
                [] ->
                    return $ Span
                        ( ""
                        , case typ of
                            InlineMath -> ["inline-eqn"]
                            DisplayMath -> ["display-eqn"]
                        , [("style", "color: red;")]
                        )
                        [Str "<missing image>"]
                (img:imgs') -> do
                    put imgs'
                    case typ of
                        InlineMath -> return $ Span
                            ("", ["inline-eqn"], [])
                            [RawInline "html" img]
                        DisplayMath -> return $ Span
                            ("", ["display-eqn"], [])
                            [ Span ("", ["display-eqn-left"], []) []
                            , Span
                                ("", ["display-eqn-center"], [])
                                [RawInline "html" img]
                            , Span
                                ("", ["display-eqn-right"], [])
                                (case T.breakOnEnd "\\doms@tag{" body of
                                    ("", _) -> []
                                    (_, label) ->
                                        [ Str "("
                                        , Str (T.takeWhile (/= '}') label)
                                        , Str ")"
                                        ]
                                )
                            ]
        transformEquation x = return x

getMeta :: Pandoc -> Meta
getMeta (Pandoc meta _) = meta

makeEquationSVGs :: Item Pandoc -> Compiler [T.Text]
makeEquationSVGs (Item _ (Pandoc meta body)) =
    if null eqnBlocks
    then return []
    else do
        TmpFile latexPath <- newTmpFile "eqnimages.tex"
        latexOpts <- load "pandoc/latexOptions.yaml"
        imgOpts <- load "pandoc/imgGenOptions.yaml"
        unsafeCompiler $ writeFile latexPath $
            imgGenLaTeX (itemBody latexOpts <> itemBody imgOpts)
        svgDocs <- runLuaLaTeX latexPath >>=
            pdfToSVGs >>= traverse (unsafeCompiler . X.readFile X.def)
        imgInfo <- getEqnDimens $ latexPath -<.> "log"
        sequenceA (zipWith3 processImage [1..] svgDocs imgInfo)
    where
        queryEquation :: Inline -> [Block]
        queryEquation (Math typ text) =
            [ RawBlock "latex" $ T.concat
                [ "\\begin{shipper}{"
                , case typ of
                    InlineMath -> "\\textstyle"
                    DisplayMath -> "\\displaystyle"
                , "}"
                , text
                , "\\end{shipper}\n"
                ]
            ]
        queryEquation x = []

        eqnBlocks :: [Block]
        eqnBlocks = query queryEquation body

        imgGenLaTeX :: Meta -> String
        imgGenLaTeX opts =
            writePandocToStandaloneLaTeX $ Pandoc opts eqnBlocks

processImage :: Integer -> X.Document -> ImageInfo -> Compiler T.Text
processImage num svg (ImageInfo dp ht wd) =
    return $ (LT.toStrict . X.renderText domsDefaultXMLRenderSettings) transformedSVG
    where
        queryID :: X.Element -> S.Set T.Text
        queryID (X.Element _ attr _) = maybe S.empty S.singleton (M.lookup "id" attr)

        allIDs :: S.Set T.Text
        allIDs = query queryID svg

        transformID :: X.Element -> X.Element
        transformID e@(X.Element nm attr nodes) =
            case M.lookup "id" attr of
                Just t ->
                    X.Element
                        nm
                        (M.insert "id" (T.concat ["eqn", T.pack (show num), "-", t]) attr)
                        nodes
                Nothing -> e

        transformTags :: X.Element -> X.Element
        transformTags (X.Element nm attr nodes) =
            X.Element nm (fmap transformAttrValue attr) nodes

        transformAttrValue :: T.Text -> T.Text
        transformAttrValue s =
            T.concat $ intersperse "#" $ head splitup:map transformTag (tail splitup)
            where
                splitup :: [T.Text]
                splitup = T.splitOn "#" s

                transformTag :: T.Text -> T.Text
                transformTag s  =
                    if S.member (T.takeWhile (\c -> C.isAlphaNum c || c == '-') s) allIDs
                        then T.concat ["eqn", T.pack (show num), "-", s]
                        else s

        adjustedDimens :: M.Map X.Name T.Text
        adjustedDimens= M.fromList
            [ ("width", T.pack $ show wd ++ "em")
            , ("height", T.pack $ show (dp + ht) ++ "em")
            , ("style", T.pack $ "vertical-align: -" ++ show dp ++ "em;")
            ]

        transformedSVG :: X.Document
        transformedSVG = case walk (transformID . transformTags) svg of
            X.Document pro (X.Element nm attr nodes) epi |
                nm == "{http://www.w3.org/2000/svg}svg" ->
                    X.Document pro (X.Element nm (adjustedDimens <> attr) nodes) epi
            d -> d




