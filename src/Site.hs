--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import           Data.Monoid                        (mappend, mconcat)
import           Data.List                          (sortBy, intersperse, intercalate)
import           Data.Ord                           (comparing)
import           Hakyll
import           Control.Monad                      (liftM, forM_)
import           System.FilePath                    (takeBaseName, (<.>), takeFileName, replaceExtension)
import           Text.Blaze.Html                    (toHtml, toValue, (!))
import qualified Text.Blaze.Html5                   as H
import qualified Text.Blaze.Html5.Attributes        as A
import           Text.Blaze.Html.Renderer.String    (renderHtml)

import           Compilers
--------------------------------------------------------------------------------

config :: Configuration
config = defaultConfiguration
  { destinationDirectory = "docs"
  }

main :: IO ()
main = hakyllWith config $ do
    match ("images/*" .||. "js/*" .||. "fonts/*") $ do
        route   idRoute
        compile copyFileCompiler

    match ("pdfs/*.pdf" .||. "elements.pdf" .||. "more-elements.pdf") $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "error/*" $ do
        route $ gsubRoute "error/" (const "") `composeRoutes` setExtension "html"
        compile $ compileToPandocAST 
            >>= renderPandocASTtoHTML
            >>= applyAsTemplate siteCtx
            >>= loadAndApplyTemplate "templates/default.html" (baseSidebarCtx <> siteCtx)
    
    match "pandoc/*.bib" $ 
        compile biblioCompiler

    match "pandoc/elsevier.csl" $
        compile cslCompiler

    match "pandoc/*.yaml" $
        compile metadataCompiler

    match "pages/*" $ version "ast" $
        compile compileToPandocAST

    match "pages/*" $ do
        route $ setExtension "html"
        compile $ do
            pageName <- takeBaseName . toFilePath <$> getUnderlying
            let pageCtx = constField pageName "" <>
                        baseNodeCtx
            let evalCtx = functionField "get-meta" getMetadataKey <>
                        functionField "eval" (evalCtxKey pageCtx)
            let activeSidebarCtx = sidebarCtx (evalCtx <> pageCtx)

            getUnderlying
                >>= loadBody . setVersion (Just "ast")
                >>= makeItem
                >>= renderPandocASTtoHTML
                >>= saveSnapshot "page-content"
                >>= loadAndApplyTemplate "templates/page.html"    siteCtx
                >>= loadAndApplyTemplate "templates/default.html" (activeSidebarCtx <> siteCtx)
                >>= relativizeUrls

    match "posts/*" $ version "ast" $
        compile compileToPandocAST

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ getUnderlying
            >>= loadBody . setVersion (Just "ast")
            >>= makeItem
            >>= renderPandocASTtoHTML
            >>= \post -> do
                    teaser <- makeTeaser post
                    saveSnapshot "teaser" teaser
                    saveSnapshot "content" post
            >>= loadAndApplyTemplate "templates/post.html" postCtx
            >>= loadAndApplyTemplate "templates/default.html" (baseSidebarCtx <> siteCtx)
            >>= relativizeUrls

    create ["index.html"] $ do
        route idRoute
        compile $ do
            posts <- fmap (take 3) . recentFirst
                        =<< loadAllSnapshots ("posts/*" .&&. hasNoVersion) "content"

            let indexCtx =
                    listField "posts" postCtx (return posts) <>
                    constField "home" ""                     <>                   
                    constField "title" "About"               <>
                    siteCtx

            body <- loadSnapshotBody "pages/About.md" "page-content"

            makeItem body
                >>= makeTeaser
                >>= loadAndApplyTemplate "templates/index.html" indexCtx
                >>= loadAndApplyTemplate "templates/page.html" indexCtx
                >>= loadAndApplyTemplate "templates/default.html" (baseSidebarCtx <> indexCtx)
                >>= relativizeUrls

    paginate <- buildPaginateWith postsGrouper ("posts/*" .&&. hasNoVersion) postsPageId

    paginateRules paginate $ \ page pat -> do
        route idRoute
        compile $ do
            let posts = recentFirst =<< loadAllSnapshots (pat .&&. hasNoVersion) "teaser"
            let indexCtx =
                    constField "title" ("News, page " ++ show page) <>
                    listField "posts" postCtx posts                       <>
                    constField "blog" ""                                  <>
                    paginateContext paginate page                         <>
                    siteCtx

            makeItem ""
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/blog.html" indexCtx
                >>= loadAndApplyTemplate "templates/default.html" (baseSidebarCtx <> indexCtx)
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx <> bodyField "description"
            posts <- fmap (take 10) . recentFirst =<<
                loadAllSnapshots ("posts/*" .&&. hasNoVersion) "teaser"
            renderAtom feedConfig feedCtx posts

    create ["rss.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx <> bodyField "description"
            posts <- fmap (take 10) . recentFirst =<<
                loadAllSnapshots ("posts/*" .&&. hasNoVersion) "teaser"
            renderRss feedConfig feedCtx posts

--------------------------------------------------------------------------------

postsGrouper :: (MonadFail m, MonadMetadata m) => [Identifier] -> m [[Identifier]]
postsGrouper = fmap (paginateEvery 5) . sortRecentFirst

postsPageId :: PageNumber -> Identifier
postsPageId n = fromFilePath $ "blog/page" ++ show n ++ ".html"

makeTeaserWithSeparator :: String -> Item String -> Compiler (Item String)
makeTeaserWithSeparator separator item =
    case needlePrefix separator (itemBody item) of
        Nothing -> fail $
            "Main: no teaser defined for " ++
                show (itemIdentifier item)
        Just t -> return (itemSetBody t item)

teaserSeparator :: String
teaserSeparator = "<!--more-->"

makeTeaser :: Item String -> Compiler (Item String)
makeTeaser = makeTeaserWithSeparator teaserSeparator

--------------------------------------------------------------------------------

feedConfig :: FeedConfiguration
feedConfig = FeedConfiguration
    { feedTitle       = "the elements"
    , feedDescription = "News related to the book 'Elements of ∞-Category Theory'"
    , feedAuthorName  = "Emily Riehl and Dom Verity"
    , feedAuthorEmail = "dominic.verity@mq.edu.au"
    , feedRoot        = "https://elements-book.github.io"
    }

--------------------------------------------------------------------------------

siteCtx :: Context String
siteCtx =
    constField "site-description" "Homepage of 'The Elements'"    <>
    constField "site-url" "https://elements-book.github.io"                                 <>
    constField "tagline" "An ∞-Cosmos Compendium"                                  <>
    constField "site-title" "Elements of ∞-Category Theory"                                 <>
    constField "copy-year" "2022"                                                           <>
    constField "site-author" "Emily Riehl & Dom Verity"                                   <>
    constField "site-email" "eriehl@maths.jhu.edu"                                          <>
    constField "github-url" "https://github.com/elements-book"                              <>
    constField "github-repo" "https://github.com/elements-book/elements-book.github.io"     <>
    constField "twitter-url" "https://twitter.com/EmilyRiehl"                               <>
    defaultContext

--------------------------------------------------------------------------------

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" <>
    modificationTimeField "modified" "%B %e, %Y" <>
    defaultContext

sidebarCtx :: Context String -> Context String
sidebarCtx nodeCtx =
    listField "list_pages" nodeCtx
              (loadAllSnapshots ("pages/*" .&&. hasNoVersion) "page-content") <>
    defaultContext

baseNodeCtx :: Context String
baseNodeCtx =
    urlField "node-url" <>
    titleField "title"

baseSidebarCtx = sidebarCtx baseNodeCtx

evalCtxKey :: Context String -> [String] -> Item String -> Compiler String
evalCtxKey context [key] item =
    unContext context key [] item >>=
    \case
        StringField s -> return s
        _             -> error "Internal error: StringField expected"

getMetadataKey :: [String] -> Item String -> Compiler String
getMetadataKey [key] item = getMetadataField' (itemIdentifier item) key
