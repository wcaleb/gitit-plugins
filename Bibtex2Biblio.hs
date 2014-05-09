module Bibtex2Biblio (plugin) where

-- Requires pandoc-citeproc library and pandoc >= 1.10
-- Requires that gitit be compiled from https://github.com/jgm/gitit using cabal file

import Network.Gitit.Interface
import Control.Monad.Trans (liftIO)
import System.IO
import qualified Data.ByteString.Lazy as L
import Text.CSL.Input.Bibtex
import Text.CSL.Output.Pandoc (renderPandoc, renderPandoc')
import Text.CSL.Parser
import Text.CSL.Proc
import Data.List (isInfixOf)
import System.FilePath

plugin :: Plugin
plugin = mkPageTransformM transformBlock

cslfile :: FilePath
cslfile = "/home/wcm1/.csl/chicago.csl"

processCSL = do
   rawCSL <- L.readFile cslfile
   return $ parseCSL' rawCSL

transformBlock :: Block -> PluginM Block
transformBlock (CodeBlock (_, classes, namevals) contents) | "bib" `elem` classes = liftIO $ do
   csl <- processCSL
   refs <- readBibtexInputString True contents
   -- don't convert bibtex items that have a "crossref" field
   if isInfixOf "crossref" contents
      then return $ CodeBlock ([], ["bib"], []) contents
      else return $ Header 1 ("", [""], [("","")]) [Strong (renderPandoc csl (head (processBibliography procOpts csl refs)))]
transformBlock x = return x
