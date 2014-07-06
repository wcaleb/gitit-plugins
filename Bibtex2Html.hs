module Bibtex2Html (plugin) where

-- Transform fenced BibTex codeblocks to formatted citations
-- Requires Bibtex2Html to be installed in $PATH
-- Download Bibtex2Html https://www.lri.fr/~filliatr/bibtex2html/

import Network.Gitit.Interface
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(ExitSuccess))
import Control.Monad.Trans (liftIO)
import Data.Text

plugin :: Plugin
plugin = mkPageTransformM transformBlock

-- Based on Dot.hs plugin at https://github.com/jgm/gitit/blob/master/plugins/Dot.hs
transformBlock :: Block -> PluginM Block
transformBlock (CodeBlock (_, classes, namevals) contents) | "bib" `elem` classes = liftIO $ do
   (ec, _out, err) <- readProcessWithExitCode "bibtex2html" ["-q", "-dl", "-nodoc"] contents
   if ec == ExitSuccess
      then return $ Header 1 ("", [""], [("","")]) [Strong [RawInline (Format "html") (getCitation _out)]]
      -- if bibtex2html returns an error, the original Pandoc codeblock will be returned
      else return $ CodeBlock ([], ["bib"], []) contents
transformBlock x = return x

-- Ugly way to get everything between <dd> tags in output
getCitation = unpack . snd . breakOnEnd (pack "<dd>") . fst . breakOnEnd (pack "</dd>") . pack
