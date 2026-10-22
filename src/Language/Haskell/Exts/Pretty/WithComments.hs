{-# LANGUAGE FlexibleInstances, Safe #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.Exts.Pretty.WithComments
-- Copyright   :  (c) Niklas Broberg 2004-2009,
--                (c) The GHC Team, Noel Winstanley 1997-2000
-- License     :  BSD-style (see the file LICENSE.txt)
--
-- Maintainer  :  Dan Burton, danburton.email@gmail.com
-- Stability   :  stable
-- Portability :  portable
--
-- Pretty printer for Haskell AST with attached comments.
--
-----------------------------------------------------------------------------

module Language.Haskell.Exts.Pretty.WithComments
    ( -- * Typeclass for comment extraction
      HasComments(..)
      -- * Comment rendering
    , ppComment
    , ppComments
      -- * Declaration rendering
    , prettyDeclWithComments
    , prettyPrintDeclWithComments
    , prettyPrintDeclWithCommentsAndMode
      -- * Module rendering
    , prettyModuleWithComments
    , prettyPrintModuleWithComments
    , prettyPrintModuleWithCommentsAndMode
    , prettyPrintModuleWithCommentsStyleMode
    ) where

import Language.Haskell.Exts.Pretty
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.Comments
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo)
import Data.Maybe (isJust)

-- | Class of annotation types from which comments can be extracted.
class HasComments l where
  getComments :: l -> [Comment]

instance HasComments [Comment] where
  getComments = id

instance HasComments () where
  getComments _ = []

instance HasComments SrcSpanInfo where
  getComments _ = []

instance HasComments l => HasComments (l, [Comment]) where
  getComments = snd

-- | Render a single 'Comment' to a 'Doc'.
ppComment :: Comment -> Doc
ppComment (Comment False _ str) = text ("--" ++ str)
ppComment (Comment True  _ str) = text ("{-" ++ str ++ "-}")

-- | Render a list of 'Comment's as vertical lines.
ppComments :: [Comment] -> Doc
ppComments [] = empty
ppComments cs = vcat (map ppComment cs)

-- | Render a 'Decl' with its attached comments placed immediately above it.
prettyDeclWithComments :: (HasComments l) => Decl l -> Doc
prettyDeclWithComments d =
  case getComments (ann d) of
    [] -> pretty d
    cs -> ppComments cs $$$ pretty d

ppDeclWithCommentsCond :: (HasComments l) => Decl l -> Doc
ppDeclWithCommentsCond d =
  (if wantsBlankline d then blankline else id) (prettyDeclWithComments d)

ppDeclsWithComments :: (HasComments l) => Bool -> [Decl l] -> [Doc]
ppDeclsWithComments True  ds     = map ppDeclWithCommentsCond ds
ppDeclsWithComments False (d:ds) = prettyDeclWithComments d : map ppDeclWithCommentsCond ds
ppDeclsWithComments _ _          = []

-- | Pretty-print a 'Decl' with comments using 'defaultMode'.
prettyPrintDeclWithComments :: (HasComments l) => Decl l -> String
prettyPrintDeclWithComments = prettyPrintDeclWithCommentsAndMode defaultMode

-- | Pretty-print a 'Decl' with comments using a custom 'PPHsMode'.
prettyPrintDeclWithCommentsAndMode :: (HasComments l) => PPHsMode -> Decl l -> String
prettyPrintDeclWithCommentsAndMode ppMode d =
  renderStyleMode style ppMode (prettyDeclWithComments d)

-- | Pretty-print a 'Module' with attached comments on the module header and declarations.
prettyModuleWithComments :: (HasComments l) => Module l -> Doc
prettyModuleWithComments (Module l mbHead os imp decls) =
  let topComments  = getComments l
      renderedMod  =
        myVcat $ map pretty os ++
            (case mbHead of
                Nothing -> id
                Just h  -> \x -> [topLevel (pretty h) x])
            (map pretty imp ++
                 ppDeclsWithComments (isJust mbHead ||
                                      not (null imp) ||
                                      not (null os))
                                     decls)
  in case topComments of
       [] -> renderedMod
       cs -> ppComments cs $$$ renderedMod

prettyModuleWithComments (XmlPage l mn os n attrs mattr cs) =
  let topComments = getComments l
      renderedMod = pretty (XmlPage () (fmap (const ()) mn) (map (fmap (const ())) os)
                                    (fmap (const ()) n) (map (fmap (const ())) attrs)
                                    (fmap (fmap (const ())) mattr) (map (fmap (const ())) cs))
  in case topComments of
       [] -> renderedMod
       cm -> ppComments cm $$$ renderedMod

prettyModuleWithComments (XmlHybrid l mbHead os imp decls n attrs mattr cs) =
  let topComments = getComments l
      renderedMod = pretty (XmlHybrid () (fmap (fmap (const ())) mbHead) (map (fmap (const ())) os)
                                      (map (fmap (const ())) imp) (map (fmap (const ())) decls)
                                      (fmap (const ()) n) (map (fmap (const ())) attrs)
                                      (fmap (fmap (const ())) mattr) (map (fmap (const ())) cs))
  in case topComments of
       [] -> renderedMod
       cm -> ppComments cm $$$ renderedMod

-- | Pretty-print a 'Module' with comments using 'defaultMode'.
prettyPrintModuleWithComments :: (HasComments l) => Module l -> String
prettyPrintModuleWithComments = prettyPrintModuleWithCommentsAndMode defaultMode

-- | Pretty-print a 'Module' with comments using a custom 'PPHsMode'.
prettyPrintModuleWithCommentsAndMode :: (HasComments l) => PPHsMode -> Module l -> String
prettyPrintModuleWithCommentsAndMode ppMode m =
  prettyPrintModuleWithCommentsStyleMode style ppMode m

-- | Pretty-print a 'Module' with comments using a custom 'P.Style' and 'PPHsMode'.
prettyPrintModuleWithCommentsStyleMode :: (HasComments l) => Style -> PPHsMode -> Module l -> String
prettyPrintModuleWithCommentsStyleMode st ppMode m =
  renderStyleMode st ppMode (prettyModuleWithComments m)
