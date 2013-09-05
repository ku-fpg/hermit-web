module HERMIT.Web.Renderer (webChannel) where

import Control.Concurrent.Chan

import Data.Monoid

import HERMIT.PrettyPrinter.Common

import HERMIT.Web.JSON

import System.IO

webChannel :: Chan (Either String [Glyph]) -> Handle -> PrettyOptions -> Either String DocH -> IO ()
webChannel chan _ _    (Left s) = writeChan chan $ Left s
webChannel chan _ opts (Right doc) = let Glyphs gs = renderCode opts doc in writeChan chan $ Right gs

newtype Glyphs = Glyphs [ Glyph ]

instance RenderSpecial Glyphs where
    renderSpecial sym = Glyphs [ Glyph [ch] (Just SYNTAX) ]
        where Unicode ch = renderSpecial sym

instance Monoid Glyphs where
        mempty = Glyphs mempty
        mappend (Glyphs gs1) (Glyphs gs2) = Glyphs $ mergeGlyphs $ gs1 ++ gs2

mergeGlyphs :: [Glyph] -> [Glyph]
mergeGlyphs [] = []
mergeGlyphs [g] = [g]
mergeGlyphs (g:h:r) = case go g h of
                        Left g' -> mergeGlyphs $ g':r
                        Right (g',h') -> g' : mergeGlyphs (h':r)
    where go (Glyph "" (Just sty)) (Glyph t Nothing) = Left $ Glyph t (Just sty)
          go (Glyph "" (Just _)) (Glyph t (Just sty)) = Left $ Glyph t (Just sty)
          go (Glyph t1 (Just s1)) (Glyph t2 (Just s2)) | s1 == s2 && not (null t2)= Left $ Glyph (unwords [t1,t2]) (Just s1)
          go g1 g2 = Right (g1,g2)

instance RenderCode Glyphs where
        rPutStr txt = Glyphs [ Glyph txt Nothing ]
        rDoHighlight _ [] = mempty
        rDoHighlight _ (Color col:_) =
            Glyphs $ case col of
                        KeywordColor  -> [ Glyph "" (Just KEYWORD) ]
                        SyntaxColor   -> [ Glyph "" (Just SYNTAX) ]
                        IdColor       -> [ Glyph "" (Just VAR) ]
                        CoercionColor -> [ Glyph "" (Just COERCION) ]
                        TypeColor     -> [ Glyph "" (Just TYPE) ]
                        LitColor      -> [ Glyph "" (Just LIT) ]
                        WarningColor  -> [ Glyph "" (Just WARNING) ]
        rDoHighlight o (_:rest) = rDoHighlight o rest

