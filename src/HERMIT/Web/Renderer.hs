module HERMIT.Web.Renderer (webChannel) where

import Control.Concurrent.STM

import Data.Monoid

import HERMIT.Core
import HERMIT.Kure
import HERMIT.PrettyPrinter.Common

import HERMIT.Web.JSON

import System.IO

webChannel :: TChan (Either String [Glyph]) -> Handle -> PrettyOptions -> Either String DocH -> IO ()
webChannel chan _ _    (Left s)    = atomically $ writeTChan chan $ Left s
webChannel chan _ opts (Right doc) = let Runes rs = renderCode opts doc
                                     in atomically $ writeTChan chan $ Right $ runesToGlyphs rs

runesToGlyphs :: [Rune] -> [Glyph]
runesToGlyphs = go [] Nothing Nothing
    where go :: Path Crumb -> Maybe Style -> Maybe (Path Crumb) -> [Rune] -> [Glyph]
          go _ _ _ [] = []
          go p s bp (Rune str:r) = Glyph str p s bp : go p s bp r
          go p _ bp (Markup s:r) = go p (Just s) bp r
          go _ s bp (PathA p :r) = go p s bp r
          go p s _  (BndrA bp:r) = go p s (Just bp) r
          go p s _  (EndBndrA:r) = go p s Nothing r

-- | Runes are precursors to Glyphs
data Rune = Rune String | Markup Style | PathA (Path Crumb) | BndrA (Path Crumb) | EndBndrA

newtype Runes = Runes [ Rune ]

instance RenderSpecial Runes where
    renderSpecial sym = Runes [ Markup SYNTAX , Rune [ch] ]
        where Unicode ch = renderSpecial sym

instance Monoid Runes where
    mempty = Runes mempty
    mappend (Runes rs1) (Runes rs2) = Runes $ mergeRunes $ rs1 ++ rs2

mergeRunes :: [Rune] -> [Rune]
mergeRunes [] = []
mergeRunes [r] = [r]
mergeRunes (g:h:r) = case merge g h of
                        Left g' -> mergeRunes (g':r)
                        Right (g',h') -> g' : mergeRunes (h':r)
    where merge (Rune s1)  (Rune s2)   = Left $ Rune (s1 ++ s2)
          merge (Markup _) (Markup s2) = Left $ Markup s2
          merge (PathA _)  (PathA p2)  = Left $ PathA p2
          merge r1         r2          = Right (r1,r2)

instance RenderCode Runes where
    rPutStr txt = Runes [ Rune txt ]
    rDoHighlight _ [] = mempty
    rDoHighlight Nothing (BndrAttr p:_) = Runes [ BndrA $ snocPathToPath p ]
    rDoHighlight (Just (BndrAttr _)) _ = Runes [ EndBndrA ]
    rDoHighlight _ (PathAttr p:_) = Runes [ PathA $ snocPathToPath p ]
    rDoHighlight _ (Color col:_) =
        Runes $ case col of
                    KeywordColor  -> [ Markup KEYWORD ]
                    SyntaxColor   -> [ Markup SYNTAX ]
                    IdColor       -> [ Markup VAR ]
                    CoercionColor -> [ Markup COERCION ]
                    TypeColor     -> [ Markup TYPE ]
                    LitColor      -> [ Markup LIT ]
                    WarningColor  -> [ Markup WARNING ]
    rDoHighlight o (_:rest) = rDoHighlight o rest

