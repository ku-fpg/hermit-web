{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module HERMIT.Web.JSON where

import HERMIT.Core

import Control.Applicative
import Control.Monad

import Data.Aeson hiding (json)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Web.Scotty (readEither)

-- | ErrorMsg = { 'msg' : String }
data ErrorMsg = ErrorMsg { msg :: String }

instance ToJSON ErrorMsg where
    toJSON (ErrorMsg m) = object [ "msg" .= m ]

instance FromJSON ErrorMsg where
    parseJSON (Object v) = ErrorMsg <$> v .: "msg"
    parseJSON _          = mzero


-- | Token = { 'unique' : Number
--           , 'token' : Number
--           }
data Token = Token { tUnique :: Integer, tToken :: Integer }

instance ToJSON Token where
    toJSON (Token u t) = object [ "unique" .= u , "token" .= t ]

instance FromJSON Token where
    parseJSON (Object v) = Token <$> v .: "unique" <*> v .: "token"
    parseJSON _          = mzero

-- | Command = { 'token' : Token -- current token
--             , 'cmd' : String  -- command string e.g. "any-call (unfold 'foo)"
--             , 'path' : [ Crumb ] -- current path in AST
--             }
data Command = Command { cToken :: Token
                       , cCmd :: String
                       }

instance ToJSON Command where
    toJSON cmd = object [ "token" .= cToken cmd , "cmd" .= cCmd cmd ]

instance FromJSON Command where
    parseJSON (Object v) = Command <$> v .: "token" <*> v .: "cmd"
    parseJSON _          = mzero

instance ToJSON Crumb where
    -- cases where there are fields
    toJSON (Rec_Def i)         = object [ "crumb" .= ("Rec_Def" :: String)         , "n" .= i ]
    toJSON (Case_Alt i)        = object [ "crumb" .= ("Case_Alt" :: String)        , "n" .= i ]
    toJSON (Alt_Var i)         = object [ "crumb" .= ("Alt_Var" :: String)         , "n" .= i ]
    toJSON (TyConApp_Arg i)    = object [ "crumb" .= ("TyConApp_Arg" :: String)    , "n" .= i ]
    toJSON (TyConAppCo_Arg i)  = object [ "crumb" .= ("TyConAppCo_Arg" :: String)  , "n" .= i ]
    toJSON (AxiomInstCo_Arg i) = object [ "crumb" .= ("AxiomInstCo_Arg" :: String) , "n" .= i ]
    -- catch all for nullary constructors
    toJSON cr = object [ "crumb" .= show cr ]

instance FromJSON Crumb where
    parseJSON (Object v) = do
        cstr :: String <- v .: "crumb"
        case cstr of
            "Rec_Def"         -> Rec_Def         <$> v .: "n"
            "Case_Alt"        -> Case_Alt        <$> v .: "n"
            "Alt_Var"         -> Alt_Var         <$> v .: "n"
            "TyConApp_Arg"    -> TyConApp_Arg    <$> v .: "n"
            "TyConAppCo_Arg"  -> TyConAppCo_Arg  <$> v .: "n"
            "AxiomInstCo_Arg" -> AxiomInstCo_Arg <$> v .: "n"
            _ -> return $ read cstr
    parseJSON _          = mzero


-- | CommandResponse = { 'token' : Number -- next token
--                     , 'ast' : String -- HTML fragment of new AST to be inserted at path
--                     , 'path' : [ Crumb ] -- echoed from request in case GUI doesn't want to store it
--                     }
data CommandResponse = CommandResponse { crToken :: Token
                                       , crGlyphs :: [Glyph]
                                       }

instance ToJSON CommandResponse where
    toJSON cr = object [ "token" .= crToken cr , "glyphs" .= crGlyphs cr ]

instance FromJSON CommandResponse where
    parseJSON (Object v) = CommandResponse <$> v .: "token" <*> v .: "glyphs"
    parseJSON _          = mzero

{-

CommandList = { 'cmds' : [ CommandInfo ] }

CommandInfo = { 'name' : String
              , 'help' : String
              , 'tags' : [ String ]
              }

CompletionQuery = { 'left' : String }

CompletionList = { 'right' : [ String ] }

Glyph = { 'text' : String  -- text to be shown
        , 'style?' : String -- optional style tag; valid values are KEYWORD, SYNTAX, VAR, TYPE, LIT.
        }
-}

data Style = KEYWORD | SYNTAX | VAR | TYPE | LIT
    deriving (Eq, Read, Show)

instance ToJSON Style where
    toJSON = String . T.pack . show

instance FromJSON Style where
    parseJSON (String s) = case readEither $ TL.fromStrict s of
                            Left _msg -> mzero
                            Right sty -> pure sty
    parseJSON _ = mzero

data Glyph = Glyph { gText :: String
                   , gStyle :: Maybe Style
                   }

instance ToJSON Glyph where
    toJSON g = object (("text" .= gText g) : (maybe [] (\s -> ["style" .= s]) (gStyle g)))

instance FromJSON Glyph where
    parseJSON (Object v) = Glyph <$> v .: "text" <*> v .:? "style"
    parseJSON _          = mzero

