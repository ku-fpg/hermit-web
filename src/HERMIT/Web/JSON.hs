{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module HERMIT.Web.JSON where

import HERMIT.Core
import HERMIT.External
import HERMIT.Kernel.Scoped (SAST(..))

import Control.Applicative
import Control.Monad

import Data.Aeson hiding (json)
import Data.Aeson.Types
import Data.Attoparsec.Number (Number(..))
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Web.Scotty (readEither)

data Msg = Msg { mMsg :: String }

instance ToJSON Msg where
    toJSON (Msg m) = object [ "msg" .= m ]

instance FromJSON Msg where
    parseJSON (Object v) = Msg <$> v .: "msg"
    parseJSON _          = mzero

data Token = Token { tUser :: Integer , tAst :: SAST }

instance ToJSON Token where
    toJSON (Token u a) = object [ "user" .= u , "ast" .= a ]

instance FromJSON Token where
    parseJSON (Object v) = Token <$> v .: "user" <*> v .: "ast"
    parseJSON _          = mzero

data Command = Command { cToken :: Token
                       , cCmd :: String
                       }

instance ToJSON Command where
    toJSON cmd = object [ "token" .= cToken cmd , "cmd" .= cCmd cmd ]

instance FromJSON Command where
    parseJSON (Object v) = Command <$> v .: "token" <*> v .: "cmd"
    parseJSON _          = mzero

instance ToJSON SAST where
    toJSON (SAST i) = integerToJSON (fromIntegral i)

instance FromJSON SAST where
    parseJSON j = SAST . fromIntegral <$> fromJSONInteger j

integerToJSON :: Integer -> Value
integerToJSON = Number . I

fromJSONInteger :: Value -> Parser Integer
fromJSONInteger (Number (I i)) = return i
fromJSONInteger _ = mzero

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


data CommandResponse = CommandResponse { crGlyphs :: [Glyph]
                                       , crAst :: SAST
                                       }

instance ToJSON CommandResponse where
    toJSON cr = object [ "glyphs" .= crGlyphs cr , "ast" .= crAst cr ]

instance FromJSON CommandResponse where
    parseJSON (Object v) = CommandResponse <$> v .: "glyphs" <*> v .: "ast"
    parseJSON _          = mzero

data CommandList = CommandList { clCmds :: [CommandInfo] }

instance ToJSON CommandList where
    toJSON cl = object [ "cmds" .= clCmds cl ]

instance FromJSON CommandList where
    parseJSON (Object v) = CommandList <$> v .: "cmds"
    parseJSON _          = mzero

data CommandInfo = CommandInfo { ciName :: String
                               , ciHelp :: String
                               , ciTags :: [CmdTag]
                               }

instance ToJSON CommandInfo where
    toJSON ci = object [ "name" .= ciName ci , "help" .= ciHelp ci , "tags" .= ciTags ci ]

instance FromJSON CommandInfo where
    parseJSON (Object v) = CommandInfo <$> v .: "name" <*> v .: "help" <*> v .: "tags"
    parseJSON _          = mzero

instance ToJSON CmdTag where
    toJSON = stringToJSON

instance FromJSON CmdTag where
    parseJSON = fromJSONString

data Style = KEYWORD | SYNTAX | VAR | COERCION | TYPE | LIT | WARNING
    deriving (Eq, Read, Show)

instance ToJSON Style where
    toJSON = stringToJSON

instance FromJSON Style where
    parseJSON = fromJSONString

stringToJSON :: Show a => a -> Value
stringToJSON = String . T.pack . show
fromJSONString :: Read a => Value -> Parser a
fromJSONString (String s) =
    case readEither $ TL.fromStrict s of
        Left _msg -> mzero
        Right sty -> pure sty
fromJSONString _ = mzero

data Glyph = Glyph { gText :: String
                   , gStyle :: Maybe Style
                   }

instance ToJSON Glyph where
    toJSON g = object (("text" .= gText g) : (maybe [] (\s -> ["style" .= s]) (gStyle g)))

instance FromJSON Glyph where
    parseJSON (Object v) = Glyph <$> v .: "text" <*> v .:? "style"
    parseJSON _          = mzero

