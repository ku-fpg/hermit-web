{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module HERMIT.Web.JSON where

import           Control.Applicative
import           Control.Monad

import           Data.Aeson hiding (json)
import           Data.Aeson.Types
import           Data.Attoparsec.Number (Number(..))
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import           HERMIT.Core (Crumb(..))
import           HERMIT.Kure (Path)
import           HERMIT.External
import           HERMIT.Kernel.Scoped (SAST(..))

import           System.Console.Haskeline.Completion (Completion(..))

import           Web.Scotty (readEither)

type UserID = Integer

-- | Msg
data Msg = Msg { mMsg :: String }

instance ToJSON Msg where
    toJSON (Msg m) = object [ "msg" .= m ]

instance FromJSON Msg where
    parseJSON (Object v) = Msg <$> v .: "msg"
    parseJSON _          = mzero

-- | Token
data Token = Token { tUser :: Integer , tAst :: SAST }

instance ToJSON Token where
    toJSON (Token u a) = object [ "user" .= u , "ast" .= a ]

instance FromJSON Token where
    parseJSON (Object v) = Token <$> v .: "user" <*> v .: "ast"
    parseJSON _          = mzero

-- | Command
data Command = Command { cToken :: Token
                       , cCmd :: String
                       }

instance ToJSON Command where
    toJSON cmd = object [ "token" .= cToken cmd , "cmd" .= cCmd cmd ]

instance FromJSON Command where
    parseJSON (Object v) = Command <$> v .: "token" <*> v .: "cmd"
    parseJSON _          = mzero

-- | SAST
instance ToJSON SAST where
    toJSON (SAST i) = integerToJSON (fromIntegral i)

instance FromJSON SAST where
    parseJSON j = SAST . fromIntegral <$> fromJSONInteger j

integerToJSON :: Integer -> Value
integerToJSON = Number . I

fromJSONInteger :: Value -> Parser Integer
fromJSONInteger (Number (I i)) = return i
fromJSONInteger _ = mzero

-- | Crumb
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

-- | CommandResponse
data CommandResponse = CommandResponse { crMsg :: Msg
                                       , crGlyphs :: [Glyph]
                                       , crAst :: SAST
                                       }

instance ToJSON CommandResponse where
    toJSON cr = object [ "msg" .= crMsg cr , "glyphs" .= crGlyphs cr , "ast" .= crAst cr ]

instance FromJSON CommandResponse where
    parseJSON (Object v) = CommandResponse <$> v .: "msg" <*> v .: "glyphs" <*> v .: "ast"
    parseJSON _          = mzero

-- | CommandList
data CommandList = CommandList { clCmds :: [CommandInfo] }

instance ToJSON CommandList where
    toJSON cl = object [ "cmds" .= clCmds cl ]

instance FromJSON CommandList where
    parseJSON (Object v) = CommandList <$> v .: "cmds"
    parseJSON _          = mzero

-- | CommandInfo
data CommandInfo = CommandInfo { ciName :: String
                               , ciHelp :: String
                               , ciTags :: [CmdTag]
                               }

instance ToJSON CommandInfo where
    toJSON ci = object [ "name" .= ciName ci , "help" .= ciHelp ci , "tags" .= ciTags ci ]

instance FromJSON CommandInfo where
    parseJSON (Object v) = CommandInfo <$> v .: "name" <*> v .: "help" <*> v .: "tags"
    parseJSON _          = mzero

-- | CmdTag
instance ToJSON CmdTag where
    toJSON = stringToJSON

instance FromJSON CmdTag where
    parseJSON = fromJSONString

-- | Style
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

-- | Glyph
data Glyph = Glyph { gText :: String
                   , gPath :: Path Crumb
                   , gStyle :: Maybe Style
                   }

instance ToJSON Glyph where
    toJSON g = object (("text" .= gText g) : ("path" .= gPath g) : (maybe [] (\s -> ["style" .= s]) (gStyle g)))

instance FromJSON Glyph where
    parseJSON (Object v) = Glyph <$> v .: "text" <*> v .: "path" <*> v .:? "style"
    parseJSON _          = mzero

data History = History { hCmds :: [HCmd]
                       , hTags :: [HTag]
                       }

data HCmd = HCmd SAST String SAST
data HTag = HTag String SAST

instance ToJSON History where
    toJSON h = object [ "cmds" .= hCmds h , "tags" .= hTags h ]

instance FromJSON History where
    parseJSON (Object v) = History <$> v .: "cmds" <*> v .: "tags"
    parseJSON _          = mzero

instance ToJSON HCmd where
    toJSON (HCmd from e to) = object [ "from" .= from , "cmd" .= e , "to" .= to ]

instance FromJSON HCmd where
    parseJSON (Object v) = HCmd <$> v .: "from" <*> v .: "cmd" <*> v .: "to"
    parseJSON _          = mzero

instance ToJSON HTag where
    toJSON (HTag tag ast) = object [ "tag" .= tag , "ast" .= ast ]

instance FromJSON HTag where
    parseJSON (Object v) = HTag <$> v .: "tag" <*> v .: "ast"
    parseJSON _          = mzero

data Complete = Complete { cpUser :: UserID
                         , cpCmd :: String
                         }

instance ToJSON Complete where
    toJSON c = object [ "user" .= cpUser c , "cmd" .= cpCmd c ]

instance FromJSON Complete where
    parseJSON (Object v) = Complete <$> v .: "user" <*> v .: "cmd"
    parseJSON _          = mzero

data Completions = Completions { cCompletions :: [Completion] }

instance ToJSON Completions where
    toJSON (Completions cs) = object [ "completions" .= cs ]

instance FromJSON Completions where
    parseJSON (Object v) = Completions <$> v .: "cs" 
    parseJSON _          = mzero

instance ToJSON Completion where
    toJSON c = object [ "replacement" .= replacement c , "display" .= display c , "isFinished" .= isFinished c ]

instance FromJSON Completion where
    parseJSON (Object v) = Completion <$> v .: "replacement" <*> v .: "display" <*> v .: "isFinished"
    parseJSON _          = mzero
