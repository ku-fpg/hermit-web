{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module HERMIT.Web.JSON where

import GhcPlugins hiding ((<>), liftIO)

import HERMIT.Kernel.Scoped
import HERMIT.Optimize
import HERMIT.Core

import Control.Applicative
import Control.Monad.IO.Class (liftIO)
import Control.Monad

import Data.Aeson hiding (json)
import Data.Char (isDigit)
import Data.Monoid
import qualified Data.Text.Lazy as T

import Web.Scotty

{-
instance Parsable SAST where
    parseParam t = if all isDigit str
                   then Right $ SAST (read str)
                   else Left "cannot parse SAST"
        where str = dropWhile (not . isDigit) $ T.unpack t

instance ToJSON EvalRequest where
  toJSON (EvalRequestId id' args)
        = object [ "id" .= id'
                 , "arguments" .= toJSON (map Word64String args)
                 ]
  toJSON (EvalRequestProgram code args)
        = object [ "program" .= code
                 , "arguments" .= toJSON (map Word64String args)
                 ]
data EvalResponse = EvalResponseOK
     { er_outputs :: [Word64]
     }
                   | EvalResponseError
     { er_message :: String
     }
     deriving Show
instance FromJSON EvalResponse where
   parseJSON (Object v) = do
           s :: String <- v .: "status"
           case s of
             "ok"      -> do
                     ws :: [Word64String] <- v .: "outputs"
                     return $ EvalResponseOK $ map (\ (Word64String w) -> w) ws
             "error"   -> EvalResponseError <$> v .: "message"
             _ -> mzero
     where
   parseJSON _          = mzero
-}

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
                       , cPath :: [Crumb]
                       }

instance ToJSON Command where
    toJSON cmd = object [ "token" .= cToken cmd , "cmd" .= cCmd cmd , "path" .= cPath cmd ]

instance FromJSON Command where
    parseJSON (Object v) = Command <$> v .: "token" <*> v .: "cmd" <*> v .: "path"
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
                                       , crAST :: String
                                       , crPath :: [ Crumb ]
                                       }

instance ToJSON CommandResponse where
    toJSON cr = object [ "token" .= crToken cr , "ast" .= crAST cr , "path" .= crPath cr ]

instance FromJSON CommandResponse where
    parseJSON (Object v) = CommandResponse <$> v .: "token" <*> v .: "ast" <*> v .: "path"
    parseJSON _          = mzero

{-

CommandList = { 'cmds' : [ CommandInfo ] }

CommandInfo = { 'name' : String
              , 'help' : String
              , 'tags' : [ String ]
              }

CompletionQuery = { 'left' : String }

CompletionList = { 'right' : [ String ] }
-}
