module Pulp.Publish ( action ) where

import Prelude
import Control.Monad.Eff.Class
import Control.Monad.Eff.Exception
import Control.Monad.Eff.Console.Unsafe (logAny)
import Control.Monad.Error.Class
import Control.Monad.Aff
import Data.Maybe
import Data.Maybe.Unsafe
import Data.Tuple
import Data.Either
import Data.Array as Array
import Data.List (List(..))
import Data.Foldable as Foldable
import Data.Foreign (parseJSON)
import Data.Foreign.Class (readProp)
import Data.Version (Version)
import Data.Version as Version
import Data.String as String
import Node.Buffer (Buffer)
import Node.Encoding (Encoding(..))
import Node.ChildProcess as CP
import Node.FS.Aff as FS
import Network.HTTP.Affjax
import Network.HTTP.Method
import Network.HTTP.RequestHeader
import Network.HTTP.MimeType
import Network.HTTP.StatusCode

import Pulp.System.FFI
import Pulp.System.Stream
import Pulp.Exec
import Pulp.Args
import Pulp.Outputter
import Pulp.System.Files
import Pulp.VersionBump
import Pulp.System.Read as Read
import Pulp.Git
import Pulp.Login (tokenFilePath)

action :: Action
action = Action \args -> do
  out <- getOutputter args

  requireCleanGitWorkingTree
  authToken <- readTokenFile
  gzippedJson <- pscPublish >>= gzip

  -- Because we've already run psc-publish, it's safe to assume this is Just
  Tuple tagStr tagVersion <- fromJust <$> getVersionFromGitTag

  name <- getNameFromBowerJson
  confirm ("Publishing " <> name <> " at v" <> Version.showVersion tagVersion <> ". Is this ok?")

  confirmRun out "git" ["push", "origin", "HEAD", "refs/tags/" <> tagStr]

  out.log "Uploading documentation to Pursuit..."
  uploadPursuitDocs authToken gzippedJson

  -- TODO: bower register
  out.log "Done."
  out.log ("You can view your package's documentation at: " <>
           pursuitUrl name tagVersion)

gzip :: String -> AffN String
gzip str = do
  gzipStream <- liftEff createGzip
  write gzipStream str
  end gzipStream
  concatStream gzipStream

pscPublish :: AffN String
pscPublish = execQuiet "psc-publish" [] Nothing

confirmRun :: Outputter -> String -> Array String -> AffN Unit
confirmRun out cmd args = do
  out.log "About to execute:"
  out.write ("> " <> cmd <> " " <> String.joinWith " " args <> "\n")
  confirm "Ok?"
  exec cmd args Nothing

confirm :: String -> AffN Unit
confirm q = do
  answer <- Read.read { prompt: q <> " [y/n] ", silent: false }
  case String.trim (String.toLower answer) of
    "y" ->
      pure unit
    _ ->
      throwError (error "Aborted")

getNameFromBowerJson :: AffN String
getNameFromBowerJson = do
  json <- FS.readTextFile UTF8 "bower.json"
  case parseJSON json >>= readProp "name" of
    Right name ->
      pure name
    Left err ->
      throwError (error (
        "Unable to get package name from bower.json:" <> show err))

readTokenFile :: AffN String
readTokenFile = do
  path <- tokenFilePath
  r <- attempt (FS.readTextFile UTF8 path)
  case r of
    Right token ->
      pure token
    Left err | isENOENT err ->
      throwError (error
        ("Pursuit authentication token not found. Try running `pulp login` " <>
         "first."))
    Left err ->
      throwError err

pursuitUrl :: String -> Version -> String
pursuitUrl name vers =
  "https://pursuit.purescript.org/packages/" <> name <> "/" <> Version.showVersion vers

uploadPursuitDocs :: String -> String -> AffN Unit
uploadPursuitDocs authToken gzippedJson = do
  r <- affjax $ defaultRequest
          { url = "https://pursuit.purescript.org/packages"
          , method = POST
          , headers = [ Accept (MimeType "application/json")
                      , RequestHeader "Authorization" ("token " <> authToken)
                      , RequestHeader "Content-Encoding" "gzip"
                      ]
          , content = Just gzippedJson
          }

  itsAString r.response

  case r.status of
    StatusCode x | x /= 201 -> do
      liftEff (logAny r)
      throwError (error ("Expected an HTTP 201 response from Pursuit, got: " <> show x))
    _ ->
      pure unit

  where
  -- type inference help
  itsAString :: String -> AffN Unit
  itsAString = const (pure unit)
