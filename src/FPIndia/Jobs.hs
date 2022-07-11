module FPIndia.Jobs (
  Job (..),
  jobsDynamic,
) where

import Control.Exception (throw)
import Control.Monad.Logger
import Data.Csv (FromField, FromNamedRecord (..), (.:))
import Data.Csv qualified as Csv
import Ema (Dynamic (Dynamic))
import GHC.IO.Exception (userError)
import System.FilePath (takeDirectory, takeFileName, (</>))
import System.UnionMount qualified as UM
import UnliftIO

data Job = Job
  { jobName :: !String
  , jobSource :: !SourceType
  , jobWebsite :: !String
  , --  , jobWebsite :: !SourceUrl
    jobLocation :: !Text
  , jobLanguages :: !Language
  , jobPermalink :: !Text
  , jobActiveStatus :: !Status
  }

-- data Source = Source
--   { sourceType :: SourceType
--   , sourceUrl :: SourceUrl
--   }

data SourceType
  = Reddit
  | Github
  | Twitter
  | LinkedIn
  | OtherJobSite
  | OtherForum
  | OtherSourceType

--data SourceUrl = Verified String | Other String
data Language
  = Haskell
  | PureScript
  | Scala
  | Ocaml
  | Rust
  | Erlang
  | Agda
  | Elixir
  | Idris

data Status = Active | NotActive

instance FromNamedRecord Job where
  parseNamedRecord r =
    Job
      <$> r .: "name"
      <*> r .: "source"
      <*> r .: "website"
      <*> r .: "location"
      <*> r .: "languages"
      <*> r .: "permalink"
      <*> r .: "active_status"

instance FromField SourceType where
  parseField s
    | s == "Reddit" = pure Reddit
    | s == "Github" = pure Github
    | s == "Twitter" = pure Twitter
    | s == "LinkedIn" = pure LinkedIn
    | s == "OtherJobSite" = pure OtherJobSite
    | s == "OtherForum" = pure OtherForum
    | s == "OtherSourceType" = pure OtherSourceType
    | otherwise = empty

instance FromField Language where
  parseField s
    | s == "Haskell" = pure Haskell
    | s == "PureScript" = pure PureScript
    | s == "Scala" = pure Scala
    | s == "Ocaml" = pure Ocaml
    | s == "Rust" = pure Rust
    | s == "Erlang" = pure Erlang
    | s == "Agda" = pure Agda
    | s == "Elixir" = pure Elixir
    | s == "Idris" = pure Idris
    --    | s == "OtherLanguage" = pure String
    | otherwise = empty

instance FromField Status where
  parseField s
    | s == "Active" = pure Active
    | s == "NotActive" = pure NotActive
    | otherwise = empty

jobsDynamic ::
  forall m.
  (MonadIO m, MonadUnliftIO m, MonadLogger m, MonadLoggerIO m) =>
  FilePath ->
  m (Dynamic m [Job])
jobsDynamic path = do
  let (baseDir, jobsFile) = takeDirectory &&& takeFileName $ path
      pats = [((), jobsFile)]
  Dynamic <$> UM.mount baseDir pats mempty mempty (const $ handleUpdate baseDir)
  where
    handleUpdate :: FilePath -> FilePath -> UM.FileAction () -> m ([Job] -> [Job])
    handleUpdate baseDir fp = \case
      UM.Refresh _ _ -> do
        let jobsFile = baseDir </> fp
        logInfoNS "Jobs" $ "Loading jobs from " <> toText jobsFile
        s <- readFileLBS jobsFile
        case Csv.decodeByName @Job s of
          Left err -> throw $ userError $ "Failed to decode CSV file " <> jobsFile <> ": " <> err
          Right (_, toList -> rows) -> pure $ const rows
      UM.Delete -> do
        logInfoNS "Jobs" $ "Deloading all jobs; file got deleted: " <> toText fp
        pure $ const mempty
