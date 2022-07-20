module FPIndia.Jobs (
  Job (..),
  jobsDynamic,
) where

import Control.Exception (throw)
import Control.Monad.Logger
import Data.Csv (FromNamedRecord (..), (.:))
import Data.Csv qualified as Csv
import Ema (Dynamic (Dynamic))
import GHC.IO.Exception (userError)
import System.FilePath (takeDirectory, takeFileName, (</>))
import System.UnionMount qualified as UM
import UnliftIO

data Job = Job
  { jobName :: !Text
  , jobWebsite :: !Text
  , jobSource :: !Text
  , jobLocation :: !Text
  , jobLanguages :: !Text
  , jobPermalink :: !Text
  , jobActiveStatus :: !Text
  }
  deriving stock (Eq, Show)

instance FromNamedRecord Job where
  parseNamedRecord r =
    Job
      <$> r .: "name"
      <*> r .: "website"
      <*> r .: "source"
      <*> r .: "location"
      <*> r .: "language"
      <*> r .: "permalink"
      <*> r .: "activestatus"

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
