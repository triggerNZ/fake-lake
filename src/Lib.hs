module Lib where  
import Data.Text
import Data.Either.Combinators
import Data.Set
import Data.Time.Clock
import Text.Regex.PCRE

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- The filesystem identifier. The value must start and end with a letter or number and must contain only letters, numbers, and the dash (-) character. Consecutive dashes are not permitted. All letters must be lowercase. The value must have between 3 and 63 characters.
-- Regex pattern: ^[$a-z0-9](?!.*--)[-a-z0-9]{1,61}[a-z0-9]$
newtype FilesystemId = FilesystemId String deriving (Eq, Ord)

newtype ETag = ETag String deriving (Eq, Ord)
data ETagOrWildcard = ExactEtag ETag | Wildcard

type ServiceState = Set Filesystem

data Filesystem = Filesystem {
    filesystem :: FilesystemId,
    created :: UTCTime,
    etag :: ETag
} deriving (Eq, Ord)

data FilesystemCreateError = CreateAlreadyExists | CreateInvalidId
data FilesystemDeleteError = DeleteNonExisting | DeleteInvalidId

stringToFilesystemId :: String -> Maybe FilesystemId
stringToFilesystemId txt
    | txt =~ "^[$a-z0-9](?!.*--)[-a-z0-9]{1,61}[a-z0-9]$" = Just (FilesystemId txt)
    | otherwise = Nothing

create :: ServiceState -> UTCTime -> ETag -> String -> Either FilesystemCreateError ServiceState
create state now etag txt = do
    fsid <- maybeToRight CreateInvalidId $ stringToFilesystemId txt    
    let fs = Filesystem fsid now etag
    if member fs state 
    then Left CreateAlreadyExists 
    else Right $ insert fs state

delete :: ServiceState -> String -> Either FilesystemDeleteError ServiceState
delete = undefined

data BlobConditions = BlobConditions {
    ifModifiedSince :: Maybe UTCTime,
    ifUnmodifiedSince :: Maybe UTCTime,
    ifMatch :: Maybe ETag,
    ifNoneMatch :: Maybe String
}

