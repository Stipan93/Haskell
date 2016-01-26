module AssignmentAndSubmission (
  listSubmissions
) where

import Data.Time
import Data.Time.Clock.POSIX
import User
import System.Directory
import Data.List
import System.FilePath
import Control.Exception
import System.IO.Error

-- | Academic year shorthand (e.g. 2015 for 2015/16)
type Year = Integer

-- | An assignment type
data Type = Homework | Exam | Project deriving Show

-- | A an assignment configuration data structure
-- | Uses Data.Time.UTCTime
-- | If files is an empty list, ANY number of files are OK
data Configuration = Configuration {
    published :: UTCTime -- When to publish
  , deadline :: UTCTime -- Submission deadline
  , lateDeadline :: UTCTime -- Late submission deadline
  , files :: [String] -- File names to expect
  , minScore :: Double -- Minimum achievable
  , maxScore :: Double -- Maximum achievable
  , required :: Double -- Score req to pass
} deriving (Show, Read)

-- | An assignment descriptor
data Assignment = Assignment {
    year :: Year,
    aType :: Type,
    number :: Int
} deriving Show


data Submission = Submission {
    assignment :: Assignment
  , userId :: UserIdentifier
  , submittedFiles :: [String]
  , reviews :: [String]
} deriving Show



assgn = Assignment 2015 Homework 5
newAssgn = Assignment 2016 Exam 4
conf = Configuration {
  published = posixSecondsToUTCTime $ fromIntegral 10 ,
  deadline = posixSecondsToUTCTime $ fromIntegral 10,
  lateDeadline = posixSecondsToUTCTime $ fromIntegral 10,
  files = ["Mate", "Ante"],
  minScore = 0 ,
  maxScore = 5,
  required = 3
}
subm = Submission {
  assignment = assgn,
  userId = "Branko",
  submittedFiles = ["a","b","c"],
  reviews = []
}

-- | Lists the user identifiers for submissions made for an assignment
listSubmissions :: Assignment -> IO [UserIdentifier]
listSubmissions asgn = do
  r <- try $ do
    files <- listDirectory $ getAssignmentStr asgn
    mapM return $ filter (\x -> x /= "Assignment.pdf") files
  case r of
    Left e  -> error $ "Error: " ++ ioeGetErrorString e
    Right r -> return r

getAssignmentStr :: Assignment -> String
getAssignmentStr asgn = intercalate "/" $ [show $ year asgn, show $ aType asgn, show $ number asgn]

-- | Views a single submission in detail
getSubmission :: Assignment -> UserIdentifier -> IO Submission
getSubmission asgn user = do
  r <- try $ do
    listOfFiles <- listDirectory $ getAssignmentStr asgn ++ "/" ++ user
    let files = filter (\x -> isPrefixOf "review" x == False ) listOfFiles
        reviews = filter (\x -> isPrefixOf "review" x) listOfFiles
    return [files,reviews]
  case r of
      Left e  -> error $ "Error: " ++ ioeGetErrorString e
      Right r -> return $ Submission asgn user (r !! 0) (r !! 1)


-- | Creates a new assignment from Assignment, configuration and PDF file
-- | The PDF file should be copied, moved or symlinked so that it is
-- | accessible from the assignment directory.
createAssignment :: Assignment -> Configuration -> FilePath -> IO ()
createAssignment assgn config pdf = do
  let assgnDir = getAssignmentStr assgn
  r <- try $ do
    createDirectoryIfMissing True assgnDir
    mapM (createDirectory . (assgnDir </>)) (files config)
    writeFile (assgnDir </> ".config") (show config)
    copyFile pdf $ assgnDir </> (takeFileName pdf)
  case r of
      Left e  -> error $ "Error: " ++ ioeGetErrorString e
      Right r -> return ()


-- | Gets the configuration object for an assignment
getConfiguration :: Assignment -> IO Configuration
getConfiguration assgn = do
  let assgnDir = getAssignmentStr assgn
  r <- try $ do
    file <- readFile (assgnDir </> ".config")
    return (read file :: Configuration)
  case r of
    Left e  -> error $ "Error: " ++ ioeGetErrorString e
    Right r -> return r

-- | Given a solution file body, adds a solution directory/file to the
-- | directory structure of an assignment. It will indicate an error
-- | (using Maybe, exceptions, or some other mechanism) if the file is
-- | not in a defined permitted list. It will override already made
-- | submissions.
-- | Assignment -> File Body -> File Name -> Error indication (?)
--upload :: Assignment -> Text -> String -> IO (Maybe Submission)


-- | Lists the files contained in a submission
listFiles :: Submission -> IO [FilePath]
listFiles sub = return $ submittedFiles sub

-- | Computes a file path for a submission
getSubmissionPath :: Submission -> FilePath
getSubmissionPath sub = (getAssignmentStr $ assignment sub) </> (userId sub)


