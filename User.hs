{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module User (
  User,
  UserIdentifier,
  createUser,
  updateUser,
  getUser,
  listUsers,
  listUsersInRole,
  deleteUser
) where


import Database.Persist
import Data.Text (Text)
import Database.Persist
import Database.Persist.Types
import Database.Persist.Sqlite
import Database.Persist.TH
import Role
import Crypto.PasswordStore
import Control.Exception
import System.IO.Error
import Data.Maybe
import Control.Monad.IO.Class (liftIO)

-- | A user identifier (not DB id) like a username or JMBAG
type UserIdentifier = String


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
 identifier String
 email      String
 pwdHash    String
 role       Role
 UserIden   identifier
 deriving   Show
|]


main :: IO ()
main = runSqlite "TestBaza" $ do
  runMigration migrateAll

-- | Takes a user identifier, e-mail, password and role.
-- | Performs password hashing and stores the user into the
-- | database, returning a filled User. If creating it fails (e.g.
-- | the user identifier is already taken), throws an appropriate
-- | exception.
createUser :: UserIdentifier -> String -> String -> Role -> IO User
createUser userId email password role = runSqlite "TestBaza" $ do
  let user = User userId email password role
  userId <- insert user
  return user

-- | Updates a given user. Identifies it by the UserIdentifier (or
-- | maybe database id field, if you added it) in the User and overwrites
-- | the DB entry with the values in the User structure. Throws an
-- | appropriate error if it cannot do so; e.g. the user does not exist.
updateUser :: User -> IO ()
updateUser user@(User idu email password role) = runSqlite "TestBaza" $ do
  userid <- getBy $ UserIden idu
  case userid of
    Nothing -> error "User does not exist"
    Just (Entity userID userB) -> replace userID $ user

-- | Deletes a user referenced by identifier. If no such user or the
-- | operation fails, an appropriate exception is thrown.
deleteUser :: UserIdentifier -> IO ()
deleteUser idu = runSqlite "TestBaza" $ do
  user <- getBy $ UserIden idu
  case user of
    Nothing -> error "User does not exists"
    Just (Entity userid userB) -> delete userid
  return ()


-- | Lists all the users
listUsers :: IO [User]
listUsers = runSqlite "TestBaza" $ do
  user <- selectList [] []
  let users = map entityVal user
  return users


-- | Lists all users in a given role
listUsersInRole :: Role -> IO [User]
listUsersInRole role = runSqlite "TestBaza" $ do
  user <- selectList [UserRole ==. role] []
  let users = map entityVal user
  return users

-- | Fetches a single user by identifier$
getUser :: UserIdentifier -> IO User
getUser identifier = runSqlite "TestBaza" $ do
  userid <- getBy $ UserIden identifier
  case userid of
    Nothing -> error "User does not exists"
    Just (Entity userid userB) -> return userB


-- | Checks whether the user has a role of AT LEAST X in a given academic
-- | year.
isRoleInYear :: User -> Role -> Integer -> Bool
isRoleInYear user role num = checkRole role num

checkRole :: Role -> Integer -> Bool
checkRole (Student x) num   = num <= x
checkRole (TA x y) num      = num <= x
checkRole Professor num = True
