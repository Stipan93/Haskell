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
  UserIdentifier
) where


import Database.Persist
import Data.Text (Text)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Role


-- | A user identifier (not DB id) like a username or JMBAG
type UserIdentifier = String


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
 identifier String
 email      String
 pwdHash    String
 role       Role
 deriving   Show
|]


--main :: IO ()
--main = runSqlite ":memory:" $ do
--  runMigration migrateAll


-- | Takes a user identifier, e-mail, password and role.
-- | Performs password hashing and stores the user into the
-- | database, returning a filled User. If creating it fails (e.g.
-- | the user identifier is already taken), throws an appropriate
-- | exception.
-- createUser :: UserIdentifier -> String -> String -> Role -> IO User

-- | Updates a given user. Identifies it by the UserIdentifier (or
-- | maybe database id field, if you added it) in the User and overwrites
-- | the DB entry with the values in the User structure. Throws an
-- | appropriate error if it cannot do so; e.g. the user does not exist.
--updateUser :: User -> IO ()


-- | Deletes a user referenced by identifier. If no such user or the
-- | operation fails, an appropriate exception is thrown.
--deleteUser :: UserIdentifer -> IO ()


-- | Lists all the users
--listUsers :: IO [User]


-- | Lists all users in a given role
--listUsersInRole :: Role -> IO [User]


-- | Fetches a single user by identifier
--getUser :: UserIdentifier -> IO User


-- | Checks whether the user has a role of AT LEAST X in a given academic
-- | year.
--isRoleInYear :: User -> Role -> Integer -> Bool

