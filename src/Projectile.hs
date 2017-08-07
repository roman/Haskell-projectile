{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Projectile where

import Protolude hiding (catch)

import Control.DeepSeq        (NFData)
import Control.Exception.Safe (MonadCatch, MonadThrow, catch, throwM)
import Data.Vector            (Vector)
import Path                   (Abs, Dir, Path, Rel, parent, parseRelFile, (</>))
import Path.IO                (isLocationOccupied)

import qualified Data.Vector as V

--------------------------------------------------------------------------------

data ProjectileException
  = ProjectRootNotFound
  deriving (Generic, NFData, Show, Eq)

instance Exception ProjectileException

data WalkAction
  = WalkFinish
  | WalkContinue
  | WalkInvalid
  deriving (Generic, NFData, Show, Eq)

isRoot :: Path Abs Dir -> Bool
isRoot path =
  parent path == path

-- | A list of files considered to mark the root of a project. The topmost match
-- has precedence.
projectRootTopLangMarkFiles :: Vector FilePath
projectRootTopLangMarkFiles =
  V.fromList
    [
      "rebar.config"       -- Rebar project file
    , "project.clj"        -- Leiningen project file
    , "build.boot"         -- Boot-clj project file
    , "SConstruct"         -- Scons project file
    , "pom.xml"            -- Maven project file
    , "build.sbt"          -- SBT project file
    , "gradlew"            -- Gradle wrapper script
    , "build.gradle"       -- Gradle project file
    , ".ensime"            -- Ensime configuration file
    , "Gemfile"            -- Bundler file
    , "requirements.txt"   -- Pip file
    , "setup.py"           -- Setuptools file
    , "tox.ini"            -- Tox file
    , "composer.json"      -- Composer project file
    , "Cargo.toml"         -- Cargo project file
    , "mix.exs"            -- Elixir mix project file
    , "stack.yaml"         -- Haskell's stack tool based project
    , "stack.yml"          -- Haskell's stack tool based project
    , "info.rkt"           -- Racket package description file
    , "DESCRIPTION"        -- R package description file
    , "TAGS"               -- etags/ctags are usually in the root of project
    , "GTAGS"              -- GNU Global tags
    ]

-- | A list of files considered to mark the root of a project. The topmost match
-- has precedence.
projectRootTopMarkFiles :: Vector FilePath
projectRootTopMarkFiles =
  V.fromList
    [
      ".projectile" -- projectile project marker
    , ".git"        -- Git VCS root dir
    , ".hg"         -- Mercurial VCS root dir
    , ".fslckout"   -- Fossil VCS root dir
    , "_FOSSIL_"    -- Fossil VCS root DB on Windows
    , ".bzr"        -- Bazaar VCS root dir
    , "_darcs"      -- Darcs VCS root dir
    ]

projectRecurringMarkFiles :: Vector FilePath
projectRecurringMarkFiles =
  V.fromList
    [
      ".svn" -- Svn VCS root dir
    , "CVS"  -- Csv VCS root dir
    , "Makefile"
    ]

locateDominatingFile
  :: (MonadIO m, MonadThrow m)
  => Path Abs Dir
  -> (Path Abs Dir -> m WalkAction)
  -> m (Path Abs Dir)
locateDominatingFile dir continueP
  | isRoot dir =
    throwM ProjectRootNotFound

  | otherwise = do
    walkNext <- continueP dir
    case walkNext of
      WalkInvalid ->
        throwM ProjectRootNotFound
      WalkFinish ->
        return dir
      WalkContinue ->
        locateDominatingFile (parent dir) continueP

doesContainAny
  :: MonadIO m
  => Vector (Path Rel t)
  -> Path b Dir
  -> m WalkAction
doesContainAny files dir = do
  matchesAnyFile <-
    (not . V.null . V.dropWhile not)
    <$> V.mapM (\file -> isLocationOccupied (dir </> file)) files

  if matchesAnyFile then
    return WalkFinish
  else
    return WalkContinue

getDirWithRootProjectFile
  :: (MonadIO m, MonadThrow m)
  => Path Abs Dir
  -> m (Path Abs Dir)
getDirWithRootProjectFile currentDir = do
  files <- mapM parseRelFile (projectRootTopMarkFiles <> projectRootTopLangMarkFiles)
  locateDominatingFile currentDir (doesContainAny files)

getDirWithRecurringProjectFile
  :: (MonadIO m, MonadThrow m)
  => Path Abs Dir
  -> m (Path Abs Dir)
getDirWithRecurringProjectFile currentDir =
  let
    parentDoesNotContainOneOf files dir = do
      fileLocated <- doesContainAny files dir
      if fileLocated == WalkFinish then do
        -- return check of parent directory not containing one of the
        -- recurring project files
        parentContains <- doesContainAny files (parent dir)
        if parentContains == WalkFinish then
          return WalkContinue
        else
          return WalkFinish
      -- if the path doesn't contain the recurring marking file, then
      -- this is not a project that can be recognizable
      else
        return WalkInvalid

  in do
    files <- mapM parseRelFile projectRecurringMarkFiles
    locateDominatingFile currentDir (parentDoesNotContainOneOf files)

getProjectRootDir
  :: (MonadCatch m, Alternative m, MonadIO m)
  => Path Abs Dir
  -> m (Path Abs Dir)
getProjectRootDir dir =
  catch (getDirWithRecurringProjectFile dir)
        (\(_ :: ProjectileException) -> getDirWithRootProjectFile dir)
