{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-
Module: Projectile

This module provides utility functions to gather various paths of a project
-}
module Projectile (getProjectRootDir) where

import Protolude hiding (catch, (<>))

import Control.Exception.Safe (MonadCatch, MonadThrow, catch, throwM)
import Data.Monoid            ((<>))
import Data.Vector            (Vector)
import Path                   (Abs, Dir, Path, Rel, parent, parseRelFile, (</>))
import Path.IO                (isLocationOccupied)

import qualified Data.Vector as V

--------------------------------------------------------------------------------

data ProjectileException
  -- | Error thrown when calling project function from
  -- a path that does not not belong to any project
  = ProjectRootNotFound
  deriving (Generic, NFData, Show, Eq)

instance Exception ProjectileException

data WalkAction
  = WalkFinish
  | WalkContinue
  | WalkInvalid
  deriving (Generic, NFData, Show, Eq)

--------------------------------------------------------------------------------

isRoot :: Path Abs Dir -> Bool
isRoot path =
  parent path == path

-- | A list of files considered to mark the root of a project. The top-most
-- match has precedence.
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

-- | A list of files considered to mark the root of a project. The top-most
-- match has precedence.
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

-- | A list of files considered to mark the root of a project. This
-- file must be in all sub-directories of a project.
projectRecurringMarkFiles :: Vector FilePath
projectRecurringMarkFiles =
  V.fromList
    [
      ".svn" -- Svn VCS root dir
    , "CVS"  -- Csv VCS root dir
    , "Makefile"
    ]

-- | Iterate from the current path to parent paths until the match function
-- returns a value for finishing the traversal
locateDominatingFile
  :: (MonadIO m, MonadThrow m)
  => Path Abs Dir                   -- ^ Directory to start from
  -> (Path Abs Dir -> m WalkAction) -- ^ Match function that will return what to do next on the iteration
  -> m (Path Abs Dir)               -- ^ The path where the match function returns @WalkFinish@
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

-- | Returns a @WalkAction@ that indicates a finish of iteration when any of the
-- given relative paths is contained on the given directory
doesContainAny
  :: MonadIO m
  => Vector (Path Rel t) -- ^ Relative path that should be contained in directory input
  -> Path b Dir          -- ^ Directory path that should contain any of the relative paths
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
  files <-
    mapM parseRelFile (projectRootTopMarkFiles
                       <> projectRootTopLangMarkFiles)
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

-- | Retrieves the root of the current project if available.
-- A @ProjectRootNotFound@ error is returned otherwise.
--
getProjectRootDir
  :: (MonadCatch m, MonadIO m)
  => Path Abs Dir      -- ^ Directory from where to look the root of the project
  -> m (Path Abs Dir)  -- ^ Root of the project directory
getProjectRootDir dir =
  catch (getDirWithRecurringProjectFile dir)
        (\(_ :: ProjectileException) -> getDirWithRootProjectFile dir)
