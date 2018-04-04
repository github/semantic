{-# LANGUAGE GADTs #-}
module Semantic
( parseBlobs
, parseBlob
, diffBlobPairs
, diffBlobPair
, diffTermPair
) where

import Semantic.Diff
import Semantic.Parse

-- This is the primary interface to the Semantic library which provides two
-- major classes of functionality: semantic parsing and diffing of source code
-- blobs.
--
-- Design goals:
--   - No knowledge of the filesystem or Git.
--   - Built in concurrency where appropriate.
--   - Easy to consume this interface from other application (e.g a cmdline or web server app).
