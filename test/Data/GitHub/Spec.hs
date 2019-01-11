{-# LANGUAGE BlockArguments #-}
module Data.GitHub.Spec
  ( spec
  ) where

import SpecHelpers

import Proto3.Suite
import Proto3.Suite.Exts

import Data.GitHub.Envelope
import Data.GitHub.Event.RepositoryPush
import Data.GitHub.User

sampleEnvelope :: ByteString
sampleEnvelope = "\n?hydro-schemas.github.net/hydro.schemas.github.v1.RepositoryPush\DC2\227\STX\DC2N\b\SOH\DC2\bmonalisa\CAN\SOH\"\ACKmedium2\fMDQ6VXNlcjE=:\ACK\b\143\203\207\224\ENQR 3b7489995ba5925f6d6c4a86f8adc22f\SUB>\b\SOH\DC2\DC4MDEwOlJlcG9zaXRvcnkx\SUB\EOTtest(\SOHJ\f\b\204\137\222\225\ENQ\DLE\200\171\165\255\STXR\ACK\b\206\157\190\225\ENQZ\ACK\b\162\254\205\225\ENQ\"(308cf87a25e096a71be8aba8bb53f5575a60c388*(d7d56aeb77d6b8434d2789b4b0b6b305dfce10a82\SYNrefs/heads/test-branch:e\n(0000000000000000000000000000000000000000\DC2(e69de29bb2d1d6434b8b29ae775ad8c2e48c5391\SUB\SOHA\"\fkjfaslkjfals\SUB$f1ef901a-ca31-4d6b-aff1-a5ea1e2a940e\"\v\b\207\137\222\225\ENQ\DLE\212\229\174D(\SOH"

spec :: Spec
spec =
  describe "RepositoryPush protobuf serialization" $
    it "should round-trip correctly" $ do
      (Right decoded) <- fromByteString @Envelope sampleEnvelope
      envelopeId decoded `shouldBe` "f1ef901a-ca31-4d6b-aff1-a5ea1e2a940e"
      (Right message) <- fromByteString @RepositoryPush (envelopeMessage decoded)
      userLogin (pushActor message) `shouldBe` "monalisa"
      toByteString decoded `shouldBe` sampleEnvelope
      
  
