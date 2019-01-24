{-# LANGUAGE BlockArguments #-}
module Data.GitHub.Spec
  ( spec
  ) where

import SpecHelpers

import Proto3.Suite
import Proto3.Suite.Exts

import Data.GitHub.Envelope
import Proto3.GitHub.Event.RepositoryPush
import Data.GitHub.User

sampleEnvelope :: ByteString
sampleEnvelope = "\n?hydro-schemas.github.net/hydro.schemas.github.v1.RepositoryPush\DC2\173\ETX\DC2N\b\SOH\DC2\bmonalisa\CAN\SOH\"\ACKmedium2\fMDQ6VXNlcjE=:\ACK\b\143\203\207\224\ENQR 3b7489995ba5925f6d6c4a86f8adc22f\SUBN\b\SOH\DC2\bmonalisa\CAN\SOH\"\ACKmedium2\fMDQ6VXNlcjE=:\ACK\b\143\203\207\224\ENQR 3b7489995ba5925f6d6c4a86f8adc22f\">\b\SOH\DC2\DC4MDEwOlJlcG9zaXRvcnkx\SUB\EOTtest(\SOHJ\f\b\156\242\136\226\ENQ\DLE\152\180\147\234\STXR\ACK\b\206\157\190\225\ENQZ\ACK\b\162\254\205\225\ENQ*(3fe2053d3e6b3a028c043288cdc5e3d9c794ab892(226958130ebe39c068833070802897497b7778d2:\SYNrefs/heads/test-branchB_\n(0000000000000000000000000000000000000000\DC2(e69de29bb2d1d6434b8b29ae775ad8c2e48c5391\SUB\SOHA\"\ACKfoo.py\SUB$a39f0025-8ff2-4d46-ac21-d0a28e7734e4\"\f\b\159\242\136\226\ENQ\DLE\176\197\170\210\SOH(\SOH"

spec :: Spec
spec =
  describe "RepositoryPush protobuf serialization" $
    it "should round-trip correctly" $ do
      let (Right decoded) = fromByteString @Envelope sampleEnvelope
      envelopeId decoded `shouldBe` "a39f0025-8ff2-4d46-ac21-d0a28e7734e4"
      let (Right message) = fromByteString @RepositoryPush (envelopeMessage decoded)
      let (Present mona) = pushActor message
      userLogin mona `shouldBe` "monalisa"
      toByteString decoded `shouldBe` sampleEnvelope
