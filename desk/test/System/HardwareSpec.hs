-- | Checks if the hardware parsers could correctly parse according to specifications.
module System.HardwareSpec (hardwareSpec) where

import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Pulp.Desk.System.Hardware.CPUStatus
import Pulp.Desk.Utils.ParseHor
import Test.Hspec
import Test.Hspec.Megaparsec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Text.Megaparsec qualified as Parse

newtype FieldName = Named T.Text
  deriving (Show, Eq, Ord)

fieldNameText :: FieldName -> T.Text
fieldNameText (Named key) = key

-- TODO Better generation of key
instance Arbitrary FieldName where
  arbitrary :: Gen FieldName
  arbitrary = Named . T.pack <$> listOf1 (oneof [chooseEnum ('a', 'z'), chooseEnum ('0', '9')])

instance (Arbitrary a, Num a, Ord a) => Arbitrary (CPUStat a) where
  arbitrary :: (Arbitrary a, Num a, Ord a) => Gen (CPUStat a)
  arbitrary = do
    userTime <- arbitrary
    niceTime <- arbitrary
    systemTime <- arbitrary
    idleTime <- arbitrary
    pure MkCPUStat{..}

  shrink :: (Arbitrary a, Num a, Ord a) => CPUStat a -> [CPUStat a]
  shrink stat = let stat0 = 0 <$ stat in [stat0 | stat /= stat0]

-- | Complete CPU Field with unknown values.
data CPUField = MkCPUField (CPUStat Int) [Int]
  deriving (Show)

instance Arbitrary CPUField where
  arbitrary :: Gen CPUField
  arbitrary = MkCPUField <$> arbitrary <*> arbitrary

  shrink :: CPUField -> [] CPUField
  shrink (MkCPUField stat others) = [MkCPUField stat' others' | (stat', others') <- shrink (stat, others)]

cpuFieldNums :: CPUField -> [Int]
cpuFieldNums (MkCPUField MkCPUStat{..} others) = userTime : niceTime : systemTime : idleTime : others

-- | Entire /proc/stat info with unknown fields.
data ProcStatInfo = MkProcStatInfo CPUField [CPUField] (M.Map FieldName [Int])
  deriving (Show)

instance Arbitrary ProcStatInfo where
  arbitrary :: Gen ProcStatInfo
  arbitrary = MkProcStatInfo <$> arbitrary <*> arbitrary <*> arbitrary

  shrink :: ProcStatInfo -> [ProcStatInfo]
  shrink (MkProcStatInfo total cpus others) =
    [MkProcStatInfo total' cpus' others' | (total', cpus', others') <- shrink (total, cpus, others)]

procStatFields :: ProcStatInfo -> M.Map T.Text [Int]
procStatFields (MkProcStatInfo total cpus others) =
  (cpuFieldNums <$> cpuFields) <> M.mapKeysMonotonic fieldNameText others
  where
    cpuFields = M.fromList $ (T.pack "cpu", total) : zip cpuKeys cpus
    cpuKeys = [T.pack "cpu" <> T.pack (show n) | n <- [0 ..]]

procStatCPUs :: ProcStatInfo -> (CPUStat Int, [CPUStat Int])
procStatCPUs (MkProcStatInfo total cpus _) = (partOf total, partOf <$> cpus)
  where
    partOf (MkCPUField stat _) = stat

listText :: Show a => [a] -> T.Text
listText = T.unwords . fmap @[] (T.pack . show)

-- TODO Likely better to check fields parsing separately
fieldsGenText :: (v -> T.Text) -> M.Map T.Text v -> Gen T.Text
fieldsGenText valText fields = T.unlines <$> shuffle lines
  where
    lines = [T.unwords [key, valText val] | (key, val) <- M.toAscList fields]

hardwareSpec :: Spec
hardwareSpec = do
  describe "CPUStatus" $ do
    describe "parseCPUStat" $ do
      prop "parses according to specifications" $ \psInfo ->
        forAll (fieldsGenText listText $ procStatFields psInfo) $ \text ->
          Parse.parse parseCPUStat.parsec "generated /proc/stat" text `shouldParse` procStatCPUs psInfo
