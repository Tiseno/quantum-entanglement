{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}

import           Control.Monad       (replicateM)
import           Control.Monad.State (MonadState (get, put), State, evalState, gets, modify, runState)
import           Data.List           (partition)
import           Data.Maybe          (fromMaybe)
import           Numeric.Extra       (intToFloat)
import           System.Random       (Random (randomR), RandomGen, StdGen, getStdGen, randomRIO)
import           Text.Printf         (printf)

newtype Angle = Angle Float deriving (Show, Eq)
radians (Angle f) = f

mkAngle :: Float -> Angle
mkAngle f | f >= pi*2 = mkAngle (f - pi*2)
mkAngle f | f < 0 = mkAngle (f + pi*2)
mkAngle f = Angle f

toRad :: Float -> Float
toRad d = pi * d / 180

class Opposite a where
  opposite :: a -> a

instance Opposite Angle where
  opposite (Angle a) = mkAngle (a + pi)

rndAngle :: RandomGen g => State g Angle
rndAngle = do
  g <- get
  let (f, g') = randomR (0, pi*2) g
  put g'
  pure $ mkAngle f

rndFloat :: RandomGen g => State g Float
rndFloat = do
  g <- get
  let (f :: Float, g') = randomR (0, 1) g
  put g'
  pure f

data Spin = Up | Down deriving (Show, Enum, Bounded, Eq)

instance Opposite Spin where
  opposite Up   = Down
  opposite Down = Up

sameDirection :: Angle -> Angle -> Bool
sameDirection (Angle a) (Angle b) =
  let diff = abs (b - a)
      aDiff = min diff (pi*2 - diff)
   in aDiff < (pi/2)

class Monad p => ParticleModel p where
  readP1AtAngle :: Angle -> p Spin
  readP2AtAngle :: Angle -> p Spin
  -- Run an experiment in the model with access to a random generator
  runWithRandomState :: RandomGen g => p a -> State g a

newtype ClassicalParticles a = ClassicalParticles (State Angle a) deriving (Functor, Applicative, Monad)

instance MonadState Angle ClassicalParticles where
  get :: ClassicalParticles Angle
  get = ClassicalParticles get
  put :: Angle -> ClassicalParticles ()
  put = error "Classical particles can not update state"

instance ParticleModel ClassicalParticles where
  readP1AtAngle :: Angle -> ClassicalParticles Spin
  readP1AtAngle readAngle = do
    particleAngle <- get
    pure $ toEnum $ fromEnum $ sameDirection readAngle particleAngle
  readP2AtAngle :: Angle -> ClassicalParticles Spin
  readP2AtAngle readAngle = do
    particleAngle <- get
    pure $ toEnum $ fromEnum $ sameDirection readAngle (opposite particleAngle)
  runWithRandomState :: RandomGen g => ClassicalParticles a -> State g a
  runWithRandomState experiment = do
    angle <- rndAngle -- In the classical interpretation an angle is randomized and determined at creation
    let (ClassicalParticles s) = experiment
    pure $ evalState s angle

type Measurement = (Angle, Spin)
-- Either we have a source of randomness, or we have made the measurement and the particle has collapsed at a specific angle and spin
type QuantumState = (Either Float Measurement, Either Float Measurement)

newtype QuantumParticles a = QuantumParticles (State QuantumState a) deriving (Functor, Applicative, Monad)

instance MonadState QuantumState QuantumParticles where
  get :: QuantumParticles QuantumState
  get = QuantumParticles get
  put :: QuantumState -> QuantumParticles ()
  put = QuantumParticles . put

measureQuantumParticle :: Angle -> Either Float Measurement -> Either Float Measurement -> Spin
measureQuantumParticle readAngle state entangledState =
  case (state, entangledState) of
    (Right (_, spin), _) -> spin -- We have already collapsed the particle
    (Left r, Left _) -> if r < 0.5 then Up else Down -- We have not collapsed the other particle, this particle collapses randomly
    (Left r, Right (otherAngle, otherSpin)) -> -- The other particle has collapsed and affects this result
      let d = deltaAngle (readAngle, otherAngle)
          probability = 0.5 * (1 + cos (d * 2))
       in if r < probability
                 then opposite otherSpin
                 else otherSpin

instance ParticleModel QuantumParticles where
  readP1AtAngle :: Angle -> QuantumParticles Spin
  readP1AtAngle readAngle = do
    (p1, p2) <- get
    let spin = measureQuantumParticle readAngle p1 p2
    put (Right (readAngle, spin), p2)
    pure spin
  readP2AtAngle :: Angle -> QuantumParticles Spin
  readP2AtAngle readAngle = do
    (p1, p2) <- get
    let spin = measureQuantumParticle readAngle p2 p1
    put (p1, Right (readAngle, spin))
    pure spin
  runWithRandomState :: RandomGen g => QuantumParticles a -> State g a
  runWithRandomState experiment = do
    -- In the quantum model the particle angle is created when measured, but we still need sources of randomness to collapse the particles
    r1 <- rndFloat
    r2 <- rndFloat
    let (QuantumParticles s) = experiment
    pure $ evalState s (Left r1, Left r2)

type DetectorPair = (Angle, Angle)
type SpinPair = (Spin, Spin)
type Result = (DetectorPair, SpinPair)

-- The actual experiment
-- Given a pair of detectors and a model of how particles work:
--  1. measure one particle
--  2. measure the other particle
readTwoParticlesAtDetectors :: ParticleModel m => DetectorPair -> m Result
readTwoParticlesAtDetectors (d1, d2) = do
  p1 <- readP1AtAngle d1
  p2 <- readP2AtAngle d2
  pure ((d1, d2), (p1, p2))

-- Type helpers to choose the underlying model of the experiment
type ClassicalExperiment = DetectorPair -> ClassicalParticles Result
type QuantumExperiment = DetectorPair -> QuantumParticles Result

staticAngleDetectors :: RandomGen g => Float -> State g DetectorPair
staticAngleDetectors deg = pure (Angle 0, Angle $ toRad deg)

randomDetectors :: RandomGen g => State g DetectorPair
randomDetectors = do
  d1 <- randomFirstDetector
  d2 <- randomSecondDetector
  pure (d1, d2)
  where
    randomFirstDetector :: RandomGen g => State g Angle
    randomFirstDetector = do
      g <- get
      let (b, g') = randomR (0 :: Int, 1) g
      put g'
      pure $ mkAngle $ toRad (intToFloat b * 90)
    randomSecondDetector :: RandomGen g => State g Angle
    randomSecondDetector = do
      g <- get
      let (b, g') = randomR (0 :: Int, 1) g
      put g'
      pure $ mkAngle $ toRad (45 + (intToFloat b * 90))

numberOfTestRuns = 1000000

deltaAngle :: DetectorPair -> Float
deltaAngle (d1, d2) = abs (radians d1 - radians d2)

average :: (a -> Float) -> [a] -> Float
average fn l = sum (fmap fn l) / fromIntegral (length l)

classicalDetectorCorrelation :: DetectorPair -> Float
classicalDetectorCorrelation detectorPair = negate (1 - (2 * deltaAngle detectorPair / pi))

quantumDetectorCorrelation :: DetectorPair -> Float
quantumDetectorCorrelation detectorPair = negate (cos (2 * deltaAngle detectorPair))

spinCorrelation :: SpinPair -> Float
spinCorrelation (s1, s2) = spinToInt s1 * spinToInt s2
  where
    spinToInt :: Spin -> Float
    spinToInt Up   = 1
    spinToInt Down = negate 1

showFloat :: Float -> String
showFloat = printf "%.5f"

runExperiment ::
  (RandomGen g, ParticleModel p) =>
  State g DetectorPair ->
  (DetectorPair -> p Result) ->
  State g Result
runExperiment detectorSetup experiment = do
  detectors <- detectorSetup
  runWithRandomState $ experiment detectors

testAndReport :: (RandomGen g, ParticleModel p) =>
  String ->
  g ->
  State g DetectorPair ->
  (DetectorPair -> p Result) ->
  IO g
testAndReport title g0 detectorSetup experiment = do -- NOTE: we only inject the experiment to be able to choose the model from the outside, it will always be the same
  putStrLn $ "Running test: " ++ title

  let (results, g1) = runState (replicateM numberOfTestRuns (runExperiment detectorSetup experiment)) g0

  putStrLn "  expected classical correlation:"
  putStrLn $ "    " ++ showFloat (average classicalDetectorCorrelation (fst <$> results))
  putStrLn ""

  putStrLn "  expected quantum correlation:"
  putStrLn $ "    " ++ showFloat (average quantumDetectorCorrelation (fst <$> results))
  putStrLn ""

  putStrLn "  spin correlation:"
  putStrLn $ "    " ++ showFloat (average spinCorrelation (snd <$> results))
  putStrLn ""

  pure g1

main :: IO ()
main = do
  g0 <- getStdGen

  -- If we measure at the same direction for both particles we do not measure anything interesting, they should always be opposite in both local and quantum models
  g1 <- testAndReport "Static up detectors Classical" g0 (staticAngleDetectors 0) (readTwoParticlesAtDetectors :: ClassicalExperiment)
  g2 <- testAndReport "Static up detectors Quantum"   g1 (staticAngleDetectors 0) (readTwoParticlesAtDetectors :: QuantumExperiment)

  g1 <- testAndReport "Static down detectors Classical" g0 (staticAngleDetectors 180) (readTwoParticlesAtDetectors :: ClassicalExperiment)
  g2 <- testAndReport "Static down detectors Quantum"   g1 (staticAngleDetectors 180) (readTwoParticlesAtDetectors :: QuantumExperiment)

  g1 <- testAndReport "Static 90 degree detectors Classical" g0 (staticAngleDetectors 90) (readTwoParticlesAtDetectors :: ClassicalExperiment)
  g2 <- testAndReport "Static 90 degree detectors Quantum"   g1 (staticAngleDetectors 90) (readTwoParticlesAtDetectors :: QuantumExperiment)

  g1 <- testAndReport "Static 120 degree detectors Classical" g0 (staticAngleDetectors 120) (readTwoParticlesAtDetectors :: ClassicalExperiment)
  g2 <- testAndReport "Static 120 degree detectors Quantum"   g1 (staticAngleDetectors 120) (readTwoParticlesAtDetectors :: QuantumExperiment)

  g3 <- testAndReport "Random detectors Classical" g2 randomDetectors (readTwoParticlesAtDetectors :: ClassicalExperiment)
  g4 <- testAndReport "Random detectors Quantum"   g3 randomDetectors (readTwoParticlesAtDetectors :: QuantumExperiment)

  pure ()


