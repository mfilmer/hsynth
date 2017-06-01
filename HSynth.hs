module HSynth where

import Data.WAVE
--import Data.Int (Int32)

type Frequency = Double
type Volume = Double
type Duration = Double

sampleRate = 44100 :: Int    -- Frames/second

header = WAVEHeader 1 sampleRate 32 Nothing

baseSin :: Frequency -> [Double]
baseSin freq = map sin [0.0, (freq*2*pi/(fromIntegral sampleRate))..]

saveWav :: [Double] -> String -> IO ()
saveWav stream filename = putWAVEFile filename wav
  where
    samples = map (round . (* (2^31-1))) stream :: WAVESamples
    wav = WAVE header samples

--sinOsc :: Frequency -> Volume -> Duration -> WAVESamples
--sinOsc freq vol dur = map sin [0.0, (freq*2*pi/(fromIntegral sampleRate))..]
--  where
--    clip level xs = map (\x -> if abs x > level then signum x * level else x)
