module HSynth where

import Data.WAVE
--import Data.Int (Int32)

type Octave = Int
type Frequency = Double
type Volume = Double
type Duration = Double
type Sound = [Double]

data Note = C | Cs | Df | D | Ds | Ef | E | F | Fs | Gf| G | Gs | Af | A | As | Bf | B
  deriving (Show)

noteNum :: Note -> Int
noteNum C = 1
noteNum Cs = 2
noteNum Df = 2
noteNum D = 3
noteNum Ds = 4
noteNum Ef = 4
noteNum E = 5
noteNum F = 6
noteNum Fs = 7
noteNum Gf = 7
noteNum G = 8
noteNum Gs = 9
noteNum Af = 9
noteNum A = 10
noteNum As = 11
noteNum Bf = 11
noteNum B = 12

instance Eq Note where
  a == b = noteNum a == noteNum b
  
getFreq :: Note -> Octave -> Frequency
getFreq note octave = 440 * a ** n
  where
    a = 2**(1/12)
    n = (fromIntegral octave - 4) * 12 + (fromIntegral (noteNum note - noteNum A))

sampleRate = 44100 :: Int    -- Frames/second
header = WAVEHeader 1 sampleRate 32 Nothing

baseSin :: Frequency -> Sound
baseSin freq = map sin [0.0, (freq*2*pi/(fromIntegral sampleRate))..]

packSignal :: Sound -> WAVESamples
packSignal xs = map ((:[]) . round . (* (2^31-1))) xs

clipUnity :: Sound -> Sound
clipUnity = clipSignal 1

clipSignal :: Volume -> Sound -> Sound
clipSignal rawCutoff = map (\x -> if (abs x) > cutoff then signum x * cutoff else x)
  where cutoff = abs(rawCutoff)

saveWav :: Sound -> String -> IO ()
saveWav stream filename = putWAVEFile filename wav
  where
    samples = packSignal stream
    wav = WAVE header samples

--sinOsc :: Frequency -> Volume -> Duration -> WAVESamples
--sinOsc freq vol dur = map sin [0.0, (freq*2*pi/(fromIntegral sampleRate))..]
--  where
--    clip level xs = map (\x -> if abs x > level then signum x * level else x)
