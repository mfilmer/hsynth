module HSynth where

import Data.WAVE
--import Data.Int (Int32)

type Octave = Int
type Frequency = Double
type Volume = Double
type Duration = Double
type Sound = [Double]
data TimeSig = TimeSig Int Duration -- Notes per measure, length of a measure in seconds
data Chord = Chord [Note] Volume Int | Rest Int

data Envelope = ADSR Duration Duration Volume Duration
type Oscillator = Volume -> Note -> Sound
data Patch = Patch Oscillator Volume Envelope

data Note = Note NoteName Octave

data NoteName = C | Cs | Df | D | Ds | Ef | E | F | Fs | Gf| G | Gs | Af | A | As | Bf | B
  deriving (Show)

noteNum :: NoteName -> Int
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

instance Eq NoteName where
  a == b = noteNum a == noteNum b

sampleEnv = ADSR 0.1 0.1 0.75 0.1
samplePatch = Patch sinOsc 1 sampleEnv

getFreq :: Note -> Frequency
getFreq (Note note octave) = 440 * a ** n
  where
    a = 2**(1/12)
    n = (fromIntegral octave - 4) * 12 + (fromIntegral (noteNum note - noteNum A))

sampleRate = 44100 :: Int    -- Frames/second
dSampleRate = fromIntegral sampleRate
header = WAVEHeader 1 sampleRate 32 Nothing

baseSin :: Frequency -> Sound
baseSin freq = map sin [0.0, (freq*2*pi/(dSampleRate))..]

packSignal :: Sound -> WAVESamples
packSignal xs = map ((:[]) . round . (* (2^31-1))) xs

clipUnity :: Sound -> Sound
clipUnity = clipSignal 1

clipSignal :: Volume -> Sound -> Sound
clipSignal rawCutoff = map (\x -> if (abs x) > cutoff then signum x * cutoff else x)
  where cutoff = abs(rawCutoff)

-- TODO: address cases where dur is shorter than attack + decay
envelope :: Envelope -> Duration -> Sound -> Sound
envelope (ADSR a d s r) rawDur rawSound = zipWith (*) rawSound env
  where
    dur = rawDur - a - d
    env = concat [attack, decay, sustain, release]
    attack = map (logScale . (\x -> x/(a*dSampleRate))) [1..a * dSampleRate]
    decay = map (logScale . (\x -> (fullDecaySamples - x)/fullDecaySamples)) [1..d * dSampleRate]
    fullDecaySamples = d/(1-s) * dSampleRate
    sustain = take (round $ dSampleRate * dur) (repeat (logScale s))
    -- TODO: implement the release part of this
    release = map (logScale . (\x -> -s/(r*dSampleRate)*x+s)) [1..r * dSampleRate]

logScale :: Double -> Double
logScale x = (exp x - 1)/(exp 1 - 1)

synth :: Patch -> Duration -> Note -> Sound
synth (Patch osc vol env) dur note = envelope env dur rawOsc
  where
    rawOsc = osc vol note

--play :: TimeSig -> Patch -> [Chord] -> Sound
--play (TimeSig count dur) (Patch oscillator volume envelope) chords = buildSignal (repeat 0) 0 0 chords
--  where
--    buildSignal signal i dur ((Chord notes vol len):xs) = buildSignal (signal + synth ) (i+1) (max dur (i+len)) xs
--    buildSignal signal i dur ((Rest len):xs) = buildSignal signal (i+dur) dur xs
--    signalPart = 

saveWav :: Sound -> String -> IO ()
saveWav stream filename = putWAVEFile filename wav
  where
    samples = packSignal stream
    wav = WAVE header samples

-- TODO: make this actually care about the specified volume
sinOsc :: Volume -> Note -> Sound
sinOsc vol note = map ((logScale vol *) . sin) [0.0, (freq*2*pi/(fromIntegral sampleRate))..]
  where
    freq = getFreq note
