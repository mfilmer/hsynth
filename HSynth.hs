module HSynth where

import Data.WAVE
import qualified Data.Vector as V
--import Data.Int (Int32)

type Octave = Int
type Frequency = Double
type Volume = Double
type Duration = Double
type Sound = [Double]
data TimeSig = TimeSig Int Duration -- Notes per measure, length of a measure in seconds
data Chord = Chord [Note] Int | Rest Int

data Envelope = ADSR Duration Duration Volume Duration
type Oscillator = Volume -> Note -> Sound
data Patch = Patch Oscillator Volume Envelope

data Note = NoteLetter NoteName Octave | NoteNumber NoteNum Octave deriving (Show)

type NoteNum = Int
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

sampleEnv = ADSR 0.05 0.05 0.75 0.05
samplePatch = Patch sinOsc 1 sampleEnv

getFreq :: Note -> Frequency
getFreq (NoteLetter note octave) = getFreq (NoteNumber (noteNum note) octave)
getFreq (NoteNumber notenum octave) = 440 * a ** n
  where
    a = 2 ** (1/12)
    n = (fromIntegral octave - 4) * 12 + (fromIntegral (notenum - noteNum A))

-- noteOffset takes a note and a number of semitones and returns the note that many semitones away
noteOffset :: Note -> Int -> Note
noteOffset (NoteLetter note octave) semitones = noteOffset (NoteNumber (noteNum note) octave) semitones
noteOffset (NoteNumber notenum octave) semitones = NoteNumber ((notenum + semitones - 1) `mod` 12 + 1) (octave + (notenum + semitones - 1) `div` 12)

majorChord :: Note -> [Note]
majorChord (NoteLetter note octave) = majorChord (NoteNumber (noteNum note) octave)
majorChord mainNote = [mainNote, majorThird, minorThird]
  where
    majorThird = noteOffset mainNote 4
    minorThird = noteOffset majorThird 3

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
--synth _ _ _ = sinOsc 0.15 (Note A 4)
synth (Patch osc vol env) dur note = envelope env dur rawOsc
  where
    rawOsc = osc vol note

play :: Duration -> Patch -> [Chord] -> Sound
play noteDur patch chords = buildSignal silence 0 0 chords
  where
    buildSignal :: Sound -> Int -> Int -> [Chord] -> Sound
    buildSignal signal i nCount [] = take (round $ fromIntegral nCount * noteDur * dSampleRate) signal
    buildSignal signal i nCount (chord@(Chord _ len):xs) = buildSignal (zipWith (+) signal (shiftSynth chord i)) (i+1) (max nCount (i+len)) xs
    buildSignal signal i nCount ((Rest len):xs) = buildSignal signal (i+nCount) nCount xs
    shiftSynth (Chord notes len) i = concat [take (round $ fromIntegral i*dSampleRate*noteDur) silence, foldr1 (zipWith (+)) $ map (synth patch noteDur) notes, silence]
    silence = repeat 0

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
