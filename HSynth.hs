module HSynth where

import Data.WAVE
import qualified Data.Vector as V
import Data.Int (Int8)

-- Notes
data NoteSym = C | Cs | Df | D | Ds | Ef | E | F | Fs | Gf| G | Gs | Af | A | As | Bf | B
  deriving (Show)
type KeySig = [NoteSym]

type NoteNum = Int8
type Octave = Int8
data Note = Note NoteNum Octave
type Chord = [Note]

-- Synthesizer
type Frequency = Double
type Volume = Double
type Duration = Double
type RawSound = [Double]
type Sound = V.Vector Double
type Oscillator = Frequency -> RawSound
data Envelope = ADSR Duration Duration Volume Duration
data Patch = Patch Oscillator Envelope Volume
type Synth = Note -> Duration -> Sound
data Playable = Playable Chord Duration

-- Sequencer
type NBeats = Int8
data Beat = Beat Chord NBeats
type Sequence = [Beat]

-- Sample Rate
sampleRate = 44100
dSampleRate = fromIntegral sampleRate

getNoteNum :: NoteSym -> NoteNum
getNoteNum C = 1
getNoteNum Cs = 2
getNoteNum Df = 2
getNoteNum D = 3
getNoteNum Ds = 4
getNoteNum Ef = 4
getNoteNum E = 5
getNoteNum F = 6
getNoteNum Fs = 7
getNoteNum Gf = 7
getNoteNum G = 8
getNoteNum Gs = 9
getNoteNum Af = 9
getNoteNum A = 10
getNoteNum As = 11
getNoteNum Bf = 11
getNoteNum B = 12

getNote :: NoteSym -> Octave -> Note
getNote noteSym octave = Note (getNoteNum noteSym) octave

getFreq :: Note -> Frequency
getFreq (Note noteNum octave) = 440 * (2 ** (1/12)) ** n
  where
    n = (fromIntegral octave - 4) * 12 + (fromIntegral (noteNum - getNoteNum A))

buildSynth :: Patch -> Synth
buildSynth (Patch osc env vol) = synth
  where
    synth note duration = applyEnvelope env duration (osc $ getFreq note)

-- Note modification functions
noteOffset :: Note -> Int8 -> Note
noteOffset (Note notenum octave) semitones = Note ((notenum + semitones - 1) `mod` 12 + 1) (octave + (notenum + semitones - 1) `div` 12)

majorChord :: Note -> Chord
majorChord = undefined

applyKeySig :: KeySig -> Chord -> Chord
applyKeySig = undefined

-- Generate sounds
applyEnvelope :: Envelope -> Duration -> RawSound -> Sound
applyEnvelope (ADSR a d s r) rawDur rawSound = V.zipWith (*) (V.fromList $ take nSamples rawSound) env
  where
    dur = rawDur - a - d
    decayRate = (1-s)/(d*dSampleRate)
    nSamples = round $ (rawDur + r) * dSampleRate
    env = V.concat [attack, decay, sustain, release]
    attack = V.generate (round $ a*dSampleRate) (logScale . (\x -> fromIntegral x/(a*dSampleRate)))
    decay = V.generate (round $ d*dSampleRate) (logScale . (\x -> -decayRate*fromIntegral x + 1))
    sustain = V.replicate (round $ dSampleRate * dur) (logScale s)
    release = V.generate (round $ r*dSampleRate) (logScale . (\x -> -s/(r*dSampleRate)*fromIntegral x+s))

play :: Synth -> Playable -> Sound
play synth (Playable chord duration) = mix sounds
  where
    sounds = map (\note -> playNote synth note duration) chord

playAll :: Synth -> [Playable] -> Sound
playAll synth playables = mix sounds
  where
    sounds = map (play synth) playables

playNote :: Synth -> Note -> Duration -> Sound
playNote synth note duration = synth note duration

playChord :: Synth -> Chord -> Duration -> Sound
playChord synth chord duration = play synth (Playable chord duration)

playBeat :: Synth -> Beat -> Duration -> Sound
playBeat synth (Beat chord nbeats) duration = play synth (Playable chord $ duration * fromIntegral nbeats)

playSequence :: Synth -> Sequence -> Duration -> Sound
playSequence = undefined

-- Mix sounds
mix :: [Sound] -> Sound
mix = foldr1 (V.zipWith (+))

-- Sequencer
arpegio :: Chord -> Duration -> Sequence
arpegio = undefined

-- Basic oscillators
basicSin :: Oscillator
basicSin freq = map sin $ [0.0, (freq*2*pi/(dSampleRate))..]

basicSquare :: Oscillator
basicSquare = undefined

basicSawtooth :: Oscillator
basicSawtooth = undefined

basicTriangle :: Oscillator
basicTriangle = undefined

-- Voltage controlled oscillators
vcoSin :: Sound -> Oscillator
vcoSin = undefined

-- Filters
lowpassFilter :: Frequency -> Sound -> Sound
lowpassFilter = undefined

highpassFilter :: Frequency -> Sound -> Sound
highpassFilter = undefined

bandpassFilter :: (Frequency, Frequency) -> Sound -> Sound
bandpassFilter (low, high) sound = highpassFilter low $ lowpassFilter high sound

-- File writing
waveFileHeader = WAVEHeader 1 sampleRate 32 Nothing

packSignal :: Sound -> WAVESamples
packSignal xs = V.toList $ V.map ((:[]) . round . (* (2^31-1))) xs

saveWav :: Sound -> String -> IO ()
saveWav stream filename = putWAVEFile filename wav
  where
    samples = packSignal stream
    wav = WAVE waveFileHeader samples

-- Helper functions
logScale :: Double -> Double
logScale x = (exp x - 1)/(exp 1 - 1)
