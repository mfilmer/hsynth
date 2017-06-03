module HSynth where

import Data.WAVE
import qualified Data.Vector as V
import Data.Int (Int8)
import Data.Hashable
import qualified Data.Map.Strict as M

-- Notes
data NoteSym = C | Cs | Df | D | Ds | Ef | E | F | Fs | Gf| G | Gs | Af | A | As | Bf | B
  deriving (Show)
type KeySig = [NoteSym]

type NoteNum = Int8
type Octave = Int8
data Note = Note NoteNum Octave deriving (Show)
type Chord = [Note]

instance Hashable Note where
  hashWithSalt salt (Note noteNum octave) = hashWithSalt salt (noteNum, octave)

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

instance Hashable Beat where
  hashWithSalt salt (Beat chord nbeats) = hashWithSalt salt (chord, nbeats)

-- Sample Rate
sampleRate = 44100
dSampleRate = fromIntegral sampleRate

getNoteNum :: NoteSym -> NoteNum
getNoteNum C = 0
getNoteNum Cs = 1
getNoteNum Df = 1
getNoteNum D = 2
getNoteNum Ds = 3
getNoteNum Ef = 3
getNoteNum E = 4
getNoteNum F = 5
getNoteNum Fs = 6
getNoteNum Gf = 6
getNoteNum G = 7
getNoteNum Gs = 8
getNoteNum Af = 8
getNoteNum A = 9
getNoteNum As = 10
getNoteNum Bf = 10
getNoteNum B = 11

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
noteOffset (Note notenum octave) semitones = Note ((notenum + semitones) `mod` 12) (octave + (notenum + semitones) `div` 12)

majorChord :: Note -> Chord
majorChord baseNote = [baseNote, majorThird, minorThird]
  where
    majorThird = noteOffset baseNote 4
    minorThird = noteOffset majorThird 3

applyKeySig :: KeySig -> Chord -> Chord
applyKeySig = undefined

-- Generate sounds
applyEnvelope :: Envelope -> Duration -> RawSound -> Sound
applyEnvelope (ADSR a d s r) rawDur rawSound = V.zipWith (*) (V.fromList $ take nSamples rawSound) env
  where
    dur = rawDur - a - d
    decayRate = (1-s)/(d*dSampleRate)
    nSamples = getSampleCount (rawDur + r)
    env = V.concat [attack, decay, sustain, release]
    attack = V.generate (getSampleCount a) (logScale . (\x -> fromIntegral x/(a*dSampleRate)))
    decay = V.generate (getSampleCount d) (logScale . (\x -> -decayRate*fromIntegral x + 1))
    sustain = V.replicate (getSampleCount dur) (logScale s)
    release = V.generate (getSampleCount r) (logScale . (\x -> -s/(r*dSampleRate)*fromIntegral x+s))

play :: Synth -> Playable -> Sound
play synth (Playable chord duration) = mixAll sounds
  where
    sounds = map (\note -> playNote synth note duration) chord

playAll :: Synth -> [Playable] -> Sound
playAll synth playables = mixAll sounds
  where
    sounds = map (play synth) playables

playNote :: Synth -> Note -> Duration -> Sound
playNote synth note duration = synth note duration

playChord :: Synth -> Chord -> Duration -> Sound
playChord synth chord duration = play synth (Playable chord duration)

playBeat :: Synth -> Beat -> Duration -> Sound
playBeat synth (Beat chord nbeats) duration = play synth (Playable chord $ duration * fromIntegral nbeats)

sequenceBeatCount :: Sequence -> NBeats
sequenceBeatCount = foldr (\(Beat _ newBeats) oldBeats -> newBeats + oldBeats) 0

-- Memoizes the synthesized beats for a performance improvement
playSequence :: Synth -> Sequence -> Duration -> Sound
playSequence synth beats dur = mixAll $ playSeq beats 0 M.empty
  where
    playSeq [] _ _ = []
    playSeq (beat:beats) i oldMap = (silence (i*dur) V.++ newBeat):playSeq beats (i+1) newMap
      where
        beatHash = hash beat
        newBeat = M.findWithDefault (playBeat synth beat dur) beatHash oldMap
        newMap = M.insert beatHash newBeat oldMap
    

-- Mix sounds
mix :: Sound -> Sound -> Sound
mix a b = V.zipWith (+) a b V.++ extra
  where
    lenA = length a
    lenB = length b
    minLen = min lenA lenB
    maxLen = max lenA lenB
    extra = case signum (lenA - lenB) of
      0 -> V.empty
      -1 -> V.slice minLen (maxLen - minLen) b
      1 -> V.slice minLen (maxLen - minLen) a

mixAll :: [Sound] -> Sound
mixAll = foldr1 mix

-- Sequencer
arpegio :: Chord -> Duration -> Sequence
arpegio = undefined

-- Oscillator shape functions
squ :: Double -> Double
squ x = if decimals < 0.5 then -1 else 1
  where
    decimals = x - fromIntegral (floor x)

saw :: Double -> Double
saw x = if decimals < 0.5 then 2 * decimals else 2 * decimals - 2
  where
    decimals = x - fromIntegral (floor x)

-- Basic oscillators
rawSilence :: RawSound
rawSilence = repeat 0

silence :: Duration -> Sound
silence dur = V.replicate (getSampleCount dur) 0

makeBasicOsc :: (Double -> Double) -> Oscillator
makeBasicOsc fn = osc
  where
    osc freq = map fn $ [0, (freq/dSampleRate)..]

basicSin :: Oscillator
basicSin = makeBasicOsc (\x -> sin (2*pi*x))
--basicSin freq = map sin $ [0, (freq*2*pi/dSampleRate)..]

basicSquare :: Oscillator
basicSquare = makeBasicOsc squ
--basicSquare freq = map squ $ [0, (freq/dSampleRate)..]

basicSawtooth :: Oscillator
basicSawtooth = makeBasicOsc saw
--basicSawtooth freq = map saw $ [0, (freq/dSampleRate)..]

basicTriangle :: Oscillator
basicTriangle = undefined

-- Voltage controlled oscillators
makeVcoOsc :: (Double -> Double) -> (RawSound -> Oscillator)
makeVcoOsc fn = vcoFn
  where
    vcoFn m = vco
      where
        vco freq = map fn $ [freq/dSampleRate * (t + intm) | (t, intm) <- zip [0..] (integrate m)]

vcoSin :: RawSound -> Oscillator
vcoSin = makeVcoOsc (\x -> sin(2*pi*x))

vcoSquare :: RawSound -> Oscillator
vcoSquare = makeVcoOsc squ


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

getSampleCount :: Duration -> Int
getSampleCount = round . (* dSampleRate)

integrate :: [Double] -> [Double]
integrate = scanl1 (+)
