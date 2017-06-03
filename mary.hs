import HSynth
import qualified Data.Vector as V

toBeat :: NoteSym -> Beat
toBeat note = Beat (majorChord (Note (getNoteNum note) 4)) 1

maryNotes = [E, D, C, D, E, E, E, D, D, D, E, G, G, E, D, C, D, E, E, E, E, D, D, E, D, C]

maryEnv = ADSR 0.05 0.05 0.75 0.1
maryPatch = Patch basicSin maryEnv 1
marySynth = buildSynth maryPatch

maryBeats = map toBeat maryNotes

marySound = playSequence marySynth maryBeats 0.25

normMarySound = V.map (/ maxLevel) marySound
  where
    maxLevel = V.maximum (V.map abs marySound)

saveMary = saveWav normMarySound "mary.wav"

main = saveMary
