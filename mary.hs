import HSynth

toChord :: NoteName -> Chord
toChord note = Chord (majorChord (NoteLetter note 4)) 1

maryNotes = [E, D, C, D, E, E, E, D, D, D, E, G, G, E, D, C, D, E, E, E, E, D, D, E, D, C]

maryEnv = ADSR 0.05 0.05 0.75 0.1
maryPatch = Patch sinOsc 1 maryEnv

maryChords = map toChord maryNotes

marySound = play 0.25 maryPatch maryChords

normMarySound = map (/ maxLevel) marySound
  where
    maxLevel = maximum (map abs marySound)

saveMary = saveWav normMarySound "mary.wav"

main = saveMary
