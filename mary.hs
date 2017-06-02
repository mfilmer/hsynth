import HSynth

toChord :: NoteName -> Chord
toChord note = Chord [Note note 4] 0.5 1

maryNotes = [E, D, C, D, E, E, E, D, D, D, E, G, G, E, D, C, D, E, E, E, E, D, D, E, D, C]

--maryNotes = [E, D]

maryChords = map toChord maryNotes

marySound = play 0.25 samplePatch maryChords

normMarySound = map (/ maxLevel) marySound
  where
    maxLevel = maximum (map abs marySound)
