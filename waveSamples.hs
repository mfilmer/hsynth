import HSynth

sampleEnv = ADSR 0.1 0.1 0.75 0.1
samplePatch = Patch basicSquare sampleEnv 0.2
sampleSynth = buildSynth samplePatch

sampleSound = playNote sampleSynth (getNote A 4) 1

main = saveWav sampleSound "testWave.wav"
