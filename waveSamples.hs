import HSynth

sampleEnv = ADSR 0.01 0.02 0 0
samplePatch = Patch basicSin sampleEnv 0.2
sampleSynth = buildSynth samplePatch

sampleSound = playNote sampleSynth (getNote A 2) 1

main = saveWav sampleSound "testWave.wav"
