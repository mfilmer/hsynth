import HSynth
import qualified Data.Vector as V

sampleEnv = ADSR 0.1 0.1 1 0.1
samplePatch = Patch (vcoSin (map (*0.02) $ basicSquare 15)) sampleEnv 1
sampleSynth = buildSynth samplePatch
sampleSound = playChord sampleSynth (majorChord (getNote E 5)) 1
normalizedSound = V.map (/ (maximum $ V.map abs sampleSound)) sampleSound
main = saveWav normalizedSound "testWave.wav"


--sampleVco = vcoSin (concat [replicate (44100 `div` 3) 440, replicate (44100 `div` 3) 220, replicate (44100 `div` 3) 880])
--sampleVco = vcoSawtooth (take 44100 [0,1/44100..1])
--sampleVcoSound = V.fromList $ take 44100 $ sampleVco 440
--main = saveWav sampleVcoSound "testWave.wav"
