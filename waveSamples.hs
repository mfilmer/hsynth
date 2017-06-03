import HSynth
import qualified Data.Vector as V

sampleEnv = ADSR 0.1 0.1 1 0.1
samplePatch = Patch basicSin sampleEnv 0.2
sampleSynth = buildSynth samplePatch
sampleSound = playNote sampleSynth (getNote A 5) 1
--main = saveWav sampleSound "testWave.wav"


--sampleVco = vcoSin (concat [replicate (44100 `div` 3) 440, replicate (44100 `div` 3) 220, replicate (44100 `div` 3) 880])
sampleVco = vcoSin (take 44100 (basicSin 1))
sampleVcoSound = V.fromList $ take 44100 $ sampleVco 440
main = saveWav sampleVcoSound "testWave.wav"
