import HSynth
import qualified Data.Vector as V

bassEnv = ADSR 0 0.1 0 0
bassPatch = Patch (basicSin) bassEnv 1
bassSynth = buildSynth bassPatch
bassBeat1 = Beat [getNote E 3] 2
bassBeat2 = Beat [getNote G 3] 2
bassBeat3 = Beat [getNote C 3] 2
--bassSound = playNote bassSynth (getNote C 3) 1
bassSequence = [bassBeat1, silentBeat, bassBeat1, silentBeat, bassBeat1, bassBeat2, silentBeat, silentBeat, bassBeat1, silentBeat, bassBeat1, silentBeat, bassBeat1, bassBeat3, silentBeat, silentBeat]
bassSound = playSequence bassSynth bassSequence 0.25
main = saveWav bassSound "testWave.wav"
