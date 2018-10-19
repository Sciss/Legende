# Légende

Software for an algorithmic sound piece. (C)opyright 2018 by Hanns Holger Rutz. All rights reserved.
This project is released under the
[GNU Affero General Public License](https://git.iem.at/sciss/Legende/blob/master/LICENSE) v3+ and 
comes with absolutely no warranties.
To contact the author, send an email to `contact at sciss.de`.

--------------

## Experiments

    --input /data/projects/Segmod/audio_work/by-ear.aif --edges-output /data/projects/Segmod/data/by-ear-edges-%d.bin --route-output /data/projects/Segmod/data/by-ear-route-%d.bin --seg-mod-output /data/projects/Segmod/data/by-ear-segmod-%d.aif --diff-output /data/projects/Segmod/data/by-ear-dif-%d.aif --gain 8.0 --phase 0.0

## To-do

- {ok} fix phase0 argument
- try constraining frequency movement ('lag')
- can we _stretch_ the routes? what happens?

## Rendering

No. 1:

    --input /data/projects/Segmod/audio_work/AlphavilleTropComplexe-1.aif --edges-output /data/projects/Segmod/data/alphaville-1-edges-%d.bin --route-output /data/projects/Segmod/data/alphaville-1-route-%d.bin --seg-mod-output /data/projects/Segmod/data/alphaville-1-segmod-%d.aif --diff-output /data/projects/Segmod/data/alphaville-1-dif-%d.aif --gain 3.0 --phase 0.25

No. 2:

    --input /data/projects/Segmod/audio_work/AlphavilleTropComplexe-2.aif --edges-output /data/projects/Segmod/data/alphaville-2-edges-%d.bin --route-output /data/projects/Segmod/data/alphaville-2-route-%d.bin --seg-mod-output /data/projects/Segmod/data/alphaville-2-segmod-%d.aif --diff-output /data/projects/Segmod/data/alphaville-2-dif-%d.aif --gain 3.0 --phase 0.25 --iterations 40 --waveform-amp 0.016423203268260675 --waveform-damp 1.111111111111111111
