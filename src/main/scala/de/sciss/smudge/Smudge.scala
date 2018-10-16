/*
 *  Smudge.scala
 *  (Smudge)
 *
 *  Copyright (c) 2018 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.smudge

import de.sciss.file._
import de.sciss.numbers.Implicits._
import de.sciss.synth.io.AudioFile

import scala.collection.mutable

/*

  N.B. needs massive stack memory, e.g.

  -Xss8m

 */
object Smudge {
  final case class Config(
                         fIn            : File    = file("in.aif"),
                         minPeriod      : Int     = 2,
                         maxPeriod      : Int     = 1500,
                         waveform       : Int     = 0,
                         octaveCost     : Double  = 1.0,
                         phase          : Double  = 0.25,
                         inGain         : Double  = 2.0,
                         )

  def main(args: Array[String]): Unit = {
    val default = Config()

    val p = new scopt.OptionParser[Config]("Smudge") {
      opt[File]('i', "input")
        .required()
        .text("Input audio file.")
        .action { (v, c) => c.copy(fIn = v) }

      opt[Int]("min-period")
        .text(s"Minimum period length in sample frames, >= 2 (default: ${default.minPeriod})")
        .validate { v => if (v >= 2) success else failure("Must be >= 2") }
        .action { (v, c) => c.copy(minPeriod = v) }

      opt[Int]("max-period")
        .text(s"Maximum period length in sample frames (default: ${default.maxPeriod})")
        .validate { v => if (v >= 2) success else failure("Must be >= 2") }
        .action { (v, c) => c.copy(maxPeriod = v) }

      opt[Int]('w', "waveform")
        .text(s"Waveform type: 0 - sine, 1 - tri, 2 - pulse, 3 - saw (default: ${default.waveform})")
        .validate { v => if (v >= 0 && v <= 3) success else failure("Must be >= 0 and <= 3") }
        .action { (v, c) => c.copy(waveform = v) }

      opt[Double]('c', "octave-cost")
        .text(s"Octave cost factor, >1 prefers lower frequencies (default: ${default.octaveCost})")
        .validate { v => if (v >= 0) success else failure("Must be >= 0") }
        .action { (v, c) => c.copy(octaveCost = v) }

      opt[Double]('p', "phase")
        .text(s"Phase offset of waveforms in periods, 0 to 1 (default: ${default.phase})")
        .validate { v => if (v >= 0 && v < 1) success else failure("Must be >= 0 and < 1") }
        .action { (v, c) => c.copy(phase = v) }

      opt[Double]('g', "gain")
        .text(s"Input gain factor (default: ${default.inGain})")
        .action { (v, c) => c.copy(inGain = v) }

      //      opt[Unit]("backup")
//        .text("Backup workspace before modification")
//        .action { (_, c) => c.copy(backupWorkspace = true) }

    }
    p.parse(args, default).fold(sys.exit(1)) { config0 =>
      val config = if (config0.minPeriod <= config0.maxPeriod) config0 else
        config0.copy(minPeriod = config0.maxPeriod, maxPeriod = config0.minPeriod)
      run(config)
    }
  }

  def run(config: Config): Unit = {
    import config._

    val bufIn = {
      val afIn = AudioFile.openRead(fIn)
      try {
        val n = afIn.numFrames.toInt
        val b = afIn.buffer(n)
        afIn.read(b)
        val res = b(0)
        if (inGain != 1.0) {
          val f = inGain.toFloat
          for (i <- b.indices) res(i) *= f
        }
        res
      } finally {
        afIn.close()
      }
    }

    val numFreq     = maxPeriod - minPeriod + 1
    val phaseRad    = phase * math.Pi * 2
    val waveTables: Array[Array[Double]] = Array.tabulate(numFreq) { i =>
      val p = i + minPeriod
      waveform match {
        case 0 => // sine
          val t = math.Pi * 2 / p
          Array.tabulate(p) { j =>
            math.sin(j * t + phaseRad)
          }
        case 1 => // tri
          val t = 4.0 / p
          Array.tabulate(p) { j =>
            (j * t + phase).fold(-1.0, +1.0)
          }
        case 2 => // pulse
          val t = 1.0 / p
          Array.tabulate(p) { j =>
            if (j * t + phase < 0.5) 1.0 else 0.0
          }
        case 3 => // saw
          val t = 1.0 / p
          Array.tabulate(p) { j =>
            ((j * t + phase) % 1.0) * 2.0 - 1.0
          }
      }
    }

//    val lastPos = Array.fill(numFreq)(0)
//    for (i <- lastPos.indices) {
//
//    }

    val edges = mutable.LongMap.empty[Double]

    def calcRMS(start: Int, periodIdx: Int): Double = {
      val table = waveTables(periodIdx)
      var i = 0
      var j = start
      var sum = 0.0
      while (i < table.length) {
        val a = bufIn(j)
        val b = table(i)
        val d = a - b
        sum += d*d
        i += 1
        j += 1
      }
      math.sqrt(sum / table.length)
    }

    var lastProg  = 0
    val numFrames = bufIn.length
    println(s"numFrames = $numFrames; numFreq = $numFreq")

    def loop(start: Int): Unit = {
//      val prog = start * 100 / numFrames
//      while (lastProg < prog) {
//        print('#')
//        lastProg += 1
//      }
      val maxPeriodC    = math.min(numFrames - start, maxPeriod)
      val maxPeriodIdx  = maxPeriodC - minPeriod
      var pi = 0
      while (pi < maxPeriodIdx) {
        val edge = start.toLong << 32 | pi
        if (!edges.contains(edge)) {
          val cost = calcRMS(start, pi)
          edges.put(edge, cost)
          if (edges.size > lastProg) {
            println(edges.size)
            lastProg = edges.size + 1000
          }
          loop(start + pi + minPeriod)
        }
        pi += 1
      }
    }

    loop(0)
  }
}
