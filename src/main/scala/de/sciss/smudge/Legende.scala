/*
 *  Legende.scala
 *  (Légende)
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

import java.io.{DataOutputStream, FileOutputStream}

import de.sciss.file._
import de.sciss.numbers.Implicits._
import de.sciss.synth.io.AudioFile

import scala.collection.mutable

/*

  N.B. needs massive stack memory, e.g.

  -Xss8m

 */
object Legende {
  final val COOKIE        = 0x536D7564 // "Smud"
  final val FILE_VERSION  = 0

  final case class Config(
                           fSoundIn     : File    = file("in.aif"),
                           fEdgesOut    : File    = file("edges.aif"),
                           minPeriod    : Int     = 2,
                           maxPeriod    : Int     = 1024,
                           periodStep   : Int     = 2,
//                           maxPeriodJump: Double  = 2.0,
                           waveform     : Int     = 0,
                           octaveCost   : Double  = 1.0,
                           phase        : Double  = 0.25,
                           inGain       : Double  = 2.0,
                         )

  def main(args: Array[String]): Unit = {
    val default = Config()

    val p = new scopt.OptionParser[Config]("Légende") {
      opt[File]('i', "input")
        .required()
        .text("Input audio file.")
        .action { (v, c) => c.copy(fSoundIn = v) }

      opt[File]('e', "edges-output")
        .required()
        .text("Edges output file.")
        .action { (v, c) => c.copy(fEdgesOut = v) }

      opt[Int]("min-period")
        .text(s"Minimum period length in sample frames, >= 2 (default: ${default.minPeriod})")
        .validate { v => if (v >= 2) success else failure("Must be >= 2") }
        .action { (v, c) => c.copy(minPeriod = v) }

      opt[Int]("max-period")
        .text(s"Maximum period length in sample frames (default: ${default.maxPeriod})")
        .validate { v => if (v >= 2) success else failure("Must be >= 2") }
        .action { (v, c) => c.copy(maxPeriod = v) }

      opt[Int]("period-step")
        .text(s"Period step in sample frames (default: ${default.periodStep})")
        .validate { v => if (v >= 1) success else failure("Must be >= 1") }
        .action { (v, c) => c.copy(periodStep = v) }

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
      val afIn = AudioFile.openRead(fSoundIn)
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

    val periods     = (minPeriod to maxPeriod by periodStep).toArray
    val numFreq     = periods.length
    val phaseRad    = phase * math.Pi * 2
    val waveTables: Array[Array[Double]] = Array.tabulate(numFreq) { i =>
      val p = periods(i) //  i + minPeriod
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

    @inline
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

//    var nextProg  = 100000
    val numFrames = bufIn.length
    val numEdgesEst = numFrames * numFreq
    var lastProg = 0
    println(s"numFrames = $numFrames; numFreq = $numFreq; estimated num edges = $numEdgesEst")
    println("_" * 100)

    def loop(start: Int): Unit = {
//      val prog = start * 100 / numFrames
//      while (lastProg < prog) {
//        print('#')
//        lastProg += 1
//      }
//      val maxPeriodC    = math.min(numFrames - start, maxPeriod)
//      val maxPeriodIdx  = maxPeriodC - minPeriod
      var pi = 0
      while (pi < numFreq) {
        val p     = periods(pi)
        val stop  = start + p
        if (stop >= numFrames) {
          pi = numFreq  // "break"
        } else {
          val edge = start.toLong << 32 | pi
          if (!edges.contains(edge)) {
            val cost = calcRMS(start, pi)
            edges.put(edge, cost)
            val prog = edges.size * 100 / numEdgesEst
            while (lastProg < prog) {
              print('#')
              lastProg += 1
            }
            loop(start + pi + minPeriod)
          }
          pi += 1
        }
      }
    }

    require (!fEdgesOut.exists(), s"Not overwriting existing file $fEdgesOut")
    require (fEdgesOut.createNewFile(), s"Cannot create output file $fEdgesOut")

    val t0 = System.currentTimeMillis()
    loop(0)
    val t1 = System.currentTimeMillis()

    println(s"\nDone (${edges.size} edges, took ${(t1-t0)/1000} sec). Writing edges...")

    {
      val fos = new FileOutputStream(fEdgesOut)
      try {
        val dos = new DataOutputStream(fos)
        dos.writeInt(COOKIE)
        dos.writeInt(FILE_VERSION)
        dos.writeInt(edges.size)
        edges.foreach { case (key, value) =>
          dos.writeLong   (key)
          dos.writeDouble (value)
        }

      } finally {
        fos.close()
      }
    }

    println("Done.")
  }
}
