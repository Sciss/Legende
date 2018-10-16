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

object Smudge {
  final case class Config(
                         fIn            : File    = file("in.aif"),
                         minPeriod      : Int     = 2,
                         maxPeriod      : Int     = 1500,
                         waveform       : Int     = 0,
                         octaveCost     : Double  = 1.0
                         )

  def main(args: Array[String]): Unit = {
    val default = Config()

    val p = new scopt.OptionParser[Config]("Preparation") {
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

  }
}
