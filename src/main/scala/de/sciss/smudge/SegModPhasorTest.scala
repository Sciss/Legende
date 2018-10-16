/*
 *  SegModPhasorTest.scala
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

import de.sciss.fscape.{Graph, graph, stream}

object SegModPhasorTest {
  def any2stringadd: Any = ()

  def main(args: Array[String]): Unit = {
    run()
  }

  def run(): Unit = {
    val g = Graph {
      import graph._
      val sz      = 400
      val periods = Seq(60, 120, 60, 120, 2, 2, 2, 2, 60)
      val freqN   = ValueDoubleSeq(periods.map(1.0 / _): _*)
      val sh      = SegModPhasor(freqN, 0.0) // 0.25
      val sig     = (sh * 2 * math.Pi).sin  // sine
      //    val sig     = (sh * -4 + 2).fold(-1, 1) // triangle
      //    val sig     = (sh < 0.5) * 2 - 1 // pulse
      //    val sig     = sh * 2 - 1 // sawtooth (1)
      //    val sig     = ((sh + 0.25) % 1.0) * 2 - 1 // sawtooth (2)
      //    val sig     = ((sh + 0.5) % 1.0) * 2 - 1 // sawtooth (3)
      //    val sig     = sh * DC(0.0) // silence
      Plot1D(sig, size = sz, label = "seg-mod")
    }

    stream.Control().run(g)
  }
}
