/*
 *  GraphSizeTest.scala
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

object GraphSizeTest {
  def main(args: Array[String]): Unit = {
    val minDiv  = 2
    val maxDiv  = 1500
    val sr      = 44100
    val inLen   = sr * 2    // for example

    // val nodes   = 0 until inLen
    var edges   = Map.empty[Int, Set[Int]]

    for (j <- minDiv to maxDiv) {
      for (i <- 0 until inLen by j) {
        val set0 = edges.getOrElse(i, Set.empty)
        val set1 = set0 + (i + j)
        edges += i -> set1
      }
    }

    val numEdges = edges.valuesIterator.map(_.size.toLong).sum
    println(s"numEdges = $numEdges")  // 608488
  }
}
