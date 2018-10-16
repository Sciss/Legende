/*
 *  DijkstraTest.scala
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

object DijkstraTest {
  final class Node(override val id: Char)
    extends de.sciss.dijkstra.Node[Char](id, 0.0, 0.0)

  def main(args: Array[String]): Unit = {
    // using this as example: https://stackoverflow.com/questions/13249057/dijkstra-find-shortest-path-in-directed-graph

    val graph0 = Map[Char, Set[(Int, Char)]](
      'A' -> Set(1 -> 'C'),
      'B' -> Set(3 -> 'S', 4 -> 'D'),
      'C' -> Set(3 -> 'D', 1 -> 'E'),
      'D' -> Set(1 -> 'E', 5 -> 'F', 3 -> 'T'),
      'E' -> Set(2 -> 'G', 4 -> 'T'),
      'F' -> Set(),
      'G' -> Set(2 -> 'E', 3 -> 'T'),
      'S' -> Set(4 -> 'A', 3 -> 'B', 7 -> 'D'),
      'T' -> Set()
    )

    val nodes = graph0.keysIterator.map(id => id -> new Node(id)).toMap

    val g = new de.sciss.dijkstra.Graph[Char](nodes, Nil) {
      override lazy val net: Map[Char, Map[Char, Double]] = {
        graph0.map {
          case (id, targets) =>
            val targetMap = targets.iterator.map {
              case (cost, targetId) =>
                targetId -> cost.toDouble
            } .toMap
            id -> targetMap
        }
      }
    }

    println(g.net)

    val res = g.shortestPath('S', 'T')
    println(res)
  }
}
