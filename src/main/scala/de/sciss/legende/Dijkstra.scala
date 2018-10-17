/*
 *  Dijkstra.scala
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

package de.sciss.legende

import de.sciss.dijkstra.{Graph, GraphCase, ShortestRoute, ShortestRouteDoesNotExist, ShortestRouteInvalidSourceOrTarget}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Dijkstra {
  // XXX TODO --- doesn't seem to use a PQ, so it's unusably slow
  def shortestPath[A](graph: Graph[A], source: A, target: A): GraphCase[A] = {
    import graph._

    def minDistanceId(dist: mutable.HashMap[A, Double], work: mutable.Set[A] /* ListBuffer[A] */): Option[A] = {
      var min: Double     = Graph.INFINITE
      var mid: Option[A]  = None
      dist.foreach { case (id, d) =>
        if (d < min && work.contains(id)) {
          min = d
          mid = Some(id)
        }
      }
      mid
    }

//    def neighborsF(nid: A): List[A] =
//      net.getOrElse(nid, Map.empty).keysIterator.toList

    def distances(src: A, dest: A): Option[Double] =
      net.get(src).flatMap(_.get(dest))

    var lastProg = 0

    if (source == target) {
      ShortestRoute(source :: Nil, 0.0)
    } else if (!nodes.contains(source) || !nodes.contains(target)) {
      ShortestRouteInvalidSourceOrTarget()
    } else {
      val distance = mutable.HashMap.empty[A, Double]
      val previous = mutable.HashMap.empty[A, A]
      val working = mutable.Set.empty[A] // ListBuffer[A]()
      val workingSz = nodes.size
      var workingDone = 0
      nodes.keysIterator.foreach { k =>
        distance += k -> Graph.INFINITE
        // previous -= k
        working  += k
      }
      println(s"workingSz = $workingSz, working.size = ${working.size}")
      distance += source -> 0.0
      println("_" * 100)

      var closest = Option.empty[A]
      while (working.nonEmpty) {
        minDistanceId(distance, working) match {
          case midS @ Some(mid) =>
            closest = midS
            val distMid = distance(mid)
            if (distMid == Graph.INFINITE) {
              println("no other nodes are accessible")
              closest = None
              working.clear()

            } else {
              working -= mid
              workingDone += 1
              val prog = workingDone * 100L / workingSz
              while (lastProg < prog) {
                print('#')
                lastProg += 1
              }

//              val neighM = neighborsF(mid)
              val neighM = net.getOrElse(mid, Map.empty).keysIterator
              neighM.foreach { neighbor =>
                distances(mid, neighbor) match {
                  case Some(dist) =>
                    val alternate = distMid + dist
                    if (alternate < distance(neighbor)) {
                      distance(neighbor) = alternate
                      previous(neighbor) = mid
                    }
                  case _ => println("""distance calc failed for edge %s and %s""".format(closest, neighbor))
                }
              }
            }
          case _ =>
            working.clear() // no more connected nodes to source
        }
      }
      if (closest.forall(distance(_) == Graph.INFINITE)) {
        ShortestRouteDoesNotExist()
      } else {
        val route     = new ListBuffer[A]()
        var location  = target
        while (previous.contains(location)) {
          route.insert(0, nodes(previous(location)).id)
          location = previous(location)
        }
        if (route.isEmpty) {
          ShortestRouteDoesNotExist()
        } else {
          route += target
          val routeL  = route.toList
          val tDist   = traversedDistance(graph, routeL)
          ShortestRoute(routeL, tDist)
        }
      }
    }
  }
}