/*
 *  ResamplePaths.scala
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

import java.io.{BufferedOutputStream, DataInputStream, DataOutputStream, FileInputStream, FileOutputStream}

import de.sciss.file._
import de.sciss.fscape.{GE, Graph, graph, stream}
import de.sciss.kollflitz.Ops._
import de.sciss.legende.Prepare.{readDijkstra, shouldWrite, writeDijkstra}
import de.sciss.numbers.Implicits._
import de.sciss.synth.io.{AudioFile, AudioFileSpec}

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration

object ResamplePaths {
  final case class Config(stage         : Int     = 3,
                          iterations    : Int     = 40,
                          waveformAmp0  : Double  = 1.0,
                          waveformDamp  : Double  = 0.9,
                          routeSizeOut  : Int     = 20,
                         )

  def main(args: Array[String]): Unit = {
    // ---- make sure merged routes exist ----

    implicit val c: Config = Config()
    import c._
    val baseDir   = file("/data/projects/Segmod")
    val dataDir   = baseDir / "data"
    val audioDir  = baseDir / "audio_work"
    val fSoundIn  = audioDir / s"AlphavilleTropComplexe-$stage.aif"
    val fSoundInA = audioDir / s"AlphavilleTropComplexe-${stage}a.aif"
    val fSoundInB = audioDir / s"AlphavilleTropComplexe-${stage}b.aif"

    for (iter <- 1 to iterations) {
      val fRouteA     = dataDir / s"alphaville-${stage}a-route-$iter.bin"
      val fRouteB     = dataDir / s"alphaville-${stage}b-route-$iter.bin"
      val fRouteOut   = dataDir / s"alphaville-$stage-route-$iter.bin"
      val fRouteRsmp  = dataDir / s"alphaville-$stage-routeR-$iter.bin"

      if (shouldWrite(fRouteOut)) {
        mergeRoutes(fSoundIn = fSoundIn, fSoundInA = fSoundInA, fSoundInB = fSoundInB, fRouteA = fRouteA, fRouteB = fRouteB,
          fRouteOut = fRouteOut)
      }

      if (shouldWrite(fRouteRsmp)) {
        mkResampleRoute(fRouteIn = fRouteOut, fRouteOut = fRouteRsmp, sizeOutFactor = routeSizeOut)
      }

      val fSegMod     = dataDir / s"alphaville-$stage-segmodRsmp-$iter.aif"
      val fSegModRvs  = dataDir / s"alphaville-$stage-segmodRsmpRvs-$iter.aif"
      val waveformAmp = waveformAmp0 * waveformDamp.pow(iter - 1)

      if (shouldWrite(fSegModRvs)) {
        val fut = mkSegMod(fSoundIn = fSoundIn, fRoute = fRouteRsmp, fSegModRvs = fSegModRvs, outGain = waveformAmp)
        Await.result(fut, Duration.Inf)
      }

      if (shouldWrite(fSegMod)) {
        reverse(fIn = fSegModRvs, fOut = fSegMod)
      }
    }
  }

  def any2stringadd: Any = ()

  def reverse(fIn: File, fOut: File): Unit = {
    val afIn = AudioFile.openRead(fIn)
    try {
      val afOut = AudioFile.openWrite(fOut, afIn.spec)
      try {
        val n = afIn.numFrames.toInt
        val bIn = afIn.buffer(n)
        afIn.read(bIn)
        val bOut = bIn.map(_.reverse)
        afOut.write(bOut)
      } finally {
        afOut.close()
      }
    } finally {
      afIn.close()
    }
  }

  def mergeRoutes(fSoundIn: File, fSoundInA: File, fSoundInB: File, fRouteA: File, fRouteB: File, fRouteOut: File)
              (implicit config: Config): Unit = {
    val routeA0   = readDijkstra(fRoute = fRouteA)
    val routeB0   = readDijkstra(fRoute = fRouteB)
    val specIn    = AudioFile.readSpec(fSoundIn)
    val specInA   = AudioFile.readSpec(fSoundInA)
    val specInB   = AudioFile.readSpec(fSoundInB)
    val numInF    = specIn .numFrames.toInt
    val numInFA   = specInA.numFrames.toInt
    val numInFB   = specInB.numFrames.toInt
    val overlap   = (numInFB + numInFA) - numInF
    require (overlap >= 0)
    val overlapA  = overlap / 2
    val overlapB  = overlap - overlapA

    def forceRoute(route0: Array[Int], numF: Int): Array[Int] = {
      var res = route0
      while (res.last > numF) {
        res = res.init
      }
      while (res.last < numF) {
        val lastPeriod = math.min(numF - res.last, res(res.length - 1) - res(res.length - 2))
        res :+= res.last + lastPeriod
      }
      res
    }

    val routeA    = forceRoute(routeA0, numInFA - overlapA)
    val routeB1   = forceRoute(routeB0, numInFB - overlapB)
    assert(routeB1.head == 0)
    val routeB    = routeB1.tail.map(_ + (numInFA - overlapA))
    val routeOut  = routeA ++ routeB
    assert(routeOut.toVector.differentiate.forall(_ > 0))
    assert(routeOut.last == numInF)
    writeDijkstra(fRouteOut, routeOut)
  }

  def mkResampleRoute(fRouteIn: File, fRouteOut: File, sizeOutFactor: Int, repeats: Int = 4, minFactor: Double = 0.25): Unit = {
    val routeIn     = readDijkstra(fRoute = fRouteIn)
    val periodsIn   = routeIn.toVector.differentiate
    val periodsDup  = (0 until repeats).flatMap(_ => periodsIn)
    val sizeOut     = periodsIn.size * sizeOutFactor
    require (periodsDup.size <= sizeOut)
    val periodsOut  = Vector.tabulate(sizeOut) { i =>
      val phase = i.linLin(0, sizeOut, 0.0, math.Pi / 2)
      val pos   = (1.0 - math.cos(phase)).pow(2.0).linLin(0.0, 1.0, 0, periodsDup.size)
      val posI  = pos.toInt
      val posJ  = (posI + 1) % periodsDup.size
      val wJ    = pos % 1.0
      val wI    = 1.0 - wJ
      periodsDup(posI) * wI + periodsDup(posJ) * wJ
    }
    val routeOut    = (0.0 +: periodsOut.integrate).toArray
    writeDijkstraF(fRouteOut, routeOut)
  }

  final val ROUTE_COOKIE_F  = 0x526F7566  // "Rouf"
  final val FILE_VERSION    = 0

  def writeDijkstraF(fRoute: File, route: Seq[Double]): Unit = {
    val fos = new FileOutputStream(fRoute)
    try {
      val dos = new DataOutputStream(new BufferedOutputStream(fos))
      dos.writeInt(ROUTE_COOKIE_F)
      dos.writeInt(FILE_VERSION)
      dos.writeInt(route.size)
      route.foreach { frame =>
        dos.writeDouble(frame)
      }
      dos.flush()

    } finally {
      fos.close()
    }
  }

  def readDijkstraF(fRoute: File): Array[Double] = {
    val fis = new FileInputStream(fRoute)
    try {
      val dis = new DataInputStream(fis)
      val cookie = dis.readInt()
      require (cookie == ROUTE_COOKIE_F, cookie.toString)
      val fileVersion = dis.readInt()
      require (fileVersion == FILE_VERSION, fileVersion.toString)
      val routeSz = dis.readInt()
      Array.fill(routeSz)(dis.readDouble())

    } finally {
      fis.close()
    }
  }

  def mkSegMod(fSoundIn: File, outGain: Double, fRoute: File, fSegModRvs: File)
              (implicit config: Config): Future[Unit] = {
    val route0  = readDijkstraF(fRoute = fRoute)
//    println(s"Route.length = ${route0.length}")
//    if (route0.length < 100) println(route0.mkString("Vector(", ", ", ")"))
    val specIn      = AudioFile.readSpec(fSoundIn)
//    val numFramesIn = specIn.numFrames.toInt
    val route: Array[Double] = route0

    import specIn.sampleRate
    val g = Graph {
      import graph._
      val periods = route.toVector.differentiate.reverse
//      val periodsSz = periods.size
//      println("PERIODS:")
//      println(periods.take(50))
      def freqN0    = ValueDoubleSeq(periods.map(1.0 / _): _*)
//      val minFactor = 1.0 / 32
//      val lineLen   = periodsSz * 10.0 // 2.0 // 1.9393   // Huh?
//      val factor    = DC(1.0).take(periodsSz) ++ Line(0.0, 1.0, length = lineLen).linLin(0.0, 1.0, 1.0, minFactor).pow(10)
      val freqN     = freqN0

      val phase0  = 0.25: GE
      val sh      = SegModPhasor(freqN, phase = 0.0 /* phase */) // .take(numFramesIn) // 0.25
      val sigDir  = ((sh + phase0) * (2 * math.Pi)).sin * outGain // sine
      //    val sig     = (sh * -4 + 2).fold(-1, 1) // triangle
      //    val sig     = (sh < 0.5) * 2 - 1 // pulse
      //    val sig     = sh * 2 - 1 // sawtooth (1)
      //    val sig     = ((sh + 0.25) % 1.0) * 2 - 1 // sawtooth (2)
      //    val sig     = ((sh + 0.5) % 1.0) * 2 - 1 // sawtooth (3)
      //    val sig     = sh * DC(0.0) // silence
      val specOut     = AudioFileSpec(numChannels = 1, sampleRate = sampleRate)
      /* val framesDir   = */ AudioFileOut(sigDir, fSegModRvs, specOut)
//      Progress(framesDir / numFramesIn, Metro(sampleRate))
    }

    val cfg = stream.Control.Config()
    cfg.useAsync = false
    val ctl = stream.Control(cfg)
    println("Rendering seg-mod...")
    ctl.run(g)
    ctl.status
  }

  def run()(implicit config: Config): Unit = {

  }
}
