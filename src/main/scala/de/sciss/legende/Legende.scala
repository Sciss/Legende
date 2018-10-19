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

package de.sciss.legende

import java.io.{BufferedInputStream, BufferedOutputStream, DataInputStream, DataOutputStream, FileInputStream, FileOutputStream}

import de.sciss.dijkstra.{GraphCase, ShortestRoute}
import de.sciss.file._
import de.sciss.kollflitz.Ops._
import de.sciss.numbers.Implicits._
import de.sciss.synth.io.{AudioFile, AudioFileSpec}

/*

  N.B. needs massive stack memory, e.g.

  -Xss32m

 */
object Legende {
  final val EDGE_COOKIE   = 0x536D7564  // "Smud"
  final val ROUTE_COOKIE  = 0x526F7574  // "Rout"
  final val FILE_VERSION  = 0

  final case class Config(
                           fSoundIn0    : File    = file("in.aif"),
                           fEdgesTemp   : File    = file("edges-%d.bin"),
                           fRouteTemp   : File    = file("route-%d.bin"),
                           fSegModTemp  : File    = file("seg-mod-%d.aif"),
                           fSumTemp     : File    = file("sum-%d.aif"),
                           fDifTemp     : File    = file("dif-%d.aif"),
                           minPeriodE   : Int     = 2,
                           minPeriodO   : Int     = 3,
                           maxPeriod    : Int     = 1024,
                           periodStep   : Int     = 2,
                           // maxPeriodJump: Double  = 2.0,
                           startFrame   : Int     = 0,
                           numFrames0   : Int     = -1,
                           waveform     : Int     = 0,
                           waveformAmp0 : Double  = 1.0,
                           waveformDamp : Double  = 0.9,
                           octaveCost   : Double  = 1.0,
                           phase        : Double  = 0.25,
                           inGain0      : Double  = 2.0,
                           iterations   : Int     = 1000,
                         )

  final class Node(override val id: Int)
    extends de.sciss.dijkstra.Node[Int](id, 0.0, 0.0)

  def any2stringadd: Any = ()

  def formatTemplate(f: File, args: Any*): File = {
    val name = f.name.format(args: _*)
    f.replaceName(name)
  }

  def main(args: Array[String]): Unit = {
    val default = Config()

    val p = new scopt.OptionParser[Config]("Légende") {
      opt[File]('i', "input")
        .required()
        .text("(First) input audio file.")
        .action { (v, c) => c.copy(fSoundIn0 = v) }

      opt[File]('e', "edges-output")
        .required()
        .text("Edges output file template, use %d as placeholder for iteration.")
        .action { (v, c) => c.copy(fEdgesTemp = v) }

      opt[File]('r', "route-output")
        .required()
        .text("Route output file template, use %d as placeholder for iteration.")
        .action { (v, c) => c.copy(fRouteTemp = v) }

      opt[File]('m', "seg-mod-output")
        .required()
        .text("Seg-mod output file template, use %d as placeholder for iteration.")
        .action { (v, c) => c.copy(fSegModTemp = v) }

//      opt[File]('u', "sum-output")
//        .required()
//        .text("Summation output file template, use %d as placeholder for iteration")
//        .action { (v, c) => c.copy(fSumTemp = v) }

      opt[File]('d', "diff-output")
        .required()
        .text("Difference output file template, use %d as placeholder for iteration")
        .action { (v, c) => c.copy(fDifTemp = v) }

      opt[Int]('t', "iterations")
        .text(s"Number of iterations to run (default: ${default.iterations})")
        .validate { v => if (v >= 1) success else failure("Must be >= 1") }
        .action { (v, c) => c.copy(iterations = v) }

      opt[Int]("min-period-even")
        .text(s"Minimum period length for even iterations in sample frames, >= 2 (default: ${default.minPeriodE})")
        .validate { v => if (v >= 2) success else failure("Must be >= 2") }
        .action { (v, c) => c.copy(minPeriodE = v) }

      opt[Int]("min-period-odd")
        .text(s"Minimum period length for odd iterations in sample frames, >= 2 (default: ${default.minPeriodO})")
        .validate { v => if (v >= 2) success else failure("Must be >= 2") }
        .action { (v, c) => c.copy(minPeriodO = v) }

      opt[Int]("max-period")
        .text(s"Maximum period length in sample frames (default: ${default.maxPeriod})")
        .validate { v => if (v >= 2) success else failure("Must be >= 2") }
        .action { (v, c) => c.copy(maxPeriod = v) }

      opt[Int]("period-step")
        .text(s"Period step in sample frames (default: ${default.periodStep})")
        .validate { v => if (v >= 1) success else failure("Must be >= 1") }
        .action { (v, c) => c.copy(periodStep = v) }

      opt[Int]('s', "start-frame")
        .text(s"Start frame in input sound file (default: ${default.startFrame})")
        .validate { v => if (v >= 0) success else failure("Must be >= 0") }
        .action { (v, c) => c.copy(startFrame = v) }

      opt[Int]('n', "num-frames")
        .text(s"Number of frames in input sound file, or -1 for all (default: ${default.numFrames0})")
        .action { (v, c) => c.copy(numFrames0 = v) }

      opt[Int]('w', "waveform")
        .text(s"Waveform type: 0 - sine, 1 - tri, 2 - pulse, 3 - saw (default: ${default.waveform})")
        .validate { v => if (v >= 0 && v <= 3) success else failure("Must be >= 0 and <= 3") }
        .action { (v, c) => c.copy(waveform = v) }

      opt[Double]('c', "octave-cost")
        .text(s"Octave cost factor, >1 prefers lower frequencies (default: ${default.octaveCost})")
        .validate { v => if (v >= 0) success else failure("Must be >= 0") }
        .action { (v, c) => c.copy(octaveCost = v) }

      opt[Double]("waveform-amp")
        .text(s"Initial waveform amplitude (default: ${default.waveformAmp0})")
        .validate { v => if (v > 0) success else failure("Must be > 0") }
        .action { (v, c) => c.copy(waveformAmp0 = v) }

      opt[Double]("waveform-damp")
        .text(s"Waveform amplitude damping per iteration (default: ${default.waveformDamp})")
//        .validate { v => if (v <= 1) success else failure("Must be <= 1") }
        .action { (v, c) => c.copy(waveformDamp = v) }

      opt[Double]('p', "phase")
        .text(s"Phase offset of waveforms in periods, 0.0, 0.25, 0.5, 0.75 (default: ${default.phase})")
        .validate { v => if (List(0.0, 0.25, 0.5, 0.75) contains v) success else failure("Must be one of 0.25, 0.5, 0.75") }
        .action { (v, c) => c.copy(phase = v) }

      opt[Double]('g', "gain")
        .text(s"Input gain factor (default: ${default.inGain0})")
        .action { (v, c) => c.copy(inGain0 = v) }

      //      opt[Unit]("backup")
//        .text("Backup workspace before modification")
//        .action { (_, c) => c.copy(backupWorkspace = true) }

    }
    p.parse(args, default).fold(sys.exit(1)) { implicit config =>
      require (config.minPeriodE <= config.maxPeriod)
      require (config.minPeriodO <= config.maxPeriod)
      run()
    }
  }

  def shouldWrite(f: File): Boolean =
    !f.exists() || (f.isFile && f.length() == 0L)

  def run()(implicit config: Config): Unit = {
    import config._

    for (iter <- 1 to iterations) {
      println(s"\n---- ITERATION $iter ---- ${new java.util.Date}\n")

      val fEdges      = formatTemplate(fEdgesTemp , iter)
      val fSoundIn    = if (iter == 1) fSoundIn0 else formatTemplate(fDifTemp, iter - 1)
      val fRoute      = formatTemplate(fRouteTemp , iter)
      val fSegMod     = formatTemplate(fSegModTemp, iter)
      val fDif        = formatTemplate(fDifTemp   , iter)
      val inGain      = if (iter == 1) inGain0 else 1.0
      val minPeriod   = if (iter % 2 == 0) minPeriodE else minPeriodO
      val waveformAmp = waveformAmp0 * waveformDamp.pow(iter - 1)

      if (shouldWrite(fEdges)) {
        fEdges.createNewFile()
        mkEdgesBin(fSoundIn = fSoundIn, inGain = inGain, minPeriod = minPeriod, waveformAmp = waveformAmp,
          fEdges = fEdges)

      } else {
        println(s"Edges binary file $fEdgesTemp already exists.")
      }

      if (shouldWrite(fRoute)) {
        fRoute.createNewFile()
        mkDijkstra(fEdges = fEdges, fRoute = fRoute, minPeriod = minPeriod)

      } else {
        println(s"Route file $fRoute already exists.")
      }

      //    printDijkstra(config)

      if (shouldWrite(fSegMod) || shouldWrite(fDif)) {
        fSegMod .createNewFile()
        fDif    .createNewFile()
        mkSegMod(fSoundIn = fSoundIn, inGain = inGain, minPeriod = minPeriod, waveformAmp = waveformAmp,
          fRoute = fRoute, fSegMod = fSegMod, fDif = fDif)

      } else {
        println(s"SegMod file $fSegMod and sum file $fDif already exist.")
      }
    }
  }

  private def readDijkstra(fRoute: File)(implicit config: Config): Array[Int] = {
    println(s"Reading route...")

    val fis = new FileInputStream(fRoute)
    try {
      val dis = new DataInputStream(fis)
      val cookie = dis.readInt()
      require (cookie == ROUTE_COOKIE, cookie.toString)
      val fileVersion = dis.readInt()
      require (fileVersion == FILE_VERSION, fileVersion.toString)
      val routeSz = dis.readInt()
      Array.fill(routeSz)(dis.readInt())

    } finally {
      fis.close()
    }
  }

  def printDijkstra(fRoute: File)(implicit config: Config): Unit = {
    val route = readDijkstra(fRoute = fRoute)
    println(route.mkString("Vector(", ", ", ")"))
  }

//  def mkSegModOLD(fSoundIn: File, inGain: Double, fRoute: File, fSegMod: File, fDif: File)
//              (implicit config: Config): Future[Unit] = {
//    import config._
//    var route0      = readDijkstra(fRoute = fRoute)
//    println(s"Route.length = ${route0.length}")
//    if (route0.length < 100) println(route0.mkString("Vector(", ", ", ")"))
//    val specIn      = AudioFile.readSpec(fSoundIn)
//    val numFramesIn = specIn.numFrames.toInt
//    while (route0.last < numFramesIn) {
//      val lastPeriod = route0(route0.length - 1) - route0(route0.length - 2)
//      route0 :+= route0.last + lastPeriod
//    }
//    val route     = route0
//    import specIn.sampleRate
//    val g = Graph {
//      import graph._
//      val periods = route.toVector.differentiate
//      println("PERIODS:")
//      println(periods.take(50))
//      val freqN   = ValueDoubleSeq(periods.map(1.0 / _): _*)
//      val sh      = SegModPhasor(freqN, phase = phase).take(numFramesIn) // 0.25
//      val sigDir  = (sh /* + phase */ * (2 * math.Pi)).sin  // sine
//      val sigIn   = AudioFileIn(fSoundIn, numChannels = 1) * inGain
//      val sigDif  = sigIn - sigDir
//      val error   = RunningSum(sigDif.squared).last
//      error.poll(0, "error")
//      //    val sig     = (sh * -4 + 2).fold(-1, 1) // triangle
//      //    val sig     = (sh < 0.5) * 2 - 1 // pulse
//      //    val sig     = sh * 2 - 1 // sawtooth (1)
//      //    val sig     = ((sh + 0.25) % 1.0) * 2 - 1 // sawtooth (2)
//      //    val sig     = ((sh + 0.5) % 1.0) * 2 - 1 // sawtooth (3)
//      //    val sig     = sh * DC(0.0) // silence
//      val specOut     = AudioFileSpec(numChannels = 1, sampleRate = sampleRate)
//      val framesDir   = AudioFileOut(sigDir, fSegMod, specOut)
//      val framesDif   = AudioFileOut(sigDif, fDif   , specOut)
//      Progress(framesDir / numFramesIn, Metro(sampleRate))
//      Progress(framesDif / numFramesIn, Metro(sampleRate))
//    }
//
//    val cfg = stream.Control.Config()
//    cfg.useAsync = false
//    val ctl = stream.Control(cfg)
//    println("Rendering seg-mod...")
//    ctl.run(g)
//    ctl.status
//  }

  def mkSegMod(fSoundIn: File, inGain: Double, minPeriod: Int, waveformAmp: Double,
               fRoute: File, fSegMod: File, fDif: File)
              (implicit config: Config): Unit = {
    import config._
    var route0      = readDijkstra(fRoute = fRoute)
    println(s"Route.length = ${route0.length}")
    if (route0.length < 100) println(route0.mkString("Vector(", ", ", ")"))
    val specIn      = AudioFile.readSpec(fSoundIn)
    val numFramesIn = specIn.numFrames.toInt
    while (route0.last < numFramesIn) {
      val lastPeriod = route0(route0.length - 1) - route0(route0.length - 2)
      route0 :+= route0.last + lastPeriod
    }
    val route     = route0
    import specIn.sampleRate

    val waveTables: Array[Array[Double]] = mkWaveTables(minPeriod = minPeriod, amp = waveformAmp)

    val periods     = route.toVector.differentiate
    println("PERIODS:")
    println(periods.take(50))

    val specOut     = AudioFileSpec(numChannels = 1, sampleRate = sampleRate)
    val sigIn       = readAudioFile(fSoundIn, startFrame = startFrame, numFrames0 = numFrames0, inGain = inGain)
    val numFrames   = sigIn.length
    val sigDir      = new Array[Float](numFrames)
    val sigDif      = new Array[Float](numFrames)
    var i = 0
    var start = 0
    var error = 0.0
    while (i < periods.length) {
      val p0    = periods(i)
      val pi    = (p0 - minPeriod) / periodStep
      val table = waveTables(pi)
      assert (table.length == p0)
      val p     = p0 // if (start > 0) p0 else (p0 * (1.0 - phase) + 0.5).toInt
      val stop  = math.min(numFrames, start + p)
      var k     = 0 // NOT: wrong for start == 0 and phase != 0.0 (because we don't use fractional cycle in the beginning)
      while (start < stop) {
        val x = table(k)
        sigDir(start) = x.toFloat
        val y = sigIn(start) - x
        sigDif(start) = y.toFloat
        error += y * y
        k     += 1
        start += 1
      }
      i += 1
    }
    val afSegMod = AudioFile.openWrite(fSegMod, specOut)
    try {
      afSegMod.write(Array(sigDir))
    } finally {
      afSegMod.close()
    }

    val afDif = AudioFile.openWrite(fDif, specOut)
    try {
      afDif.write(Array(sigDif))
    } finally {
      afDif.close()
    }

    println(s"error: $error")
  }

  def mkDijkstra(fEdges: File, fRoute: File, minPeriod: Int)(implicit config: Config): Unit = {
    import config._

    println(s"Reading edges...")

    val fis = new FileInputStream(fEdges)
    try {
      val dis = new DataInputStream(new BufferedInputStream(fis))
      val cookie      = dis.readInt()
      require (cookie == EDGE_COOKIE, cookie.toString)
      val fileVersion = dis.readInt()
      require (fileVersion == FILE_VERSION, fileVersion.toString)
      /* val numFrames = */ dis.readInt()
      val numFreq     = dis.readInt()
      val edgeCount   = dis.readInt()

      val periods     = (minPeriod to maxPeriod by periodStep).toArray
      require (periods.length == numFreq)

      var _net: Map[Int, Map[Int, Double]] = Map.empty

      var lastProg = 0
      println(s"edgeCount = $edgeCount")
      println("_" * 100)

      for (ei <- 1 to edgeCount) {
        val edge    = dis.readInt()
        val pi      = edge % numFreq
        val period  = periods(pi)
        val start   = edge / numFreq
        val value   = dis.readDouble()
        val stop    = start + period

        val map0    = _net.getOrElse(start, Map.empty)
        val map1    = map0 + (stop -> value)
        _net += start -> map1
        _net.get(stop) match {
          case None => _net += stop -> Map.empty
          case _ =>
        }

        val prog = ei * 100L / edgeCount
        while (lastProg < prog) {
          print('#')
          lastProg += 1
        }
      }

      val nodes = _net.keysIterator.map(id => id -> new Node(id)).toMap
      require (_net.contains(0))
      val maxNode = _net.keysIterator.max
      // require (maxNode == numFrames, s"$maxNode != $numFrames")

      println(s"\nRunning Dijkstra...")

      val g = new de.sciss.dijkstra.Graph[Int](nodes, Nil) {
        override lazy val net: Map[Int, Map[Int, Double]] = _net
      }

      val t0  = System.currentTimeMillis()
//      val res: GraphCase[Int] = g.shortestPath(0, maxNode /* numFrames */)
      val res: GraphCase[Int] = Dijkstra.shortestPath(g, 0, maxNode /* numFrames */)
      val t1  = System.currentTimeMillis()

      println(s"\nTook ${(t1-t0)/1000} sec.")

      res match {
        case ShortestRoute(route, dist) =>
          val routeSz = route.size
          println(s"Route of size $routeSz and distance $dist")

          {
            val fos = new FileOutputStream(fRoute)
            try {
              val dos = new DataOutputStream(new BufferedOutputStream(fos))
              dos.writeInt(ROUTE_COOKIE)
              dos.writeInt(FILE_VERSION)
              dos.writeInt(routeSz)
              route.foreach { frame =>
                dos.writeInt(frame)
              }
              dos.flush()

            } finally {
              fos.close()
            }
          }

        case other =>
          println(s"FAILED: $other")
      }

    } finally {
      fis.close()
    }
  }

  def mkOctaveCosts(minPeriod: Int)(implicit config: Config): Array[Double] = {
    import config._
    val periods     = (minPeriod to maxPeriod by periodStep).toArray
    val numFreq     = periods.length
    if (octaveCost == 1.0) Array.fill(numFreq)(1.0)
    else {
      val maxPeriodD = maxPeriod.toDouble
      Array.tabulate(numFreq) { i =>
        val p = periods(i)
        val octaves = (maxPeriodD / p).log2
        octaveCost.pow(octaves)
      }
    }
  }

  def mkWaveTables(minPeriod: Int, amp: Double)(implicit config: Config): Array[Array[Double]] = {
    import config._
    val periods     = (minPeriod to maxPeriod by periodStep).toArray
    val numFreq     = periods.length
    Array.tabulate(numFreq) { i =>
      val p = periods(i) //  i + minPeriod
      waveform match {
        case 0 => // sine
          val t         = math.Pi * 2 / p
          val phaseRad  = phase * math.Pi * 2
          Array.tabulate(p) { j =>
            math.sin(j * t + phaseRad) * amp
          }
        case 1 => // tri
          val t = 4.0 / p
          Array.tabulate(p) { j =>
            (j * t + phase).fold(-1.0, +1.0) * amp
          }
        case 2 => // pulse
          val t = 1.0 / p
          Array.tabulate(p) { j =>
            (if (j * t + phase < 0.5) 1.0 else 0.0) * amp
          }
        case 3 => // saw
          val t = 1.0 / p
          Array.tabulate(p) { j =>
            (((j * t + phase) % 1.0) * 2.0 - 1.0) * amp
          }
      }
    }
  }

  def readAudioFile(f: File, startFrame: Int, numFrames0: Int, inGain: Double): Array[Float] = {
    val afIn = AudioFile.openRead(f)
    try {
      val st = math.min(afIn.numFrames, startFrame).toInt
      afIn.seek(st)
      val n0  = afIn.numFrames.toInt - st
      val n   = if (numFrames0 < 0) n0 else math.min(n0, numFrames0)
      val b   = afIn.buffer(n)
      afIn.read(b)
      val res = b(0)
      if (inGain != 1.0) {
        val f = inGain.toFloat
        for (i <- res.indices) res(i) *= f
      }
      res
    } finally {
      afIn.close()
    }
  }

  def mkEdgesBin(fSoundIn: File, inGain: Double, minPeriod: Int, waveformAmp: Double, fEdges: File)
                (implicit config: Config): Unit = {
    import config._
    val bufIn       = readAudioFile(fSoundIn, startFrame = startFrame, numFrames0 = numFrames0, inGain = inGain)
    val periods     = (minPeriod to maxPeriod by periodStep).toArray
    val numFreq     = periods.length
    val waveTables  : Array[Array[Double]]  = mkWaveTables(minPeriod = minPeriod, amp = waveformAmp)
    val octaveCosts : Array[Double]         = mkOctaveCosts(minPeriod = minPeriod)

    @inline
    def calcCost(start: Int, tableStart: Int, tableStop: Int, periodIdx: Int): Double = {
      val table = waveTables(periodIdx)
      var i     = tableStart
      var j     = start
      var sum   = 0.0
      while (i < tableStop) {
        val a = bufIn(j)
        val b = table(i)
        val d = a - b
        sum += d*d
        i += 1
        j += 1
      }
      sum * octaveCosts(periodIdx)
    }

    val numFrames   = bufIn.length
    val numEdgesEst = numFrames * numFreq // / 2
    var lastProg = 0
    println(s"numFrames = $numFrames; numFreq = $numFreq; estimated num edges = $numEdgesEst")
    println("_" * 100)

    //    val edges = mutable.LongMap.empty[Double]
    val edges = Array.fill(numFrames * numFreq)(-1.0) // mutable.LongMap.empty[Double]
    var edgeCount = 0

    def loop(start: Int): Unit = {
      var pi = 0
      while (pi < numFreq) {
        val p0          = periods(pi)
        val tableStart  = 0 // if (start > 0) 0 else ((1.0 - phase) + 0.5).toInt
        val p           = p0 - tableStart
        val stop        = start + p
        if (stop >= numFrames) {
          pi = numFreq  // "break"
        } else {
          // if (start > 0 || phase == 0.0 || (p0 % 4) == 0) {  // ensure we can reduce the cycle
            val edge = start * numFreq + pi
            val containsNot = edges(edge) < 0.0
            if (containsNot) {
              val cost = calcCost(start = start, periodIdx = pi, tableStart = tableStart, tableStop = p0)
              edges(edge) = cost
              edgeCount += 1
              val prog = edgeCount * 100L / numEdgesEst
              while (lastProg < prog) {
                print('#')
                lastProg += 1
              }
              loop(stop)
            }
          // }
          pi += 1
        }
      }
    }

    val t0 = System.currentTimeMillis()
    loop(0)
    val t1 = System.currentTimeMillis()

    println(s"\nDone ($edgeCount edges, took ${(t1-t0)/1000} sec). Writing edges...")

    {
      val fos = new FileOutputStream(fEdges)
      try {
        val dos = new DataOutputStream(new BufferedOutputStream(fos))
        dos.writeInt(EDGE_COOKIE)
        dos.writeInt(FILE_VERSION)
        dos.writeInt(numFrames)
        dos.writeInt(numFreq)
        dos.writeInt(edgeCount)
        for (edge <- edges.indices) {
          val value = edges(edge)
          if (value >= 0) {
//            dos.writeLong   (key)
            dos.writeInt(edge)
            dos.writeDouble(value)
          }
        }
        dos.flush()

      } finally {
        fos.close()
      }
    }

    println("Done binary edges.")
  }
}
