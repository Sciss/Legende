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

import de.sciss.equal.Implicits._
import de.sciss.file._
import de.sciss.legende.Builder._
import de.sciss.legende.Util._
import de.sciss.lucre.artifact.{Artifact, ArtifactLocation}
import de.sciss.lucre.expr.{BooleanObj, DoubleObj, IntObj, LongObj, SpanLikeObj, StringObj}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Folder
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.lucre.synth.Sys
import de.sciss.mellite.gui.TimelineObjView
import de.sciss.mellite.gui.edit.Edits
import de.sciss.span.Span
import de.sciss.synth
import de.sciss.synth.io.AudioFile
import de.sciss.synth.proc.Implicits._
import de.sciss.synth.proc.{AudioCue, Code, CurveObj, EnvSegment, FadeSpec, Grapheme, ObjKeys, Proc, SoundProcesses, TimeRef, Timeline, Workspace}
import de.sciss.synth.{Curve, SynthGraph, proc}

import scala.util.control.NonFatal

object Legende {
  final case class Config(baseDir: File = file("/data") / "projects" / "Segmod") {
    lazy val dataDir      : File = baseDir / "data"
    lazy val workspacesDir: File = baseDir / "workspaces"
    lazy val wsMllt       : File = workspacesDir / "Legende.mllt"
  }

  def main(args: Array[String]): Unit = {
    val default = Config()
    val p = new scopt.OptionParser[Config]("Légende") {
      opt[File]('b', "base")
        .text(s"Base directory (default: ${default.baseDir})")
        .action { (v, c) => c.copy(baseDir = v) }
    }
    p.parse(args, default).fold(sys.exit(1)) { implicit config =>
      run()
    }
  }

  def run()(implicit config: Config): Unit = {
    SoundProcesses.init()

    if (config.wsMllt.isDirectory) {
      val store = BerkeleyDB.factory(config.wsMllt, createIfNecessary = false)
      val ws = Workspace.read(config.wsMllt, store)
      runWithWorkspace(ws)
    } else {
      val store = BerkeleyDB.factory(config.wsMllt, createIfNecessary = true)
      val ws = Workspace.Durable.empty(config.wsMllt, store)
      runWithWorkspace(ws)
    }
    sys.exit()
  }

  final val VERSION = 1

  private def runWithWorkspace[S <: Sys[S]](workspace: Workspace[S])(implicit config: Config): Unit = {
    implicit val _workspace: Workspace[S] = workspace
    try {
      workspace.cursor.step { implicit tx =>
        val r = workspace.root
        /* val tl = */ mkObj[S, Timeline         ](r, "timeline" , VERSION)(mkTimeline())
//        val fAna  = mkFolder(r, "analysis")
      }
    } catch {
      case NonFatal(ex) =>
        ex.printStackTrace()
    } finally {
      workspace.cursor.step { implicit tx => workspace.dispose() }
    }
  }

  def mkLoc[S <: Sys[S]]()(implicit tx: S#Tx, config: Config): ArtifactLocation[S] =
    ArtifactLocation.newVar(ArtifactLocation.newConst(config.baseDir))

  def mkAudioFolder[S <: Sys[S]]()(implicit tx: S#Tx, config: Config, workspace: Workspace[S]): Folder[S] = {
    val f = Folder[S]
    val segModFiles = config.dataDir.children(_.name.contains("segmod")).sorted(File.NameOrdering)
    val loc = mkObj[S, ArtifactLocation ](workspace.root, "base" , VERSION)(mkLoc())
    segModFiles.foreach { fIn =>
      val a     = Artifact(loc, fIn)
      val spec  = AudioFile.readSpec(fIn)
      val cue   = AudioCue.Obj[S](a, spec, offset = LongObj.newVar(0L), gain = DoubleObj.newVar(1.0))
      cue.name  = fIn.name
      f.addLast(cue)
    }
    f
  }

  def any2stringadd: Any = ()  // mierda

  def mkTapeGraph[S <: Sys[S]](numInChannels: Int)(implicit tx: S#Tx, config: Config): Proc[S] = {
    val p = Proc[S]
    import synth.proc.graph.Ops._
    import synth.proc.graph._
    import synth.ugen.{VDiskIn => _, _}

    val sourcePan = if (numInChannels == 1)
      """{
        |  val pan = "pan".kr(0.0)
        |  Pan2.ar(disk, pan)
        |}"""
    else "disk"

    val source = s"""val disk  = VDiskIn.ar("sig")
                    |val gain  = "gain".kr(1.0)
                    |val mute  = "mute".kr(0.0)
                    |val env   = FadeInOut.ar
                    |val amp   = env * ((1 - mute) * gain)
                    |val sig   = $sourcePan
                    |val sigA  = sig * amp
                    |val ch1   = "ch-1".kr(0.0)
                    |val ch2   = "ch-2".kr(1.0)
                    |val sig1  = sigA.out(0)
                    |val sig2  = sigA.out(1)
                    |val out   = Vector.tabulate(2) { ch =>
                    |  sig1 * (ch1 sig_== ch) + sig2 * (ch2 sig_== ch)
                    |}
                    |ScanOut(out)
                    |""".stripMargin

    p.graph() = SynthGraph {
      val disk  = VDiskIn.ar("sig")
      val gain  = "gain".kr(1.0)
      val mute  = "mute".kr(0.0)
      val env   = FadeInOut.ar
      val amp   = env * ((1 - mute) * gain)
      val sig  = if (numInChannels == 1) {
        val pan = "pan".kr(0.0)
        Pan2.ar(disk, pan)
      } else {
        require (numInChannels == 2)
        disk
      }
      val sigA  = sig * amp
      val ch1   = "ch-1".kr(0.0)
      val ch2   = "ch-2".kr(1.0)
      val sig1  = sigA.out(0)
      val sig2  = sigA.out(1)
      val out   = Vector.tabulate(2) { ch =>
        sig1 * (ch1 sig_== ch) + sig2 * (ch2 sig_== ch)
      }
      ScanOut(out)
    }

    p.attr.put(Proc.attrSource, Code.Obj.newVar(Code.SynthGraph(source)))

    p
  }

  def mkAudioRegion[S <: Sys[S]](tl         : Timeline.Modifiable[S],
                                 time       : Span,
                                 audioCue   : AudioCue.Obj[S],
                                 pMain      : Proc[S],
                                 gOffset    : Long      = 0L,
                                 fadeIn     : FadeSpec  = FadeSpec(0L),
                                 fadeOut    : FadeSpec  = FadeSpec(0L),
                                 pan        : Double    = 0.0,
                                 gain       : Double    = 1.0,
                                 mute       : Boolean   = false,
                                 trackIdx   : Int       = 0,
                                 trackHeight: Int       = 8
                                )
                                (implicit tx: S#Tx, config: Config, workspace: Workspace[S]): (SpanLikeObj[S], Proc[S]) = {
    val spanV   = time
    val span    = SpanLikeObj.newVar[S](spanV)
    val p       = Proc[S]
    val out     = p.outputs.add(Proc.mainOut)
    import proc.Ops._

    val audioCueOff = if (gOffset == 0L) audioCue else {
      audioCue match {
        case AudioCue.Obj.Shift(peer, amt) =>
          AudioCue.Obj.Shift(peer , LongObj.newVar[S](amt + gOffset))
        case other =>
          AudioCue.Obj.Shift(other, LongObj.newVar[S](      gOffset))
      }
    }
    val prAttr = p.attr
    prAttr.put(Proc.graphAudio, audioCueOff)
    val numInChans = audioCue.numChannels

    val pTape  = mkObj[S, Proc](workspace.root, "tape-1", VERSION)(mkTapeGraph(1))
    val sourceOpt = pTape.attr.get(Proc.attrSource)
    sourceOpt.foreach(source => p.attr.put(Proc.attrSource, source))
    p.graph() = pTape.graph()
    p.name    = audioCue.value.artifact.base

    if (fadeIn.numFrames > 0L) {
      val fd    = FadeSpec.Obj.newVar[S](fadeIn)
      prAttr.put(ObjKeys.attrFadeIn, fd)
    }
    if (fadeOut.numFrames > 0L) {
      val fd    = FadeSpec.Obj.newVar[S](fadeOut)
      prAttr.put(ObjKeys.attrFadeOut, fd)
    }
    if (gain !== 1.0) {
      val gainObj = DoubleObj.newVar[S](gain)
      prAttr.put(ObjKeys.attrGain, gainObj)
    }
    if (mute) {
      val muteObj = BooleanObj.newVar[S](mute)
      prAttr.put(ObjKeys.attrMute, muteObj)
    }
    if (numInChans == 1 && pan != 0.0) {
      val panObj = DoubleObj.newVar[S](pan)
      prAttr.put("pan", panObj)
    }
//    if (config.numChannels > 2) {
//      val chanObj1 = IntObj.newVar[S](ctx.chanOff1)
//      val chanObj2 = IntObj.newVar[S](ctx.chanOff2)
//      prAttr.put("ch-1", chanObj1)
//      prAttr.put("ch-2", chanObj2)
//    }
    if (trackIdx != 0) {
      val trackIdxObj = IntObj.newVar[S](trackIdx)
      prAttr.put(TimelineObjView.attrTrackIndex, trackIdxObj)
    }
    if (trackHeight != 8) {
      val trackHeightObj = IntObj.newVar[S](trackHeight)
      prAttr.put(TimelineObjView.attrTrackHeight, trackHeightObj)
    }

    tl.add(span, p)
//    import ctx.cursor
    implicit val cursor: stm.Cursor[S] = workspace.cursor
    Edits.addLink(source = out, sink = /* ctx. */ pMain, key = Proc.mainIn)

    (span, p)
  }

  def segModTemp(idx: String)(implicit config: Config): File =
    config.dataDir / s"alphaville-$idx-segmod-%d.aif"

  def segModRsmpTemp(idx: String)(implicit config: Config): File =
    config.dataDir / s"alphaville-$idx-segmodRsmp-%d.aif"

  def getAudioCue[S <: Sys[S]](f: File)(implicit tx: S#Tx, config: Config, workspace: Workspace[S]): AudioCue.Obj[S] = {
    val r = workspace.root
    val fAudio = mkObj[S, Folder](r, "audio", VERSION)(mkAudioFolder())
    val opt = fAudio.iterator.collectFirst {
      case a: AudioCue.Obj[S] if a.value.artifact == f => a
    }
    opt.getOrElse(sys.error(s"File not found: $f"))
  }

  def addDefaultGlobalProc[S <: Sys[S]](tl: Timeline.Modifiable[S])(implicit tx: S#Tx, config: Config): Proc[S] = {
    val p = Proc[S]
    import synth.proc.graph.Ops._
    import synth.proc.graph._
    import synth.ugen._

    val source = s"""val in      = ScanInFix(2)
                    |val gain    = "gain".kr(1f)
                    |val chGain  = "ch-gain".kr(Vector.fill(2)(1f))
                    |val mute    = "mute".kr(0f)
                    |val bus     = "bus" .kr(0f)
                    |val amp     = gain * chGain * (1 - mute)
                    |val mul     = in * amp
                    |val sig     = mul
                    |Out.ar(bus, sig)
                    |""".stripMargin

    p.graph() = SynthGraph {
      val in      = ScanInFix(2)
      val gain    = "gain".kr(1f)
      val chGain  = "ch-gain".kr(Vector.fill(2)(1f))
      val mute    = "mute".kr(0f)
      val bus     = "bus" .kr(0f)
      val amp     = gain * chGain * (1 - mute)
      val mul     = in * amp
      val sig     = mul
      Out.ar(bus, sig)
    }
    p.name = "main"
    p.attr.put(Proc.attrSource, Code.Obj.newVar(Code.SynthGraph(source)))

    tl.add(SpanLikeObj.newConst(Span.All), p)
    p
  }

  def mkTimeline[S <: Sys[S]]()(implicit tx: S#Tx, config: Config, workspace: Workspace[S]): Timeline.Modifiable[S] = {
    val tl = Timeline[S]()
    // section 1: 50% overlapping fade-in / layering and repeating starting from last iteration
    // (smallest amplitude). We try odd iterations on left and even iterations on right channel.

    tl.attr.put("bounce-file", StringObj.newVar[S]((config.baseDir / "audio_work" / "legende.aif").path))

    val trackHeight = 4

    val pMain = addDefaultGlobalProc(tl)

    def oneTwo(isOne: Boolean, timeOff: Long): Long = {
      val temp        = segModTemp(if (isOne) "1" else "2")
      val gain        = 1.0 / (if (isOne) 9.0 else 6.0)
      val leftIter    = if (isOne) 40 to 1 by -2 else 24 to 1 by -2
      val rightIter   = if (isOne) 39 to 1 by -2 else 23 to 1 by -2
      val numSide     = leftIter.size
      assert (numSide === rightIter.size)
      //    val numFull     = (numSide + 1) / 2
      val fdShortLen  = (TimeRef.SampleRate * 0.1).toLong
      val fdShortOut  = FadeSpec(fdShortLen, Curve.parametric(+1.763f))
      val fdShortIn   = FadeSpec(fdShortLen, Curve.parametric(-1.763f))
      val tlLen       = {
        val f   = formatTemplate(temp, 1)
        val cue = getAudioCue(f)
        (cue.numFrames * TimeRef.SampleRate / cue.sampleRate + 0.5).toLong
      }
      val tlLenH      = tlLen/2
      val tlLenM      = tlLen - fdShortLen
      val fdMedOut    = FadeSpec(tlLenH, Curve.parametric(+1.763f))
      val fdMedIn     = FadeSpec(tlLenH, Curve.parametric(-1.763f))

      def loop(isOdd: Boolean): Long = {
        val it  = if (isOdd) rightIter else leftIter
        val pan = if (isOdd) +1.0 else -1.0
        val trackIdxOff = if (isOdd) numSide else 0
        val penUltimatePan = {
          val g = Grapheme[S]()
          //        val curve = CurveObj.newVar[S](CurveObj.newConst(Curve.sine))
          //        g.add(LongObj.newVar(0L       ), EnvSegment.Obj(DoubleObj.newVar(pan), curve))
          //        g.add(LongObj.newVar(tlLen /* * 2 */), EnvSegment.Obj(DoubleObj.newVar(0.0), curve))
          val curve = CurveObj.newVar[S](CurveObj.newConst(Curve.lin))
          g.add(LongObj.newVar(0L   ), EnvSegment.Obj(DoubleObj.newVar(pan), curve))
          g.add(LongObj.newVar(tlLen), EnvSegment.Obj(DoubleObj.newVar(pan/2), curve))
          g
        }
        val ultimatePan = {
          val g = Grapheme[S]()
          //        val curve = CurveObj.newVar[S](CurveObj.newConst(Curve.sine))
          //        g.add(LongObj.newVar(-tlLen), EnvSegment.Obj(DoubleObj.newVar(pan), curve))
          //        g.add(LongObj.newVar(tlLen ), EnvSegment.Obj(DoubleObj.newVar(0.0), curve))
          //        g
          //        DoubleObj.newVar[S](0.0)
          val curve = CurveObj.newVar[S](CurveObj.newConst(Curve.lin))
          g.add(LongObj.newVar(0L   ), EnvSegment.Obj(DoubleObj.newVar(pan/2), curve))
          g.add(LongObj.newVar(tlLen), EnvSegment.Obj(DoubleObj.newVar(0.0), curve))
          g
        }

        val stops = it.zipWithIndex.map { case (iter, idx) =>
          val f         = formatTemplate(temp, iter)
          val cue       = getAudioCue(f)
          //        val startHalf = idx.isOdd
          val trackIdx  = (idx + trackIdxOff) * trackHeight
          //        if (startHalf) {
          //          val gOffset = cue.numFrames/2
          //          val fadeIn  = FadeSpec(tlLenH - fdShortLen)
          //          val fadeOut = fdShortOut
          //          val time    = Span(tlLenH, tlLen)
          //          mkAudioRegion(tl, time = time, audioCue = cue, pMain = pMain, gOffset = gOffset,
          //            fadeIn = fadeIn, fadeOut = fadeOut, pan = pan, trackIdx = trackIdx)
          //        }
          //        val fullIdxStart = (idx + 1)/2
          def isMuted(j: Int): Boolean = j >= 0 && j <= numSide && {
            if (isOne) {
              if (!isOdd) {
                j > idx+1 && j < numSide - idx
              } else {
                j >= ((idx + 1) * 3)/2+1 && j <= numSide - (15 - idx)/2
              }
            } else {
              if (!isOdd) {
                j >= ((idx + 1) * 3)/2+1 && j < (numSide - idx/2)
              } else {
                j > idx+1 && j < numSide - idx
              }
            }
          }

          (idx to numSide).foldLeft(0L) { case (_, i) =>
            val gOffset = 0L
            val mute    = isMuted(i)
            val fadeIn  = if (!mute && (i == idx || isMuted(i - 1))) fdMedIn  else fdShortIn
            val fadeOut = if (!mute &&              isMuted(i + 1))  fdMedOut else fdShortOut
            val start   = tlLenM * i + timeOff
            val time    = Span(start, start + tlLen)
            val (_, p) = mkAudioRegion(tl, time = time, audioCue = cue, pMain = pMain, gOffset = gOffset, gain = gain,
              mute = mute, fadeIn = fadeIn, fadeOut = fadeOut, pan = pan,
              trackIdx = trackIdx, trackHeight = trackHeight)
            if (i == numSide - 1) {
              p.attr.put("pan", penUltimatePan)
            } else if (i == numSide) {
              p.attr.put("pan", ultimatePan)
            }
            time.stop
          }
        }

        stops.max
      }

      loop(isOdd = false)
      loop(isOdd = true )
    }

    def three(timeOff: Long): Long = {
      val tempRsmp    = segModRsmpTemp("3")
      val tempA       = segModTemp("3a")
      val tempB       = segModTemp("3b")
      val gainRsmp    = 1.0 / 12.0
      val gainA       = 1.0 / 8.0
      val gainB       = 1.0 / 6.0
      val leftIter    = 40 to 1 by -2
      val rightIter   = 39 to 1 by -2
      val numSide     = leftIter.size

      val fdShortLen  = (TimeRef.SampleRate * 0.1).toLong
      val fdShortOut  = FadeSpec(fdShortLen, Curve.parametric(+1.763f))
      val fdShortIn   = FadeSpec(fdShortLen, Curve.parametric(-1.763f))

      val tlLen = (leftIter ++ rightIter).map { iter =>
        val f         = formatTemplate(tempRsmp, iter)
        val cue       = getAudioCue(f)
        (cue.numFrames * TimeRef.SampleRate / cue.sampleRate + 0.5).toLong
      } .max

      def loop(isOdd: Boolean): Long = {
        val it  = if (isOdd) rightIter else leftIter
        val pan = if (isOdd) +1.0 else -1.0
        val trackIdxOff = if (isOdd) numSide else 0

        val stops = it.zipWithIndex.map { case (iter, idx) =>
          val fRsmp       = formatTemplate(tempRsmp, iter)
          val fA          = formatTemplate(tempA   , iter)
          val fB          = formatTemplate(tempB   , iter)
          val cueRsmp     = getAudioCue(fRsmp)
          val cueA        = getAudioCue(fA)
          val cueB        = getAudioCue(fB)
          val trackIdx    = (idx + trackIdxOff) * trackHeight
          val cueRsmpLen  = (cueRsmp.numFrames * TimeRef.SampleRate / cueRsmp.sampleRate + 0.5).toLong
          val cueALen     = (cueA   .numFrames * TimeRef.SampleRate / cueA   .sampleRate + 0.5).toLong
          val cueBLen     = (cueB   .numFrames * TimeRef.SampleRate / cueB   .sampleRate + 0.5).toLong

          val panRsmp = {
            val g = Grapheme[S]()
            val curve = CurveObj.newVar[S](CurveObj.newConst(Curve.lin))
            g.add(LongObj.newVar(0L         ), EnvSegment.Obj(DoubleObj.newVar(pan), curve))
            g.add(LongObj.newVar(cueRsmpLen ), EnvSegment.Obj(DoubleObj.newVar(pan/2), curve))
            g
          }

          val panA = {
            val g = Grapheme[S]()
            val curve = CurveObj.newVar[S](CurveObj.newConst(Curve.lin))
            g.add(LongObj.newVar(0L     ), EnvSegment.Obj(DoubleObj.newVar(pan/2), curve))
            g.add(LongObj.newVar(cueALen), EnvSegment.Obj(DoubleObj.newVar(pan/4), curve))
            g
          }

          val panB = {
            val g = Grapheme[S]()
            val curve = CurveObj.newVar[S](CurveObj.newConst(Curve.lin))
            g.add(LongObj.newVar(0L     ), EnvSegment.Obj(DoubleObj.newVar(pan/4), curve))
            g.add(LongObj.newVar(cueALen), EnvSegment.Obj(DoubleObj.newVar(0.0  ), curve))
            g
          }

          def isMuted: Boolean = false

//          def isMuted(j: Int): Boolean = j >= 0 && j <= numSide && {
//            if (isOne) {
//              if (!isOdd) {
//                j > idx+1 && j < numSide - idx
//              } else {
//                j >= ((idx + 1) * 3)/2+1 && j <= numSide - (15 - idx)/2
//              }
//            } else {
//              if (!isOdd) {
//                j >= ((idx + 1) * 3)/2+1 && j < (numSide - idx/2)
//              } else {
//                j > idx+1 && j < numSide - idx
//              }
//            }
//          }

          val gOffset   = 0L
          val mute      = isMuted
          val fadeIn    = /* if (!mute && (i == idx || isMuted(i - 1))) fdMedIn  else */ fdShortIn
          val fadeOut   = /* if (!mute &&              isMuted(i + 1))  fdMedOut else */ fdShortOut
          val startRsmp = timeOff + tlLen - cueRsmpLen
          val timeRsmp  = Span(startRsmp, startRsmp + cueRsmpLen)
          val (_, pRsmp) = mkAudioRegion(tl, time = timeRsmp, audioCue = cueRsmp, pMain = pMain, gOffset = gOffset, gain = gainRsmp,
            mute = mute, fadeIn = fadeIn, fadeOut = fadeOut, pan = pan,
            trackIdx = trackIdx, trackHeight = trackHeight)
//            if (i == numSide - 1) {
//              p.attr.put("pan", penUltimatePan)
//            } else if (i == numSide) {
//              p.attr.put("pan", ultimatePan)
//            }
          pRsmp.attr.put("pan", panRsmp)

          val startA  = timeRsmp.stop - fdShortLen
          val timeA   = Span(startA, startA + cueALen)
          val (_, pA) = mkAudioRegion(tl, time = timeA, audioCue = cueA, pMain = pMain, gOffset = gOffset, gain = gainA,
            mute = mute, fadeIn = fadeIn, fadeOut = fadeOut, pan = pan,
            trackIdx = trackIdx, trackHeight = trackHeight)
          pA.attr.put("pan", panA)

          val startB  = timeA.stop - fdShortLen
          val timeB   = Span(startB, startB + cueBLen)
          val (_, pB) = mkAudioRegion(tl, time = timeB, audioCue = cueB, pMain = pMain, gOffset = gOffset, gain = gainB,
            mute = mute, fadeIn = fadeIn, fadeOut = fadeOut, pan = pan,
            trackIdx = trackIdx, trackHeight = trackHeight)
          pB.attr.put("pan", panB)

          timeB.stop
        }

        stops.max
      }

      loop(isOdd = false)
      loop(isOdd = true )
    }

    val stopOne = oneTwo(isOne = true , timeOff = 0L)
    val stopTwo = oneTwo(isOne = false, timeOff = (stopOne + TimeRef.SampleRate * 1.3).toLong)
    three(timeOff = (stopTwo + TimeRef.SampleRate * 1.8).toLong)

    tl
  }
}
