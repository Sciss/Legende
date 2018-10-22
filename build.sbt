lazy val deps = new {
  val main = new {
    val dijkstra        = "0.1.1"
    val fscape          = "2.18.1"
    val kollFlitz       = "0.2.2"
    val lucre           = "3.10.1"
    val mellite         = "2.28.0"
    val scopt           = "3.7.0"
    val soundProcesses  = "3.22.0"
  }
}

lazy val root = project.withId("legende").in(file("."))
  .settings(
    name         := "Legende",
    version      := "1.0.0",
    description  := "Software for an algorithmic sound piece",
    organization := "de.sciss",
    homepage     := Some(url(s"https://git.iem.at/sciss/${name.value}")),
    licenses     := Seq("agpl-3.0" -> url("http://www.gnu.org/licenses/agpl-3.0.txt")),
    scalaVersion := "2.12.7",
    scalacOptions in (Compile, compile) ++= Seq(
      "-deprecation", "-unchecked", "-feature", "-encoding", "utf8", "-Xfuture", "-Xlint", "-Xsource:2.13"
    ),
    scalacOptions += "-Yrangepos",  // this is needed to extract source code
    resolvers     += "Oracle Repository" at "http://download.oracle.com/maven",  // required for sleepycat
    updateOptions := updateOptions.value.withLatestSnapshots(false),
    libraryDependencies ++= Seq(
      "de.sciss"          %% "dijkstra"             % deps.main.dijkstra,
//      "de.sciss"          %% "soundprocesses-core"  % deps.main.soundProcesses,
      "de.sciss"          %% "mellite"              % deps.main.mellite,  // we need `Edits` unfortunately
      "de.sciss"          %% "fscape"               % deps.main.fscape,
      "de.sciss"          %% "lucre-bdb"            % deps.main.lucre,
      "de.sciss"          %% "kollflitz"            % deps.main.kollFlitz,
      "com.github.scopt"  %% "scopt"                % deps.main.scopt,
    ),
    test            in assembly := {},
    mainClass       in assembly := Some("de.sciss.legende.Prepare"),
    target          in assembly := baseDirectory.value,
    assemblyJarName in assembly := s"${name.value}.jar",
  )
