lazy val deps = new {
  val main = new {
    val dijkstra  = "0.1.1"
    val fscape    = "2.18.1-SNAPSHOT"
    val scopt     = "3.7.0"
  }
}

lazy val root = project.withId("legende").in(file("."))
  .settings(
    name         := "Legende",
    version      := "0.1.0-SNAPSHOT",
    description  := "Software for an algorithmic sound piece",
    organization := "de.sciss",
    homepage     := Some(url(s"https://git.iem.at/sciss/${name.value}")),
    licenses     := Seq("agpl-3.0" -> url("http://www.gnu.org/licenses/agpl-3.0.txt")),
    scalaVersion := "2.12.7",
    scalacOptions in (Compile, compile) ++= Seq(
      "-deprecation", "-unchecked", "-feature", "-encoding", "utf8", "-Xfuture", "-Xlint", "-Xsource:2.13"
    ),
    resolvers     += "Oracle Repository" at "http://download.oracle.com/maven",  // required for sleepycat
    updateOptions := updateOptions.value.withLatestSnapshots(false),
    libraryDependencies ++= Seq(
      "de.sciss"          %% "dijkstra" % deps.main.dijkstra,
      "de.sciss"          %% "fscape"   % deps.main.fscape,
      "com.github.scopt"  %% "scopt"    % deps.main.scopt,
    )
  )
