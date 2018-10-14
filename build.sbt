lazy val root = project.withId("smudge").in(file("."))
  .settings(
    name         := "Smudge",
    organization := "de.sciss",
    licenses     := Seq("agpl-3.0" -> url("http://www.gnu.org/licenses/agpl-3.0.txt")),
    scalaVersion := "2.12.7",
    libraryDependencies ++= Seq(
      "de.sciss" %% "fscape" % "2.18.1-SNAPSHOT"
    )
  )
