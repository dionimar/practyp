import Dependencies._

ThisBuild / scalaVersion     := "2.12.8"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

libraryDependencies += "org.typelevel" %% "cats-core" % "2.3.0"
libraryDependencies += "org.typelevel" %% "cats-effect" % "3.2.9"
//libraryDependencies += "org.scala-lang" % "jline" % "2.10.7"
libraryDependencies += "com.googlecode.lanterna" % "lanterna" % "3.0.0"
libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "3.0.0"

lazy val root = (project in file("."))
  .settings(
    name := "practyp",
    libraryDependencies += scalaTest % Test
  )

scalacOptions ++= Seq(
  "-Xlog-implicits"
)
