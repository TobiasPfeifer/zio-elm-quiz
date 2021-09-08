ThisBuild / scalaVersion     := "2.13.5"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "ch.tpfeifer.demo"
ThisBuild / organizationName := "example"

val zioVersion             = "1.0.10"
val zioHttpVersion         = "1.0.0.0-RC17"
val zioOpticsVersion       = "0.1.0"

lazy val root = (project in file("backend"))
  .settings(
    name := "backend",
    scalacOptions += "-deprecation",
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio" % zioVersion,
      "dev.zio" %% "zio-streams" % zioVersion,
      "dev.zio" %% "zio-test" % zioVersion % Test,
      "dev.zio" %% "zio-optics" % zioOpticsVersion,

      "dev.zio"      %% "zio-json"      % "0.1.5",
      "io.github.kitlangton" %% "zio-magic" % "0.3.6",

      "com.github.ghostdogpr" %% "caliban" % "1.1.1",
      "com.github.ghostdogpr" %% "caliban-zio-http" % "1.1.1",
      "io.d11"   %% "zhttp"         % zioHttpVersion

    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),
    scalacOptions in Global += "-Ymacro-annotations"
  )

//
//"dev.zio" %% "zio" % "1.0.6",
//"dev.zio" %% "zio-test" % "1.0.6" % Test,
//"dev.zio" %% "zio-streams"   % "1.0.6",
//"dev.zio" %% "zio-json"      % "0.1.4",
//"io.github.kitlangton" %% "zio-magic" % "0.2.3",
//"com.github.ghostdogpr" %% "caliban" % "0.9.5",
//"com.github.ghostdogpr" %% "caliban-http4s" % "0.9.5",
//"dev.zio" %% "zio-interop-cats" % "2.4.0.0",
//"org.typelevel" %% "cats-effect" % "2.4.1",