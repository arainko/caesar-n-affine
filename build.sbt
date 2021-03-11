val zioVersion = "1.0.4"

lazy val `caesar-n-affine` = (project in file("."))
  .settings(
    name := "caesar-n-affine",
    version := "1.0.0",
    scalaVersion := "2.13.4",
    scalacOptions ++= Seq("-Wconf:cat=unused:info", "-Ymacro-annotations"),
    semanticdbEnabled := true,
    semanticdbVersion := scalafixSemanticdb.revision,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core"         % "2.4.2",
      "dev.zio"       %% "zio-nio"           % "1.0.0-RC10",
      "dev.zio"       %% "zio"               % zioVersion,
      "dev.zio"       %% "zio-test"          % zioVersion % "test",
      "dev.zio"       %% "zio-test-sbt"      % zioVersion % "test",
      "dev.zio"       %% "zio-test-magnolia" % zioVersion % "test"
    )
  )
  .configs(IntegrationTest)
  .settings(Defaults.itSettings)

testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")

scalafixScalaBinaryVersion in ThisBuild := CrossVersion.binaryScalaVersion(scalaVersion.value)
ThisBuild / scalafixDependencies += "com.github.liancheng" %% "organize-imports" % "0.5.0"
