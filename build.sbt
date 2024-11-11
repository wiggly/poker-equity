import sbt.addCompilerPlugin
import sbtcrossproject.CrossPlugin.autoImport.crossProject

inThisBuild(
  List(
    scalaVersion := "2.13.14",
    semanticdbEnabled := true,
    semanticdbVersion := scalafixSemanticdb.revision,
  )
)

lazy val sharedSettings = List(
  name := "Poker Equity",
  moduleName := "poker-equity",
  fork := true
)

lazy val languageSettings = List(
  scalacOptions ++= Seq(
    "-deprecation", // Emit warning and location for usages of deprecated APIs.
    "-feature", // Emit warning and location for usages of features that should be imported explicitly.
    "-language:higherKinds" // Allow higher-kinded types
  ),
  addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1"),
  addCompilerPlugin(
    "org.typelevel" % "kind-projector" % "0.13.3" cross CrossVersion.full
  )
)

lazy val root = crossProject(JVMPlatform, NativePlatform)
  .withoutSuffixFor(JVMPlatform)
  .in(file("."))
  .settings(sharedSettings)
  .jvmConfigure(
      _.settings(languageSettings)
      .withDependencies
  )
  .nativeConfigure(
      _.settings(languageSettings)
      .withDependencies
  )
