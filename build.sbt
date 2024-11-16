import sbtcrossproject.CrossPlugin.autoImport.crossProject

inThisBuild(
  List(
    scalaVersion := "3.3.0",
    semanticdbEnabled := true,
    semanticdbVersion := scalafixSemanticdb.revision,
  )
)

lazy val sharedSettings = List(
  name := "Poker Equity",
  moduleName := "poker-equity",
)

lazy val root = crossProject(JVMPlatform, NativePlatform)
  .in(file("."))
  .settings(sharedSettings)
  .jvmConfigure(
      _.withDependencies
  )
  .nativeConfigure(
      _.withDependencies
  )
