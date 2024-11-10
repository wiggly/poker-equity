import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

inThisBuild(
  List(
    scalaVersion := "2.13.14",
    semanticdbEnabled := true,
    semanticdbVersion := scalafixSemanticdb.revision
  )
)

lazy val languageSettings = List(
  scalacOptions ++= Seq(
    "-deprecation", // Emit warning and location for usages of deprecated APIs.
    "-feature", // Emit warning and location for usages of features that should be imported explicitly.
    "-language:higherKinds" // Allow higher-kinded types
  )
)

lazy val root = crossProject(JVMPlatform, NativePlatform)
  .withoutSuffixFor(JVMPlatform)
  .in(file("."))
  .jvmConfigure(_.withDependencies)
  .nativeConfigure(_.withDependencies)
  .settings(
    name := "Poker Equity",
    moduleName := "poker-equity",
    fork := true
  )
  .settings(languageSettings)
  .settings(
    addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1"),
    addCompilerPlugin(
      "org.typelevel" % "kind-projector" % "0.13.3" cross CrossVersion.full
    )
  )
