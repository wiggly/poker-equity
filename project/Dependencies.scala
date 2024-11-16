import sbt.Keys.*
import sbt.*
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport.*

object Dependencies extends AutoPlugin {

  object autoImport {
    implicit class DependencySettings(val project: Project) extends AnyVal {
      def withDependencies: Project =
        project
          .settings(libraryDependencies ++= deps.value)
          .settings(libraryDependencies ++= testDeps.value)
    }
  }

  object Version {
    val cats = "2.12.0"
    val catsEffect = "3.2.3"
    val scalaTest = "3.2.19"
    val scalaCheck = "1.15.4"
    val scalatTestPlus = "3.2.19.0"
  }

  lazy val deps = Def.setting(
    Seq(
      "org.typelevel" %%% "cats-core" % Version.cats,
      "org.typelevel" %%% "cats-effect" % Version.catsEffect
    )
  )

  lazy val testDeps = Def.setting(
    Seq(
      "org.scalatest" %%% "scalatest" % Version.scalaTest,
      "org.scalacheck" %% "scalacheck" % Version.scalaCheck,
      "org.scalatestplus" %% "scalacheck-1-18" % Version.scalatTestPlus
    ).map(_ % Test)
  )
}
