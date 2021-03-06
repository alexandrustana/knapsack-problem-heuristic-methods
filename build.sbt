import sbt._

lazy val root = (project in file("."))
  .settings(commonsSettings)


val ScalaCheckVersion      = "1.13.5"
val ScalaTestVersion       = "3.0.4"
val ScalaLoggingVersion    = "3.9.0"
val MonixVersion           = "3.0.0-RC1"

def commonsSettings: Seq[Setting[_]] = Seq(
  scalaVersion := "2.12.6",
  name         := "ksp",
  version      := "0.0.1-SNAPSHOT",
  libraryDependencies ++= Seq(
    "com.typesafe.scala-logging" %% "scala-logging"        % ScalaLoggingVersion    withSources (),
    "io.monix"                   %% "monix"                % MonixVersion           withSources (),
    "org.scalacheck"             %% "scalacheck"           % ScalaCheckVersion      % Test withSources (),
    "org.scalatest"              %% "scalatest"            % ScalaTestVersion       % Test withSources ()
  ),
  /*
   * Eliminates useless, unintuitive, and sometimes broken additions of `withFilter`
   * when using generator arrows in for comprehensions. e.g.
   *
   * Vanila scala:
   * {{{
   *   for {
   *      x: Int <- readIntIO
   *      //
   *   } yield ()
   *   // instead of being `readIntIO.flatMap(x: Int => ...)`, it's something like .withFilter {case x: Int}, which is tantamount to
   *   // a runtime instanceof check. Absolutely horrible, and ridiculous, and unintuitive, and contrary to the often-
   *   // parroted mantra of "a for is just sugar for flatMap and map
   * }}}
   *
   * https://github.com/oleg-py/better-monadic-for
   */
  addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.2.0"),
  scalacOptions ++= customScalaCompileFlags,
  /**
    * This is here to eliminate eviction warnings from SBT.
    * The eco-system is mid-upgrade, so not all dependencies
    * depend on this newest cats, and cats-effect. But the
    * old versions cats-core 1.0.1, and cats-effect 0.10
    * are guaranteed to be binary compatible with the newer
    * ones which "choose" over them.
    *
    * By guarantee I mean that the library authors ran
    * a binary compatability analysis.
    *
    * See more on binary compatability:
    * https://docs.oracle.com/javase/specs/jls/se7/html/jls-13.html
    *
    * It is an important issue that you need to keep track of if
    * you build apps on the JVM
    */
  dependencyOverrides += "org.typelevel" %% "cats-core"   % "1.1.0",
  dependencyOverrides += "org.typelevel" %% "cats-effect" % "0.10.1"
)

/**
  * tpolecat's glorious compile flag list:
  * https://tpolecat.github.io/2017/04/25/scalac-flags.html
  */
def customScalaCompileFlags: Seq[String] = Seq(
  "-deprecation", // Emit warning and location for usages of deprecated APIs.
  "-encoding",
  "utf-8", // Specify character encoding used by source files.
  "-explaintypes", // Explain type errors in more detail.
  "-feature", // Emit warning and location for usages of features that should be imported explicitly.
  "-language:existentials", // Existential types (besides wildcard types) can be written and inferred
  "-language:higherKinds", // Allow higher-kinded types
  "-language:implicitConversions", // Allow definition of implicit functions called views
  "-unchecked", // Enable additional warnings where generated code depends on assumptions.
  "-Xcheckinit", // Wrap field accessors to throw an exception on uninitialized access.
  "-Xfuture", // Turn on future language features.
  "-Xlint:adapted-args", // Warn if an argument list is modified to match the receiver.
  "-Xlint:by-name-right-associative", // By-name parameter of right associative operator.
  "-Xlint:constant", // Evaluation of a constant arithmetic expression results in an error.
  "-Xlint:delayedinit-select", // Selecting member of DelayedInit.
  "-Xlint:doc-detached", // A Scaladoc comment appears to be detached from its element.
  "-Xlint:inaccessible", // Warn about inaccessible types in method signatures.
  "-Xlint:infer-any", // Warn when a type argument is inferred to be `Any`.
  "-Xlint:missing-interpolator", // A string literal appears to be missing an interpolator id.
  "-Xlint:nullary-override", // Warn when non-nullary `def f()' overrides nullary `def f'.
  "-Xlint:nullary-unit", // Warn when nullary methods return Unit.
  "-Xlint:option-implicit", // Option.apply used implicit view.
  "-Xlint:package-object-classes", // Class or object defined in package object.
  "-Xlint:poly-implicit-overload", // Parameterized overloaded implicit methods are not visible as view bounds.
  "-Xlint:private-shadow", // A private field (or class parameter) shadows a superclass field.
  "-Xlint:stars-align", // Pattern sequence wildcard must align with sequence component.
  "-Xlint:type-parameter-shadow", // A local type parameter shadows a type already in scope.
  "-Xlint:unsound-match", // Pattern match may not be typesafe.
  "-Yno-adapted-args", // Do not adapt an argument list (either by inserting () or creating a tuple) to match the receiver.
  "-Ywarn-extra-implicit", // Warn when more than one implicit parameter section is defined.
  "-Ywarn-inaccessible", // Warn about inaccessible types in method signatures.
  "-Ywarn-infer-any", // Warn when a type argument is inferred to be `Any`.
  "-Ywarn-nullary-override", // Warn when non-nullary `def f()' overrides nullary `def f'.
  "-Ywarn-nullary-unit", // Warn when nullary methods return Unit.
  "-Ywarn-numeric-widen", // Warn when numerics are widened.
  "-Ywarn-unused:implicits", // Warn if an implicit parameter is unused.
  "-Ywarn-unused:imports", // Warn if an import selector is not referenced.
  "-Ywarn-unused:locals", // Warn if a local definition is unused.
  "-Ywarn-unused:params", // Warn if a value parameter is unused.
  "-Ywarn-unused:patvars", // Warn if a variable bound in a pattern is unused.
  "-Ywarn-unused:privates", // Warn if a private member is unused.
  "-Ywarn-value-discard",  // Warn when non-Unit expression results are unused.
  "-Ypartial-unification", // Enable partial unification in type constructor inference

  //"-Xfatal-warnings",                  // Fail the compilation if there are any warnings.
  /*
   * These are flags specific to the "better-monadic-for" plugin:
   * https://github.com/oleg-py/better-monadic-for
   */
  "-P:bm4:no-filtering:y", // see https://github.com/oleg-py/better-monadic-for#desugaring-for-patterns-without-withfilters--pbm4no-filteringy
  "-P:bm4:no-map-id:y", // see https://github.com/oleg-py/better-monadic-for#final-map-optimization--pbm4no-map-idy
  "-P:bm4:no-tupling:y" // see https://github.com/oleg-py/better-monadic-for#desugar-bindings-as-vals-instead-of-tuples--pbm4no-tuplingy
)