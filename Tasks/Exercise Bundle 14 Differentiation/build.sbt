lazy val copl = project.in(file(".")).settings(
  // explicitly depend on a scala version
  scalaVersion := "3.7.3",
  // our testing library
  libraryDependencies += "org.scalameta" %% "munit" % "1.2.1",
  // libraries used for some research project with the interpreters
  libraryDependencies ++= Seq(
    "org.eclipse.lsp4j"  % "org.eclipse.lsp4j" % "0.24.0",
    "com.github.j-mie6" %% "parsley"           % "4.6.1"
  ),
  // additional compiler options
  scalacOptions ++= List(
    // enable “feature warnings” and require explicit imports.
    // this is usually for advanced/tricky features you should be aware of before using.
    "-feature",
    // enable deprecation warnings when using some standard library functions and language features no longer considered best practice.
    "-deprecation",
    // abort when there are any warnings.
    // if you need to silence certain warnings, use the @nowarn annotation
    "-Werror",
    // makes null no longer a subtype of everything, very inconvenient when working with Java,
    // but does allow safe use of null in Scala
    "-Yexplicit-nulls",
    // enable for scalafix
    // "-Wunused:imports"
  ),
  // this is for scalafix
  // semanticdbEnabled := true,
  // these options are only set for compiling the main project, not for the tests
  Compile / compile / scalacOptions ++= List(
    // ensures that object fields are initialized before they are used
    // this is mostly relevant when using complex class hierarchies
    "-Wsafe-init",
  ),
  // the following metadata is not strictly necessary
  version      := "0.0.1",
  organization := "de.tu-darmstadt.stg",

  // the following two lines add additional scala source directories that are compiled as part of the normal compilation of the project above.
  // these are not part of the main repository for access control reasons.
  // having these directories not exist should not cause any problems for normal compilation
  Compile / unmanagedSourceDirectories += baseDirectory(_ / "Tasks/src/main/scala").value,
  Test / unmanagedSourceDirectories += baseDirectory(_ / "Tasks/src/test/scala").value,
)
