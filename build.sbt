lazy val demo = (project in file("."))
  .settings(
    name := "Demo",
    libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.3",
    libraryDependencies += "org.scalatest" %% "scalatest-flatspec" % "3.2.2" % Test,
    libraryDependencies += "org.scalatestplus" %% "easymock-3-2" % "3.2.2.0" % Test
  )
