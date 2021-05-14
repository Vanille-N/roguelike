lazy val root = (project in file(".")).

settings(
    name := "Roguelike",
    fork in run := true,
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-swing" % "2.0.1",
      "com.typesafe.akka" %% "akka-actor" % "2.5.32",
      "com.typesafe.akka" %% "akka-testkit" % "2.5.21" % Test
    )
)
