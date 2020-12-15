val log4jVersion = "2.14.0"
val dottyVersion = "0.26.0-RC1"
val graphStreamVersion = "2.0"
// scalaVersion := "3.0.0-M2",
val jgraphtVersion = "1.5.0"
val codecVersion = "1.15"
val mathVersion = "3.6.1"
val rngVersion = "1.3"
val textVersion = "1.9"

lazy val root = project
  .in(file("."))
  .settings(
    name := "aoc2020",
    description := "aoc2020",
    version := "0.1.0",
    scalaVersion := dottyVersion,
    useScala3doc := true,
    libraryDependencies += "org.apache.logging.log4j" % "log4j-api" % log4jVersion,
    libraryDependencies += "org.apache.logging.log4j" % "log4j-core" % log4jVersion,
    libraryDependencies += "org.graphstream" % "gs-core" % graphStreamVersion,
    libraryDependencies += "org.graphstream" % "gs-algo" % graphStreamVersion,
    libraryDependencies += "org.graphstream" % "gs-ui-swing" % graphStreamVersion,
    libraryDependencies += "org.jgrapht" % "jgrapht-core" % jgraphtVersion,
    libraryDependencies += "org.jgrapht" % "jgrapht-ext" % jgraphtVersion,
    libraryDependencies += "org.jgrapht" % "jgrapht-io" % jgraphtVersion,
    libraryDependencies += "commons-codec" % "commons-codec" % codecVersion,
    libraryDependencies += "org.apache.commons" % "commons-math3" % mathVersion,
    libraryDependencies += "org.apache.commons" % "commons-rng-simple" % rngVersion,
    libraryDependencies += "org.apache.commons" % "commons-text" % textVersion

)
