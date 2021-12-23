organization := "com.github.kmizu"

name := "minimalang"

version := "0.0.1-SNAPSHOT"

scalaVersion := "2.13.6"

publishMavenStyle := true

scalacOptions ++= {
  Seq("-unchecked", "-deprecation", "-feature", "-language:implicitConversions")
}

libraryDependencies ++= Seq(
  "com.github.kmizu" %% "scomb" % "0.9.0",
  "junit" % "junit" % "4.7" % "test",
  "org.scalatest" %% "scalatest" %  "3.2.10"
)

assemblyJarName in assembly := "minimalang.jar"

mainClass in assembly := Some("com.github.minimalang.Main")

initialCommands in console += {
  Iterator(
    "com.github.minimalang._"
  ).map("import "+).mkString("\n")
}

pomExtra := (
  <url>https://github.com/kmizu/minimalang</url>
  <licenses>
    <license>
      <name>The MIT License</name>
      <url>http://www.opensource.org/licenses/MIT</url>
      <distribution>repo</distribution>
    </license>
  </licenses>
  <scm>
    <url>git@github.com:kmizu/minimalang.git</url>
    <connection>scm:git:git@github.com:kmizu/minimalang.git</connection>
  </scm>
  <developers>
    <developer>
      <id>kmizu</id>
      <name>Kota Mizushima</name>
      <url>https://github.com/kmizu</url>
    </developer>
  </developers>
) 
