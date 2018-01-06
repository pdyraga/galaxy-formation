name := "Galaxy formation process simulation"

version := "0.0.1"

organization := "com.ontheserveride"

scalaVersion := "2.12.4"

resolvers ++= Seq(
  "snapshots" at "http://maven.jzy3d.org/snapshots",
  "releases"  at "http://maven.jzy3d.org/releases"
)


libraryDependencies ++= {
  Seq(
    "ch.epfl.scala" %% "collection-strawman" % "0.8.0",
    "org.specs2" %% "specs2-core" % "4.0.0" % "test"
  )
}
