name := "fby_examples"

version := "0.1"

scalaVersion := "2.13.1"

lazy val V = new {
  val cats            = "2.0.0"
}

libraryDependencies ++= Seq(
  "org.typelevel"          %% "cats-core" % V.cats
)

