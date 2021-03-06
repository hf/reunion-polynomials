name := "reunion-polynomials"

organization := "me.stojan"

version := "0.0.2-SNAPSHOT"

publishMavenStyle := true

scalaVersion := "2.11.7"

resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"

libraryDependencies += "org.scalamock" %% "scalamock-scalatest-support" % "3.2" % "test"

libraryDependencies += "me.stojan" %% "polynome" % "0.0.1-SNAPSHOT"

libraryDependencies += "me.stojan" %% "reunion" % "0.0.2-SNAPSHOT"

homepage := Some(url("https://github.com/hf/reunion-polynomials"))

licenses := Seq("MIT" -> url("https://github.com/hf/reunion/blob-polynomials/master/LICENSE.txt"))

publishArtifact in Test := false

pomIncludeRepository := { _ => false }
