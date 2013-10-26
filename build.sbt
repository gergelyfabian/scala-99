name := "scala-99"

version := "1.0"

description := "This is a translation of Ninety-Nine Haskell Problems " +
"which are themselves translations of Ninety-Nine Lisp Problems which are " +
"themselves translations of Ninety-Nine Prolog Problems.\n\n" +
"See http://www.haskell.org/haskellwiki/H-99:_Ninety-Nine_Haskell_Problems " +
"for more info."

scalaVersion := "2.10.2"

scalacOptions ++= Seq("-deprecation", "-feature")

libraryDependencies += "org.scalatest" %% "scalatest" % "1.9.1" % "test"

libraryDependencies += "junit" % "junit" % "4.10" % "test"
