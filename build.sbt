name := "findclass"

version := "1.0-SNAPSHOT"

scalaVersion := "2.9.0-1"

libraryDependencies ++= Seq(
  "com.novocode" % "junit-interface" % "0.7" % "test"
)

scalacOptions ++= Seq("-unchecked", "-deprecation")

seq(ProguardPlugin.proguardSettings :_*)

proguardOptions += keepMain("findclass.Main")

proguardOptions += "-dontnote scala.Enumeration"
