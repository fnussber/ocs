import OcsKeys._

name := "edu.gemini.spModel.core"

unmanagedJars in Compile ++= Seq(
  new File(baseDirectory.value, "../../lib/bundle/osgi.core-4.3.1.jar"),
  new File(baseDirectory.value, "../../lib/bundle/squants_2.11-0.6.1.jar")
)

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-xml" % "1.0.5",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
  "org.scalaz" %% "scalaz-core" % "7.1.6",
  "org.scalaz" %% "scalaz-effect" % "7.1.6")

osgiSettings

ocsBundleSettings

OsgiKeys.bundleActivator := None

OsgiKeys.bundleSymbolicName := name.value

OsgiKeys.dynamicImportPackage := Seq("")

OsgiKeys.exportPackage := Seq(
  "edu.gemini.spModel.core",
  "edu.gemini.spModel.core.osgi")
        
sourceGenerators in Compile += Def.task {
  val ocsVer = ocsVersion.value
  val outDir = (sourceManaged in Compile).value / "edu" / "gemini" / "spModel" / "core"
  val outFile = new File(outDir, ocsVer.sourceFileName)
  outDir.mkdirs
  IO.write(outFile, ocsVer.toClass) // UTF-8 is default
  Seq(outFile)
}.taskValue

initialCommands := "import edu.gemini.spModel.core._, scalaz._, Scalaz._"

scalacOptions in (Compile, doc) ++= Seq(
  "-groups",
  "-sourcepath", (baseDirectory in LocalRootProject).value.getAbsolutePath, 
  "-doc-source-url", "https://github.com/gemini-hlws/ocs/master€{FILE_PATH}.scala" 
)

publishArtifact in (ThisBuild, packageSrc) := true

publishMavenStyle in ThisBuild := true

