scalaVersion := "0.18.0-bin-20190725-8095c23-NIGHTLY"

updateOptions := updateOptions.value.withLatestSnapshots(false)

scalacOptions ++= Seq("-Xmax-inlines", "1000")

enablePlugins(JmhPlugin)

// The config below is to have JMH use the tests instead of the main sources
// for benchmarks. This is sligtly simpler to setup that independent projects.
sourceDirectory in Jmh := (sourceDirectory in Test).value
classDirectory in Jmh := (classDirectory in Test).value
dependencyClasspath in Jmh := (dependencyClasspath in Test).value
compile in Jmh := (compile in Jmh).dependsOn(compile in Test).value
run in Jmh := (run in Jmh).dependsOn(Keys.compile in Jmh).evaluated
