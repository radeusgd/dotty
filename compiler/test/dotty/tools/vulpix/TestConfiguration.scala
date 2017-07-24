package dotty
package tools
package vulpix

object TestConfiguration {
  implicit val defaultOutputDir: String = "../out/"

  implicit class RichStringArray(val xs: Array[String]) extends AnyVal {
    def and(args: String*): Array[String] = {
      val argsArr: Array[String] = args.toArray
      xs ++ argsArr
    }
  }

  val noCheckOptions = Array(
    "-pagewidth", "120",
    "-color:never"
  )

  val checkOptions = Array(
    "-Yno-deep-subtypes",
    "-Yno-double-bindings",
    "-Yforce-sbt-phases"
  )

  val classPath = mkClassPath(Jars.dottyTestDeps)

  def mkClassPath(classPaths: List[String], classPathFlag: String = "-classpath"): Array[String] = {
    val paths = classPaths map { p =>
      val file = new java.io.File(p)
      assert(
        file.exists,
        s"""|File "$p" couldn't be found. Run `packageAll` from build tool before
            |testing.
            |
            |If running without sbt, test paths need to be setup environment variables:
            |
            | - DOTTY_LIBRARY
            | - DOTTY_COMPILER
            | - DOTTY_INTERFACES
            | - DOTTY_EXTRAS
            |
            |Where these all contain locations, except extras which is a colon
            |separated list of jars.
            |
            |When compiling with eclipse, you need the sbt-interfaces jar, put
            |it in extras."""
      )
      file.getAbsolutePath
    } mkString (":")

    Array(classPathFlag, paths)
  }

  private val yCheckOptions = Array("-Ycheck:tailrec,resolveSuper,mixin,arrayConstructors,labelDef")

  val basicDefaultOptions = noCheckOptions ++ checkOptions ++ yCheckOptions
  val defaultUnoptimised = basicDefaultOptions ++ classPath
  val defaultOptimised = defaultUnoptimised :+ "-optimise"
  val defaultOptions = defaultUnoptimised
  val allowDeepSubtypes = defaultOptions diff Array("-Yno-deep-subtypes")
  val allowDoubleBindings = defaultOptions diff Array("-Yno-double-bindings")
  val picklingOptions = defaultUnoptimised ++ Array(
    "-Xprint-types",
    "-Ytest-pickler",
    "-Yprintpos"
  )
  val scala2Mode = defaultOptions ++ Array("-language:Scala2")
  val explicitUTF8 = defaultOptions ++ Array("-encoding", "UTF8")
  val explicitUTF16 = defaultOptions ++ Array("-encoding", "UTF16")
  val basicLinkOptimise = basicDefaultOptions ++ Array("-Xlink-optimise")
}
