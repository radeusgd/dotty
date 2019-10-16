package dotty.entropy

import java.io.File


object SaneExceptions
  def jarNameToPrefix(name: String): String =
    if name.startsWith("dotty-compiler") then "compiler/src"
    else if name.startsWith("dotty-library") then "library/src"
    else name

  def classFileToPackageSrc(cf: String): String =
    val jarPath :: srcPath :: Nil = cf.split('!').toList
    val jarName    = File(jarPath.replace("jar:file", "")).getName
    val fileFolder = File(srcPath).getParent
    val prefix = jarNameToPrefix(jarName)
    s"$prefix$fileFolder"

  def apply(f: => Unit): Unit = try f catch
    case thr =>
      val trace = thr.getStackTrace.map { t =>
        val cn = t.getClassName.replace('.', '/') + ".class"
        val classFile = this.getClass.getClassLoader.getResource(cn).toString

        val scalaFile = s"${classFileToPackageSrc(classFile)}/${t.getFileName}"
        val classNameShort = t.getClassName.reverse.takeWhile(_ != '.').reverse

        s"\u001b[44;1m$classNameShort.${t.getMethodName}\u001b[0m $scalaFile:${t.getLineNumber}"
      }.mkString("\n    ")
      println(s"\u001b[48;5;88m${thr.getMessage}\u001b[0m\n    $trace")
