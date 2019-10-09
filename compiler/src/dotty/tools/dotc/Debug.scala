package dotty

import java.io.{ File, PrintWriter, FileWriter }
import scala.collection.mutable.{ Map => MutMap }
import dotty.tools.dotc.core.Contexts.Context

object Debug
  val writtenFiles: MutMap[String, File] = MutMap.empty
  var lastEncounteredName = ""

  def writeToFile(str: String)(implicit ctx: Context): Unit =
    val outDirPath = ctx.settings.YlogToFile.value

    var targetName = ctx.source.name
    if targetName.isEmpty && lastEncounteredName.nonEmpty then targetName = lastEncounteredName
    lastEncounteredName = targetName

    if outDirPath != null && targetName.nonEmpty then
      val outDir = File(outDirPath)
      if !outDir.exists then outDir.mkdirs()

      val runId      = outDir.listFiles.filter(_.getName.dropWhile(_ != '-').drop(1) == targetName).length
      val runIdStr   = "%03d".format(runId)
      val targetFile = writtenFiles.getOrElseUpdate(targetName, File(outDir, s"$runIdStr-$targetName"))

      val writer = PrintWriter(FileWriter(targetFile, true))
      try writer.println(str) finally writer.close()
