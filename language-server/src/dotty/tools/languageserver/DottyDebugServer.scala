package dotty.tools
package languageserver

import com.microsoft.java.debug.core._
import com.microsoft.java.debug.core.adapter._
import com.microsoft.java.debug.plugin.internal.JdtVirtualMachineManagerProvider

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

import java.io.{ InputStream, OutputStream }

import java.net._
import java.nio.channels._

import java.util.logging.Level
import java.util.logging.Logger

object DottyDebugServer {
  def newServer(languageServer: DottyLanguageServer): Int = {
    println("Starting DDS")

    val logger = Logger.getLogger("java-debug")
    logger.setLevel(Level.FINEST)

    val providerContext = {
      val c = new ProviderContext
      val sourceLookup = new DottySourceLookUpProvider(languageServer)

      // FIXME: Would like to use this (to support custom env and cwd), but
      // depends on org.eclipse.jdi.internal.VirtualMachineManagerImpl
      // Maybe copy-paste and replace VirtualMachineManagerImpl (what does it do
      // anyway?)

      // val vmManager = new JdtVirtualMachineManagerProvider
      val vmManager = new DottyVirtualMachineManagerProvider
      c.registerProvider(classOf[ISourceLookUpProvider],
        sourceLookup)
      c.registerProvider(classOf[IVirtualMachineManagerProvider],
        vmManager)
      c.registerProvider(classOf[IEvaluationProvider],
        new DottyEvaluationProvider(languageServer, sourceLookup, vmManager.getVirtualMachineManager))
      c.registerProvider(classOf[IHotCodeReplaceProvider],
        new DottyHotCodeReplaceProvider())
      c
    }

    val serverSocket = new ServerSocket(0)
    Runtime.getRuntime().addShutdownHook(new Thread(
      new Runnable {
        def run(): Unit = {
          serverSocket.close()
        }
      }))

    val f = Future {
      println("Waiting for client")
      val clientSocket = serverSocket.accept()
      println(s"Accepted client: $clientSocket")

      val ps = new ProtocolServer(clientSocket.getInputStream, clientSocket.getOutputStream, providerContext)
      ps.run()
    }
    import scala.util.{Success, Failure}
    f.onComplete {
      case Success(s) =>
        println("#success: " + s)
      case Failure(f) =>
        println("#failure")
        f.printStackTrace
    }

    val port = serverSocket.getLocalPort
    println(s"DDS started on port $port")
    port
  }
}
