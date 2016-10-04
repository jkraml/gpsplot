package eu.kraml.cli

import eu.kraml.Main
import eu.kraml.Main.{EventMonitor, Process}
import eu.kraml.model.InvocationConfig

import scala.collection.mutable

object CLI {

    def main(args: Array[String]): Unit = {
        val cmdLineConfig = try {
            parseArgs(args)
        } catch {
            case e:IllegalStateException => sys.exit(1)
        }

        Main.run(cmdLineConfig)(FancyCliEventMonitor)
    }

    private def parseArgs(args: Array[String]): InvocationConfig = {
        CmdLineConfigParser.parse(args, InvocationConfig()) match {
            case Some(parsedConf) => parsedConf
            case None => throw new IllegalStateException("config could not be parsed")
        }
    }

    //TODO refactor this class to make it both cleaner and thread safe
    private object FancyCliEventMonitor extends EventMonitor {
        private val messages = new mutable.Queue[String]
        private var lastPrintWasByProcess = false
        private var currentRunningProcess: Option[SimplePrintingProcess] = None

        override def startProcess(label: String): Process = new SimplePrintingProcess(label)

        override def printMessage(text: String): Unit = {
            messages += text
            triggerOutput()
        }

        private def triggerOutput(): Unit = {
            if (lastPrintWasByProcess) {
                print("\r")
            }
            messages
                .dequeueAll(_ => true)
                .foreach(println)
            currentRunningProcess match {
                case Some(p) => printProcessProgress(p)
                case _ =>
            }
        }

        private def printProcessProgress(proc: SimplePrintingProcess): Unit = {
            print(s"${proc.label}: ${proc.currentValue}/${proc.maxValue}")
            lastPrintWasByProcess = true
            currentRunningProcess = Some(proc)
        }

        private class SimplePrintingProcess(val label: String) extends Process {
            var currentValue = 0
            var maxValue = 0

            override def setMaxProgressValue(maxValue: Integer): Unit =
                this.maxValue = maxValue

            override def indicateAbsoluteProgress(newValue: Integer): Unit = {
                currentValue = newValue
                currentRunningProcess = Some(this)
                triggerOutput()
            }

            override def indicateCompletion(): Unit = {
                currentRunningProcess = None
                lastPrintWasByProcess = false
                println
            }

            override def indicateRelativeProgress(delta: Integer): Unit = {
                currentValue += delta
                currentRunningProcess = Some(this)
                triggerOutput()
            }
        }
    }


}
