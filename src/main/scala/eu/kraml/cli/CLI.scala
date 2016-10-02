package eu.kraml.cli

import eu.kraml.Main
import eu.kraml.Main.{Progress, ProgressMonitor}
import eu.kraml.model.InvocationConfig

object CLI {

    //apparently has to be defined above main
    implicit private object CliProgressMonitor extends ProgressMonitor {
        override def registerProcess(label: String): Progress = new SimplePrintingProgress(label)
    }

    def main(args: Array[String]): Unit = {
        val cmdLineConfig = try {
            parseArgs(args)
        } catch {
            case e:IllegalStateException => sys.exit(1)
        }

        Main.run(cmdLineConfig)
    }

    private def parseArgs(args: Array[String]): InvocationConfig = {
        CmdLineConfigParser.parse(args, InvocationConfig()) match {
            case Some(parsedConf) => parsedConf
            case None => throw new IllegalStateException("config could not be parsed")
        }
    }

    private class SimplePrintingProgress(val label: String) extends Progress {
        private var currentValue = 0
        private var maxValue = 0

        override def setMaxValue(maxValue: Integer): Unit =
            this.maxValue = maxValue

        override def indicateAbsoluteProgress(newValue: Integer): Unit = {
            currentValue = newValue
            print()
        }

        override def indicateCompletion(): Unit = {}

        override def indicateRelativeProgress(delta: Integer): Unit = {
            currentValue += delta
            print()
        }

        def print(): Unit =
            println(s"$label: $currentValue/$maxValue")
    }
}
