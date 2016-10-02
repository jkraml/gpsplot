package eu.kraml.cli

import eu.kraml.Main
import eu.kraml.model.InvocationConfig

object CLI {

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
}
