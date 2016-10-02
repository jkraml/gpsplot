package eu.kraml

import java.io.File

//TODO add option for forcing a complete re-rendering of all files

case class CmdLineConfig(rootConfig: File = null) {
    def assertConfigurationIsComplete(): Unit = {
        if (rootConfig == null)
            throw new IllegalStateException("root configuration path has not been set")
    }
}

object CmdLineConfig {
    private val parser = new scopt.OptionParser[CmdLineConfig]("gpsplot") {
        override def showUsageOnError = true

        arg[File]("<rootConfig>").text("root configuration file").action( (x,c) =>
                c.copy(rootConfig = x)
        )

        help("help").abbr("h")text("print this help text")

    }

    def parse(args: Seq[String], init: CmdLineConfig) = parser.parse(args, init)
}
