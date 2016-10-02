package eu.kraml

import java.io.File

import eu.kraml.model.InvocationConfig

object CmdLineConfigParser {
    private val parser = new scopt.OptionParser[InvocationConfig]("gpsplot") {
        override def showUsageOnError = true

        opt[Unit]("force-render").abbr("f")
            .text("re-render even if nothing has changed").action(
            (_,c) => c.copy(forceRender = true)
        )

        arg[File]("<rootConfig>").text("root configuration file").action( (x,c) =>
                c.copy(rootConfig = x)
        )

        help("help").abbr("h").text("print this help text")
    }

    def parse(args: Seq[String], init: InvocationConfig) = parser.parse(args, init)
}
