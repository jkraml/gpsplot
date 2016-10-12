package eu.kraml

import java.io.File
import java.time.Instant

import eu.kraml.io.{GpxFileReader, MainConfigReader, RenderConfigReader, TileCache}
import eu.kraml.model.{InvocationConfig, Record, RenderConfig}
import eu.kraml.render.RenderingProcess

import scala.collection.mutable

object Main {

    //TODO add logging

    def run(invocationConfig: InvocationConfig)
           (implicit monitor: EventMonitor = SimpleCliEventMonitor): Unit = {
        invocationConfig.assertConfigurationIsComplete()
        val mainConfig = MainConfigReader.readMainConfig(invocationConfig.rootConfig)
        val mainConfigModificationDate = Instant.ofEpochSecond(invocationConfig.rootConfig.lastModified())

        val cache = new TileCache(mainConfig.cacheDir)
        val records: List[Record] = readGpxFiles(mainConfig.dataDir)
        val renderConfigs: Map[String, (RenderConfig, Instant)]= readRenderConfigs(mainConfig.configDir)
        warnIfSameOutputFilesAreUsed(renderConfigs)

        val renderer = new RenderingProcess(cache, mainConfigModificationDate, mainConfig.outputDir, invocationConfig.forceRender)

        renderConfigs foreach {
            case (name, (conf, modDate)) =>
                monitor.printMessage("processing " + name)
                renderer.render(records, conf, modDate)
        }
    }

    private def warnIfSameOutputFilesAreUsed(renderConfigs: Map[String, (RenderConfig, Instant)])
                                            (implicit monitor: EventMonitor): Unit = {
        val outputFileToConfigs = new mutable.HashMap[String, mutable.Set[String]] with mutable.MultiMap[String, String]
        renderConfigs.foreach {
            case (confFile, (config, modDate)) => outputFileToConfigs.addBinding(config.outputFileName, confFile)
        }

        outputFileToConfigs
            .filter({
                case (outFile, configFiles) => configFiles.size > 1
            })
            .foreach( {
                case (outFile, configFiles) =>
                    val filesString = configFiles.mkString(",")
                    monitor.printMessage(s"Config files $filesString all write their output to $outFile" )
                    //TODO this is useless unless we quit
            })
    }

    private def readGpxFiles(dataDir: File)
                            (implicit monitor: EventMonitor): List[Record] = {
        val files = dataDir.listFiles
            .filter(f => f.isFile && f.getName.endsWith(".gpx")).toList

        val progressObserver = monitor.startProcess("loading gpx files")
        progressObserver.setMaxProgressValue(files.size)

        val records = files
            .map(GpxFileReader.read)
            .map(e => {
                progressObserver << 1
                e
            })
            .flatMap(_.head)

        progressObserver.indicateCompletion()

        records
    }

    private def readRenderConfigs(configDir: File)
                                 (implicit monitor: EventMonitor): Map[String, (RenderConfig, Instant)] = {
        //TODO gracefully exit if configDir does not exist
        configDir.listFiles
            .filter(f => f.isFile && f.getName.endsWith(".xml"))
            .flatMap( f => {
                val modDate = Instant.ofEpochSecond(f.lastModified())
                    RenderConfigReader.readRenderConfig(f).map(c => f.getName -> (c, modDate))
            })
            .toMap
    }

    trait EventMonitor {
        def startProcess(label: String): Process
        def printMessage(text: String): Unit
    }

    trait Process {
        def setMaxProgressValue(maxValue: Integer): Unit
        def indicateProgress(): Unit = indicateRelativeProgress(1)
        def indicateAbsoluteProgress(newValue: Integer): Unit
        def indicateRelativeProgress(delta: Integer): Unit
        def <<(delta: Integer): Unit = indicateRelativeProgress(delta)
        def indicateCompletion(): Unit
    }

    object SimpleCliEventMonitor extends EventMonitor {
        override def startProcess(label: String): Process = new SimpleCliProcess(label)

        override def printMessage(text: String): Unit = println(text)

        private class SimpleCliProcess(val label: String) extends Process {
            private var currentValue = 0
            private var maxValue = 0

            override def setMaxProgressValue(maxValue: Integer): Unit =
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

}
