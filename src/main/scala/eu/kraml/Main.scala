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
           (implicit progress: ProgressMonitor = DummyProgressMonitor): Unit = {
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
                println("processing " + name)
                renderer.render(records, conf, modDate)
        }
    }

    private def warnIfSameOutputFilesAreUsed(renderConfigs: Map[String, (RenderConfig, Instant)]): Unit = {
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
                    println(s"Config files $filesString all write their output to $outFile" )
            })
    }

    private def readGpxFiles(dataDir: File)
                            (implicit progress: ProgressMonitor): List[Record] = {
        val files = dataDir.listFiles
            .filter(f => f.isFile && f.getName.endsWith(".gpx")).toList

        val progressObserver = progress.registerProcess("loading gpx files")
        progressObserver.setMaxValue(files.size)

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

    private def readRenderConfigs(configDir: File): Map[String, (RenderConfig, Instant)] = {
        configDir.listFiles
            .filter(f => f.isFile && f.getName.endsWith(".xml"))
            .flatMap( f => {
                val modDate = Instant.ofEpochSecond(f.lastModified())
                    RenderConfigReader.readRenderConfig(f).map(c => f.getName -> (c, modDate))
            })
            .toMap
    }

    trait ProgressMonitor {
        def registerProcess(label: String): Progress
    }

    trait Progress {
        def setMaxValue(maxValue: Integer): Unit
        def indicateProgress(): Unit = indicateRelativeProgress(1)
        def indicateAbsoluteProgress(newValue: Integer): Unit
        def indicateRelativeProgress(delta: Integer): Unit
        def <<(delta: Integer): Unit = indicateRelativeProgress(delta)
        def indicateCompletion(): Unit
    }

    object DummyProgressMonitor extends ProgressMonitor {
        override def registerProcess(label: String): Progress = DummyProgress

        private object DummyProgress extends Progress {
            override def setMaxValue(maxValue: Integer): Unit = {}

            override def indicateProgress(): Unit = {}

            override def indicateAbsoluteProgress(newValue: Integer): Unit = {}

            override def indicateRelativeProgress(delta: Integer): Unit = {}

            override def indicateCompletion(): Unit = {}
        }

    }
}
