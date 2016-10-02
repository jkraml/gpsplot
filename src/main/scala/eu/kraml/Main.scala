package eu.kraml

import java.io.File
import java.time.Instant

import eu.kraml.io.{GpxFileReader, MainConfigReader, RenderConfigReader, TileCache}
import eu.kraml.model.{InvocationConfig, Record, RenderConfig}
import eu.kraml.render.Renderer

import scala.collection.mutable

object Main {

    //TODO add logging

    def run(invocationConfig: InvocationConfig): Unit = {
        invocationConfig.assertConfigurationIsComplete()
        val mainConfig = MainConfigReader.readMainConfig(invocationConfig.rootConfig)
        val mainConfigModificationDate = Instant.ofEpochSecond(invocationConfig.rootConfig.lastModified())

        val cache = new TileCache(mainConfig.cacheDir)
        val records: List[Record] = readGpxFiles(mainConfig.dataDir)
        val renderConfigs: Map[String, (RenderConfig, Instant)]= readRenderConfigs(mainConfig.configDir)
        warnIfSameOutputFilesAreUsed(renderConfigs)

        val renderer = new Renderer(cache, mainConfigModificationDate, mainConfig.outputDir, invocationConfig.forceRender)
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

    private def readGpxFiles(dataDir: File): List[Record] = {
        dataDir.listFiles
            .filter(f => f.isFile && f.getName.endsWith(".gpx"))
            .map( GpxFileReader.read)
            .flatMap(_.head)
            .toList
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

}
