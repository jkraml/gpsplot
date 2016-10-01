package eu.kraml.io

import java.io.File

import scala.xml.XML


object MainConfigReader {

    def readMainConfig(configFile: String): MainConfig = readMainConfig(new File(configFile))

    def readMainConfig(configFile: File): MainConfig = {
        val conf = XML.loadFile(configFile)
        val dataDirPath = (conf \ "dataDir").text
        val cacheDirPath = (conf \ "cacheDir").text
        val configDirPath = (conf \ "configDir").text
        val outputDirPath = (conf \ "outputDir").text

        val configRoot = configFile.getParentFile
        val dataDir = makeAbsolute(configRoot, dataDirPath)
        val cacheDir = makeAbsolute(configRoot, cacheDirPath)
        val configDir = makeAbsolute(configRoot, configDirPath)
        val outputDir = makeAbsolute(configRoot, outputDirPath)

        MainConfig(dataDir, cacheDir, configDir, outputDir)
    }

    def makeAbsolute(root: File, path: String): File = {
        val withoutRoot = new File(path)
        if (withoutRoot.isAbsolute)
            return withoutRoot

        new File(root, path)
    }

    case class MainConfig(
                         dataDir: File,
                         cacheDir: File,
                         configDir: File,
                         outputDir: File
                         )
}
