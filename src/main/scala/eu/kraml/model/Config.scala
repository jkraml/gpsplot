package eu.kraml.model

import java.io.File

import eu.kraml.img.MapCanvas.PointStyle


case class InvocationConfig(rootConfig: File = null, forceRender: Boolean = false) {
    def assertConfigurationIsComplete(): Unit = {
        if (rootConfig == null)
            throw new IllegalStateException("root configuration path has not been set")
    }
}

case class MainConfig(dataDir: File,
                      cacheDir: File,
                      configDir: File,
                      outputDir: File)

case class RenderConfig(outputFileName: String,
                        targetWidth: Int,
                        boundingBox: BoundingBox,
                        groups: List[PointGroupConfig])

case class PointGroupConfig(style: PointStyle,
                            filter: RecordFilter)


