package eu.kraml.io

import java.io.File
import java.net.URL
import java.util.concurrent.TimeUnit

import com.sksamuel.scrimage.Image
import eu.kraml.Main.EventMonitor
import eu.kraml.io.TileCache._
import eu.kraml.model.TileDescriptor

import scala.sys.process._
import scala.util.Random

//TODO add "soft" timeout for files
//TODO make file timeout configurable

class TileCache(private val cacheFolder: File)
               (implicit eventMonitor: EventMonitor) {
    if (!cacheFolder.isDirectory)
        throw new IllegalArgumentException(s"'$cacheFolder' is not a folder")

    private val monitor = eventMonitor

    def get(tileDescriptor: TileDescriptor): Image = {
        val tileFile = new File(cacheFolder, fileName(tileDescriptor))
        if (needsToDownload(tileFile)) {
            download(tileDescriptor, tileFile)
        }
        Image.fromFile(tileFile)
    }

    def needsToDownload(tileFile: File): Boolean = {
        if (!tileFile.exists())
            return true
        if (tileFile.length() == 0) //that's what aborted downloads usually look like
            return true

        tileFile.lastModified() < (System.currentTimeMillis() - MAX_AGE)
    }

    def download(tileDescriptor: TileDescriptor, tileFile: File) = {
        monitor.printMessage("downloading "+tileDescriptor)
        // weird scala syntax to magically download stuff to a file
        downloadUrl(tileDescriptor) #> tileFile !!
    }

}

object TileCache {
    private val IMAGE_EXTENSION = "png"
    private val MAX_AGE = TimeUnit.DAYS.toMillis(14)
    private[this] val RANDOMIZER = new Random()

    private def fileName(tileDescriptor: TileDescriptor): String = {
        val (x, y, zoom) = unpack(tileDescriptor)
        s"x${x}_y${y}_z$zoom.$IMAGE_EXTENSION"
    }

    private def downloadUrl(tileDescriptor: TileDescriptor): URL = {
        val host = randomChoice("a", "b", "c")
        val (x, y, zoom) = unpack(tileDescriptor)
        val url = s"http://$host.tile.openstreetmap.org/$zoom/$x/$y.$IMAGE_EXTENSION?layers=Q"
        new URL(url)
    }

    private[this] def randomChoice(strings:String*): String = {
        strings(RANDOMIZER.nextInt(strings.size))
    }

    private[this] def unpack(tileDescriptor: TileDescriptor): (Int, Int, Int) = {
        (tileDescriptor.x, tileDescriptor.y, tileDescriptor.zoom)
    }
}
