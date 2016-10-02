package eu.kraml.io

import java.io.File
import java.time._

import com.sksamuel.scrimage.Color
import eu.kraml.img.MapCanvas.{Circle, PointStyle}
import eu.kraml.model.Filters._
import eu.kraml.model.{BoundingBox, RecordFilter}

import scala.xml.{Node, NodeSeq, XML}

object RenderConfigReader {

    case class RenderConfig(outputFileName: String,
                            targetWidth: Int,
                            boundingBox: BoundingBox,
                            groups: List[PointGroupConfig])

    case class PointGroupConfig(style: PointStyle,
                                filter: RecordFilter)


    def readRenderConfig(configFile: String): Option[RenderConfig] = readRenderConfig(new File(configFile))

    def readRenderConfig(configFile: File): Option[RenderConfig] = {
        print("reading render config " + configFile.getPath + " ")
        try {
            val result = readRenderConfigInternal(configFile)
            println("done")
            Some(result)
        } catch {
            case e:Exception =>
                println("failed")
                None
        }
    }

    private def readRenderConfigInternal(configFile: File): RenderConfig = {
        val conf = XML.loadFile(configFile)
        val outputFileName = (conf \ "outputFileName").text
        val targetWidthText = (conf \ "targetWidth").text
        val targetWidth = targetWidthText.toInt
        val bounds = conf \ "bounds"
        val east = (bounds \ "east").text.toDouble
        val west = (bounds \ "west").text.toDouble
        val north = (bounds \ "north").text.toDouble
        val south = (bounds \ "south").text.toDouble

        RenderConfig(outputFileName, targetWidth, new BoundingBox(west, east, south, north), readPointGroupConfig(conf \ "groups"))
    }

    private def readPointGroupConfig(groupRoot: NodeSeq): List[PointGroupConfig] = {
        (groupRoot \ "group").map(readPointGroupConfig).toList
    }

    private def readPointGroupConfig(group: Node): PointGroupConfig = {
        val styleNode = group \ "style"
        val styleType = styleNode \@ "type"
        val style = styleType match {
            case "circle" =>
                val diameter = (styleNode \ "diameter").text.toDouble
                val colorString = (styleNode \ "color").text
                val color = readColor(colorString)
                Circle(diameter.toInt, color)
        }
        val filter = readFilterConfig((group \ "filter").head)
        PointGroupConfig(style, filter)
    }

    private val hexRgbPattern = "#([0-9A-Fa-f]{2})([0-9A-Fa-f]{2})([0-9A-Fa-f]{2})".r

    private def readColor(colorCode: String): Color = {
        val hexRgbPattern(red, green, blue) = colorCode.toUpperCase
        Color.apply(
            Integer.parseInt(red, 16),
            Integer.parseInt(green, 16),
            Integer.parseInt(blue, 16)
        )
    }

    private def readFilterConfig(filter: Node): RecordFilter = {
        evaluateFilter((filter \ "_").head)
    }

    val simpleDate = "(\\d?\\d).(\\d?\\d).".r

    private def parseSimpleDate(date: String): SimpleDate = {
        val simpleDate(parsedDay, parsedMonth) = date
        SimpleDate(parsedDay.toInt, Month.of(parsedMonth.toInt))
    }

    private val timeOfDay = "([0-2]?\\d)[:h]([0-5]\\d)".r

    private def parseTimeAsMinuteOfDay(time: String): Int = {
        val timeOfDay(hours, minutes) = time
        hours.toInt*60 + minutes.toInt
    }

    private def evaluateFilter(filter: Node): RecordFilter = {
        filter.label match {
            case "and" =>
                val children = filter \ "_"
                And(children.map(evaluateFilter).toList)
            case "or" =>
                val children = filter \ "_"
                Or(children.map(evaluateFilter).toList)
            case "weekDayIs" =>
                val weekDay = DayOfWeek.valueOf(filter.text.toUpperCase.trim)
                WeekDayIs(weekDay)
            case "monthIs" =>
                val month = Month.valueOf(filter.text.toUpperCase.trim)
                MonthIs(month)
            case "yearIs" =>
                val year = filter.text.toInt
                YearIs(year)
            case "dateIsBetween" =>
                val startString = (filter \ "start").text
                val endString = (filter \ "end").text
                val start = parseSimpleDate(startString)
                val end = parseSimpleDate(endString)
                DateIsBetween(start, end)
            case "timeIsBetween" =>
                val startString = (filter \ "start").text
                val endString = (filter \ "end").text
                val start = parseTimeAsMinuteOfDay(startString)
                val end = parseTimeAsMinuteOfDay(endString)
                TimeIsBetween(start, end)
            case other =>
                throw new IllegalArgumentException("unknown filter type "+other)
        }
    }

}
