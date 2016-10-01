package eu.kraml.io

import java.io.File
import java.time.DayOfWeek.MONDAY
import java.time.Month.{AUGUST, FEBRUARY, MARCH}

import com.sksamuel.scrimage.Color
import eu.kraml.img.MapCanvas.Circle
import eu.kraml.model.Filters._
import org.scalatest.FlatSpec

class RenderConfigReaderTest extends FlatSpec {
    val confXml = new File(getClass.getResource("/renderConfig.xml").getFile)

    behavior of "RenderConfigReader"

    it should "parse bounds" in {
        val config = RenderConfigReader.readRenderConfig(confXml)

        val bBox = config.boundingBox
        assert(bBox.north == -10)
        assert(bBox.east == 10)
        assert(bBox.south == -20)
        assert(bBox.west == 20)
    }

    it should "read point styles" in {
        val config = RenderConfigReader.readRenderConfig(confXml)

        val firstGroup = config.groups(0)

        assert(firstGroup.style == Circle(5, Color.apply(119, 0, 51)))
    }

    it should "read filters" in {
        val config = RenderConfigReader.readRenderConfig(confXml)

        val firstGroup = config.groups(0)

        val filter =
            or(
                and(
                    WeekDayIs(MONDAY),
                    YearIs(2015)
                ),
                MonthIs(FEBRUARY),
                DateIsBetween(SimpleDate(27, MARCH), SimpleDate(7, AUGUST)),
                TimeIsBetween(7*60, 21*60+1)
            )

        assert(firstGroup.filter == filter)
    }
}
