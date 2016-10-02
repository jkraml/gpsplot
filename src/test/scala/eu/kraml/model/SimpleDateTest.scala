package eu.kraml.model

import java.time.Month.{APRIL, OCTOBER}

import eu.kraml.model.Filters.SimpleDate
import org.scalatest.FlatSpec


class SimpleDateTest extends FlatSpec {

    behavior of "SimpleDate"

    private val apr24 = SimpleDate(24, APRIL)
    private val oct1 = SimpleDate(1, OCTOBER)

    it should "evaluate 'greater than' correctly" in {
        assert(oct1 > apr24)
    }

    it should "evaluate 'greater than or equal' correctly" in {
        assert(oct1 >= apr24)
        assert(oct1 >= oct1)
    }

    it should "evaluate 'less than' correctly" in {
        assert(apr24 < oct1)
    }

    it should "evaluate 'less than or equal' correctly" in {
        assert(apr24 <= oct1)
        assert(apr24 <= apr24)
    }

}
