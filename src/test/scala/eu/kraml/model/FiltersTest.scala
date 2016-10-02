package eu.kraml.model

import java.time.Instant

import eu.kraml.model.Filters.{And, Or, and, or}
import org.scalatest.FlatSpec


//noinspection SimplifyBoolean
class FiltersTest extends FlatSpec {
    val _true = new RecordFilter {
        override def filter(record: Record): Boolean = true
    }

    val _false = new RecordFilter {
        override def filter(record: Record): Boolean = false
    }

    val someRecord = Record(GpsCoordinate(1,1), Instant.now())


    "And" should "evaluate correctly" in {
        assert(And(List(_true, _true, _true)).filter(someRecord) == true)
        assert(And(List(_true, _true, _false)).filter(someRecord) == false)
        assert(And(List(_false, _false, _false)).filter(someRecord) == false)
        assert(And(List()).filter(someRecord) == true)
    }

    "Or" should "evaluate correctly" in {
        assert(Or(List(_true, _true, _true)).filter(someRecord) == true)
        assert(Or(List(_true, _true, _false)).filter(someRecord) == true)
        assert(Or(List(_false, _false, _false)).filter(someRecord) == false)
        assert(Or(List()).filter(someRecord) == false)
    }

    "and(..)" should "build correct filter" in {
        assert(and(_true, _false) == And(List(_true, _false)))
    }

    "or(..)" should "build correct filter" in {
        assert(or(_true, _false) == Or(List(_true, _false)))
    }
}
