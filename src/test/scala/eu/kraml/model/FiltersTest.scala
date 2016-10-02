package eu.kraml.model

import java.time.Instant

import eu.kraml.model.Filters._
import org.scalatest.FlatSpec


//noinspection SimplifyBoolean
class FiltersTest extends FlatSpec {
    val someRecord = Record(GpsCoordinate(1,1), Instant.now())


    "And" should "evaluate correctly" in {
        assert(And(List(True(), True(), True())).recordMatches(someRecord) == true)
        assert(And(List(True(), True(), False())).recordMatches(someRecord) == false)
        assert(And(List(False(), False(), False())).recordMatches(someRecord) == false)
        assert(And(List()).recordMatches(someRecord) == true)
    }

    "Or" should "evaluate correctly" in {
        assert(Or(List(True(), True(), True())).recordMatches(someRecord) == true)
        assert(Or(List(True(), True(), False())).recordMatches(someRecord) == true)
        assert(Or(List(False(), False(), False())).recordMatches(someRecord) == false)
        assert(Or(List()).recordMatches(someRecord) == false)
    }

    "and(..)" should "build correct filter" in {
        assert(and(True(), False()) == And(List(True(), False())))
    }

    "or(..)" should "build correct filter" in {
        assert(or(True(), False()) == Or(List(True(), False())))
    }
}
