package eu.kraml.model

import java.time.temporal.ChronoField._
import java.time.{DayOfWeek, Month}

sealed trait RecordFilter {
    def filter(record: Record): Boolean
}

object Filters {

    case class YearIs(year: Int) extends RecordFilter {
        override def filter(record: Record): Boolean =
            record.timestamp.get(YEAR) == year
    }

    case class MonthIs(month: Month) extends RecordFilter {
        override def filter(record: Record): Boolean =
            record.timestamp.get(MONTH_OF_YEAR) == month.getLong(MONTH_OF_YEAR)
    }

    case class WeekDayIs(day: DayOfWeek) extends RecordFilter {
        override def filter(record: Record): Boolean =
            record.timestamp.get(DAY_OF_WEEK) == day.getLong(DAY_OF_WEEK)
    }

    case class Or(clauses: List[RecordFilter]) extends RecordFilter {
        override def filter(record: Record): Boolean = clauses.exists(_.filter(record))
    }

    case class SimpleDate(day: Int, month: Month) {

        private val monthAsInt = month.get(MONTH_OF_YEAR)

        def >(other: SimpleDate): Boolean = {
            this.monthAsInt > other.monthAsInt ||
                (this.monthAsInt == other.monthAsInt && this.day > other.day    )
        }

        def >=(other: SimpleDate): Boolean = {
            this == other || this > other
        }

        def <(other: SimpleDate): Boolean = {
            this.monthAsInt < other.monthAsInt ||
                (this.monthAsInt == other.monthAsInt && this.day < other.day    )
        }

        def <=(other: SimpleDate): Boolean = {
            this == other || this < other
        }
    }

    case class DateIsBetween(start: SimpleDate, end: SimpleDate) extends RecordFilter {
        override def filter(record: Record): Boolean = {
            val timestamp = record.timestamp
            val recordDate = SimpleDate(timestamp.get(DAY_OF_MONTH), Month.of(timestamp.get(DAY_OF_MONTH)))
            recordDate >= start && recordDate <= end
        }
    }

    //TODO introduce isBefore(Instant) and isAfter(Instant)

    case class TimeIsBetween(startAsMinuteOfDay: Int, endAsMinuteOfDay: Int) extends RecordFilter {
        override def filter(record: Record): Boolean = {
            val minuteOfDay = record.timestamp.get(MINUTE_OF_DAY)
            minuteOfDay >= startAsMinuteOfDay && minuteOfDay <= endAsMinuteOfDay
        }
    }

    def or(clauses: RecordFilter*) = Or(clauses.toList)

    case class And(clauses: List[RecordFilter]) extends RecordFilter {
        override def filter(record: Record): Boolean = clauses.forall(_.filter(record))
    }

    def and(clauses: RecordFilter*) = And(clauses.toList)

}
