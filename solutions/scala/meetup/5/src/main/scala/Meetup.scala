import Schedule.Schedule
import java.time.{DayOfWeek, LocalDate}

case class Meetup(month: Int, year: Int):

   def day(dayOfWeek: Int, schedule: Schedule): LocalDate =
      val monthSeq = createMonthSeq(year, month).sortBy(_._1)
      val daysByWeek = monthSeq.groupBy { case (_, day) => day }
      val dayOfMonth = schedule match
         case Schedule.Teenth => monthSeq.filter(t => t._1 > 12 & t._1 < 20 & t._2 == dayOfWeek).head._1
         case Schedule.First  => daysByWeek(dayOfWeek).head._1
         case Schedule.Second => daysByWeek(dayOfWeek)(1)._1
         case Schedule.Third  => daysByWeek(dayOfWeek)(2)._1
         case Schedule.Fourth => daysByWeek(dayOfWeek)(3)._1
         case Schedule.Last   => daysByWeek(dayOfWeek).last._1
      LocalDate.of(year, month, dayOfMonth)

   private def createMonthSeq(year: Int, month: Int): Seq[(Int, Int)] =
      val firstDayOfMonth = LocalDate.of(year, month, 1)
      val lastDayOfMonth = firstDayOfMonth.lengthOfMonth()
      (1 to lastDayOfMonth).map { day =>
         val date = firstDayOfMonth.plusDays(day - 1)
         day -> date.getDayOfWeek.getValue
      }

object Schedule extends Enumeration:
   type Schedule = Value
   val Teenth, First, Second, Third, Fourth, Last = Value

object Meetup:
   val Mon: Int = DayOfWeek.MONDAY.getValue
   val Tue: Int = DayOfWeek.TUESDAY.getValue
   val Wed: Int = DayOfWeek.WEDNESDAY.getValue
   val Thu: Int = DayOfWeek.THURSDAY.getValue
   val Fri: Int = DayOfWeek.FRIDAY.getValue
   val Sat: Int = DayOfWeek.SATURDAY.getValue
   val Sun: Int = DayOfWeek.SUNDAY.getValue
