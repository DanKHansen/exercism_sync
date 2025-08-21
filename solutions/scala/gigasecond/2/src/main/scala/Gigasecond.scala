import java.time.{LocalDate, LocalDateTime}
object Gigasecond {

  def add(startDate: LocalDate): LocalDateTime =
    startDate.atStartOfDay.plusSeconds(10e8.toLong)

  def add(startDateTime: LocalDateTime): LocalDateTime =
    startDateTime.plusSeconds(10e8.toLong)

}
