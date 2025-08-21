import java.time.{LocalDate, LocalDateTime}
object Gigasecond {

  def add(startDate: LocalDate): LocalDateTime =
    startDate.atStartOfDay.plusSeconds(1e9.toLong)

  def add(startDateTime: LocalDateTime): LocalDateTime =
    startDateTime.plusSeconds(1e9.toLong)

}
