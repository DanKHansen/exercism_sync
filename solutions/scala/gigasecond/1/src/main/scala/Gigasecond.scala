import java.time.{LocalDate, LocalDateTime, ZoneOffset}
object Gigasecond {
  val Gs: Long = 1000000000
  def add(startDate: LocalDate): LocalDateTime =
    toLDT(toEpoch(startDate.atStartOfDay()) + Gs)

  def add(startDateTime: LocalDateTime): LocalDateTime =
    toLDT(toEpoch(startDateTime) + Gs)

  private def toEpoch(ldt: LocalDateTime): Long =
    ldt.toEpochSecond(ZoneOffset.UTC)

  private def toLDT(epoch: Long): LocalDateTime =
    LocalDateTime.ofEpochSecond(epoch, 0, ZoneOffset.UTC)
}
