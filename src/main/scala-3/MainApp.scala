import com.github.tototoshi.csv.*
import scala.util.Using
import scala.io.Codec

@main def MainApp(): Unit =
  println("Hotel analysis app starting... (CSV test)")

  // Read CSV from src/main/resources using a lenient codec (CSV has characters that break strict UTF-8)
  val source = scala.io.Source.fromResource("Hotel_Dataset.csv")(Codec.ISO8859)

  Using.resource(CSVReader.open(source)) { reader =>
    val rows = reader.all()

    if rows.nonEmpty then
      println(s"Header: ${rows.head}")

    if rows.size > 1 then
      println(s"First data row: ${rows(1)}")
    else
      println("No data rows found in CSV.")
  }