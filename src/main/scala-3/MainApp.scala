import com.github.tototoshi.csv.*
import scala.util.Using
import scala.io.Codec

@main def MainApp(): Unit =
  println("Hotel analysis app starting... (CSV test)")

  // Read CSV from src/main/resources using a lenient codec (CSV has characters that break strict UTF-8)
  val source = scala.io.Source.fromResource("Hotel_Dataset.csv")(Codec.ISO8859)

  Using.resource(CSVReader.open(source)) { reader =>

    val data: List[Map[String, String]] = reader.allWithHeaders()

    if data.nonEmpty then {
      val bookingCountAnalysis = new BookingCountAnalysis()
      bookingCountAnalysis.analyze(data) //run the question
      val bookingPriceAnalysis = new BookingPriceAnalysis()
      bookingPriceAnalysis.analyze(data) //run the question

    } else
      println("No data rows found in CSV.")
  }

trait IndicatorAnalysis {
  def analyze(data:List[Map[String,String]]): Unit
}

object StringToDouble {
  def safeToDouble(str:String): Double =
    try str.toDouble catch {case _: Throwable => 0.0}
}

// Question 1
class BookingCountAnalysis extends IndicatorAnalysis {
  def analyze(data:List[Map[String,String]]): Unit = {
     val countryCounts = data
      // group bookings by origin country
      .groupBy(_("Origin Country"))
      .view
      // count bookings for each country
      .mapValues(_.size).toMap

     val (topCountry, count) = countryCounts.maxBy(_._2) // gets second element of each key value pair

     println("Country with the highest number of bookings:")
     println(s"- Country: $topCountry")
     println(s"- Number of Bookings: $count")
  }
}

//Question 2 a
class BookingPriceAnalysis extends IndicatorAnalysis {
  import StringToDouble._
  def analyze(data:List[Map[String,String]]): Unit = {
    val bookingPriceRow = data.filter(row => row.getOrElse("Booking Price[SGD]", "").nonEmpty) //filer out booking price
    val cheapestBooking = bookingPriceRow.minByOption(row => //find the lowest number then convert to double
      safeToDouble(row.getOrElse("Booking Price[SGD]", ""))
    )
    cheapestBooking.foreach { row =>  //print out the result
      println("Most Economical Hotel")
      println(s"- Hotel Name: ${row.getOrElse("Hotel Name","unknown")}")
      println(s"- Rooms: ${row.getOrElse("Rooms","unknown")}")
      println(s"- Booking Price: ${row.getOrElse("Booking Price[SGD]", "unknown")}")
    }
  }
}