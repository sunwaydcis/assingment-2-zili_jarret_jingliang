import com.github.tototoshi.csv.*
import scala.util.Using
import scala.io.Codec

@main def MainApp(): Unit =
  println("Hotel analysis app starting... (CSV test)\n")

  // Read CSV from src/main/resources using a lenient codec (CSV has characters that break strict UTF-8)
  val source = scala.io.Source.fromResource("Hotel_Dataset.csv")(Codec.ISO8859)

  Using.resource(CSVReader.open(source)) { reader =>

    val data: List[Map[String, String]] = reader.allWithHeaders()

    if data.nonEmpty then {
      //put all class into List for calling them at the same time
      val runList: List[IndicatorAnalysis] = List(
        new BookingCountAnalysis(),
        new BookingPriceAnalysis(),
        new DiscountAnalysis(),
        new MostProfitableHotel()
      )
      runList.foreach{ item =>
        item.analyze(data)
      }

      //val bookingCountAnalysis = new BookingCountAnalysis()
      //.analyze(data) //run the question
      //val bookingPriceAnalysis = new BookingPriceAnalysis()
      //bookingPriceAnalysis.analyze(data) //run the question

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

object StringToInt {
  def safeToInt(str:String): Int =
    try str.dropRight(1).toInt //remove the % symbol then convert to Int
    catch {case _: NumberFormatException => 0}
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
     println(s"- Number of Bookings: $count\n")
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
      println("Cheapest Booking Price:")
      println(s"- Hotel Name: ${row.getOrElse("Hotel Name","unknown")}")
      println(s"- Rooms: ${row.getOrElse("Rooms","unknown")}")
      println(s"- Booking Price: ${row.getOrElse("Booking Price[SGD]", "unknown")}\n")
    }
  }
}

//Question 2 b
class DiscountAnalysis extends IndicatorAnalysis {
  import StringToInt._
  def analyze(data:List[Map[String,String]]): Unit = {
    val discountRow = data.filter(row => row.getOrElse("Discount", "").nonEmpty) //filer out discount
    val highestDiscount = discountRow.maxByOption(row => //find the highest discount then convert to Int
      safeToInt(row.getOrElse("Discount", ""))
    )
    highestDiscount.foreach { row =>  //print out the result
      println("Highest Discount:")
      println(s"- Hotel Name: ${row.getOrElse("Hotel Name","unknown")}")
      println(s"- Discount: ${row.getOrElse("Discount", "unknown")}\n")
    }
  }
}



// Question 3

class MostProfitableHotel extends IndicatorAnalysis {
  import StringToDouble._
  def analyze(data: List[Map[String, String]]): Unit = {
    val hotelProfits: Map[String, (Double, Double)] = data
      .groupBy(_("Hotel Name"))
      .view
      .mapValues { rows =>
        val totalProfits = rows.map { row =>
          // find most profitable hotel by summing up profits based on hotel name
          val numberOfPeople = safeToDouble(row.getOrElse("No. Of People", "0"))
          val bookingPrice = safeToDouble(row.getOrElse("Booking Price[SGD]", "0"))
          val profitMargin = safeToDouble(row.getOrElse("Profit Margin", "0"))
          numberOfPeople * bookingPrice * profitMargin
        }.sum
        
        val totalVisitors = rows.map(row =>
          StringToDouble.safeToDouble(row.getOrElse("No. Of People", "0"))
        ).sum

        (totalProfits, totalVisitors)
      }
      .toMap
    // returns row with highest profit
    val (mostProfitableHotel, (totalProfit, totalVisitor)) = hotelProfits.maxBy(_._2._1)

    // print results with 2 decimals
    println("Most profitable hotel when considering number of visitors and profit margin:")
    println(s"- Hotel Name: $mostProfitableHotel")
    println(f"- Total Number of Customers: $totalVisitor%.2f")
    println(f"- Total Profit: SGD $totalProfit%.2f\n")
  }
}