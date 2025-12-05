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
        new MostEconomicalHotelAnalysis(),
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
    try
      if(str.contains("%")) {
        str.dropRight(1).toDouble
      } else {
        str.toDouble
      }
        catch {case _: Throwable => 0.0}
}

object NormalizationCalculation {
  def normalizeHigherBetter(values: List[Double]): List[Double] = {
    if (values.isEmpty) return List()
    val min = values.min
    val max = values.max
    val range = max - min
    if (range == 0) values.map(_ => 100.0) // All get max score if same
    else values.map(value => ((value - min) / range * 100))
  }

  def normalizeLowerBetter(values: List[Double]): List[Double] = {
    if (values.isEmpty) return List()
    val min = values.min
    val max = values.max
    val range = max - min
    if (range == 0) values.map(_ => 100.0) // All get max score if same
    else values.map(value => 100 - ((value - min) / range * 100))
  }
}

//not used for now
/*object StringToInt {
  def safeToInt(str:String): Int =
    try str.dropRight(1).toInt //remove the % symbol then convert to Int
    catch {case _: NumberFormatException => 0}
}*/

// Question 1
class BookingCountAnalysis extends IndicatorAnalysis {
  def analyze(data:List[Map[String,String]]): Unit = {
     val countryCounts = data
      // group bookings by origin country
      .groupBy(_("Destination Country"))
      .view
      // count bookings for each country
      .mapValues(_.size).toMap

     val (topCountry, count) = countryCounts.maxBy(_._2) // gets second element of each key value pair

     println("Country with the highest number of bookings:")
     println(s"- Country: $topCountry")
     println(s"- Number of Bookings: $count\n")
  }
}

//New Question 2
class MostEconomicalHotelAnalysis extends IndicatorAnalysis {

  import StringToDouble._
  import NormalizationCalculation._

  /* --original version

  def analyze(data: List[Map[String, String]]): Unit = {
    val scoresForCriteria: Map[(String, String, String), List[(Double, Double, Double, String, String, String)]] =
      data.groupBy(row => (row("Hotel Name"), row("Destination Country"), row("Destination City"))).view.mapValues { rows =>
        rows.map { row =>
          val hotelName = row("Hotel Name")
          val country = row("Destination Country")
          val city = row("Destination City")

          val bookingPrice = safeToDouble(row.getOrElse("Booking Price[SGD]", "0")) //filer out booking price
          val RoomsNum = safeToDouble(row.getOrElse("Rooms", "1")) //filer out Rooms
          val numberOfDaysBooked = safeToDouble(row.getOrElse("No of Days", "1"))

          val bookingPricePerRoomPerDay = bookingPrice / (numberOfDaysBooked * RoomsNum)

          val discount = row.get("Discount")
            .map(StringToDouble.safeToDouble) // remove %
            .map(_ / 100.0) // convert to decimal
            .getOrElse(0.0)

          val profitMargin = safeToDouble(row.getOrElse("Profit Margin", "0"))

          (bookingPricePerRoomPerDay, discount, profitMargin, hotelName, country, city)
        }
      }.toMap

    // lower is better
    val allPrices = scoresForCriteria.values.flatMap(_.map(_._1)).toList
    val normalizedPrices = normalizeLowerBetter(allPrices)

    // higher is better
    val allDiscounts = scoresForCriteria.values.flatMap(_.map(_._2)).toList
    val normalizedDiscounts = normalizeHigherBetter(allDiscounts)

    // lower is better
    val allMargins = scoresForCriteria.values.flatMap(_.map(_._3)).toList
    val normalizedMargins = normalizeLowerBetter(allMargins)

    val allNormalizedScores = normalizedPrices
      .zip(normalizedDiscounts)
      .zip(normalizedMargins)
      .zip(scoresForCriteria.values.flatten.toList)
      .map { case (((priceScore, discountScore), marginScore), (_,_,_,hotelName, country, city)) =>
        (priceScore, discountScore, marginScore, hotelName, country, city)
      }

    val averageResult: List[(Double, String, String, String)] = allNormalizedScores.map { case (priceScore, discountScore, marginScore, hotelName, country, city) =>
      val averageScore = (priceScore + discountScore + marginScore) / 3.0
      (averageScore, hotelName, country, city)
    }

    val highestAverageScore = averageResult.maxBy(_._1)._1  // Get highest average
    val highestAverageHotels = averageResult.filter(_._1 == highestAverageScore)  // Filter hotels that match that score

    println("Most Economical Hotel Analysis Result")
    highestAverageHotels.foreach { case (score, hotelName, country, city) =>
      println(f"- Hotel Name: $hotelName")
      println(f"- Country: $country")
      println(f"- City: $city")
      println(f"- Score: $score%.2f\n")
    }
  }
}


   */
  // version 1 - all avg


  def analyze(data: List[Map[String, String]]): Unit = {
    val scoresForCriteria: Map[(String, String, String), (Double, Double, Double, String, String, String)] =
      data.groupBy(row => (row("Hotel Name"), row("Destination Country"), row("Destination City"))).view.mapValues { rows =>
        val hotelName = rows.head("Hotel Name")
        val country = rows.head("Destination Country")
        val city = rows.head("Destination City")

        val bookingPricePerRoomPerDay = rows.map { row =>
          val bookingPrice = safeToDouble(row.getOrElse("Booking Price[SGD]", "0")) //filer out booking price
          val RoomsNum = safeToDouble(row.getOrElse("Rooms", "1")) //filer out Rooms
          val numberOfDaysBooked = safeToDouble(row.getOrElse("No of Days", "1"))

          bookingPrice / (numberOfDaysBooked * RoomsNum)
        }.sum / rows.size

        val discount = rows.map { row =>
          row.get("Discount")
            .map(StringToDouble.safeToDouble) // remove %
            .map(_ / 100.0) // convert to decimal
            .getOrElse(0.0)
        }.sum / rows.size

        val profitMargin = rows.map { row =>
          safeToDouble(row.getOrElse("Profit Margin", "0"))
        }.sum / rows.size

        (bookingPricePerRoomPerDay, discount, profitMargin, hotelName, country, city)

    }.toMap

    // lower is better
    val allPrices = scoresForCriteria.values.map(_._1).toList
    val normalizedPrices = normalizeLowerBetter(allPrices)

    // higher is better
    val allDiscounts = scoresForCriteria.values.map(_._2).toList
    val normalizedDiscounts = normalizeHigherBetter(allDiscounts)

    // lower is better
    val allMargins = scoresForCriteria.values.map(_._3).toList
    val normalizedMargins = normalizeLowerBetter(allMargins)

    val allNormalizedScores = normalizedPrices
      .zip(normalizedDiscounts)
      .zip(normalizedMargins)
      .zip(scoresForCriteria.values.toList)
      .map { case (((priceScore, discountScore), marginScore), (_, _, _, hotelName, country, city)) =>
        (priceScore, discountScore, marginScore, hotelName, country, city)
      }

    val averageResult: List[(Double, String, String, String)] = allNormalizedScores.map { case (priceScore, discountScore, marginScore, hotelName, country, city) =>
      val averageScore = (priceScore + discountScore + marginScore) / 3.0
      (averageScore, hotelName, country, city)
    }

    val highestAverageScore = averageResult.maxBy(_._1)._1 // Get highest average
    val highestAverageHotels = averageResult.filter(_._1 == highestAverageScore) // Filter hotels that match that score

    println("Most Economical Hotel Analysis Result")
    highestAverageHotels.foreach { case (score, hotelName, country, city) =>
      println(f"- Hotel Name: $hotelName")
      println(f"- Country: $country")
      println(f"- City: $city")
      println(f"- Score: $score%.2f\n")
    }
  }
}



// Question 3
class MostProfitableHotel extends IndicatorAnalysis {
  import StringToDouble._
  import NormalizationCalculation._

  def analyze(data: List[Map[String, String]]): Unit = {
    val hotelProfits: Map[(String, String, String), (Double, Double, String, String, String)] =
      data.groupBy(row => (row("Hotel Name"), row("Destination Country"), row("Destination City"))).view.mapValues { rows =>
        val hotelName = rows.head("Hotel Name")
        val country = rows.head("Destination Country")
        val city = rows.head("Destination City")

        // get total visitors based on country, city, hotel
        val totalVisitors = rows.map(row => safeToDouble(row.getOrElse("No. Of People", "0"))).sum
        // get average profit margin by dividing the total number of records
        val avgProfitMargin = rows.map(row => safeToDouble(row.getOrElse("Profit Margin", "0"))).sum / rows.size

        (totalVisitors, avgProfitMargin, hotelName, country, city)

      }.toMap

    // converting total visitors and average profitMargin to scores
    val normalizedVisitors = normalizeHigherBetter(hotelProfits.values.map(_._1).toList)
    val normalizedProfitMargin = normalizeHigherBetter(hotelProfits.values.map(_._2).toList)

    // zip lists together
    val combinedListForHotelProfits = normalizedVisitors
      .zip(normalizedProfitMargin)
      .zip(hotelProfits.values.toList)
      .map { case ((visitorScore, profitMarginScore), (_,_,hotelName, country, city)) =>
        (visitorScore, profitMarginScore, hotelName, country, city)
      }

    // get final score for all hotels
    val finalScores: List[(Double, String, String, String)] = combinedListForHotelProfits.map { case (visitorScore, profitMarginScore, hotelName, country, city) =>
      val averageScores = (visitorScore + profitMarginScore) / 2.0
      (averageScores, hotelName, country, city)
    }

    // get hotel name with highest score
    val highestProfitScore = finalScores.maxBy(_._1)._1
    val highestProfitHotel = finalScores.filter(_._1 == highestProfitScore)


    // print results with 2 decimals
    println("Most profitable hotel when considering number of visitors and profit margin:")
    highestProfitHotel.foreach { case (finalScore, hotelName, country, city) =>
      println(s"- Hotel Name: $hotelName")
      println(f"- Country: $country")
      println(f"- City: $city")
      println(f"- Combined Score: $finalScore%.2f\n")
    }
  }
}