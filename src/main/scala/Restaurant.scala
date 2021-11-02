import com.github.tototoshi.csv.CSVReader

import java.io.File
import scala.annotation.tailrec
import scala.concurrent.Future
import scala.concurrent.ExecutionException

object Restaurant extends App {

  trait Activity {
    val name: String
    val city: String
    val country: String
    val lat: Double
    val lon: Double

  }

  case class Restaurant(ranking: Int, name: String, city: String, country: String, lat: Double, lon: Double, stars: String, chef: String, website: String, menu: Double, currency: String, description: String) extends Activity

  case class RestaurantWithHigherOrder(ranking: Int, name: String, city: String, country: String, lat: Double, lon: Double, stars: String, chef: String, website: String, menu: Double => Double, currency: String, description: String) extends Activity

  case class Menu(menu: Double)

  case class RestaurantName(name: String)

  def intial(menu: Double) = menu

  def increase(menu: Double) = (menu + menu * 0.5).toDouble

  //CSV Reader
  val reader = CSVReader.open(new File("src/main/02-50BestRestaurants.csv"))
  val dataset = reader.toStream.toList.drop(1)

  //Initailization of the data
  val restaurants = dataset.map(resto => Restaurant(resto(0).toInt, resto(1).trim, resto(2), resto(3), resto(4).toDouble, resto(5).toDouble, resto(6), resto(7), resto(8), resto(9).toDouble, resto(10), resto(11)))
  val restaurantsName = dataset.map(resto => RestaurantName(resto(1).trim))
  val restaurantsMenu = dataset.map(resto => Menu(resto(9).toDouble))

  //Recursion
  def recursiveLenghtCalculator(list: List[Restaurant]): Long = list match {
    case Nil => 0
    case head :: tail => {
      val accumulator = recursiveLenghtCalculator(tail)
      1 + accumulator
    }
  }

  def reverseList(list: List[Restaurant]): List[Restaurant] = {
    def rlRec(result: List[Restaurant], list: List[Restaurant]): List[Restaurant] = {
      list match {
        case Nil => result
        case (x :: xs) => {
          rlRec(x :: result, xs)
        }
      }
    }

    rlRec(Nil, list)
  }

  //reverseList(restaurants).foreach(println)

  //val restaurantsEuro = restaurants.filter(x => x.currency.contains("EUR"))

  //Ranking modification
  /*  def changeRank(restaurant: Restaurant, rankinPos: Int, rankin2 : Int) =
    restaurants.foreach(println)

    val newRestaurantsRanking = restaurants.map(resto => Restaurant(resto.ranking, changeRank(resto, 2, 4), resto.city, resto.country, resto.lat, resto.lon, resto.stars, resto.chef, resto.website, resto.menu, resto.currency, resto.description))
    newRestaurantsRanking.foreach(println)*/


  //Match case
  def stars(restaurant: Restaurant) = restaurant.stars match {
    case "0" => "No star"
    case "1" => "1 star"
    case "2" => "2 stars"
    case "3" => "3 stars"
    case _ => "Star unknown"
  }

  val restaurant2 = restaurants.map(resto => Restaurant(resto.ranking, resto.name, resto.city, resto.country, resto.lat, resto.lon, stars(resto), resto.chef, resto.website, resto.menu, resto.currency, resto.description))
  //restaurant2.foreach(println)

  //Futures



  //Exception handling
  val totalRestaurant = restaurants.size
  val totalRestaurantFrance = restaurants.count(x => x.country.contains("France"))
  val totalRestaurantSuisse = restaurants.count(x => x.country.contains("Suisse"))

  def ratioRestaurantPerCountry(totalRestaurantInACountry: Int, totalRestaurant: Int)= {
    try {
      val ratio = (totalRestaurantInACountry / totalRestaurant) * 100.toFloat

    } catch {
      case ex: Exception => println("We got an error" + ex.getMessage)
    }


  }

  ratioRestaurantPerCountry(totalRestaurant, totalRestaurantFrance)
  ratioRestaurantPerCountry(totalRestaurant, totalRestaurantSuisse)

  // Higher order function
  val test1 = restaurants(0)

  val resto1IncreasePrice = RestaurantWithHigherOrder(test1.ranking, test1.name, test1.city, test1.country, test1.lat, test1.lon, test1.stars, test1.chef, test1.website, increase, test1.currency, test1.description)

  val menu = Seq(resto1IncreasePrice)

  val menuResto1 = menu.map(e => e.menu(test1.menu))
  //println(menuResto1)


  // 5 data exploration
  //Display the french restaurant in the top 10
  val dataExploration1 = restaurants.filter(x => x.ranking <= 10).filter(x => x.country.equals("France"))
  //dataExploration1.foreach(println)


  //Diplay all restaurants name with a https website
  val dataExploration2 = restaurants.groupBy(x => x.website.contains("https")).map(x => x._2)
  //dataExploration2.drop(1).map(x => x.map { y => y.name }.foreach(println))


  //Filter alphabetic order
  val dataExploration3 = restaurants.sortBy(_.name).map(_.name)
  //dataExploration3.foreach(println)


  val dataExploration4 = restaurants.filter(x => x.currency.contains("EUR")).map(x => x.name)

  //dataExploration4.foreach(println)


  //Display the avreage price for the restaurants with 3 Stars and with the currency EUR //Aggregation fonction
  val restaurants3StarsWithEuro = restaurants.filter(x => x.stars.contains("3")).filter(x => x.currency.contains("EUR"))
  val dataExploration5 = restaurants.filter(x => x.stars.contains("3")).filter(x => x.currency.contains("EUR")).map(x => x.menu).sum / recursiveLenghtCalculator(restaurants3StarsWithEuro)

  //println("Prix moyen du menu des restaurants 3 étoiles en EUR : " + dataExploration5.toInt + "\n")

  val dataExploration6 = restaurantsName.zip(restaurantsMenu)
  //dataExploration6.foreach(println)

  val AverageMenuPerCountry = restaurants.groupBy(m => m.country).view.mapValues({ gs => gs.map(_.menu).sum / gs.length.toFloat })
  //AverageMenuPerCountry.foreach(println)
}
