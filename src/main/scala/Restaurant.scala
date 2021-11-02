import Restaurant.totalRestaurant
import com.github.tototoshi.csv.CSVReader

import java.io.File
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object Restaurant extends App {

  /**
   * Trait activity
   */
  trait Activity {
    val name: String
    val city: String
    val country: String
    val lat: Double
    val lon: Double
  }

  /**
   * Restaurant collection inherited for Activity traits
   * @param ranking
   * @param name
   * @param city
   * @param country
   * @param lat
   * @param lon
   * @param stars
   * @param chef
   * @param website
   * @param menu
   * @param currency
   * @param description
   */
  case class Restaurant(ranking: Int, name: String, city: String, country: String, lat: Double, lon: Double, stars: String, chef: String, website: String, menu: Double, currency: String, description: String) extends Activity

  /**
   * Restaurant collection inherited for Activity traits
   * this collection is used to modify the menu price
   * @param ranking
   * @param name
   * @param city
   * @param country
   * @param lat
   * @param lon
   * @param stars
   * @param chef
   * @param website
   * @param menu
   * @param currency
   * @param description
   */
  case class RestaurantWithHigherOrder(ranking: Int, name: String, city: String, country: String, lat: Double, lon: Double, stars: String, chef: String, website: String, menu: Double => Double, currency: String, description: String) extends Activity

  /**
   * Collection use in order to Zip the chief and the restaurants name
   * @param menu
   */
  case class Chief(chief: String)

  /**
   * Collection use in order to Zip the chief and the restaurants name
   * @param name
   */
  case class RestaurantName(name: String)

  /**
   * Method that return the initial menu price
   * @param menu
   * @return
   */
  def initial(menu: Double) = menu

  /**
   * Method that return the initial menu price increased by 50%
   * @param menu
   * @return
   */
  def increase(menu: Double) = (menu + menu * 0.5).toDouble

  /**
   * Usage of CSV Reader to read the dataset
   */
  val reader = CSVReader.open(new File("src/main/02-50BestRestaurants.csv"))
  val dataset = reader.toStream.toList.drop(1)

  /**
   * Initailization of the data taken from the dataset
   */
  val restaurants = dataset.map(resto => Restaurant(resto(0).toInt, resto(1).trim, resto(2), resto(3), resto(4).toDouble, resto(5).toDouble, resto(6), resto(7), resto(8), resto(9).toDouble, resto(10), resto(11)))

  /**
   *
   * 5 data operations
   *
   */

  /**
   * Calculate list lenght with recursion
   *
   * @param list
   * @return
   */
  def recursiveLenghtCalculator(list: List[Restaurant]): Long = list match {
    case Nil => 0
    case head :: tail => {
      val accumulator = recursiveLenghtCalculator(tail)
      1 + accumulator
    }
  }

  /**
   * Reverse the ranked list from 50 to 1
   *
   * @param list
   * @return
   */
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


  /**
   * stars is a function that return string depending on the input stars
   *
   * @param restaurant
   * @return
   */
  def stars(restaurant: Restaurant) = restaurant.stars match {
    case "0" => "No star"
    case "1" => "1 star"
    case "2" => "2 stars"
    case "3" => "3 stars"
    case _ => "Star unknown"
  }

  /**
   * Creation of a new collection with the newest stars value
   */
  val starsRestaurants = restaurants.map(resto => Restaurant(resto.ranking, resto.name, resto.city, resto.country, resto.lat, resto.lon, stars(resto), resto.chef, resto.website, resto.menu, resto.currency, resto.description))
  //restaurant2.foreach(println)


  val totalRestaurant = restaurants.size
  val totalRestaurantFrance = restaurants.count(x => x.country.contains("France")) //number of restaurant 5
  val totalRestaurantSuisse = restaurants.count(x => x.country.contains("Suisse")) //number of restaurant 0
  /**
   * ratioRestaurantPerCountry is a method that gives the ratio of restaurant in the top 50 for a given country
   * using exception handling
   *
   * @param totalRestaurantInACountry
   * @param totalRestaurant
   */
  def ratioRestaurantPerCountry(totalRestaurantInACountry: Int, totalRestaurant: Int) = {
    try {
      val ratio = (totalRestaurantInACountry / totalRestaurant.toFloat) * 100.toFloat
      println("the ratio of restaurant in the top 50 for the chosen country is equals to " + ratio + " %")
    } catch {
      case ex: Exception => println("We got an error" + ex.getMessage)
    }
  }

  //ratioRestaurantPerCountry(totalRestaurantFrance, totalRestaurant)
  //ratioRestaurantPerCountry(totalRestaurant, totalRestaurantSuisse)


  /**
   * Using future in order to calculate the ratio
   *
   * @param overallRestoInACountry
   * @return the ratio of restaurant from your country in the top 50
   */
  def tryDivide(overallRestoInACountry: Int): Future[Int] = {
    if (overallRestoInACountry == 0) {
      Future.failed(new IllegalArgumentException("There is 0 restaurants from your country in the top 50 !"))
    } else {
      Future(overallRestoInACountry / totalRestaurant)
    }
  }

  //println(tryDivide(totalRestaurantFrance))
  //println(tryDivide(totalRestaurantSuisse))


  /**
   * High order function to increase the price of the menu for a specific restaurant
   * @param restaurant
   */
  def increaseMenuPrice(restaurant: Restaurant) {
    val resto1IncreasePrice = RestaurantWithHigherOrder(restaurant.ranking, restaurant.name, restaurant.city, restaurant.country, restaurant.lat, restaurant.lon, restaurant.stars, restaurant.chef, restaurant.website, increase, restaurant.currency, restaurant.description)
    val menu = Seq(resto1IncreasePrice)
    val menuResto1 = menu.map(e => e.menu(restaurant.menu))
    println("The new menu price is " + menuResto1.toString().substring(5, 10))
  }

  //increaseMenuPrice(restaurants(0))

  /**
   * Function to show the average menu price per country in the top 50
   * @param restaurants
   */
  def AverageMenuPerCountry(restaurants: List[Restaurant]) {
    val AverageMenuPerCountry = restaurants.groupBy(m => m.country + " average in " + m.currency).view.mapValues({ gs => gs.map(_.menu).sum / gs.length.toFloat })
    AverageMenuPerCountry.foreach(println)
  }
  //AverageMenuPerCountry(restaurants)


  /**
   *
   * 5 data exploration
   *
   */


  /**
   * Display the french restaurant in the top 10
   */
  val topTenRestaurant = restaurants.filter(x => x.ranking <= 10).filter(x => x.country.equals("France"))
  //topTenRestaurant.foreach(println)


  /**
   * Display all restaurants name with a https website
   */
  val httpsRestaurants = restaurants.groupBy(x => x.website.contains("https")).map(x => x._2)
  //httpsRestaurants.drop(1).map(x => x.map { y => y.name + " " + y.website }.foreach(println))


  /**
   * Filter restaurants name in alphabetic order
   */
  val sortedRestaurants = restaurants.sortBy(_.name).map(_.name)
  //sortedRestaurants.foreach(println)


  /**
   * Display all restaurant that have menu price in EURO
   */
  val euroMenuRestaurants = restaurants.filter(x => x.currency.contains("EUR")).map(x => x.name +
    " have their menu price in " + x.currency)
  //euroMenuRestaurants.foreach(println)


  /**
   * Display the average price for the restaurants with 3 Stars and with the currency EUR
   */
  val restaurants3StarsWithEuro = restaurants.filter(x => x.stars.contains("3")).filter(x => x.currency.contains("EUR"))
  val averagePriceForInput = restaurants3StarsWithEuro.map(x => x.menu).sum / recursiveLenghtCalculator(restaurants3StarsWithEuro)
  //println("Average price for 3 stars EURO restaurants : " + averagePriceForInput.toInt + "\n")


  /**
   * Zip a list containing the restaurants name with a list containing the chief name, in order to have one list containing both
   */
  val restaurantsName = dataset.map(resto => RestaurantName(resto(1).trim))
  val restaurantsChief = dataset.map(resto => Chief(resto(7)))
  val nameZipToChief = restaurantsName.zip(restaurantsChief)
  nameZipToChief.foreach(println)

}
