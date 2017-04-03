package fruits

abstract class Fruit2 (val price: BigDecimal = 0.0, val name:String = "")

object Apple2 extends Fruit2(0.6, "Apple")

object Orange2 extends Fruit2(0.25, "Orange")

trait ShoppingCart2 {

  def checkOut: BigDecimal

  def printSum (fruits: Iterable[Fruit2],sum: BigDecimal) {
    val format = "[" + (fruits :\ List[String]())((i,l) => i.name :: l).mkString(",") + "] => "
    sum match {
      case s if s < 1.0 => println(format  + s.toString +  "p")
      case s if s >= 1.0 => println( format +  "£" + s.toString)
    }
  }
}

object ShoppingCart2 {


object ShoppingCartStandard2 {
  def apply (fruits: Iterable[Fruit2]):ShoppingCart2 = new ShoppingCartStandard2 (fruits)
  private class ShoppingCartStandard2
  (private val fruits: Iterable[Fruit2]) extends ShoppingCart2 {

    /** Step 1
      *
      * @return sum of all elements
      */
    override def checkOut:BigDecimal = fruits.map(_.price).sum
  }

}

object ShoppingCartOffer2 {
  def apply (fruits: Iterable[Fruit2]):ShoppingCart2 = new ShoppingCartOffer2 (fruits)
  private class ShoppingCartOffer2
  (private val fruits: Iterable[Fruit2]) extends ShoppingCart2 {
    /**
      * Step 2
      *
      * @return sum based on the following rule
      *         buy one, get one free on Apples
      *         3 for the price of 2 on Oranges
      */

    override def checkOut:BigDecimal = {

      val prices = fruits.groupBy(_.name).map {

        case (f, l) if f == Apple2.name =>
          l.map(_.price).zipWithIndex
            .collect { case (e, i) if ((i + 1) % 2) != 0 => e }.sum

        case (f, l) if f == Orange2.name =>
          l.map(_.price).zipWithIndex
            .collect { case (e, i) if ((i + 1) % 3) != 0 => e }.sum
      }
      prices.sum
    }
  }

}

  def apply (strategy: String, l: Iterable[Fruit2]):ShoppingCart2=
    strategy match
    {
      case "OFFER" => ShoppingCartOffer2(l)
      case _ => ShoppingCartStandard2(l)
    }

}

object RunExample2 extends App {

  def toFruitsList (a: Array[String]):Iterable[Fruit2]= {
    require(a.lengthCompare(0) > 0)
    val argsList = a.toList.map(_.toUpperCase match {
      case "APPLE" => Apple2
      case "ORANGE" => Orange2
      case _ => new IllegalArgumentException
    }
    ).asInstanceOf[Iterable[Fruit2]]
    argsList
  }
  val fruits:Iterable[Fruit2] = toFruitsList(args)
  val sc = ShoppingCart2("",fruits)
  val co = sc.checkOut
  sc.printSum(fruits,co)

  val scO = ShoppingCart2("OFFER",fruits)
  val coO = scO.checkOut
  scO.printSum(fruits,coO)

}



