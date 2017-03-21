package fruits

abstract class Fruit (val price: BigDecimal = 0.0, val name:String)

object Apple extends Fruit(0.6, "Apple")

object Orange extends Fruit(0.25, "Orange")

trait ShoppingCart {

  def accumulate: Iterable[BigDecimal] => BigDecimal = {
    case l => l.foldLeft[BigDecimal](0)((x, y) => x + y)
  }

  def checkOut: BigDecimal


  def printSum (fruits: Iterable[Fruit],sum: BigDecimal) {
    val format = "[" + (fruits :\ List[String]())((i,l) => i.name :: l).mkString(",") + "] => "  ;
    sum match {
      case s if s < 1.0 => println(format  + s.toString +  "p")
      case s if s >= 1.0 => println( format +  "£" + s.toString)
    }
  }
}

object ShoppingCart {

  private case class ShoppingCartStandard
  (val fruits: Iterable[Fruit]) extends ShoppingCart {

    /** Step 1
      *
      * @return sum of all elements
      */
    override def checkOut = accumulate(fruits.map(_.price));
  }

  private case class ShoppingCartOffer
  (val fruits: Iterable[Fruit]) extends ShoppingCart {
    /**
      * Step 2
      *
      * @return sum based on the following rule
      *         buy one, get one free on Apples
      *         3 for the price of 2 on Oranges
      */

    override def checkOut = {

      val prices = fruits.groupBy(identity).map {

        case (f, l) if f.name == Apple.name =>
          accumulate(l.map(_.price).zipWithIndex
            .collect { case (e, i) if ((i + 1) % 2) != 0 => e })

        case (f, l) if f.name == Orange.name =>
          accumulate(l.map(_.price).zipWithIndex
            .collect { case (e, i) if ((i + 1) % 3) != 0 => e })
      }
      accumulate(prices)
    }
  }

  def apply (strategy: String, l: Iterable[Fruit])={
    strategy match
    {
      case "OFFER" => ShoppingCartOffer(l)
      case _ => ShoppingCartStandard(l)
    }
  }

}

object RunExample extends App {

  def toFruitsList (a: Array[String])= {
    require(a.lengthCompare(0) > 0)
    val argsList = a.toList.map(_.toUpperCase match {
      case "APPLE" => Apple
      case "ORANGE" => Orange
      case _ => new IllegalArgumentException
    }
    ).asInstanceOf[Iterable[Fruit]]
    argsList
  }
  val fruits:Iterable[Fruit] = toFruitsList(args)
  val sc = ShoppingCart("",fruits)
  val co = sc.checkOut
  sc.printSum(fruits,co)

  val scO = ShoppingCart("OFFER",fruits)
  val coO = scO.checkOut
  scO.printSum(fruits,coO)

}


