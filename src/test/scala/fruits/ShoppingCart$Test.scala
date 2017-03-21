package fruits


import org.scalatest.FunSuite

/**
  * Created by moobar on 21/03/2017.
  */
class ShoppingCart$Test extends FunSuite {


  test("test apply for ShoppingStandard  ") {
    val testList = List(Apple, Orange)
    val sc = ShoppingCart("", testList)
    assert(sc.getClass.getCanonicalName.
      equalsIgnoreCase("fruits.ShoppingCart.ShoppingCartStandard"))

  }
  test("test apply for ShoppingOffer") {
    val testList = List(Apple, Orange)
    val sc = ShoppingCart("OFFER", testList)
    assert(sc.getClass.getCanonicalName.
      equalsIgnoreCase("fruits.ShoppingCart.ShoppingCartOffer"))
  }
  test ("check sum for shopping standard ")
  {
    val testList = List(Apple, Orange)
    val sc = ShoppingCart("", testList)
    assert(sc.checkOut == 0.85)
  }
  test ("check sum of shopping offer-Apple: by one get one for free")
  {
    val testList = List(Apple,Apple, Orange)
    val sc = ShoppingCart("OFFER", testList)
    assert(sc.checkOut == 0.85)
  }
  test ("check sum of shopping offer-Orange: 3 for the price of 2 ")
  {
    val testList = List(Orange,Orange,Orange,Orange)
    val sc = ShoppingCart("OFFER", testList)
    assert(sc.checkOut == 0.75)
  }
}