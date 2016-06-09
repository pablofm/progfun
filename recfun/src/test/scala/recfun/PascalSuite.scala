package recfun

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PascalSuite extends FunSuite {
  import Main.pascal
    test("pascal: col=0,row=2") {
      assert(pascal(0,2) === 1)
  }

    test("pascal: col=1,row=2") {
      assert(pascal(1,2) === 2)
  }

    test("pascal: col=1,row=3") {
      assert(pascal(1,3) === 3)
  }

  import Main.edge
  test("First Row"){
    assert(edge(0,0) === true)
  }
  test("no edge row"){
    assert(edge(1,2) === false)
  }
  test("edge column"){
    assert(edge(0,1) === true)
  }
  test("no edge column"){
    assert(edge(2,1) === false)
  }
  test("no edge column 2"){
    assert(edge(4,4) === true)
  }
}
