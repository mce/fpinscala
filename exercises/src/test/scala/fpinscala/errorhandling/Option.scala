package fpinscala.errorhandling

import org.scalatest.FunSuite
import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter
import Option._

class OptionSuite extends FunSuite {
    test("map") {
        assert(Some(1).map(_ * 2) == Some(2))
    }

    test("getOrElse") {
        assert(Some(1).getOrElse(2) == 1)
        val none: Option[Int] = None
        assert(none.getOrElse(2) == 2)
    }

    test("flatMap") {
        assert(Some(1).flatMap(value => Some(value * 2)) == Some(2))
        val none: Option[Int] = None
        assert(none.flatMap(_ => Some(1)) == None)
    }

    test("orElse") {
        assert(Some(1).orElse(Some(3)) == Some(1))
    }

    test("map2") {
        assert(map2(Some(1), Some("1"))(_ + _.toInt) == Some(2))
    }

    test("sequence") {
        assert(sequence(List(Some(1), Some(2))) == Some(List(1, 2)))
        assert(sequence(List(Some(1), Some(2), None)) == None)
    }

    test("traverse") {
        assert(traverse(List(1, 2,3))(Some(_)) == Some(List(1, 2, 3)))
    }
} 