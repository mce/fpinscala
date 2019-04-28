package fpinscala.laziness

import org.scalatest.FunSuite
import Stream._

class StreamSuite extends FunSuite {
    test("toList") {
        assert(Stream(1, 2, 3).toList == List(1, 2, 3))
    }

    test("take") {
        assert(Stream(1, 2, 3).take(2).toList == List(1, 2))
        assert(Stream(1, 2, 3).take(0).toList == List())
        assert(Stream().take(10).toList == List())

        assert(Stream(1, 2, 3).takeUnfold(2).toList == List(1, 2))
        assert(Stream(1, 2, 3).takeUnfold(0).toList == List())
        assert(Stream().takeUnfold(10).toList == List())
    }

    test("drop") {
        assert(Stream(1, 2, 3).drop(1).toList == List(2,3))
        assert(Stream(1, 2, 3).drop(3).toList == List())
        assert(Stream(1, 2, 3).drop(13).toList == List())
        assert(Stream(1, 2, 3).drop(-13).toList == List())
    }

    test("takeWhile") {
        assert(Stream(1, 2, -1, -2, 3).takeWhile(_ > 0).toList == List(1, 2))
        assert(Stream(1, 2, -1, -2, 3).takeWhile(_ < 0).toList == List())

        assert(Stream(1, 2, -1, -2, 3).takeWhileUnfold(_ > 0).toList == List(1, 2))
        assert(Stream(1, 2, -1, -2, 3).takeWhileUnfold(_ < 0).toList == List())
    }

    test("takeWhile2") {
        assert(Stream(1, 2, -1, -2, 3).takeWhile2(_ > 0).toList == List(1, 2))
        assert(Stream(1, 2, -1, -2, 3).takeWhile2(_ < 0).toList == List())
    }

    test("forAll") {
        assert(Stream(1, 2, 3).forAll(_ > 0) == true)
        assert(Stream(1, 0, 3).forAll(_ > 0) == false)
        assert(Stream[Int]().forAll(_ > 0) == true)
    }

    test("headOption") {
        assert(Stream(1, 2, 3).headOption == Some(1))
        assert(empty[Int].headOption == None)
    }

    test("map") {
        assert(Stream(1, 2).map(_ * 2).toList == List(2, 4))
        assert(Stream(1, 2).mapUnfold(_ * 2).toList == List(2, 4))
    }

    test("filter") {
        assert(Stream(1, 2).filter(_ % 2 == 0).toList == List(2))
    }

    test("append") {
        assert(Stream(1, 2).append(cons(3, empty[Int])).toList == List(1, 2, 3))
    }

    test("flatMap") {
        assert(Stream(1, 2).flatMap(e => cons(e * 2, empty[Int])).toList == List(2, 4))
    }

    test("from") {
        assert(Stream.from(10).take(3).toList == List(10, 11, 12))
        assert(Stream.from2(10).take(3).toList == List(10, 11, 12))
    }

    test("constant") {
        assert(Stream.constant(1).take(5).toList == List.fill(5)(1))
        assert(Stream.constant2(1).take(5).toList == List.fill(5)(1))
    }

    test("fib") {
        assert(Stream.fibs.take(5).toList == List(0, 1, 1, 2, 3))
        assert(Stream.fibs2.take(5).toList == List(0, 1, 1, 2, 3))
    }

    test("unfold") {
        assert(Stream.unfold(1)(e => Some(e, e+1)).take(3).toList == List(1, 2, 3))
    }

    test("zipWith") {
        assert(Stream(1,2,3).zipWith(Stream(4, 5, 6))(_ + _).toList == List(5, 7, 9))
    }

    test("startsWith") {
        assert(Stream(1,2,3).startsWith(Stream(1, 2)) == true)
        assert(Stream(1,2,3).startsWith(Stream(0, 1, 2, 3)) == false)
    }

    test("tails") {
        assert(Stream(1, 2, 3).tails.toList.map(_.toList) == List(List(1, 2, 3), List(2, 3), List(3), List()))
    }

    test("scanRight") {
        assert(Stream(1, 2, 3).scanRight(0)(_ + _).toList == List(6,5,3,0))
    }
}