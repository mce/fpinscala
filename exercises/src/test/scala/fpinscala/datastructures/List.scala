package fpinscala.datastructures

import org.scalatest.FunSuite
import List._

class ListSuite extends FunSuite {
    test("tail should drop the first element") {
        assert(tail(List(1, 2, 3)) == List(2, 3))
    }

    test("tail should return Nil for empty List") {
        assert(tail(List()) == Nil)
        assert(tail(Nil) == Nil)
    }

    test("setHead should update the first element") {
        assert(setHead(List(1, 2, 3), 0) == List(0, 2, 3))
        val emptyList: List[Int]= Nil
        assert(setHead(emptyList, 1) == List(1))
    }

    test("drop should remove first n elements") {
        assert(drop(List(1, 2, 3), 2) == List(3))
        assert(drop(List(1, 2, 3), 3) == Nil)
        assert(drop(List(1, 2, 3), 4) == Nil)
        assert(drop[Int](Nil, 2) == Nil)
    }

    test("dropWhile should remove elements while predicate returns true") {
        assert(dropWhile(List(1, 2, 3))(_ % 2 != 0) == List(2, 3))
        assert(dropWhile(List(0, 2, 4, 6, 3))(_ % 2 == 0) == List(3))
    }

    test("init should remove the last element") {
        assert(init(List(1, 2, 3)) == List(1, 2))
        assert(init(List(13)) == Nil)
        assert(init(Nil) == Nil)
    }

    test("length") {
        assert(length(List(1, 2, 3)) == 3)
        assert(length(List()) == 0)
        assert(length(Nil) == 0)
    }

    test("foldLeft") {
        assert(foldLeft(List(1, 2, 3), 0)(_ + _) == 6)
        assert(foldLeft(List[Int](), 0)(_ + _) == 0)
    }

    test("reverse") {
        assert(reverse(List(1, 2, 3)) == List(3, 2, 1))
        assert(reverse(List()) == Nil)
    }

    test("append2") {
        assert(append2(List(1, 2), List(3, 4)) == List(1, 2, 3, 4))
    }

    test("concatenate") {
        assert(concatenate(List(List(1, 2), List(3, 4))) == List(1,2, 3,4))
    }

    test("plusOne") {
        assert(plusOne(List(1, 2)) == List(2, 3))
    }

    test("doubleToString") {
        assert(doubleToString(List(1, 2)) == List("1.0", "2.0"))
    }

    test("map") {
        assert(map(List(1, 2))(_ * 2) == List(2, 4))
    }

    test("filter") {
        assert(filter(List(1, 2))( _ % 2 == 0) == List(2))
        assert(filter2(List(1, 2))( _ % 2 == 0) == List(2))
    }

    test("flatMap that ...") {
        assert(flatMap(List(1,2,3))(i => List(i,i)) == List(1,1,2,2,3,3))
    }

    test("listSum") {
        assert(listSum(List(1, 2), List(3, 4)) == List(4, 6))
    }

    test("hasSubsequence") {
        assert(hasSubsequence(List(1, 2, 3, 4), List(1, 2)))
        assert(hasSubsequence(List(1, 2, 3, 4), List(2, 3)))
        assert(hasSubsequence(List(1, 2, 3, 4), List(4)))
    }
}