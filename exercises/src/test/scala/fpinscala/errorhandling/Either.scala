package fpinscala.errorhandling

import org.scalatest.FunSuite
import Either._

class EitherSuite extends FunSuite {
    test("map") {
        assert(Right(1).map(_ * 2) == Right(2))
        assert(Left(1).map(v => v.toString) == Left(1))
    }

    test("flatMap") {
        assert(Right(1).flatMap(v => Right(v * 2)) == Right(2))
        assert(Left(1).flatMap(v => Right(v.toString)) == Left(1))
        assert(Right(1).flatMap(v => Left(10)) == Left(10))
    }

    test("orElse") {
        assert(Right(1).orElse(Right(10)) == Right(1))
        assert(Left(1).orElse(Right(10)) == Right(10))
    }

    test("map2") {
        assert(Right(1).map2(Right(2))(_ * _) == Right(2))
    }

    test("traverse") {
        val list = List(1, 2, 3, 4)
        def f: Int => Either[String, Int] = i => Right(i) 
        assert(traverse(list)(f) == Right(List(1, 2, 3, 4)))

        def f2: Int => Either[String, Int] = i => if (i <= 2) Right(i) else Left(s"err$i")
        assert(traverse(list)(f2) == Left("err3"))
    }

    test("sequence") {
        val list: List[Either[String, Int]] = List(Right(1), Right(2))
        assert(sequence(list) == Right(List(1, 2)))

        val listWithError = List(Right(1), Left("err"), Left("err2"), Right(2))
        assert(sequence(listWithError) == Left("err"))
    }
}