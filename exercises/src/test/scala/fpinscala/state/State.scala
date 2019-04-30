package fpinscala.state

import org.scalatest.FunSuite

class StateSuite extends FunSuite {
    test("nonNegativeInt") {
        for (i <- -100 to 100) {
            assert(RNG.nonNegativeInt(RNG.Simple(i))._1 >= 0)
        }
    }

    test("double"){
        for(i <- -100 to 100) {
            val v = RNG.double(RNG.Simple(i))._1
            assert(v >= 0 && v < 1)
        }
    }

    test("ints") {
        val (list, r) = RNG.ints(10)(RNG.Simple(0))
        assert(list.size == 10)
    }

    test("sequence") {
        val rng = RNG.ints(10)(_)
        val list = List(rng)
        assert(RNG.sequence(list)(RNG.Simple(0)) == RNG.sequence(list)(RNG.Simple(0)))
    }

    test("simulateMachine") {
        val inputs = List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn, Turn, Turn)
        val result = State.simulateMachine(inputs).run(Machine(true, 5, 10))
        val (coin, candies) = result._1
        assert(14 == coin)
        assert(1 == candies)
    }
}