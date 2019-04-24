package fpinscala.datastructures

import org.scalatest.FunSuite
import Tree._

class TreeSuite extends FunSuite {

    test("size") {
        assert(size(Branch(Leaf(1), Leaf(2))) == 3)
        assert(size2(Branch(Leaf(1), Leaf(2))) == 3)
    }

    test("maximum") {
        assert(maximum(Branch(Leaf(1), Leaf(2))) == 2)
        assert(maximum2(Branch(Leaf(1), Leaf(2))) == 2)
    }

    test("depth") {
        val l = Branch(Leaf(1), Leaf(2))
        val r = Leaf(3)
        val tree = Branch(l,r)
        assert(depth(tree) == 2)
        assert(depth2(tree) == 2)
    }

    test("map") {
        val l = Branch(Leaf(1), Leaf(2))
        val r = Leaf(3)
        val tree = map(Branch(l,r))(_ * 2)
        val tree2 = map2(Branch(l,r))(_ * 2)
        assert(tree == Branch(Branch(Leaf(2), Leaf(4)), Leaf(6)))
        assert(tree2 == tree)
    }



}