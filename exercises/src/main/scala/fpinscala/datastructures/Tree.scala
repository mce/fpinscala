package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

    //Write a function size that counts the number of nodes (leaves and branches) in a tree. 
    def size[A](tree: Tree[A]): Int = tree match {
        case Leaf(_) => 1
        case Branch(l,r) => 1 + size(l) + size(r)
    }

    //Write a function maximum that returns the maximum element in a Tree[Int]. (Note: In Scala, you can use x.max(y) or x max y to compute the maximum of two integers x and y.) 
    def maximum(t: Tree[Int]): Int = {
        def go(tree: Tree[Int], maxSoFar: Int): Int = tree match {
            case Leaf(value) => value.max(maxSoFar)
            case Branch(l, r) => go(l, maxSoFar).max(go(r, maxSoFar))
        }

        go(t, Int.MinValue)
    }

    //Write a function depth that returns the maximum path length from the root of a tree to any leaf. 
    def depth[A](t: Tree[A]): Int = {

        def go[A](t: Tree[A], depthSoFar: Int): Int = t match {
            case Leaf(_) => depthSoFar
            case Branch(l, r) => go(l, depthSoFar + 1).max(go(r, depthSoFar + 1))
        }

        go(t, 0)
    }

    //Write a function map, analogous to the method of the same name on List, that modifies each element in a tree with a given function. 
    def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
        case Leaf(value) => Leaf(f(value))
        case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }

    //Generalize size, maximum, depth, and map, writing a new function fold that abstracts over their similarities. Reimplement them in terms of this more general function. Can you draw an analogy between this fold function and the left and right folds for List? 
    def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
        case Leaf(value) => f(value)
        case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }

    def map2[A, B](t: Tree[A])(f: A => B): Tree[B] = fold[A, Tree[B]](t)(v => Leaf(f(v)))( (l, r) => Branch(l, r))
    def depth2[A](t: Tree[A]): Int = fold(t)(a => 0)(1 + _ + _)
    def maximum2(t: Tree[Int]): Int = fold(t)(a => a)((x,y) => x max y)
    def size2[A](t: Tree[A]): Int = fold(t)(a => 1)(1 + _ + _)
}