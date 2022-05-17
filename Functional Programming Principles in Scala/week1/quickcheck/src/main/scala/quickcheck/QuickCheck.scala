package quickcheck

import org.scalacheck.*
import Arbitrary.*
import Gen.*
import Prop.forAll

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap :
  lazy val genHeap: Gen[H] = for {
    n <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(n, h)

  given Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if isEmpty(h) then 0 else findMin(h)
    findMin(insert(m, h)) == m
  }


  property("min1") = forAll { (a: Int) =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    findMin(h) == (a min b)
  }

  property("deleteLast") = forAll { (a: Int) =>
    val h = insert(a, empty)
    deleteMin(h) == empty
  }

  property("minmelded") = forAll { (h1: H, h2: H) =>
    val h = meld(h1, h2)
    val hmin = findMin(h)

    hmin == findMin(h1) || hmin == findMin(h2)
  }

  property("insert into correct position") = forAll { (a: Int, seq: Vector[Int]) =>
    val seqWithA = seq :+ a
    val h = insertAll(seqWithA, empty)

    seqWithA.sorted == collectMinimas(h, Vector())
  }

  def collectMinimas(h: H, minimas: Vector[Int]): Vector[Int] =
    if (isEmpty(h)) minimas
    else collectMinimas(deleteMin(h), minimas :+ findMin(h))

  def insertAll(seq: Vector[Int], h: H): H =
    if (seq.isEmpty) h
    else insertAll(seq.init, insert(seq.last, h))
