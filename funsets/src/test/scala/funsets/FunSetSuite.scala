package funsets

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

/**
  * This class is a test suite for the methods in object FunSets. To run
  * the test suite, you can either:
  *  - run the "test" command in the SBT console
  *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
  */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
  }

  test("singletonSet(1) contains 1") {
    new TestSets {
      assert(contains(s1, 1), "Singleton")
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersect contains only the common elements between two sets") {
    new TestSets {
      val s12 = union(s1, s2)
      val s23 = union(s2, s3)
      val is = intersect(s12, s23)
      assert(contains(is, 2), "Intersect 1")
      assert(!contains(is, 1), "Intersect 2")
      assert(!contains(is, 3), "Intersect 3")
    }
  }

  test("diff") {
    new TestSets {
      val s12 = union(s1, s2)
      val s23 = union(s2, s3)
      val is = diff(s12, s23)
      assert(contains(is, 1), "Diff 1")
      assert(!contains(is, 2), "Diff 2")
      assert(!contains(is, 3), "Diff 3")
    }
  }

  test("filter") {
    new TestSets {
      def even(x: Int) = x % 2 == 0
      val s12 = union(s1, s2)
      val s123 = union(s12, s3)
      val fs = filter(s123, even)
      assert(!contains(fs, 1), "Filter 1")
      assert(contains(fs, 2), "Filter 2")
      assert(!contains(fs, 3), "Filter 3")
    }
  }

  test("forall") {
    new TestSets {
      def odd(x: Int) = x % 2 == 1
      val s13 = union(s1, s3)
      val s12 = union(s1, s2)
      assert(forall(s13, odd), "Forall 1")
      assert(!forall(s12, odd), "Forall 2")
    }
  }

  test("exists") {
    new TestSets {
      def even(x: Int) = x % 2 == 0
      def odd(x: Int) = !even(x)
      val s12 = union(s1, s2)
      val s13 = union(s1, s3)
      val s123 = union(s12, s3)
      assert(exists(s123, odd), "Exists 1")
      assert(exists(s123, even), "Exists 2")
      assert(!exists(s13, even), "Exists 3")
      assert(!exists(s2, odd), "Exists 4")
    }
  }

  test("map") {
    new TestSets {
      def doubl(x: Int) = x*2
      def even(x: Int) = x % 2 == 0
      val s12 = union(s1, s2)
      val s123 = union(s12, s3)
      assert(contains(map(s2, doubl), 4), "Map 1")
      assert(contains(map(s3, doubl), 6), "Map 2")
      assert(forall(map(s123, doubl), even), "Map 3")
    }
  }

}
