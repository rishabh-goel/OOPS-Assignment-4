import com.rishabh.hw4.Computation.SetExp.*
import com.rishabh.hw4.Computation.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

import scala.collection.mutable
import scala.collection.mutable.{ListBuffer, Map, Set}

class ComputationTest extends AnyFlatSpec with Matchers{

  // Test 1
  it should "check the working of Else condition in If" in {
    Assign("E", Value(Set(1,5,6))).eval()
    If(Check(Variable("E"), Value(10)),
      Then(Assign("E", Value(Set(1))), Assign("E", Value(Set(10))), Assign("E", Value(Set(100)))),
      Else(Assign("E", Value(Set(1,5,6,10))), Assign("E", Value(Set(1,5,6,1000))))
    ).eval()
    assert(Variable("E").eval() === mutable.HashSet(1, 5, 6, 1000))
  }

  // Test 2
  it should "check single level If condition failing with try catch" in {

    assert(Scope("myScope", Assign("E", Value(Set(1,2,3,4,5))),
      ExceptionClassDef("ExceptionClass", Field("Reason")),
      If(Check(Variable("E"), Value(6)), Then(Insert(Variable("E"), Value(6)), Insert(Variable("E"), Value(7))), Else(ThrowException(ExceptionClassDef("ExceptionClass"), "Check Failed"))),
      Insert(Variable("E"), Value(8)),
      Insert(Variable("E"), Value(9)),
      CatchException(ExceptionClassDef("ExceptionClass"))
    ).eval() === "Check Failed")
  }

  // Test 3
  it should "check single level If condition succeeding with try catch" in {

    assert(Scope("myScope1", Assign("F", Value(Set(1,2,3,4,5))),
      ExceptionClassDef("AnotherExceptionClass", Field("ErrorReason")),
      If(Check(Variable("F"), Value(1)), Then(Insert(Variable("F"), Value(1)), Insert(Variable("F"), Value(7))), Else(ThrowException(ExceptionClassDef("AnotherExceptionClass"), "Check Failed"))),
      Insert(Variable("F"), Value(8)),
      Insert(Variable("F"), Value(9)),
      CatchException(ExceptionClassDef("AnotherExceptionClass"))
    ).eval() === mutable.HashSet(1, 2, 3, 4, 5, 7, 8, 9))
  }

  // Test 4
  it should "check for multi-level If condition" in {
    Assign("E", Value(Set(1,5,6))).eval()
    If(Check(Variable("E"), Value(1)),
      Then(
        If(Check(Variable("E"), Value(7)), Then(Assign("E", Value(Set(1,5,6,7)))), Else())
      ),
      Else(Assign("E", Value(Set(1,5,6,10))), Assign("E", Value(Set(1,5,6,1000))))
    ).eval()
    assert(Variable("E").eval() === mutable.HashSet(1, 5, 6))
  }

  // Test 5
  it should "check if Scala try-catch replaced with Scaset try-catch" in {
    assert(Interface("MyInterface",
      Public(CreateMethod("m4", Params("a", "b"), Assign("c", Cross(Variable("a"), Variable("b"))), Variable("c")))
    ).eval() === "Interface methods can't have body")
  }
}
