package scallion

import org.scalatest._

class NavigationTests extends ParsersTestHelper with SyntaxesNavigation with BooleanSyntaxes {
  import Direction._

  val simpleSyntax = (tru ~ falz).map(orComb) | epsT

  "Syntax zipper" should "allow to move around a syntax (method-directed)" in {
    val zipper1 = Zipper(simpleSyntax)
    assert(zipper1.isRoot)
    
    val zipper2 = zipper1.downRight
    assertResult(epsT)(zipper2.focus)
    assert(!zipper2.isRoot)

    val zipper3 = zipper2.up.downLeft.down.downRight
    assertResult(falz)(zipper3.focus)
    assert(!zipper3.isRoot)
  }

  it should "allow to move around a syntax (argument-directed)" in {
    val zipper1 = Zipper(simpleSyntax)
    assert(zipper1.isRoot)
    
    val zipper2 = zipper1.move(DownRight)
    assertResult(epsT)(zipper2.focus)
    assert(!zipper2.isRoot)

    val zipper3 = zipper2.move(Up, DownLeft, Down, DownRight)
    assertResult(falz)(zipper3.focus)
    assert(!zipper3.isRoot)
  }

  it should "throw exceptions in case of illegal moves" in {
    val zipper1 = Zipper(simpleSyntax)
    
    assertThrows[IllegalStateException](zipper1.up)
    assertThrows[IllegalStateException](zipper1.down)

    val zipper2 = zipper1.downLeft.down.downLeft
    assertThrows[IllegalStateException](zipper2.down)
    assertThrows[IllegalStateException](zipper2.downLeft)
    assertThrows[IllegalStateException](zipper2.downRight)
  }

  it should "be able to indicate (in)valid directions" in {
    val zipper1 = Zipper(simpleSyntax)
    assertResult(Set(DownLeft, DownRight))(zipper1.validDirections)
    assertResult(Set(DownLeft, DownRight))(zipper1.validDownDirections)
    assertResult(Set())(zipper1.validLateralDirections)
    assertResult(Set(Up, Down, Left, Right))(zipper1.invalidDirections)
    
    val zipper2 = zipper1.move(DownRight)
    assertResult(Set(Up, Left))(zipper2.validDirections)
    assertResult(Set())(zipper2.validDownDirections)
    assertResult(Set(Left))(zipper2.validLateralDirections)
    assertResult(Set(Down, DownLeft, DownRight, Right))(zipper2.invalidDirections)

    val zipper3 = zipper2.move(Up, DownLeft)
    assertResult(Set(Up, Down, Right))(zipper3.validDirections)
    assertResult(Set(Down))(zipper3.validDownDirections)
    assertResult(Set(Right))(zipper3.validLateralDirections)
    assertResult(Set(DownLeft, DownRight, Left))(zipper3.invalidDirections)
  }

  it should "return a (structurally) equivalent syntax once zipped up" in {
    val zipper1 = Zipper(simpleSyntax)
    val zipper2 = zipper1.downRight
    val zipper3 = zipper2.up.downLeft.down.downRight

    assertStructuralEquivalence(simpleSyntax)(zipper1.zipUp)
    assertStructuralEquivalence(simpleSyntax)(zipper2.zipUp)
    assertStructuralEquivalence(simpleSyntax)(zipper3.zipUp)
  }

}