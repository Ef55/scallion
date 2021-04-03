package scallion

import org.scalatest._

class NavigationTests extends ParsersTestHelper with SyntaxesNavigation with BooleanSyntaxes {
  import Zipper._

  val simpleSyntax = (tru ~ falz).map(orComb) | epsT

  "Syntax zipper" should "allow to move around a syntax" in {
    val zipper1 = Zipper(simpleSyntax)
    
    val zipper2 = zipper1.downRight
    assertResult(epsT)(zipper2.focus)

    val zipper3 = zipper2.up.downLeft.down.downRight
    assertResult(falz)(zipper3.focus)
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

  it should "return a (structurally) equivalent syntax once zipped up" in {
    val zipper1 = Zipper(simpleSyntax)
    val zipper2 = zipper1.downRight
    val zipper3 = zipper2.up.downLeft.down.downRight

    assertStructuralEquivalence(simpleSyntax)(zipper1.zipUp)
    assertStructuralEquivalence(simpleSyntax)(zipper2.zipUp)
    assertStructuralEquivalence(simpleSyntax)(zipper3.zipUp)
  }

}