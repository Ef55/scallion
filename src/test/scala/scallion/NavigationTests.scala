package scallion

import org.scalatest._

class NavigationTests extends ParsersTestHelper with SyntaxesNavigation with BooleanSyntaxes {
  import Direction._

  val mapFragment = (tru ~ falz).map(orComb)
  val simpleSyntax = mapFragment | epsT
  val predicate = (e: Syntax[_]) => e match {
    case Syntax.Elem(_)           => true
    case Syntax.Disjunction(_, _) => true
    case _                        => false
  }
  val leafsPredicate = (e: Syntax[_]) => e match {
    case Syntax.Elem(_)           => true
    case Syntax.Success(_)        => true
    case _                        => false
  }

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

    assertStructuralEquivalence(simpleSyntax)(zipper1.close)
    assertStructuralEquivalence(simpleSyntax)(zipper2.close)
    assertStructuralEquivalence(simpleSyntax)(zipper3.close)
  }

  it should "allow node replacement" in {
    val zipper = Zipper(simpleSyntax)
    val replacement = ((tru ~ any).map(andComb) ~ falz).map(orComb)

    val result = zipper
      .move(DownLeft, Down, DownRight).replace(replacement)._2
      .move(Up, Up, Up, DownRight).replace(replacement)._2

    val expected = (tru ~ replacement).map(orComb) | replacement

    assertStructuralEquivalence(expected)(result.close)
  }

  def assertWalkIteratorCorrespondance[T](walk: Walk[T], expected: Iterator[Syntax[_]]): Unit = {
    var i = 0
    do {
      walk.next match {
        case Some(value) => {
          assert(expected.hasNext, "Iterator is shorter than walk")
          assertResult(expected.next(), s"at index ${i}")(value)
          i += 1
        }
        case None => assert(!expected.hasNext, "Iterator is longer than walk")
      }
    }while(walk.current.isDefined)
  }

  "Syntax walk" should "work on simple example (post order)" in {
    val walk = Zipper(simpleSyntax).walkPostOrder
    val expected = List(tru, falz, tru ~ falz, mapFragment, epsT, simpleSyntax).iterator

    assertWalkIteratorCorrespondance(walk, expected)
  }

  it should "work on simple example (pre order)" in {
    val walk = Zipper(simpleSyntax).walkPreOrder
    val expected = List(simpleSyntax, mapFragment, tru ~ falz, tru, falz, epsT).iterator

    assertWalkIteratorCorrespondance(walk, expected)
  }

  it should "be filterable" in {
    val walk = Zipper(simpleSyntax).walkPostOrder.filter(predicate)
    val expected = List(tru, falz, simpleSyntax).iterator

    assertWalkIteratorCorrespondance(walk, expected)
  }

  it should "be usable as transformative tool" in {
    val walk = Zipper(simpleSyntax).walkPostOrder.filter(leafsPredicate)
    val expected = (any ~ any).map(orComb) | any

    assertStructuralEquivalence(expected)(walk.mapNexts(_ => any).close)
  }

  it should "be list/iterator convertible" in {
    val walk = Zipper(simpleSyntax).walkPostOrder
    val expected = List(tru, falz, tru ~ falz, mapFragment, epsT, simpleSyntax)

    assertResult(expected)(walk.toList)
    assertWalkIteratorCorrespondance(walk, walk.toIterator)
  }

  it should "be restartable" in {
    val walk = Zipper(simpleSyntax).walkPostOrder.filter(predicate)
    val expected = List(tru, falz, simpleSyntax)

    assertWalkIteratorCorrespondance(walk, expected.iterator)
    assert(walk.current.isEmpty)
    assertWalkIteratorCorrespondance(walk.restart, expected.iterator)
  }

  it should "map every element, regardless of current state" in {
    val walk = Zipper(simpleSyntax).walkPostOrder.filter(leafsPredicate)
    walk.next
    walk.next
    val expected = (any ~ any).map(orComb) | any

    assertStructuralEquivalence(expected)(walk.map(_ => any).close)
  }
}