/* Copyright 2020 EPFL, Lausanne
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package scallion

import scala.collection.mutable.Queue

import scallion.util.internal.enums._

/** Provides pretty printing capabilites to syntaxes.
  *
  * @group pretty
  */
trait PrettyPrinting { self: Syntaxes with Parsing =>

  /** Pretty printer of values.
    *
    * @group pretty
    */
  trait PrettyPrinter[A] {

    /** Returns an iterator over token sequences that produce a given value.
      *
      * @group pretty
      */
    def apply(value: A): Iterator[Iterator[Token]]
  }

  /** Factory of pretty printers.
    *
    * @group pretty
    */
  object PrettyPrinter {
    import Syntax._

    /** Returns a pretty printer for a syntax.
      * The resulting pretty printer returns values in order of increasing length.
      *
      * This method relies on the presence of inverse transformations
      * in the various parts of the argument syntax.
      * Be sure to provide an inverse function as argument to the corresponding
      * optionnal parameter of the various combinators used to describe the syntax,
      * where applicable.
      *
      * @param syntax The syntax used to pretty print.
      *
      * @group pretty
      */
    def apply[A](syntax: Syntax[A]): PrettyPrinter[A] = { (value: A) =>

      var recs: Map[(Any, Int), EnvEntry[Token]] = Map()
      val probe: Probe = new Probe

      def go[B](syntax: Syntax[B], value: B, subscriber: Tree[Token] => Unit): Cell = {
        if (!syntax.isProductive) {
          EmptyCell
        }
        else {
          syntax match {
            case Success(_) => EmptyCell
            case Failure() => EmptyCell
            case Elem(kind) => if (getKind(value) == kind) new ElemCell(value, subscriber) else EmptyCell
            case Disjunction(left, right) => {
              val res = new DisjunctionCell(subscriber)
              val l = go(left, value, (tree: Tree[Token]) => res.informLeft(tree))
              val r = go(right, value, (tree: Tree[Token]) => res.informRight(tree))
              res.setLeftCell(l)
              res.setRightCell(r)
              res
            }
            case Sequence(left, right) => {
              val leftValue ~ rightValue = value
              val res = new SequenceCell(
                left.nullable == Some(leftValue),
                right.nullable == Some(rightValue),
                probe, subscriber)
              val l = go(left, leftValue, (tree: Tree[Token]) => res.informLeft(tree))
              val r = go(right, rightValue, (tree: Tree[Token]) => res.informRight(tree))
              res.setLeftCell(l)
              res.setRightCell(r)
              res
            }
            case Transform(_, inv, inner) => {
              val invValues = inv(value)
              if (invValues.size == 0) {
                // No inverse.
                EmptyCell
              }
              else if (invValues.size == 1) {
                // Unique inverse.
                go(inner, invValues(0), subscriber)
              }
              else {
                // Multiple inverses
                val sharedCell: SharedCell[Token] = new SharedCell(subscriber)

                for (invValue <- invValues) {
                  val cell = go(inner, invValue, (tree: Tree[Token]) => sharedCell.inform(tree))
                  sharedCell.addCell(cell)
                }
                sharedCell
              }
            }
            case Marked(_, inner) => {
              go(inner, value, subscriber)
            }
            case Recursive(id, inner) => recs.get((value, id)) match {
              case Some(entry) => {
                entry.addVarCell(subscriber)
              }
              case None => {
                val entry = new EnvEntry[Token](probe)
                recs += ((value, id) -> entry)
                val i = go(inner, value, (tree: Tree[Token]) => entry.inform(tree))
                entry.setInner(i)
                entry.addVarCell(subscriber)
              }
            }
          }
        }
      }

      val queue: Queue[Tree[Token]] = new Queue()
      val receive: Tree[Token] => Unit = (tree: Tree[Token]) => {
        queue.enqueue(tree)
      }
      val cell = go(syntax, value, receive)


      val it = new Runner[Token](cell, queue, probe)
      if (syntax.nullable == Some(value)) {
        Iterator(Empty.values) ++ it
      }
      else {
        it
      }
    }
  }
}