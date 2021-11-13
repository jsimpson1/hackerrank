package hackerrank.functionalprogramming.functionalstructures

import scala.runtime.Nothing$

object TreeManager {

  import model._


  def main(args: Array[String]): Unit = {

    val inputStr = """11
                     |change 1
                     |print
                     |insert child 2
                     |visit child 1
                     |insert right 3
                     |visit right
                     |print
                     |insert right 4
                     |delete
                     |visit child 2
                     |print""".stripMargin

    val input = Input.parse(inputStr)

    println(s"input=$input")

  }

  def solve(str: String): Unit =
    solve(Input.parse(str))

  def solve(input: Input): Unit = {

//    input
//      .operations
//      .foldLeft(Tree.root){ (tree, operation) =>
//        doOperation(operation, tree)
//      }
    ???
  }

  def doOperation(operation: String, tree: Tree): Tree = {
//    operation
//      .split(" ") match {
//      case Array(cmd) =>
//        cmd match {
//          case "print" =>
//            tree.print()
//          case "delete" =>
//            tree.delete()
//        }
//      case Array(cmd, target) =>
//        cmd match {
//          case "change" =>
//            tree.change(target.toInt)
//          case "visit" =>
//            target match {
//              case "left" =>
//                tree.visitLeft()
//              case "right" =>
//                tree.visitRight()
//              case "parent" =>
//                tree.visitParent()
//
//            }
//        }
//      case Array(cmd, target, num) =>
//        val num0 = num.toInt
//        cmd match {
//          case "visit" =>
//            tree.visitChild(num0)
//          case "insert" =>
//            target match {
//              case "left" =>
//                tree.insertLeft(num0)
//              case "right" =>
//                tree.insertRight(num0)
//              case "child" =>
//                tree.insertChild(num0)
//            }
//        }
//    }
    ???
  }

  object model {

    sealed trait Tree {

      def valueOpt: Option[Int]
      def parent: Option[Tree]
      def left: Tree
      def right: Tree

      def parentNode: Option[Node] =
        parent
          .flatMap{
            case Empty(_) =>
              None
            case n: Node =>
              Some(n)
          }

      def parentOfParent: Option[Node] =
        parentNode
          .flatMap(_.parentNode)

    }

    case class Empty(parent: Option[Tree]) extends Tree {
      override def valueOpt: Option[Int] = None
      override def left: Tree = Empty(None)
      override def right: Tree = Empty(None)
    }

    case class Node(
      left: Tree,
      value: Int,
      right: Tree,
      parent: Option[Tree],
    ) extends Tree {

      override def valueOpt: Option[Int] =
        Some(value)

    }

    sealed trait Sibling

    case class NoSibling() extends Sibling

    case class Right(parent: Tree, tree: Tree) extends Sibling

    case class Left(parent: Tree, tree: Tree) extends Sibling

    case class Zipper(
      current: Tree,
      sibling: Sibling
    ) {

      def parentNodeOpt: Option[Node] =
        current
          .parent
          .map(p =>
            p.asInstanceOf[Node]
          )

      def print(): Zipper = {
        println(current)
        this
      }

      def throwNoParentNodeException(context: String) =
        throw new RuntimeException(s"${context} -- failing on corner case of no parentNode which should not happen in this exercise")

      def resolveParentSibling: Sibling = {
        current
          .parentNode
          .flatMap { pn =>
            pn
              .parentNode
              .map{ n =>
                if (n.left == pn)
                  Right(n, n.right)
                else if (n.right == pn)
                  Left(n, n.left)
                else
                  NoSibling()
              }
          }.getOrElse(
            NoSibling()
        )
      }

      def delete(): Zipper = {
        current
          .parentNode match {
            case None =>
              throwNoParentNodeException("delete")
            case Some(pn) =>
              val nextParent: Node =
                if ( pn.left == current )
                  pn.copy(left = Empty(Some(pn)))
                else if ( pn.right == current )
                  pn.copy(right = Empty(Some(pn)))
                else
                  throw new RuntimeException(s"delete -- current $this is not the left or right of the parent $pn")

              Zipper(nextParent, resolveParentSibling)
          }

      }

      def change(i: Int): Zipper = {
        val nextCurrent = current match {
          case Empty(p) =>
            val n = Node(Empty(None), i, Empty(None), p)
            n.copy(
              left = Empty(Some(n)),
              right = Empty(Some(n))
            )
          case node: Node =>
            node
              .copy(
                value = i
              )
        }
        Zipper(nextCurrent, sibling)
      }

      def visitLeft(): Zipper = {
        current
          .parentNode match {
            case None =>
              throwNoParentNodeException("visitLeft")
            case Some(pn) =>
              Zipper(pn.left, Right(pn, this.current))
          }
      }

      def visitRight(): Zipper = {
        current
          .parentNode match {
          case None =>
            throwNoParentNodeException("visitRight")
          case Some(pn) =>
            Zipper(pn.right, Left(pn, this.current))
        }
      }

      def visitParent(): Zipper = {
        current
          .parentNode match {
            case None =>
              throwNoParentNodeException("visitParent")
            case Some(pn) =>
              Zipper(pn, resolveParentSibling)
        }
      }
      def visitChild(i: Int): Zipper = {
        
      }
//
//      def insertLeft(i: Int): Zipper
//      def insertRight(i: Int): Zipper
//
//      def insertChild(i: Int): Zipper

    }

//    object Tree {
//
//      implicit val defaultRootValue: Int = 0
//
//      lazy val root: Tree = Zipper(defaultRootValue, Array(), Empty)
//
//    }
//
//    trait Tree {
//
//      def throwException(context: String) = throw new UnsupportedOperationException(context)
//
//      def value: Int
//      def children: Array[Tree]
//      def parent: Tree
//
//      def print(): Tree
//
//      def delete(): Tree
//
//      def change(i: Int): Tree
//      def visitLeft(): Tree
//      def visitRight(): Tree
//      def visitParent(): Tree
//      def visitChild(i: Int): Tree
//
//      def insertLeft(i: Int): Tree
//      def insertRight(i: Int): Tree
//
//      def insertChild(i: Int): Tree
//
//    }
//
//    case object Empty extends Tree {
//
//      override def value: Int = throwException("value")
//
//      override def children: Array[Tree] = Array()
//
//      override def parent: Tree = Empty
//
//      override def print(): Tree = {
//        println("Empty")
//        this
//      }
//
//      override def delete(): Tree = throwException("delete")
//
//      override def change(i: Int): Tree = throwException(s"change(${i})")
//
//      override def visitLeft(): Tree = throwException("visitLeft")
//
//      override def visitRight(): Tree = throwException("visitRight")
//
//      override def visitParent(): Tree = throwException("visitParent")
//
//      override def visitChild(i: Int): Tree = throwException("visitChild")
//
//      override def insertLeft(i: Int): Tree = throwException("insertLeft")
//
//      override def insertRight(i: Int): Tree = throwException("insertRight")
//
//      override def insertChild(i: Int): Tree = throwException("insertChild")
//    }
//
//
//    case class Zipper(
//      value: Int,
//      children: Array[Tree],
//      parent: Tree
//    ) extends Tree {
//
//      def parentIndexForThis: Int = parent.children.indexOf(this)
//
//      def leftIndex: Int = parentIndexForThis - 1
//
//      def rightIndex: Int = parentIndexForThis + 1
//
//      def parentAsZipper: Zipper = parent match {
//        case Empty =>
//          throwException("parent is empty")
//        case z: Zipper =>
//          z
//      }
//
//      def insertSibling(position: Int, value: Int): Tree = {
//        val sibling = Zipper(value, Array(), parent)
//        val nextParent: Zipper = {
//          val siblingIndex = parentIndexForThis + position
//          val zParent = parentAsZipper
//          val nextChildren = (zParent.children.take(siblingIndex) :+ sibling) ++ zParent.children.drop(siblingIndex)
//          zParent
//            .copy(
//              children = nextChildren
//            )
//        }
//        this.copy(
//          parent = nextParent
//        )
//      }
//
//      // Tree methods
//
//      def print(): Tree = {
//        println(value)
//        this
//      }
//
//      def delete(): Tree = {
//        val z = parentAsZipper
//        val index = z.children.indexOf(this)
//        z.copy(
//          children = children.drop(index)
//        )
//      }
//
//      def change(i: Int): Tree = this.copy(value = i)
//
//      def visitLeft(): Tree =
//        parent
//          .children(leftIndex)
//
//      def visitRight(): Tree =
//        parent.children(rightIndex)
//
//      def visitParent(): Tree =
//        parent
//
//      def visitChild(i: Int): Tree =
//        children(i - 1)
//
//      def insertLeft(i: Int): Tree =
//        insertSibling(0, i)
//
//      def insertRight(i: Int): Tree =
//        insertSibling(1, i)
//
//      def insertChild(i: Int): Tree = {
//        this.copy(
//          children = Zipper(i, Array(), this) +: children
//        )
//      }
//
//    }

    object Input {

      def parse(str: String): Input = {
        str
          .split("\n")
          .toList match {
            case h :: tail =>
              Input(h.toInt, tail)
            case _ =>
             throw new RuntimeException("invalid input string")
          }
      }

    }

    case class Input(
      numOfOperations: Int,
      operations: List[String]
    )

  }


}
