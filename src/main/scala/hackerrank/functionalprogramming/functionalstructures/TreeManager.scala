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

    input
      .operations
      .foldLeft(Tree.root){ (tree, operation) =>
        doOperation(operation, tree)
      }
  }

  def doOperation(operation: String, tree: Tree): Tree = {
    operation
      .split(" ") match {
      case Array(cmd) =>
        cmd match {
          case "print" =>
            tree.print()
          case "delete" =>
            tree.delete()
        }
      case Array(cmd, target) =>
        cmd match {
          case "change" =>
            tree.change(target.toInt)
          case "visit" =>
            target match {
              case "left" =>
                tree.visitLeft()
              case "right" =>
                tree.visitRight()
              case "parent" =>
                tree.visitParent()

            }
        }
      case Array(cmd, target, num) =>
        val num0 = num.toInt
        cmd match {
          case "visit" =>
            tree.visitChild(num0)
          case "insert" =>
            target match {
              case "left" =>
                tree.insertLeft(num0)
              case "right" =>
                tree.insertRight(num0)
              case "child" =>
                tree.insertChild(num0)
            }
        }
    }
  }

  object model {

    object Tree {

      implicit val defaultRootValue: Int = 0

      lazy val root: Tree = Zipper(defaultRootValue, Array(), Empty)

    }

    trait Tree {

      def throwException(context: String) = throw new UnsupportedOperationException(context)

      def value: Int
      def children: Array[Tree]
      def parent: Tree

      def print(): Tree

      def delete(): Tree

      def change(i: Int): Tree
      def visitLeft(): Tree
      def visitRight(): Tree
      def visitParent(): Tree
      def visitChild(i: Int): Tree

      def insertLeft(i: Int): Tree
      def insertRight(i: Int): Tree

      def insertChild(i: Int): Tree

    }

    case object Empty extends Tree {

      override def value: Int = throwException("value")

      override def children: Array[Tree] = Array()

      override def parent: Tree = Empty

      override def print(): Tree = {
        println("Empty")
        this
      }

      override def delete(): Tree = throwException("delete")

      override def change(i: Int): Tree = throwException(s"change(${i})")

      override def visitLeft(): Tree = throwException("visitLeft")

      override def visitRight(): Tree = throwException("visitRight")

      override def visitParent(): Tree = throwException("visitParent")

      override def visitChild(i: Int): Tree = throwException("visitChild")

      override def insertLeft(i: Int): Tree = throwException("insertLeft")

      override def insertRight(i: Int): Tree = throwException("insertRight")

      override def insertChild(i: Int): Tree = throwException("insertChild")
    }


    case class Zipper(
      value: Int,
      children: Array[Tree],
      parent: Tree
    ) extends Tree {

      def parentIndexForThis: Int = parent.children.indexOf(this)

      def leftIndex: Int = parentIndexForThis - 1

      def rightIndex: Int = parentIndexForThis + 1

      def parentAsZipper: Zipper = parent match {
        case Empty =>
          throwException("parent is empty")
        case z: Zipper =>
          z
      }

      def insertSibling(position: Int, value: Int): Tree = {
        val sibling = Zipper(value, Array(), parent)
        val nextParent: Zipper = {
          val siblingIndex = parentIndexForThis + position
          val zParent = parentAsZipper
          val nextChildren = (zParent.children.take(siblingIndex) :+ sibling) ++ zParent.children.drop(siblingIndex)
          zParent
            .copy(
              children = nextChildren
            )
        }
        this.copy(
          parent = nextParent
        )
      }

      // Tree methods

      def print(): Tree = {
        println(value)
        this
      }

      def delete(): Tree = {
        val z = parentAsZipper
        val index = z.children.indexOf(this)
        z.copy(
          children = children.drop(index)
        )
      }

      def change(i: Int): Tree = this.copy(value = i)

      def visitLeft(): Tree =
        parent
          .children(leftIndex)

      def visitRight(): Tree =
        parent.children(rightIndex)

      def visitParent(): Tree =
        parent

      def visitChild(i: Int): Tree =
        children(i - 1)

      def insertLeft(i: Int): Tree =
        insertSibling(0, i)

      def insertRight(i: Int): Tree =
        insertSibling(1, i)

      def insertChild(i: Int): Tree = {
        this.copy(
          children = Zipper(i, Array(), this) +: children
        )
      }

    }

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
