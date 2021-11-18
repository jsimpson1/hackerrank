package hackerrank.functionalprogramming.functionalstructures

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

  def doOperation(operation: String, tree: Node): Node = {
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

    object Zipper {

      sealed trait MoveResult {

        import MoveResult._

        def get: Zipper = this match {
          case Success(zipper, _) =>
            zipper
          case Failure(_) =>
            throw new UnsupportedOperationException("failed to move the zipper")
        }

      }

      object MoveResult {
        case class Success(zipper: Zipper, origin: Zipper) extends MoveResult
        case class Failure(origin: Zipper) extends MoveResult
      }

    }

    trait ExerciseMethods {
      def print(): Zipper
      def delete(): Zipper
      def change(i: Int): Zipper
      def visitLeft(): Zipper
      def visitRight(): Zipper
      def visitParent(): Zipper
      def visitChild(i: Int): Zipper
      def insertLeft(i: Int): Zipper
      def insertRight(i: Int): Zipper
      def insertChild(i: Int): Zipper
    }

    case class Zipper(
      left: List[Int],
      focus: Int,
      right: List[Int],
      up: Option[Zipper]
    ) extends ExerciseMethods {

      import Zipper._

      def moveTo(z: Zipper): MoveResult = MoveResult.Success(z, this)
      def fail: MoveResult = MoveResult.Failure(this)

      def tapFocus(f: Int => Unit): Zipper = {
        f(focus)
        this
      }

      def tryMoveLeft: MoveResult = {
        left match {
          case h :: tail =>
            moveTo(
              copy(
                left = tail,
                focus = h,
                right = focus :: right
              )
            )
          case Nil =>
            fail
        }
      }

      def tryMoveRight: MoveResult = {
        right match {
          case h :: tail =>
            moveTo(
              copy(
                right = tail,
                focus = h,
                left = focus :: left
              )
            )
          case Nil =>
            fail
        }
      }

      def tryDeleteAndMoveUp: MoveResult = {
        up
          .fold(fail){ zipper =>
            moveTo(
              zipper.copy(
                left = left.filterNot(_ == focus),
                right = right.filterNot(_ == focus),
              )
            )
          }
      }

      def moveToChild(i: Int): MoveResult = {
        (left ::: focus :: right) match {
          case Nil =>
            fail
          case List(one) =>
            ???
          case values =>
            values
              .splitAt(i - 1) match {
                case (l, r) =>
                  r match {
                    case Nil =>
                  l match {
                    case List(one) =>
                      ???
                    case values =>
                      values
                        .splitAt(values.length - 1) match {
                          case (lefts, List(last)) =>
                            ???
//                            moveTo(Zipper(l, h, tail, up))
                        }
                  }
                  case h :: tail =>
                    moveTo(Zipper(l, h, tail, up))
                }
              }
        }

      }

      def print(): Zipper =
        tapFocus(
          _ => println(focus)
        )

      def delete(): Zipper =
        tryDeleteAndMoveUp
          .get

      def change(i: Int): Zipper =
        copy(focus = i)

      def visitLeft(): Zipper =
        tryMoveLeft
          .get

      def visitRight(): Zipper =
        tryMoveRight
          .get

      def visitParent(): Zipper =
        tryDeleteAndMoveUp
          .get

      def visitChild(i: Int): Zipper = {
        moveToChild(i)
        ???
      }

      def insertLeft(i: Int): Zipper =
        copy(
          left = i :: left
        )

      def insertRight(i: Int): Zipper =
        copy(
          right = i :: right
        )

      def insertChild(i: Int): Zipper =
        copy(
          left = left :+ i
        )

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
