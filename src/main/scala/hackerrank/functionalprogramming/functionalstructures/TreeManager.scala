package hackerrank.functionalprogramming.functionalstructures

import hackerrank.functionalprogramming.functionalstructures.TreeManager.model.Tree

import scala.annotation.tailrec
import scala.collection.immutable.HashMap

object TreeManager {

  object model {
    case class Tree(
      left: List[Int],
      focus: Int,
      right: List[Int],
    ) {

      def changeFocus(childIndexStartingFromOne: Int): Tree = {
        val nextTree =
          (left ::: (focus :: right))
            .splitAt(childIndexStartingFromOne - 1) match {
            case (l, r) =>
              Tree(l, r.head, r.tail)
          }
        nextTree
      }

      def insertRight(value: Int): Tree = {
        copy(
          right = value :: right
        )
      }

      def insertLeft(value: Int): Tree = {
        copy(
          left = left :+ value
        )
      }

      def visitRight: Tree = {
        copy(
          left = left :+ focus,
          focus = right.head,
          right = right.tail
        )
      }

      def visitLeft: Tree = {
        val leftSplit = left.splitAt(left.length - 1)
        copy(
          left = leftSplit._1,
          focus = leftSplit._2.head,
          right = focus :: right
        )
      }

      def delete: Tree = {
        left ::: right match {
          case Nil =>
            this
          case h :: tail =>
            copy(
              left = Nil,
              focus = h,
              right = tail
            )
        }
      }

    }
  }


  private def isStringInt(str: String): Boolean = try {
    str.toInt
    true
  } catch {
    case _: Throwable =>
      false
  }

  private def inputToOperations(str: String): List[String] = {
    str
      .split("\n")
      .toList match {
        case Nil =>
          Nil
        case h :: tail =>
          if ( isStringInt(h) ){
            tail
          } else {
            h :: tail
          }
      }
  }

  private def doOperationOnTree(manager: TreeManager, operation: String): TreeManager = {
    val parts = operation.split(" ")

    parts match {
      case Array(cmd) =>
        cmd match {
          case "print" =>
            println(manager.levelToTreeMap(manager.currentLevel).focus)
            manager
          case "delete" =>
            manager.delete
        }
      case Array(cmd, value) =>
        def valueAsInt = value.toInt
        cmd match {
          case "change" =>
            manager.change(manager.currentLevel, valueAsInt)
          case "visit" =>
            value match {
              case "right" =>
                manager.visitRight
              case "left"=>
                manager.visitLeft
              case "parent" =>
                manager.visitParent
            }
        }
      case Array(cmd, target, value) =>
        def valueAsInt = value.toInt
        cmd match {
          case "insert" =>
            target match {
              case "child" =>
                manager.insertChild(valueAsInt)
              case "right" =>
                manager.insertRight(valueAsInt)
              case "left" =>
                manager.insertLeft(valueAsInt)
            }
          case "visit" =>
            target match {
              case "child" =>
                manager.visitChild(valueAsInt)
            }
          case o =>
            throw new RuntimeException(s"implement case for ${o.toList}")
        }
      case o =>
        throw new RuntimeException(s"implement case for ${o.toList}")
    }
  }

  private def performOperations(root: TreeManager, operations: List[String]): TreeManager = {
    @tailrec
    def r(manager: TreeManager, operations: List[String]): TreeManager = {
      operations match {
        case Nil =>
          manager
        case h :: tail =>
          val nextManager = doOperationOnTree(manager, h)
          r(nextManager, tail)
      }
    }
    r(root, operations)
  }

  def doOperations(inputStr: String): TreeManager = {
    val rootMap: HashMap[Int, Tree] = HashMap((0, Tree(Nil, 0, Nil)))
    val manager: TreeManager = TreeManager(0, rootMap)
    val operations: List[String] = inputToOperations(inputStr)
    performOperations(manager, operations)
  }

}

case class TreeManager(
  currentLevel: Int,
  levelToTreeMap: HashMap[Int, Tree]
) {

  def change(level: Int, value: Int): TreeManager = {
    val updatedTree = levelToTreeMap(level).copy(focus = value)
    val updatedHashMap = levelToTreeMap.updated(level, updatedTree)
    val nextManager = copy(levelToTreeMap = updatedHashMap)
    nextManager
  }

  def insertChild(value: Int): TreeManager = {

    val childLevel = currentLevel + 1

    val nextChildLevelTree: Tree =
      levelToTreeMap
        .get(childLevel) match {
          case None =>
            Tree(Nil, value, Nil)
          case Some(tree) =>
            tree
              .copy(
                left = value :: tree.left
              )
      }
    val nextLevelToTreeMap: HashMap[Int, Tree] = levelToTreeMap.updated(childLevel, nextChildLevelTree)
    val nextManager = copy(levelToTreeMap = nextLevelToTreeMap)
    nextManager
  }


  def visitChild(childIndexStartingFromOne: Int): TreeManager = {
    val childLevel = currentLevel + 1
    val childIndexFocusedTree: Tree = {
      val currentChildTree = levelToTreeMap(childLevel)
      val focusedTree = currentChildTree.changeFocus(childIndexStartingFromOne)
      focusedTree
    }
    val nextLevelToTreeMap = levelToTreeMap.updated(childLevel, childIndexFocusedTree)
    val nextManager = copy(
      currentLevel = childLevel,
      levelToTreeMap = nextLevelToTreeMap
    )
    nextManager
  }

  def insertLeft(value: Int): TreeManager = {
    val insertToTheLeftTree = levelToTreeMap(currentLevel).insertLeft(value)
    val nextLevelToTreeMap = levelToTreeMap.updated(currentLevel, insertToTheLeftTree)
    val nextManager = copy(levelToTreeMap = nextLevelToTreeMap)
    nextManager
  }

  def insertRight(value: Int): TreeManager = {
    val insertToTheRightTree = levelToTreeMap(currentLevel).insertRight(value)
    val nextLevelToTreeMap = levelToTreeMap.updated(currentLevel, insertToTheRightTree)
    val nextManager = copy(levelToTreeMap = nextLevelToTreeMap)
    nextManager
  }

  def visitRight: TreeManager = {
    val focusToTheRightTree = levelToTreeMap(currentLevel).visitRight
    val nextLevelToTreeMap = levelToTreeMap.updated(currentLevel, focusToTheRightTree)
    val nextManager = copy(levelToTreeMap = nextLevelToTreeMap)
    nextManager
  }

  def visitLeft: TreeManager = {
    val focusToTheLeftTree = levelToTreeMap(currentLevel).visitLeft
    val nextLevelToTreeMap = levelToTreeMap.updated(currentLevel, focusToTheLeftTree)
    val nextManager = copy(levelToTreeMap = nextLevelToTreeMap)
    nextManager
  }

  def visitParent: TreeManager = {
    val nextManager = copy(currentLevel = currentLevel - 1)
    nextManager
  }

  def delete: TreeManager = {
    val deleteNextTree: Tree = levelToTreeMap(currentLevel).delete
    val nextLevelToTreeMap = levelToTreeMap.updated(currentLevel, deleteNextTree)
    val levelUp = currentLevel - 1
    val nextManager =
      copy(
        currentLevel = levelUp,
        levelToTreeMap = nextLevelToTreeMap
      )
    nextManager
  }

}
