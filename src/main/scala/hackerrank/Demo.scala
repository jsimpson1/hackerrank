package hackerrank

import hackerrank.functionalprogramming.memoizationanddp.SherlockAndTheMaze.{GridSquare, Path}

import scala.collection.mutable

object Demo extends App {

  val cache: mutable.HashMap[GridSquare, Set[Path]] = mutable.HashMap[GridSquare, Set[Path]](
    (GridSquare(1,1), Set(Path(List(GridSquare(1,1)), 0)))
  )

  println(cache)

  val key = GridSquare(1,2)

  cache.getOrElseUpdate(key, Set())

  println(cache)

  val key0 = GridSquare(2,2)

  cache(key0) = Set()

  println(cache)

}
