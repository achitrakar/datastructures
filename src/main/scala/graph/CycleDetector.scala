package graph

import scala.annotation.tailrec
import scala.collection.mutable

class CycleDetector[T] {

  def hasCycleIterative(graph: Graph[T]): Boolean = {
    val whiteSet: mutable.Set[Vertex[T]] = mutable.HashSet[Vertex[T]]()
    val graySet: mutable.Set[Vertex[T]] = mutable.HashSet[Vertex[T]]()
    val blackSet: mutable.Set[Vertex[T]] = mutable.HashSet[Vertex[T]]()

    graph.getAllVertex().foreach(whiteSet.add)
    while(whiteSet.nonEmpty) {
      val current: Vertex[T] = whiteSet.iterator.next()
      if(dfs(current, whiteSet, graySet, blackSet)) {
        return true
      }
    }
    false
  }

  def hasCycleRecursive(graph: Graph[T]): Boolean = {
    val whiteSet: mutable.Set[Vertex[T]] = mutable.HashSet[Vertex[T]]()
    val graySet: mutable.Set[Vertex[T]] = mutable.HashSet[Vertex[T]]()
    val blackSet: mutable.Set[Vertex[T]] = mutable.HashSet[Vertex[T]]()

    graph.getAllVertex().foreach(whiteSet.add)

    hasCycle_recursive(whiteSet, graySet, blackSet)
  }

  @tailrec
  private def hasCycle_recursive(whiteSet: mutable.Set[Vertex[T]],
                                 graySet: mutable.Set[Vertex[T]],
                                 blackSet: mutable.Set[Vertex[T]]): Boolean = {
    whiteSet.size match {
      case 0 => false
      case _ =>
        val current: Vertex[T] = whiteSet.iterator.next()
        if (dfs(current, whiteSet, graySet, blackSet)) {
          true
        } else {
          hasCycle_recursive(whiteSet, graySet, blackSet)
        }
    }
  }

  /**
   * Move current vertex from white set to gray set from white set and then explore it.
   * For every neighbor:
   *    if the neighbor is in the black set means the vertex is already explored so continue.
   *    If the neighbor is in the gray set then the cycle has been found; stop.
   *    Else, recursively call dfs for this neighbor.
   * Move vertex from gray set to black set when done exploring.
   *
   * @param current: Current Vertex
   * @param whiteSet: This set contains all unvisited nodes.
   * @param graySet: This set contains node that is being explored.
   * @param blackSet: This set contains node that has been completely visited.
   * @return
   */
  private def dfs(current: Vertex[T],
                  whiteSet: mutable.Set[Vertex[T]],
                  graySet: mutable.Set[Vertex[T]],
                  blackSet: mutable.Set[Vertex[T]]
                 ): Boolean = {
    moveVertex(current, whiteSet, graySet)

    current.getAdjacentVertexes().foreach {
      case neighbor if blackSet.contains(neighbor) => // noop
      case neighbor if graySet.contains(neighbor) => return true
      case neighbor if dfs(neighbor, whiteSet, graySet, blackSet) => return true
      case _ => // noop
    }

    moveVertex(current, graySet, blackSet)

    // If we reach this far, we return false
    false
  }

  private def moveVertex(vertex: Vertex[T],
                         sourceSet: mutable.Set[Vertex[T]],
                         destinationSet: mutable.Set[Vertex[T]]
                        ): Unit = {
    sourceSet.remove(vertex)
    destinationSet.add(vertex)
  }
}