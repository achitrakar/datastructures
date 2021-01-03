package graph

import scala.collection.mutable

class TopologicalSort[T] {
  def topSort(graph: Graph[T]): List[Vertex[T]] = {
    val stack = new mutable.ArrayDeque[Vertex[T]]()
    val visited = new mutable.HashSet[Vertex[T]]()

    graph.getAllVertex().foreach { vertex =>
      visited.contains(vertex) match {
        case false => topSortUtil(vertex, stack, visited)
        case true => // noop
      }
    }
    stack.toList
  }

  private def topSortUtil(vertex: Vertex[T], stack: mutable.ArrayDeque[Vertex[T]], visited: mutable.HashSet[Vertex[T]]): Unit = {
    visited.add(vertex)
    vertex.getAdjacentVertexes().foreach {childVertex =>
      visited.contains(childVertex) match {
        case false => topSortUtil(childVertex, stack, visited)
        case true => // noop
      }
    }
    stack.prepend(vertex)
  }
}