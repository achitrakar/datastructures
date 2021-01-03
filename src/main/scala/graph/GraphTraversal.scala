package graph

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class GraphTraversal[T] {
  def DFS(graph: Graph[T]): List[T] = {
    val dfsTraversalResult = new ListBuffer[T]()
    def DFSUtil(v: Vertex[T], visited: mutable.HashSet[T]): Unit = {
      visited.add(v.getId())
      dfsTraversalResult.addOne(v.getId()) // Store the result

      v.getAdjacentVertexes().foreach{ vertex =>
        visited.contains(vertex.getId()) match {
          case false => DFSUtil(vertex, visited)
          case true => // noop
        }
      }
    }

    val visited: mutable.HashSet[T] = new mutable.HashSet[T]()
    graph.getAllVertex().foreach { vertex =>
      visited.contains(vertex.getId()) match {
        case false => DFSUtil(vertex, visited)
        case true => // noop
      }
    }
    dfsTraversalResult.toList
  }
}