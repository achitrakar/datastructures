package graph

import scala.collection.mutable.Map
import scala.collection.mutable.ListBuffer

class Graph[T](isDirected: Boolean) {
  private val allEdges: ListBuffer[Edge[T]] = new ListBuffer[Edge[T]]()
  private val allVertex: Map[T, Vertex[T]] = Map.empty

  def getEdgeCount(): Int = allEdges.size

  def getAllVertex(): Iterable[Vertex[T]] = allVertex.values

  def addEdge(id1: T, id2: T, weightOpt: Option[Int] = None): Unit = {
    def getOrElseInsertVertex(id: T): Vertex[T] = {
      allVertex.getOrElse(id, {
        val v = new Vertex[T](id)
        allVertex += (id -> v)
        v
      })
    }

    val vertex1 = getOrElseInsertVertex(id1)
    val vertex2 = getOrElseInsertVertex(id2)

    val edge = new Edge[T](vertex1, vertex2, isDirected, weightOpt)
    allEdges += edge
    vertex1.addAdjacentVertex(edge, vertex2)

    if (!isDirected) {
      vertex2.addAdjacentVertex(edge, vertex1)
    }
  }
}

class Vertex[T](id: T) {
  private val edges: ListBuffer[Edge[T]] = new ListBuffer[Edge[T]]()
  private val adjacentVertex: ListBuffer[Vertex[T]] = new ListBuffer[Vertex[T]]()

  def getId(): T = id

  def getAdjacentVertexes(): List[Vertex[T]] = adjacentVertex.toList

  def addAdjacentVertex(e:Edge[T] , v:Vertex[T]): Unit = {
    edges += e
    adjacentVertex += v
  }
}

class Edge[T](v1: Vertex[T], v2: Vertex[T], isDirected: Boolean, weightOpt: Option[Int])