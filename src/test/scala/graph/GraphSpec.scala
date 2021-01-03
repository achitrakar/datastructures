package graph

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class GraphSpec extends AnyFlatSpec {

  var intDirectedGraph: Graph[Int] = _
  var stringDirectedGraph: Graph[String] = _

  behavior of "A Directed Graph Setup"

  it should "setup a directed graph containing Int nodes" in {
    intDirectedGraph = new Graph[Int](true)
    intDirectedGraph.addEdge(1, 3)
    intDirectedGraph.addEdge(1, 2)
    intDirectedGraph.addEdge(3, 4)
    intDirectedGraph.addEdge(5, 6)
    intDirectedGraph.addEdge(6, 3)
    intDirectedGraph.addEdge(3, 8)
    intDirectedGraph.addEdge(8, 11)

    intDirectedGraph.getEdgeCount() should be(7)
    intDirectedGraph.getAllVertex().size should be(8)
  }

  it should "setup a directed graph containing String nodes" in {
    stringDirectedGraph = new Graph[String](true)
    stringDirectedGraph.addEdge("Anthony", "Cantona")
    stringDirectedGraph.addEdge("Anthony", "Berbatov")
    stringDirectedGraph.addEdge("Cantona", "David")
    stringDirectedGraph.addEdge("Evra", "Ferdinand")
    stringDirectedGraph.addEdge("Ferdinand", "Cantona")
    stringDirectedGraph.addEdge("Cantona", "Hernandez")
    stringDirectedGraph.addEdge("Hernandez", "Keane")

    stringDirectedGraph.getEdgeCount() should be(7)
    stringDirectedGraph.getAllVertex().size should be(8)
  }

  behavior of "Graph Traversal using DFS"
  it should "traverse a graph containing int nodes using DFS traversal" in {
    val g:GraphTraversal[Int] = new GraphTraversal[Int]()
    val res = g.DFS(intDirectedGraph)
    res.size should be(8)
    res should be(List(1, 3, 4, 8, 11, 2, 5, 6)) // depends on starting node
  }

  it should "traverse a graph containing string nodes using DFS traversal" in {
    val g:GraphTraversal[String] = new GraphTraversal[String]()
    val res = g.DFS(stringDirectedGraph)
    res.size should be(8)
    res should be(List("Evra", "Ferdinand", "Cantona", "David", "Hernandez", "Keane", "Berbatov", "Anthony")) // depends on starting node
  }

  behavior of "Topological Sort"
  it should "topologically sort graph containing int nodes" in {
    val ts: TopologicalSort[Int] = new TopologicalSort[Int]()
    val sortedGraph = ts.topSort(intDirectedGraph)
    sortedGraph.size should be(8)
    val resList = sortedGraph.map(v => v.getId())
    resList should be(List(5, 6, 1, 2, 3, 8, 11, 4))
  }

  it should "topologically sort graph containing string nodes" in {
    val ts: TopologicalSort[String] = new TopologicalSort[String]()
    val sortedGraph = ts.topSort(stringDirectedGraph)
    sortedGraph.size should be(8)
    val resList = sortedGraph.map(v => v.getId())
    List("Anthony", "Berbatov", "Evra", "Ferdinand", "Cantona", "Hernandez", "Keane", "David")
  }
}