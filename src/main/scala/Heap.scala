/**
  * Created by Ken.J.Zheng on 8/16/2018.
  */
//ref:https://en.wikipedia.org/wiki/Heap_(data_structure)
//In computer science, a heap is a specialized tree-based data structure that satisfies the heap property: if
// P is a parent node of C, then the key (the value) of P is either greater than or equal to (in a max heap) or
// less than or equal to (in a min heap) the key of C.[1] The node at the "top" of the heap (with no parents)
// is called the root node.

//A heap is a useful data structure when you need to remove the object with the highest (or lowest) priority.
//heap is an array visualized as a tree.
import scala.util.control.Breaks._
object HeapTest extends App {
  val min = new HeapMin()
  val test1 = Array(3,1,7,8,2,4)
  test1.foreach(a => min.insert(a))
  println(min.sort.mkString(","))

  val test2 = Array(3,4,5,11,22,1,3,6,7,9,22,2,1,5,8,16,7,55,100,22,33,44,8,2,1)
  test2.foreach(a => {
    min.insert(a)
    val t = 0
    }
  )
  println(min.sort.mkString(","))
}

import scala.collection.mutable.ArrayBuffer

class HeapMin {
  val heap = new ArrayBuffer[Int]()

  def hasParent(childIndex : Int):Boolean = {
    if(childIndex > 0) true
    else false
  }
  def getParentIndex(childIndex : Int) : Int = {
    (childIndex - 1)/2
  }
  def getLeftChildIndex(parentIndex:Int) : Int = {parentIndex * 2 + 1}
  def getRightChildIndex(parentIndex:Int) : Int = {parentIndex * 2 + 2}
  def parent(childIndex : Int) = {heap(getParentIndex(childIndex))}
  def current(index: Int) = {heap(index)}
  def hasLeftChild(parentIndex: Int) : Boolean = {
    getLeftChildIndex(parentIndex) < heap.length
  }
  def hasRightChild(parentIndex: Int) : Boolean = {
    getRightChildIndex(parentIndex) < heap.length
  }
  def leftChild(parentIndex: Int):Int = {
    heap(getLeftChildIndex(parentIndex))
  }
  def rightChild(parentIndex: Int):Int = {
    heap(getRightChildIndex(parentIndex))
  }

  def swap(xIndex: Int, yIndex: Int) = {
    val tmp = heap(xIndex)
    heap(xIndex) = heap(yIndex)
    heap(yIndex) = tmp
  }

  def insert(value: Int)={
    heap.append(value)
    heapifyUp
  }

  def poll : Int = {
    if(heap.length>0) {
      val root = heap(0)
      heap(0) = heap(heap.length - 1)
      heap.remove(heap.length - 1, 1)
      heapifyDown
      root
    }
    else
      throw new Exception("No more!")
  }

  def sort : Array[Int] = {
    val sorted = new ArrayBuffer[Int]()
    while (heap.length > 0){
      sorted.append(poll)
    }
    sorted.toArray
  }

  def heapifyUp = {
    var index = heap.length - 1
    while(hasParent(index) && parent(index) > current(index)){
      swap(getParentIndex(index),index)
      index = getParentIndex(index)
    }
  }

  def heapifyDown = {
    var index = 0
    var continue = true

    while (hasLeftChild(index) && continue) {
      var childIndex = getLeftChildIndex(index)
      if (hasRightChild(index) && rightChild(index) < leftChild(index))
        childIndex = getRightChildIndex(index)

      if (current(index) > current(childIndex)) {
        swap(index, childIndex)
        index = childIndex
      }
      else
        continue = false
    }
  }
}
