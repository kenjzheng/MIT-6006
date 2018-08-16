import scala.collection.mutable.ArrayBuffer

/**
  * Created by Ken.J.Zheng on 8/16/2018.
  * Ref -> https://youtu.be/Kg4bqzAqRBM?list=PLUl4u3cNGP61Oq3tWYp6V_F-5jb5L2iHb
  */
//why do we need sorting?
//1. constant time to get specific values, like min, max, medium, etc.
//2. can find a value through binary search with O(lg n) time, instead of linear time O(n)
//3. data compression
//https://en.wikipedia.org/wiki/Sorted_array

object InsertSortAndMergeSort extends App {
  val sort = new Sort()
  val test1 = Array(1,4)
  println(sort.insertionSort(test1,0,0).mkString(","))
  println(sort.mergeSort(test1).mkString(","))

  val test2 = Array(4,1)
  println(sort.insertionSort(test2,0,0).mkString(","))
  println(sort.mergeSort(test2).mkString(","))

  val test3 = Array(3,1,7,8,2,4)
  println(sort.insertionSort(test3,0,0).mkString(","))
  println(sort.mergeSort(test3).mkString(","))

  val test4 = Array(3,4,5,11,22,1,3,6,7,9,22,2,1,5,8,16,7,55,100,22,33,44,8,2,1)
  println(sort.insertionSort(test4,0,0).mkString(","))
  println(sort.mergeSort(test4).mkString(","))
}

class Sort {
  def insertionSort(array: Array[Int],index: Int, checkPoint: Int): Array[Int] = {
    val sortedArray = if(index<array.length) {
      if (index > 0 && array(index - 1) > array(index)) {
        val tmp = array(index)
        array(index) = array(index - 1)
        array(index - 1) = tmp

        if (index > 1 && array(index - 2) > array(index - 1)) {
          insertionSort(array, index - 1, checkPoint)
        }
        else {
          insertionSort(array, checkPoint + 1, checkPoint+1) //restart from previous checkpoint
        }
      }
      else {
        insertionSort(array, index + 1,index + 1)
      }
    }
    else
      array

    sortedArray
  }

  def mergeSort(array: Array[Int]):Array[Int] = {
    //split in half
    var left = array.take(array.length/2)
    var right = array.drop(array.length/2)

    left = if(left.length>1){
      mergeSort(left)
    }
    else left

    right = if(right.length>1){
      mergeSort(right)
    }
    else right

    var sortedArray = new ArrayBuffer[Int]()

    var leftCheckpoint = 0
    var rightCheckpoint = 0
    //println("left->"+left.mkString(","),"right->"+right.mkString(","))

    while (leftCheckpoint<left.length || rightCheckpoint<right.length) {
      if(leftCheckpoint<left.length && rightCheckpoint==right.length){ //run out of right
      val tmp = left.drop(leftCheckpoint)
        sortedArray = sortedArray ++ tmp
        leftCheckpoint += tmp.length
      }
      else if(rightCheckpoint<right.length && leftCheckpoint==left.length){ //run out of left
      val tmp = right.drop(rightCheckpoint)
        sortedArray = sortedArray ++ tmp
        rightCheckpoint += tmp.length
      }
      else if(leftCheckpoint<left.length && left(leftCheckpoint)<=right(rightCheckpoint)){
        sortedArray.append(left(leftCheckpoint))
        leftCheckpoint +=1
      }
      else if (rightCheckpoint<right.length) {
        sortedArray.append(right(rightCheckpoint))
        rightCheckpoint +=1
      }
    }
    //println("sorted->",sortedArray.mkString(","))
    sortedArray.toArray
  }
}
