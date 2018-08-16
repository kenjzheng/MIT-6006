/**
  * Created by Ken.J.Zheng on 8/16/2018.
  */
import scala.collection.mutable.ArrayBuffer

object TwoDPeakFinding extends App {
  def findPeak(dimArray:Array[Array[Int]]):(Int,Int) = {
    val columns = dimArray(0).length //it is a grid, all arrays have same length

    //cut half
    val j = columns/2
    //find maximum in column j
    val columnArray = dimArray.zipWithIndex.map(x => (x._2,x._1(j)))
    val maxInColumn: (Int, Int) = columnArray.sortBy(x=> x._2).last

    val (index,value) = maxInColumn
    val peak= if(columns==1){
      maxInColumn
    }
    else if(columns==2){
      //compare left only, no right side
      if(value < dimArray(index)(j-1)){
        val arrayBuffer = ArrayBuffer[Array[Int]]()
        dimArray.foreach(innerArray => {
          arrayBuffer += innerArray.take(j)
        })
        findPeak(arrayBuffer.toArray)
      }
      else
        maxInColumn
    }
    else if(value < dimArray(index)(j-1)){
      //search left
      val arrayBuffer = ArrayBuffer[Array[Int]]()
      dimArray.foreach(innerArray => {
        arrayBuffer += innerArray.take(j)
      })
      findPeak(arrayBuffer.toArray)
    }
    else if(value < dimArray(index)(j+1)){
      //search right
      val arrayBuffer = ArrayBuffer[Array[Int]]()
      dimArray.foreach(innerArray => {
        arrayBuffer += innerArray.drop(j+1)
      })
      findPeak(arrayBuffer.toArray)
    }
    else //answer is here
      maxInColumn

    peak
  }

  val test1 = Array(
    Array(1),
    Array(3),
    Array(1),
    Array(2),
    Array(1),
    Array(5)
  )

  //expecting 9
  val test2 = Array(
    Array(1,9),
    Array(3,8),
    Array(1,2),
    Array(2,3),
    Array(1,1),
    Array(5,5)
  )

  //expecting 7
  val test3 = Array(
    Array(1,1),
    Array(3,2),
    Array(6,5),
    Array(7,3),
    Array(1,1),
    Array(5,2)
  )

  //expecting 9
  val test4 = Array(
    Array(1,1,2),
    Array(3,2,5),
    Array(4,5,6),
    Array(7,3,7),
    Array(1,1,8),
    Array(5,2,9)
  )

  //play with array(2)(5) - 5 and numbers beside it to redirect data flow.
  val test5 = Array(
    Array(1,2,7,4,5,2,7,8,1,15),
    Array(9,5,3,4,5,3,7,1,9,10),
    Array(1,2,5,4,2,5,8,8,1,10),
    Array(1,9,3,7,5,1,7,2,9,11),
    Array(1,5,3,5,5,2,2,1,9,10)
  )

  println(findPeak(test1))
  println(findPeak(test2))
  println(findPeak(test3))
  println(findPeak(test4))
  println(findPeak(test5))
}
