/**
  * Created by Ken.J.Zheng on 8/16/2018.
  */
object PeakFinding extends App {
  def findPeak(list:List[Int]): Any = {
    val middle =list.length/2
    //println(list(middle))
    val peak = if(list.length == 1) { //if there is only one number
      list(0)
    }
    else if(list.length == 2) { //if there are two numbers to compare
      if(list(0)>list(1))list(0)
      else list(1)
    }
    else if(list(middle)<list(middle-1)){
      //search left
      val leftList = list.take(middle)
      //println(leftList)
      findPeak(leftList)
    }
    else if(list(middle)<list(middle+1)){
      //search right
      val rightList = list.drop(middle+1)
      //println(rightList)
      findPeak(rightList)
    }
    else
      list(middle)

    peak
  }

  println(findPeak(List(10))) //10
  println(findPeak(List(8,2))) //8
  println(findPeak(List(1,3,2))) //3
  println(findPeak(List(1,3,2,5)))
  println(findPeak(List(1,3,2,4,5,8,6,7,10))) //10
  println(findPeak(List(1,3,2,6,5,8,6,7,10))) //3
  println(findPeak(List(1,2,3,4,5,6,7,8,9,10,11,12,13)))
  println(findPeak(List(1,3,2,4,5,8,6,7,10,11,12,13,1,1)))
}
