package algorithm

import scala.collection.mutable

object question1 {
  def twoSumMy(nums: Array[Int], target: Int): Array[Int] = {
    for (start <- nums.indices) {
      for (end <- start + 1 until nums.length) {
        if (target - nums.apply(start) == nums.apply(end)) {
          return Array(start, end)
        }
      }
    }
    Array.empty
  }

  def twoSumOfficial3(nums: Array[Int], target: Int): Array[Int] = {
    val map: mutable.HashMap[Int, Int] = mutable.HashMap.empty
    for (start <- nums.indices) {
      val diff = target - nums.apply(start)
      if (map.contains(diff)) {
        return Array(map(diff), start)
      }
      map.put(nums.apply(start), start)
    }
    Array.empty
  }

  def main(args: Array[String]): Unit = {
    val result = twoSumOfficial3(Array(3,3), 6)
    println(result.apply(0))
    println(result.apply(1))
  }
}