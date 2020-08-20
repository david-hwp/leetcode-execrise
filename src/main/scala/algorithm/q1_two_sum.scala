package algorithm

import scala.collection.mutable

/**
 *
给定一个整数数组 nums 和一个目标值 target，请你在该数组中找出和为目标值的那 两个 整数，并返回他们的数组下标。

你可以假设每种输入只会对应一个答案。但是，数组中同一个元素不能使用两遍。

示例:

给定 nums = [2, 7, 11, 15], target = 9

因为 nums[0] + nums[1] = 2 + 7 = 9
所以返回 [0, 1]
 */
object q1_two_sum {
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