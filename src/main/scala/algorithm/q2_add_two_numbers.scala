package algorithm

/**
 * Definition for singly-linked list.
 * class ListNode(var _x: Int = 0) {
 * var next: ListNode = null
 * var x: Int = _x
 * }
 */
/**
 * 给出两个 非空 的链表用来表示两个非负的整数。其中，它们各自的位数是按照 逆序 的方式存储的，并且它们的每个节点只能存储 一位 数字。
 *
 * 如果，我们将这两个数相加起来，则会返回一个新的链表来表示它们的和。
 *
 * 您可以假设除了数字 0 之外，这两个数都不会以 0 开头。
 *
 * 示例：
 *
 * 输入：(2 -> 4 -> 3) + (5 -> 6 -> 4)
 * 输出：7 -> 0 -> 8
 * 原因：342 + 465 = 807
 *
 * 来源：力扣（LeetCode）
 * 链接：https://leetcode-cn.com/problems/add-two-numbers
 * 著作权归领扣网络所有。商业转载请联系官方授权，非商业转载请注明出处。
 */
object q2_add_two_numbers {

  class ListNode(var _x: Int = 0) {
    var next: ListNode = null
    var x: Int = _x
  }

  def addTwoNumbersMy(l1: ListNode, l2: ListNode): ListNode = {
    var ll1: ListNode = l1
    var ll2: ListNode = l2
    val n: ListNode = new ListNode(0)
    var l: ListNode = n
    var p: ListNode = null
    while (ll1 != null || ll2 != null) {
      val n1 = if (ll1 == null) 0 else ll1.x
      val n2 = if (ll2 == null) 0 else ll2.x
      val c = plus(n1, n2)
      if (l == null) {
        l = new ListNode(c._2)
        p.next = l
      } else {
        plusHigh(l, c._2)
      }
      if (c._1) {
        l.next = new ListNode(1)
      }
      p = l
      l = l.next
      if (ll1 == null) ll1 = null else ll1 = ll1.next
      if (ll2 == null) ll2 = null else ll2 = ll2.next
    }
    n
  }

  def addTwoNumbersOfficial(l1: ListNode, l2: ListNode): ListNode = {
    var ll1: ListNode = l1
    var ll2: ListNode = l2
    val n: ListNode = new ListNode(0)
    var l: ListNode = n
    var carry: Int = 0
    while (ll1 != null || ll2 != null) {
      val n1 = if (ll1 == null) 0 else ll1.x
      val n2 = if (ll2 == null) 0 else ll2.x
      val c = carry + n1 + n2
      carry = c / 10
      l.next = new ListNode(c % 10)
      l = l.next
      if (ll1 == null) ll1 = null else ll1 = ll1.next
      if (ll2 == null) ll2 = null else ll2 = ll2.next
    }
    if (carry > 0) {
      l.next = new ListNode(1)
    }
    n.next
  }

  def plusHigh(h: ListNode, num: Int): Unit = {
    if (h != null) {
      val p = plus(h.x, num)
      h.x = p._2
      if (p._1) {
        if (h.next == null) {
          h.next = new ListNode(0)
        }
        plusHigh(h.next, 1)
      }
      println(h.x + "->")
    }
  }

  def plus(n1: Int, n2: Int): (Boolean, Int) = {
    if (n1 + n2 >= 10) {
      (true, n1 + n2 - 10)
    } else {
      (false, n1 + n2)
    }
  }

  def main(args: Array[String]): Unit = {
    val n = new ListNode(9)
    val n1 = new ListNode(9)
    n.next = n1
    val n2 = new ListNode(1)


    //    val n = new ListNode(5)
    //    val n1 = new ListNode(2)
    //    val n2 = new ListNode(3)
    //    val n3 = new ListNode(1)
    //    n.next = n1
    //    n1.next = n2
    //    n2.next = n3
    // val nn = if(n.next==null) 0 else n.next.x
    // println(nn)
    // val x = reverseNode(n)
    // println(x.x)
    //    val res = addTwoNumbersMy(n, n2)
    //    println(res.next.next.x)

    val res = addTwoNumbersOfficial(n, n2)
    println(res.next.next.x)
    //
    //    plusHigh(n3)
    //    println(n3.next.x)
  }

  def reverseNode(source: ListNode): ListNode = {
    var r = source
    var arr: Array[ListNode] = Array.empty
    arr = arr :+ r
    while (r.next != null) {
      arr = arr :+ r.next
      r = r.next
    }
    arr = arr.reverse
    for (i <- arr.indices) {
      if (i < arr.length - 1) {
        arr(i).next = arr(i + 1)
      }
    }
    arr.apply(0)
  }


}