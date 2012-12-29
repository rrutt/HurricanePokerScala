package com.live.rrutt

import com.live.rrutt.hurricane.HurricanePoker
import com.live.rrutt.tuprolog.util._

package object prologfunctors {
  
  val gPeekEnabled = true

  var parent: HurricanePoker = null
  
  // Prolog cut !
  def bang = {}

  def nl = {
    println
    parent.write("\n")
  }

  def write(s: String) = {
    print(s)
    parent.write(s)
  }
  
  def peek_nl = {
    if (gPeekEnabled) {
      println
    }
  }
  
  def peek_write(s: String) {
    if (gPeekEnabled) {
      print(s)
    }
  }
  
  def random_int(n: Int): Int = {
    val d = new java.lang.Double(1 + (scala.math.random * n))
    val i = d.intValue
    
    return i
  }
}