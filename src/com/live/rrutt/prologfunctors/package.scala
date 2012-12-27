package com.live.rrutt

import com.live.rrutt.hurricane.HurricanePoker

package object prologfunctors {
  
  val debugging = true

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
  
  def debug_nl = {
    if (debugging) {
      println
    }
  }
  
  def debug_write(s: String) {
    if (debugging) {
      print(s)
    }
  }
  
  def random_int(n: Int): Int = {
    val d = new java.lang.Double(1 + (Math.random * n))
    val i = d.intValue
    
    return i
  }
}