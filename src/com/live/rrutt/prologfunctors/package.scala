package com.live.rrutt

import com.live.rrutt.hurricane.HurricanePoker

package object prologfunctors {

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
}