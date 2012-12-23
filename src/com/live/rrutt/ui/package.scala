package com.live.rrutt

import com.live.rrutt.tuprolog.util._

package object ui {

  var textWindow: TextWindow = null;

  def showTextWindow() = {
    if (textWindow == null) {
      textWindow = new TextWindow();
    }

    textWindow.setDefaultBounds();
    textWindow.setVisible(true);
  }

  def text_title(title: String): Boolean = {
    showTextWindow();
    textWindow.setTitle(title);
    return true;
  }

  def text_close: Boolean = {
    if (textWindow != null) {
      textWindow.clear();
      textWindow.setVisible(false);
    }
    return true;
  }
}