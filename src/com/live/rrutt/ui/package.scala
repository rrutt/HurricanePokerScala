package com.live.rrutt

import com.live.rrutt.tuprolog.util._
import collection.JavaConversions._

package object ui {

  var textWindow: TextWindow = null;

  def showTextWindow = {
    if (textWindow == null) {
      textWindow = new TextWindow();
    }

    textWindow.setDefaultBounds();
    textWindow.setVisible(true);
  }

  def disposeTextWindow = {
    if (textWindow != null) {
      textWindow.setVisible(false);
      textWindow.dispose();
      textWindow = null;
    }
  }

  def text_title(title: String) = {
    showTextWindow;
    textWindow.setTitle(title);
  }

  def text_close = {
    if (textWindow != null) {
      textWindow.clear();
      textWindow.setVisible(false);
    }
  }

  def text_clear = {
    showTextWindow;
    textWindow.clear();
  }

  def text_cursor(row: Int, col: Int) = {
    showTextWindow;
    textWindow.setCursorRowCol(row, col);
  }

  def text_write(text: String) = {
    showTextWindow;
    textWindow.writeText(text);
  }

  def text_nl = {
    showTextWindow;
    textWindow.newLine();
  }

  def menu(menuCaption: String, choiceList: List[String]): Int = {
    val md = new MenuDialog(new javax.swing.JFrame(), true, menuCaption, choiceList)
    md.setVisible(true)
    val choice = md.choice()
    
    return choice
  }
}