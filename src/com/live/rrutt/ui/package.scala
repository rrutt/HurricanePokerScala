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

  def ask_ok_0: Boolean = {
    val msg = "Click to proceed.";
    val okd = new OkDialog(new javax.swing.JFrame(), true, msg);
    okd.setVisible(true);
    val ok = okd.isOk();

    return ok;
  }

  def ask_ok_1(text: String): Boolean = {
    val okd = new OkDialog(new javax.swing.JFrame(), true, text);
    okd.setVisible(true);
    val ok = okd.isOk();

    return ok;
  }

  def ask_ok_2(text0: String, text1: String): Boolean = {
    val msg = text0 + text1;
    val okd = new OkDialog(new javax.swing.JFrame(), true, msg);
    okd.setVisible(true);
    val ok = okd.isOk();

    return ok;
  }
}