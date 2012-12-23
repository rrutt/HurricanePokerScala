package com.live.rrutt.hurricane.scala

import com.live.rrutt.hurricane.HurricanePoker
import com.live.rrutt.prologfunctors._
import com.live.rrutt.ui._

object PokerTable {
  
  def PlayPoker(p: HurricanePoker): Unit = {
    parent = p
    
    write("Rick Rutt's Hurricane Poker - Using Scala " + scala.util.Properties.versionString); nl;
    
	text_title(" Hurricane Poker w/ Scala ");
	write(" Hurricane is two-card draw poker;");
	nl; write(" played as high/low; with deuces wild.");
	nl; write(" Aces and deuces play both high and low;");
	nl; write(" two deuces always pair.");
	nl; nl; write(" No straights; no flushes.");
	nl; write(" Checking allowed.");
	nl; write(" Maximum bet/raise is $3.");
	nl; write(" 1 bet and 2 raises per round.");
	nl;
	shuffle_deck_new;
	shuffle_deck_old;
	nl; write(" Shuffled new deck "); nl; bang;
	clear_player_amt_pot;
	clear_player_amt_hand;
	set_player_init;
	show_players_clear;
	main_loop;
	text_close;    
  }
  
  def main_loop: Unit = {
    peekaboo
    val CHOICE = main_menu
    if (game_over(CHOICE)) {
      return Unit
    }
    main_loop
  }
  
  def main_menu: Int = {
    return 0;
  }
  
  def game_over(CHOICE: Int): Boolean = {
    return true
  }
  
  def shuffle_deck_new = {}
  
  def shuffle_deck_old = {}
  
  def clear_player_amt_pot = {}
  
  def clear_player_amt_hand = {}
  
  def set_player_init = {}
  
  def show_players_clear = {}
  
  def peekaboo = {}
}