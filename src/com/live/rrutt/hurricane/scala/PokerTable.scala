package com.live.rrutt.hurricane.scala

import com.live.rrutt.hurricane.HurricanePoker
import com.live.rrutt.prologfunctors._
import com.live.rrutt.ui._

object PokerTable {

  val gCardSuit = List(
    ('A', 14, 1), // High, Low value 
    ('2', 2, 2), // Wild 
    ('3', 3, 3),
    ('4', 4, 4),
    ('5', 5, 5),
    ('6', 6, 6),
    ('7', 7, 7),
    ('8', 8, 8),
    ('9', 9, 9),
    ('T', 10, 10),
    ('J', 11, 11),
    ('Q', 12, 12),
    ('K', 13, 13))
  
  var gCardDeckStock = new_deck
  var gCardDeckDiscard = empty_deck
  
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
      return
    }
    main_loop
  }
  
  def main_menu: Int = {
    return 0;
  }
  
  def game_over(CHOICE: Int): Boolean = {
    return true
  }
  
  def shuffle_deck_new = {
    gCardDeckStock = new_deck
    gCardDeckDiscard = empty_deck
  }
  
  def shuffle_deck_old = {
    for (i <- 1 to 5) shuffle_deck_riffle    
  }
  
  def shuffle_deck_riffle = {
    debug_nl
    debug_write("** Discard = "); debug_write(gCardDeckDiscard.toString); debug_nl
    debug_write("   Stock = "); debug_write(gCardDeckStock.toString); debug_nl
    val deck = gCardDeckDiscard ::: gCardDeckStock
    val (leftDeck, rightDeck) = deck partition(_ => Math.random < 0.5)
    debug_write("   Left = "); debug_write(leftDeck.toString); debug_nl
    debug_write("   Right = "); debug_write(rightDeck.toString); debug_nl
    gCardDeckStock = leftDeck ::: rightDeck
    gCardDeckDiscard = empty_deck
    debug_write(">> New Stock = "); debug_write(gCardDeckStock.toString); debug_nl
  }
  
  def clear_player_amt_pot = {}
  
  def clear_player_amt_hand = {}
  
  def set_player_init = {}
  
  def show_players_clear = {}
  
  def peekaboo = {}
  
  def new_deck: List[Tuple3[Char, Int, Int]] = {
    val suit1 = gCardSuit
    val suit2 = suit1 reverse
    val suit3 = suit2
    val suit4 = suit1
    
    val deck = suit1 ::: suit2 ::: suit3 ::: suit4
    
    return deck
  }
  
  def empty_deck: List[Tuple3[Char, Int, Int]] = {
    val deck = List[Tuple3[Char, Int, Int]]()
    
    return deck
  }
}