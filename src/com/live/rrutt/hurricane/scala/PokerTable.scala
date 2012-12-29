package com.live.rrutt.hurricane.scala

import com.live.rrutt.hurricane.HurricanePoker
import com.live.rrutt.prologfunctors._
import com.live.rrutt.ui._

object PokerTable {
  val gPlayerCount = 8
  val gPlayerInitialStake = 100
  
  val gCardSuit = List(
    ("A", 14, 1), // High, Low value 
    ("2", 2, 2),  // Wild 
    ("3", 3, 3),
    ("4", 4, 4),
    ("5", 5, 5),
    ("6", 6, 6),
    ("7", 7, 7),
    ("8", 8, 8),
    ("9", 9, 9),
    ("T", 10, 10),
    ("J", 11, 11),
    ("Q", 12, 12),
    ("K", 13, 13))

  var gPlayerDealer = 0
  var gPlayerHuman = 0
  var gPlayerMode = new_player_text_map("init")
  var gPlayerText = new_player_text_map("")
  var gPlayerAmountStake = new_player_amount_map(gPlayerInitialStake)
  var gPlayerAmountBet = new_player_amount_map(0)
  var gPlayerAmountHigh = new_player_amount_map(0)
  var gPlayerAmountLow = new_player_amount_map(0)
  var gPlayerAmountDraws = new_player_amount_map(0)
  var gPlayerHand = new_player_hand_map
  var gPotAmount = 0
  var gHandNumber = 0
  var gDrawCount = 0
  
  var gPlayerPeek = false
  
  var gCardDeckStock = new_deck
  var gCardDeckDiscard = empty_deck
  
  def PlayPoker(p: HurricanePoker): Unit = {
    parent = p
    
    write("Rick Rutt's Hurricane Poker - Using Scala " + scala.util.Properties.versionString); nl;
//    write("  Java encoding: " + java.nio.charset.Charset.defaultCharset()); nl;
    
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
	nl; write(" Shuffled new deck. "); nl; bang;
	clear_player_amt_pot;
	clear_player_amt_hand;
	initialize_players;
	show_players_clear;
	main_loop;
	text_close;    
  }
  
  def main_loop: Unit = {
    peekaboo
    val choice = main_menu
    if (game_over(choice)) {
      return
    }
    main_loop
  }

  def main_menu: Int = {
    val choice = menu(" Main Menu ",
      List(
        "Deal",
        "Players",
        "New Deck",
        "Exit Game"))

    val confirmedChoice = process(choice)
    
    return confirmedChoice
  }

  def process(choice: Int): Int = {
    choice match {
      case 1 => {
        process_round_deal
        return choice
      }
      case 2 => {
        gPlayerPeek = false
        show_players_info
        return choice
      }
      case 3 => {
        shuffle_deck_new
        shuffle_deck_old
	    nl; write(" Shuffled new deck. "); nl
        return choice
      }
      case 4 => {
        val ok = ask_yes_no_1("Do you want to quit?")
        if (ok) {
          return choice
        } else {
          return 0
        }
      }
    }
  }

  def process_round_deal = {
  	show_players_clear
  	deal_cards_start
  	gDrawCount = 0
//	process_round_bet
//	process_round_draw
//	process_round_bet
	ask_ok_1(" Ready for Showdown! ")
  	show_players_hands
  	decide_hands
  }

  /*
  process_round(bet) :-
	player_amt(P, stake, _),
		clear_player_amt(P, bet),
		fail.
  process_round(bet) :-
	player_mode(D, dealer),
	next(D, P),
  	player_round(bet, P, 8, 3, 0, 0).

  process_round(draw) :-
	player_mode(D, dealer),
	next(D, P),
  	player_round(draw, P, 8, 0, 0, 0),
  	show_players(deal).
*/

  def deal_cards_start = {
    gHandNumber += 1
  	deal_cards_ante
  	deal_cards_around
  	deal_cards_around
  	show_players_deal
  	show_players_pot
  	show_players_human
  	ask_ok_1(" Ready for betting. ")
  }

  def deal_cards_ante = {
//    for (p <- 1 to gPlayerCount) {
//      make_player_bet(p, 1)
//    }
  }

  def deal_cards_around = {
    for (p <- 1 to gPlayerCount) {
      deal_a_card(p)
    }
  }
  
  def deal_cards_done = {
    for (p <- 1 to gPlayerCount) {
      val hand = gPlayerHand(p)
      gPlayerHand(p) = ("*", "*")

      val (c1, c2) = hand
      
      val c1hi = card_high_value(c1)
      val c1lo = card_low_value(c1)
      
      val c2hi = card_high_value(c2)
      val c2lo = card_low_value(c2)
      
      if (c1hi > 0) {
        gCardDeckDiscard ::= (c1, c1hi, c1lo)
      }
      
      if (c2hi > 0) {
        gCardDeckDiscard ::= (c2, c2hi, c2lo)
      }      
    }
  }

  def deal_a_card(p: Int) = {
    val card = gCardDeckStock.head
    val dealt = deal_to_player(p, card)
    if (dealt) {
      gCardDeckStock = gCardDeckStock.tail
    }
  }

  def deal_to_player(p: Int, card: Tuple3[String, Int, Int]): Boolean = {  
    // Sorts as "high" card followed by "low" card
  	val (cc, chi, clo) = card

  	val hand = gPlayerHand(p)
  	val (c1, c2) = hand
  	val d1 = card_high_value(c1)
  	val d2 = card_high_value(c2)
  	
  	hand match {
  	  case ("*", "*") => {
  	    gPlayerHand(p) = (cc, "*")
  	    return true
  	  }
  	  case (c1, "*") if chi > d1 => {
  	    gPlayerHand(p) = (cc, c1)
  	    return true
  	  }
  	  case (c1, "*") => {
  	    gPlayerHand(p) = (c1, cc)
  	    return true
  	  }
  	  case _ => {
  	    // Already had two cards.
  	    return false
  	  }
  	}
  }

  def decide_hands = {
	show_players_pot
	show_players_human
    gPlayerDealer = next_player(gPlayerDealer)
  	deal_cards_done
  	shuffle_deck_old
  }
/*  
  decide_hands :-
  	player_hand_score(P, HL, V),
  		retract_player_hand_score(P, HL, V),
  		fail.  % Loop to erase old scores 
  decide_hands :-
  	player_hand(P, C1, C2),
  		hand_value(high, C1,C2, VH),
  		decide_player_hand(P, high, VH),
  		hand_value(low,  C1,C2, VL),
  		decide_player_hand(P, low, VL),
  		fail.  % Loop for all hands 
  decide_hands :-
  	player_hand_score(P, low, _),
  		add_player_amt(P, low, 1),
  		text_cursor(P, 2),
  		text_write("▼"),
  		fail.  % Loop for any ties 
  decide_hands :-
  	player_hand_score(P, high, _),
  		add_player_amt(P, high, 1),
  		text_cursor(P, 5),
  		text_write("▲"),
  		fail.  % Loop for any ties 
  decide_hands :-  % Terminate loop 
	findall(P, player_hand_score(P, low,  _), LP_LIST),
	pay_winners(count, low, LP_LIST, 0, NL),
	findall(P, player_hand_score(P, high, _), HP_LIST),
	pay_winners(count, high, HP_LIST, 0, NH),
	player_amt(0, pot, A),
	AL is (A // 2) // NL,  % Odd extra goes to High winners 
	AH is (A - (AL * NL)) // NH,  % Odd breakage stays as ante 
	pay_winners(pay, low, LP_LIST, AL, _),
	pay_winners(pay, high, HP_LIST, AH, _),
	show_players(pot),
	show_players(human),
  	set_player(dealer),
  	deal_cards(done),
  	shuffle_deck(old).
*/

  def game_over(choice: Int): Boolean = {
    val result = choice match {
      case 4 => true
      case _ => false
    }
    
    if (result) {
      end_of_game
    }
    
    return result
  }
  
  def end_of_game = {
	text_title(" Game Over. ")
	text_clear
	
	text_cursor(0, 1)
	text_write("Won:")	
    val winningPlayers = gPlayerAmountStake.toList.withFilter { case (p, a) => a > gPlayerInitialStake }
    winningPlayers foreach (pa => {
      val (p, a) = pa
      text_cursor(p, 1)
      text_write("#"); text_write(p.toString()); text_write(" $"); text_write(a.toString())
      if (p == gPlayerHuman) {
	    text_cursor(p, 1)
  	    text_write("\u00f6")  // Smiley face 
      }
    })

	text_cursor(0, 10)
	text_write("Even:")
    val evenPlayers = gPlayerAmountStake.toList.withFilter { case (p, a) => a == gPlayerInitialStake }
    evenPlayers foreach (pa => {
      val (p, a) = pa
      text_cursor(p, 10)
      text_write("#"); text_write(p.toString()); text_write(" $"); text_write(a.toString())
      if (p == gPlayerHuman) {
	    text_cursor(p, 10)
  	    text_write("\u00f6")  // Smiley face 
      }
    })

	text_cursor(0, 19)
	text_write("Lost:")
    val losingPlayers = gPlayerAmountStake.toList.withFilter { case (p, a) => (a >= 0) && (a < gPlayerInitialStake) }
    winningPlayers foreach (pa => {
      val (p, a) = pa
      text_cursor(p, 19)
      text_write("#"); text_write(p.toString()); text_write(" $"); text_write(a.toString())
      if (p == gPlayerHuman) {
	    text_cursor(p, 19)
  	    text_write("\u00f6")  // Smiley face 
      }
    })

	text_cursor(0, 28)
	text_write("In Debt:")
    val debtorPlayers = gPlayerAmountStake.toList.withFilter { case (p, a) => a < 0 }
    debtorPlayers foreach (pa => {
      val (p, a) = pa
      text_cursor(p, 28)
      text_write("#"); text_write(p.toString()); text_write(" $"); text_write(a.toString())
      if (p == gPlayerHuman) {
	    text_cursor(p, 28)
  	    text_write("\u00f6")  // Smiley face 
      }
    })

  	text_cursor(10, 1)
  	text_write("(Original stakes were $"); text_write(gPlayerInitialStake.toString()); text_write(")")
	text_cursor(12, 1)
  	text_write("Hand #"); text_write(gHandNumber.toString()); text_nl
  	ask_ok_0
  	gPlayerPeek = true
	show_players_info
	ask_ok_1("Game Over.")
  }
  
  def shuffle_deck_new = {
    gCardDeckStock = new_deck
    gCardDeckDiscard = empty_deck
  }
  
  def shuffle_deck_old = {
    for (i <- 1 to 5) shuffle_deck_riffle    
  }
  
  def shuffle_deck_riffle = {
//    peek_nl
//    peek_write("** Discard = "); peek_write(gCardDeckDiscard.toString); peek_nl
//    peek_write("   Stock = "); peek_write(gCardDeckStock.toString); peek_nl

    val deck = gCardDeckDiscard ::: gCardDeckStock
    val (leftDeck, rightDeck) = deck partition(_ => scala.math.random < 0.5)
    gCardDeckStock = leftDeck ::: rightDeck
    gCardDeckDiscard = empty_deck
    
//    peek_write("   Left = "); peek_write(leftDeck.toString); peek_nl
//    peek_write("   Right = "); peek_write(rightDeck.toString); peek_nl
//    peek_write(">> New Stock = "); peek_write(gCardDeckStock.toString); peek_nl
  }
  
  def clear_player_amt_pot = {
    gPotAmount = 0
  }
  
  def clear_player_amt_hand = {
    gHandNumber = 0
  }

  def initialize_players = {
    gPlayerMode = new_player_text_map("clear")
    gPlayerHuman = random_int(gPlayerCount)
    gPlayerDealer = random_int(gPlayerCount)

    for ((player, mode) <- gPlayerMode) {
      if (player == gPlayerHuman) {
        gPlayerMode(gPlayerHuman) = "human"
      } else {
        val n = random_int(7)
        n match {
          case 1 => gPlayerMode(player) = "random"
          case 2 => gPlayerMode(player) = "checker"
          case 3 => gPlayerMode(player) = "pairwise"
          case 4 => gPlayerMode(player) = "highrise"
          case 5 => gPlayerMode(player) = "lowdown"
          case 6 => gPlayerMode(player) = "hilo"
          case 7 => gPlayerMode(player) = "foldout"
        }
      }
    }
  }

  def next_player(player: Int): Int = {
    val next = player match {
      case p if p == gPlayerCount => 1
      case _ => player + 1
    }
    
    return next;
  }
  
  def show_players_clear = {
    var gPlayerText = new_player_text_map("")
	text_title(" Hurricane Poker ")
	text_clear
	for (p <- 1 to gPlayerCount) {
	  text_nl; text_write("#"); text_write(p.toString)
	}
	show_players_human
	show_players_dealer
	show_players_pot
  }

  def show_players_human = {
    text_cursor(gPlayerHuman, 0)
    text_write("\u00f6") // Smiley face
    val stake = gPlayerAmountStake(gPlayerHuman)
    text_cursor(gPlayerCount + 4, 1)
    text_write("\u00f6 $"); text_write(stake.toString()); text_write("     ") // Smiley face
  }
  
  def show_players_dealer = {
	text_cursor(gPlayerDealer, 2)
	text_write("\u2261")  // Triple-bar
  }

  def show_players_deal = {
    gPlayerAmountStake.toList.foreach(pa => {
      val (p, a) = pa
      text_cursor(p, 3)
      text_write("--")
      if (p == gPlayerHuman) {
        val hand = gPlayerHand(p)
        val (c1, c2) = hand
        text_cursor(p, 3)
        text_write(c1); text_write(c2)
      }
    })
  }

  def show_players_hands = {
    gPlayerAmountStake.toList.foreach(pa => {
      val (p, a) = pa
      text_cursor(p, 3)
      val hand = gPlayerHand(p)
      val (c1, c2) = hand
      text_cursor(p, 3)
      text_write(c1); text_write(c2)
    })
  }
  
  def show_players_pot = {
	text_cursor(gPlayerCount + 2, 1)
	text_write("Pot: $"); text_write(gPotAmount.toString()); text_write("     ")
  }

  def show_players_info = {
	text_title(" Player Information ")
	text_clear
	
    gPlayerAmountStake.toList.foreach(pa => {
      val (p, a) = pa
      text_cursor(p, 0)
      text_write("#"); text_write(p.toString()); text_write(" $"); text_write(a.toString())
      
      if (p == gPlayerHuman) {
	    text_cursor(p, 0)
  	    text_write("\u00f6")  // Smiley face 
      }
      
      text_cursor(p, 12)
      text_write("Hi: "); text_write(gPlayerAmountHigh(p).toString())
      
      text_cursor(p, 20)
      text_write("Lo: "); text_write(gPlayerAmountLow(p).toString())
      
      if (gPlayerPeek) {
        text_cursor(p, 28)
        text_write(gPlayerMode(p))
      }
    })

    text_cursor(10, 3)
  	text_write("Hand #"); text_write(gHandNumber.toString())
  }

  def show_players_debug = {
    text_title(" Player Amounts ")
    gPlayerAmountStake.toList.foreach(pa => {
      val (p, a) = pa
      text_cursor(p, 0)
      text_write("#"); text_write(p.toString()); text_write("="); text_write(a.toString())

      if (p == gPlayerHuman) {
        text_cursor(p, 0)
        text_write("\u00f6") // Smiley face 
      }
    })
    ask_ok_0
  }
  
  def peekaboo = {}
  
  def new_deck: List[Tuple3[String, Int, Int]] = {
    val suit1 = gCardSuit
    val suit2 = suit1 reverse
    val suit3 = suit2
    val suit4 = suit1
    
    val deck = suit1 ::: suit2 ::: suit3 ::: suit4
    
    return deck
  }
  
  def empty_deck: List[Tuple3[String, Int, Int]] = {
    val deck = List[Tuple3[String, Int, Int]]()
    
    return deck
  }
  
  def new_player_text_map(initialValue: String): collection.mutable.Map[Int, String] = {    
    val textMap = (1 to gPlayerCount) map(p => (p, initialValue)) toMap
    var mutableMap = collection.mutable.Map(textMap.toSeq: _*)
    
    return mutableMap
  }
  
  def new_player_amount_map(initialValue: Int): collection.mutable.Map[Int, Int] = {    
    val amountMap = (1 to gPlayerCount) map(p => (p, initialValue)) toMap
    var mutableMap = collection.mutable.Map(amountMap.toSeq: _*)
    
    return mutableMap
  }

  // TODO: Store player hand as Tuple2[Tuple3[String, Int, Int], Tuple3[String, Int, Int]].
  def new_player_hand_map: collection.mutable.Map[Int, Tuple2[String, String]] = {
    val emptyHand = ("*", "*")
    val handMap = (1 to gPlayerCount) map(p => (p, emptyHand)) toMap
    var mutableMap = collection.mutable.Map(handMap.toSeq: _*)
    
    return mutableMap
  }

  // TODO: Remove this when player hand includes each card as Tuple3[String, Int, Int].
  def card_low_value(card: String): Int = {
    val optionCard = gCardSuit.find {case (c, hi, lo) => c.equals(card)}
    val (c, hi, lo) = optionCard.getOrElse(("*", 0, 0))
    
    return lo
  }
  
  // TODO: Remove this when player hand includes each card as Tuple3[String, Int, Int].
  def card_high_value(card: String): Int = {
    val optionCard = gCardSuit.find {case (c, hi, lo) => c.equals(card)}
    val (c, hi, lo) = optionCard.getOrElse(("*", 0, 0))
    
    return hi
  }
}