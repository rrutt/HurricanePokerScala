package com.live.rrutt.hurricane.scala

import com.live.rrutt.hurricane.HurricanePoker
import com.live.rrutt.prologfunctors._
import com.live.rrutt.ui._

object PokerTable {
  val gMaxBetsPerRound = 3
  val gPlayerCount = 8
  val gPlayerInitialStake = 100
  val gWorstBetValue = -99000
  val gCardEmpty = " "
  val gCardFolded = "x"
  val gHandEmpty = (gCardEmpty, gCardEmpty)
  val gHandFolded = (gCardFolded, gCardEmpty)
  val gHighLowList = List("high", "low")
  
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
  var gPlayerBestHands = new_player_best_hand_map
  
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
        gPlayerText = new_player_text_map("")
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
	process_round_bet
	process_round_draw
	process_round_bet
	ask_ok_1(" Ready for Showdown! ")
  	show_players_hands
  	decide_hands
  }

  def process_round_bet = {
    gPlayerAmountBet = new_player_amount_map(0)
    val firstPlayerToAct = next_player(gPlayerDealer)
  	player_round_bet(firstPlayerToAct, gPlayerCount, gMaxBetsPerRound, 0)
  }

  def process_round_draw = {
    val firstPlayerToAct = next_player(gPlayerDealer)
  	player_round_draw(firstPlayerToAct, gPlayerCount)
  	show_players_deal
  }

  def player_round_bet(player: Int, remainingPlayers: Int, remainingBets: Int, totalBet: Int): Unit = {
    remainingPlayers match {
      case 0 => {
        show_players_pot
        show_players_human
        return // Terminate recursion.
      }
      case _ => {
        val playerHand = gPlayerHand(player)
        playerHand match {
          case h if h == gHandFolded => {
            // Player folded.
            val nextPlayer = next_player(player)
            val stillRemainingPlayers = remainingPlayers - 1
            player_round_bet(nextPlayer, stillRemainingPlayers, remainingBets, totalBet)
          }
          case _ => {
            val mode = gPlayerMode(player)
            val (action, betAmount) = get_action_bet(mode, player, remainingPlayers, remainingBets, totalBet)
            process_player_bet_action(player, action, betAmount, remainingPlayers, remainingBets, totalBet)
          }
        }
      }
    }
  }

  def player_round_draw(player: Int, remainingPlayers: Int): Unit = {
    remainingPlayers match {
      case 0 => {
        return // Terminate recursion.
      }
      case _ => {
        val playerHand = gPlayerHand(player)
        playerHand match {
          case h if h == gHandFolded => {
            // Player folded; do nothing.
          }
          case _ => {
            val mode = gPlayerMode(player)
            val action = get_action_draw(mode, player)
            process_player_draw_action(player, action)
          }
        }
      }
      
      val nextPlayer = next_player(player)
      val stillRemainingPlayers = remainingPlayers - 1
      player_round_draw(nextPlayer, stillRemainingPlayers)
    }
  }

  def process_player_bet_action(player: Int, action: String, betAmount: Int, remainingPlayers: Int, remainingBets: Int, totalBet: Int): Unit = {
    action match {
      case "raise" => {
        add_player_text(player, "+" + betAmount.toString())
        totalBet match {
          case 0 => {
            write("Player "); write(player.toString()); write(" bets $"); write(betAmount.toString()); nl
          }
          case _ => {
            write("Player "); write(player.toString()); write(" raises $"); write(betAmount.toString()); nl
          }
        }
        make_player_bet(player, betAmount)
        val nextPlayer = next_player(player)
        val stillRemainingPlayers = gPlayerCount - 1
        val stillRemainingBets = remainingBets - 1
        val newTotalBet = totalBet + betAmount
        player_round_bet(nextPlayer, stillRemainingPlayers, stillRemainingBets, newTotalBet)
      }
      case "fold" => {
        totalBet match {
          case 0 => {
            // Convert "fold" to "call".
            process_player_bet_action(player, "call", 0, remainingPlayers, remainingBets, 0)
          }
          case _ => {
            add_player_text(player, "x ")
            write("Player "); write(player.toString()); write(" folds"); nl
            discard_player_hand(player)
            text_cursor(player, 3); text_write(gCardFolded); text_write(gCardEmpty)
            val nextPlayer = next_player(player)
            val stillRemainingPlayers = remainingPlayers - 1
            player_round_bet(nextPlayer, stillRemainingPlayers, remainingBets, totalBet)
          }
        }
      }
      case _ => { // "call"
        val playerBet = gPlayerAmountBet(player)
        playerBet match {
          case b if b == totalBet => {
            add_player_text(player, "# ")
            write("Player "); write(player.toString()); write(" checks"); nl
          }
          case _ => {
            val callAmount = totalBet - playerBet
            make_player_bet(player, callAmount)
            add_player_text(player, "=" + callAmount.toString())
            write("Player "); write(player.toString()); write(" calls $"); write(callAmount.toString()); nl
          }
        }
        val nextPlayer = next_player(player)
        val stillRemainingPlayers = remainingPlayers - 1
        player_round_bet(nextPlayer, stillRemainingPlayers, remainingBets, totalBet)
      }
    }
  }
  
  def process_player_draw_action(player: Int, action: String) = {
    action match {
      case "high" => {
        val (toss, keep) = gPlayerHand(player)
        draw_player_card(player, keep, toss)
      }
      case "low" => {
        val (keep, toss) = gPlayerHand(player)
        draw_player_card(player, keep, toss)
      }
      case _ => {
        // "keep"
        add_player_text(player, "- ")
        write("Player "); write(player.toString()); write(" keeps both cards"); nl
      }
    }
  }

  def draw_player_card(player: Int, cardToKeep: String, cardToDiscard: String) = {
    add_player_text(player, "\u00b1 ") // Plus-or-minus
    write("Player "); write(player.toString()); write(" draws a card"); nl
    gPlayerAmountDraws(player) += 1
    discard(cardToDiscard)
    gPlayerHand(player) = (cardToKeep, gCardEmpty)
    deal_a_card(player)
  }

  /*
  player_round(high, P, N, _, _, _) :-
	add_player_text(P, "± "), !,  % Plus-or-minus 
	write("Player "), write(P), write(" draws a card"), nl,
	add_player_amt(0, draws, 1),
	player_hand(P, C1, C2),
	retract_player_hand(P, C1, C2),
	assertz(player_hand(P, C2, '*')),
	denomination_value(C1, D1, _),  % Use "high" value 
	assertz_card_deck(discard, D1),
	deal_a_card(P),
	next(P, NP),
	N1 is N - 1,
	player_round(draw, NP, N1, 0, 0, 0).
  player_round(low, P, N, _, _, _) :-
	add_player_text(P, "± "), !,  % Plus-or-minus 
	write("Player "), write(P), write(" draws a card"), nl,
	add_player_amt(0, draws, 1),
	player_hand(P, C1, C2),
	retract_player_hand(P, C1, C2),
	assertz(player_hand(P, C1, '*')),
	denomination_value(C2, D2, _),  % Use "high" value 
	assertz_card_deck(discard, D2),
	deal_a_card(P),
	next(P, NP),
	N1 is N - 1,
	player_round(draw, NP, N1, 0, 0, 0).
	*/

  // Returns (action, betAmount)
  def get_action_bet(mode: String, player: Int, remainingPlayers: Int, remainingBets: Int, totalBet: Int): (String, Int) = {
    mode match {
      case "human" => {
        show_players_pot
        show_players_human
        val playerBet = gPlayerAmountBet(player)
        val callAmount = totalBet - playerBet
  	    val actionBetAmount = bet_menu(remainingBets, callAmount)
  	    
  	    return actionBetAmount
      }
      case "checker" => {
        return ("call", 0)
      }
      case _ => {  // "random"
        val index = random_int(5) - 1
        val actionBetAmount = get_action_bet_index(index, remainingBets)
  	    
  	    return actionBetAmount
      }
    }
  }
  
  def get_action_draw(mode: String, player: Int): String = {
    mode match {
      case "human" => {
  	    val action = draw_menu
  	    
  	    return action
      }
      case "checker" => {
        return "keep"
      }
      case _ => {  // "random"
        val index = random_int(3) - 1
        val action = get_action_draw_index(index)
  	    
  	    return action
      }
    }
  }

  /*
  get_action(bet, pairwise, P, _, _, _, raise, 3) :-  % Raise on wild 
  	player_hand(P, _, '2'), !.
  get_action(bet, pairwise, P, _, _, _, raise, 3) :-  % Raise on A,K,Q pair 
  	player_hand(P, C1, C2),
  	C1 = C2,
	denomination_value(C1, D1, _),  % Use "high" value 
	D1 > 11, !.  	
  get_action(bet, pairwise, P, _, _, _, raise, 2) :-  % Raise on pair 
  	player_hand(P, C1, C2),
  	C1 = C2, !.
  get_action(bet, pairwise, P, _, _, _, raise, 1) :-  % Raise on face card 
  	player_hand(P, C, _),
	denomination_value(C, D, _),  % Use "high" value 
	D > 10, !.  	
  get_action(bet, pairwise, _, _, _, _, raise, 1) :-  % Defensive raise 
  	player_amt(0, draws, 0), !.  % Only before draw 
  get_action(bet, pairwise, _, _, _, T, fold, 0) :-  % Fold if big bet without pair 
  	player_amt(0, draws, N), N > 0,  % Only after draw 
  	T > 2, !.
  get_action(bet, pairwise, _, _, _, _, call, 0).

  get_action(draw, pairwise, P, N, R, T, ACT, B) :-  % Same as highrise 
	get_action(draw, highrise, P, N, R, T, ACT, B).


  get_action(bet, highrise, P, _, _, _, raise, 3) :-  % Raise on wild 
  	player_hand(P, _, '2'), !.
  get_action(bet, highrise, P, _, _, _, raise, 3) :-  % Raise on A,K,Q pair 
  	player_hand(P, C1, C2),
  	C1 = C2,
	denomination_value(C1, D1, _),  % Use "high" value 
	D1 > 11, !.  	
  get_action(bet, highrise, P, _, _, _, raise, 2) :-  % Raise on pair 
  	player_hand(P, C1, C2),
  	C1 = C2, !.
  get_action(bet, highrise, P, _, _, _, raise, 1) :-  % Raise on A,K,Q 
  	player_hand(P, C, _),
	denomination_value(C, D, _),  % Use "high" value 
	D > 11, !.
  get_action(bet, highrise, P, _, _, _, fold, 0) :-  % Fold if poor high card 
  	player_amt(0, draws, 0),  % Only before draw 
  	player_hand(P, C1, C2),
  	C1 \= C2,  % Stay on a pair 
	denomination_value(C1, D1, _),  % Use "high" value 
	D1 < 10, !.
  get_action(bet, highrise, P, _, _, _, fold, 0) :-  % Fold if "medium" 
  	player_amt(0, draws, N), N > 0,  % Only after draw 
  	player_hand(P, C1, C2),
  	C1 \= C2,  % Stay on a pair 
	denomination_value(C1, D1, _),  % Use "high" value 
	D1 > 5, D1 < 12, !.
  get_action(bet, highrise, _, _, _, _, raise, 1) :-  % Defensive raise 
  	player_amt(0, draws, 0), !.  % Only before draw 
  get_action(bet, highrise, _, _, _, T, fold, 0) :-  % Fold if BIG bet 
  	player_amt(0, draws, N), N > 0,  % Only after draw 
  	T > 6, !.
  get_action(bet, highrise, _, _, _, _, call, 0) :- !.

  get_action(draw, highrise, P, _, _, _, keep, 0) :-  % Hold on pair 
  	player_hand(P, C1, C2),
  	C1 = C2, !.
  get_action(draw, highrise, P, _, _, _, keep, 0) :-  % Hold on wild, face 
  	player_hand(P, C, '2'),
	denomination_value(C, D, _),  % Use "high" value 
	D > 10, !.
  get_action(draw, highrise, P, _, _, _, high, 0) :-  % Try for higher wild 
  	player_hand(P, _, '2'), !.
  get_action(draw, highrise, _, _, _, _, low, 0).  % Toss low card 


  get_action(bet, lowdown, P, _, _, _, raise, 3) :-
  	player_hand(P, 'A', '2'), !.
  get_action(bet, lowdown, P, _, _, _, raise, 2) :-
  	player_hand(P, 'A', _), !.
  get_action(bet, lowdown, P, _, _, _, raise, 1) :-  % Raise on low hi-card 
  	player_hand(P, C, _),
	denomination_value(C, D, _),  % Use "high" value 
	D < 7, !.
  get_action(bet, lowdown, P, _, _, _, fold, 0) :-  % Fold if high low-card 
  	player_amt(0, draws, 0),  % Only before draw 
  	player_hand(P, C1, C2),
  	C1 \= C2,  % Stay on a pair 
	denomination_value(C2, D2, _),  % Use "high" value 
	D2 > 6, !.
  get_action(bet, lowdown, P, _, _, T, fold, 0) :-  % Fold if not low and big bet 
  	player_amt(0, draws, N), N > 0,  % Only after draw 
  	player_hand(P, C1, C2),
  	C1 \= C2,  % Stay on a pair 
	denomination_value(C1, D1, _),  % Use "high" value 
	D1 > 6,
	T > 1, !.
  get_action(bet, lowdown, _, _, _, _, call, 0).

  get_action(draw, lowdown, P, _, _, _, keep, 0) :-
  	player_hand(P, 'A', '2'), !.
  get_action(draw, lowdown, P, _, _, _, keep, 0) :-  % Hold if low hi-card 
  	player_hand(P, C, _),
	denomination_value(C, D, _),  % Use "high" value 
	D < 7, !.
  get_action(draw, lowdown, _, _, _, _, high, 0).


  get_action(bet, hilo, P, _, _, _, raise, 3) :-
  	player_hand(P, 'A', '2'), !.
  get_action(bet, hilo, P, _, _, _, raise, 2) :-
  	player_hand(P, 'A', _), !.
  get_action(bet, hilo, P, _, _, _, raise, 2) :-  % Raise on low hi-card 
  	player_hand(P, C, _),
	denomination_value(C, D, _),  % Use "high" value 
	D < 7, !.
  get_action(bet, hilo, P, _, _, T, fold, 0) :-  % Fold if both medium and big bet 
  	player_hand(P, C1, C2),
  	C1 \= C2,  % Stay on a pair 
	denomination_value(C1, D1, _),  % Use "high" value 
	denomination_value(C2, D2, _),  % Use "high" value 
	D1 > 6, D1 < 10,
	D2 > 6, D2 < 10,
	T > 1, !.
  get_action(bet, hilo, P, _, _, _, raise, 1) :-  % Defensive raise if both medium 
  	player_amt(0, draws, N), N > 0,  % Only after draw 
  	player_hand(P, C1, C2),
  	C1 \= C2,  % Stay on a pair 
	denomination_value(C1, D1, _),  % Use "high" value 
	denomination_value(C2, D2, _),  % Use "high" value 
	D1 > 6, D1 < 10,
	D2 > 6, D2 < 10, !.
  get_action(bet, hilo, P, N, R, T, ACT, B) :-  % Else, same as highrise 
	get_action(bet, highrise, P, N, R, T, ACT, B).

  get_action(draw, hilo, P, _, _, _, keep, 0) :-
  	player_hand(P, 'A', '2'), !.
  get_action(draw, hilo, P, _, _, _, keep, 0) :-  % Hold if low hi-card 
  	player_hand(P, C, _),
	denomination_value(C, D, _),  % Use "high" value 
	D < 6, !.
  get_action(draw, hilo, P, _, _, _, high, 0) :-  % Discard poor high if good low card 
  	player_hand(P, C1, C2),
  	C1 \= C2,  % Hold on pair 
	denomination_value(C1, D1, _),  % Use "high" value 
	denomination_value(C2, D2, _),
	D1 < 12,
	D2 < 6, !.
  get_action(draw, hilo, P, N, R, T, ACT, B) :-  % Else, same as highrise 
	get_action(draw, highrise, P, N, R, T, ACT, B).


  get_action(bet, foldout, P, _, _, _, raise, 3) :-
  	player_hand(P, 'A', '2'), !.
  get_action(bet, foldout, P, _, _, _, raise, 2) :-
  	player_hand(P, 'A', _), !.
  get_action(bet, foldout, P, _, _, _, raise, 2) :-  % Raise on low hi-card 
  	player_hand(P, C, _),
	denomination_value(C, D, _),  % Use "high" value 
	D < 6, !.
  get_action(bet, foldout, P, _, _, _, fold, 0) :-  % Fold if both medium and big bet 
  	player_hand(P, C1, C2),
  	C1 \= C2,  % Stay on a pair 
	denomination_value(C1, D1, _),  % Use "high" value 
	denomination_value(C2, D2, _),  % Use "high" value 
	D1 > 5, D1 < 12,
	D2 > 5, D2 < 12, !.
  get_action(bet, foldout, P, N, R, T, ACT, B) :-  % Else, same as hilo 
	get_action(bet, hilo, P, N, R, T, ACT, B).

  get_action(draw, foldout, P, N, R, T, ACT, B) :-  % Else, same as hilo 
	get_action(draw, hilo, P, N, R, T, ACT, B).


  get_action(bet, human, P, _, R, T, ACT, B) :-
  	show_players(pot),
  	show_players(human),
  	player_amt(P, bet, PB),
  	CB is T - PB,  % Call bet amount 
	peekaboo,
  	bet_menu(R, CB, ACT, B).

  get_action(draw, human, P, _, _, _, ACT, 0) :-
	peekaboo,
  	draw_menu(P, ACT).


  get_action(bet, _, _, _, _, _, call, 0).   % Safety net 

  get_action(draw, _, _, _, _, _, keep, 0).  % Safety net 
*/

  // Returns (action, betAmount)
  def get_action_bet_index(index: Int, remainingBets: Int): (String, Int) = {
    val actionBetAmount = (index, remainingBets) match {
      case (0, _) => ("call", 0)
      case (4, _) => ("fold", 0)
      case (c, 0) if c < 4 => ("call", 0)
      case (c, _) if c < 4 => ("raise", index)
      case _ => ("call", 0)  // Safety net.
    }
    
    return actionBetAmount
  }
  
  def get_action_draw_index(index: Int): String = {
    val action = index match {
      case 1 => "high"
      case 2 => "low"
      case _ => "keep"
    }
    
    return action
  }

  // Returns (action, betAmount)
  def bet_menu(remainingBets: Int, callAmount: Int): (String, Int) = {
    val (c1, c2) = gPlayerHand(gPlayerHuman)
    val caption = " Bet? Hand = " + c1 + c2
    (remainingBets, callAmount) match {
      case (_, 0) => {
        val choice = menu(caption,
          List(
            "Check   ",
            "Bet $1",
            "Bet $2",
            "Bet $3",
            "" // No sense folding, since no $ needed 
            ))
        val actionBetAmount = bet_choice(choice)
        return actionBetAmount
      }
      case (0, _) => {
        val callText = "Call $" + callAmount.toString()
        val choice = menu(caption,
          List(
            callText,
            "",
            "",
            "",
            "Fold    "))
        val actionBetAmount = bet_choice(choice)
        return actionBetAmount
      }
      case _ => {
        val callText = "Call $" + callAmount.toString()
        val choice = menu(caption,
          List(
            callText,
            "Raise $1",
            "Raise $2",
            "Raise $3",
            "Fold    "))
        val actionBetAmount = bet_choice(choice)
        return actionBetAmount
      }
    }
  }
  
  // Returns (action, betAmount)
  def bet_choice(choice: Int): (String, Int) = {
    val actionBetAmount = choice match {
      case 0 => ("call", 0)
      case 1 => ("call", 0)
      case 2 => ("raise", 1)
      case 3 => ("raise", 2)
      case 4 => ("raise", 3)
      case 5 => ("fold", 0)
    }
    
    return actionBetAmount
  }

  def draw_menu: String = {
    val (c1, c2) = gPlayerHand(gPlayerHuman)
    val discard1 = "Discard " + c1
    val discard2 = "Discard " + c2
    val choice = menu(" Draw? ",
      List(
        "Keep",
        discard1,
        discard2))
    val action = draw_choice(choice)
    return action
  }

  def draw_choice(choice: Int): String = {
    val action = choice match {
      case 0 => "keep"
      case 1 => "keep"
      case 2 => "high"
      case 3 => "low"
    }

    return action
  }

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
    for (p <- 1 to gPlayerCount) {
      make_player_bet(p, 1)
    }
  }

  def deal_cards_around = {
    for (p <- 1 to gPlayerCount) {
      deal_a_card(p)
    }
  }
  
  def deal_cards_done = {
    for (p <- 1 to gPlayerCount) {
      discard_player_hand(p)
    }
  }

  def discard_player_hand(p: Int) = {
    val (c1, c2)  = gPlayerHand(p)
    
    discard(c1)
    discard(c2)
    
    gPlayerHand(p) = gHandFolded
  }
  
  def discard(card: String) = {
    val hi = card_high_value(card)
    val lo = card_low_value(card)

    if (hi > 0) {
      gCardDeckDiscard ::= (card, hi, lo)
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
  	  case h if h == gHandEmpty => {
  	    gPlayerHand(p) = (cc, gCardEmpty)
  	    return true
  	  }
  	  case (c1, gCardEmpty) if chi > d1 => {
  	    gPlayerHand(p) = (cc, c1)
  	    return true
  	  }
  	  case (c1, gCardEmpty) => {
  	    gPlayerHand(p) = (c1, cc)
  	    return true
  	  }
  	  case _ => {
  	    // Already had two cards.
  	    return false
  	  }
  	}
  }
  
  def make_player_bet(player: Int, bet: Int) = {
    gPlayerAmountStake(player) -= bet
    gPlayerAmountBet(player) += bet
    gPotAmount += bet
  }

  def decide_hands = {
    gPlayerBestHands = new_player_best_hand_map
    
    for (p <- 1 to gPlayerCount) {
      val hand = gPlayerHand(p)

      gHighLowList.foreach(hilo => {
        val handValue = hand_value(hand, hilo)
        decide_player_hand(p, hilo, handValue)
      })
    }

    val bestLowHands = gPlayerBestHands("low")
    val lowWinnerCount = bestLowHands.size
    
    val bestHighHands = gPlayerBestHands("high")
    val highWinnerCount = bestHighHands.size

    val lowPot = gPotAmount / 2  // Truncated; odd extra goes to high pot.
    val lowPotPerPlayer = lowPot / lowWinnerCount  // Again, odd extra goes to high pot.
    
    val highPot = gPotAmount - (lowPotPerPlayer * lowWinnerCount)
    val highPotPerPlayer = highPot / highWinnerCount  // Odd extra remains as extra ante.
    
    bestLowHands foreach (pv => {
      val (player, value) = pv
      gPlayerAmountLow(player) += 1
      text_cursor(player, 2); text_write("\u25bc")  // Down-arrow
      pay_winner(player, "low", lowPotPerPlayer)
    })
    
    bestHighHands foreach (pv => {
      val (player, value) = pv
      gPlayerAmountHigh(player) += 1
      text_cursor(player, 5); text_write("\u25b2")  // Up-arrow
      pay_winner(player, "high", highPotPerPlayer)
    })
    
	show_players_pot
	show_players_human
    gPlayerDealer = next_player(gPlayerDealer)
  	deal_cards_done
  	shuffle_deck_old
  }
  
  def pay_winner(player: Int, hilo: String, amount: Int) = {
    gPlayerAmountStake(player) += amount
    gPotAmount -= amount
    add_player_text(player, " $" + amount.toString())
  	write("Player "); write(player.toString()); write(" wins $"); write(amount.toString()); write(" for "); write(hilo); write(" hand"); nl
  }
  
  def decide_player_hand(player: Int, hilo: String, handValue: Int) = {
    val bestHands = gPlayerBestHands(hilo)
    val betterHandCount = bestHands count {case (p, v) => v > handValue}
    
    if (betterHandCount == 0) {
      // We are the best hand.
      
      val worseHandCount = bestHands count {case (p, v) => v < handValue}
      if (worseHandCount == 0) {
        // We may be tied with other hands.
        gPlayerBestHands(hilo) = (player, handValue) :: bestHands
      } else {
        // We are the only best hand (so far).
         gPlayerBestHands(hilo) = List((player, handValue))
      }
    }
  }
  
  def hand_value(hand: Tuple2[String, String], hilo: String): Int = {
    val value = (hilo, hand) match {
      case (_, ("x", _)) => {
        // Player has folded.
        return gWorstBetValue
      }
      
      case ("high", ("2", "2")) => {
        // Wild pair = aces.
        val aceValue = card_high_value("A")
        return 1000 * aceValue
      }
      case ("high", (c, "2")) => {
        // Wild makes pair.        
        val cardValue = card_high_value(c)
        return 1000 * cardValue
      }
      case ("high", ("2", c)) => {
        // Wild makes pair.        
        val cardValue = card_high_value(c)
        return 1000 * cardValue
      }
      case ("high", (c1, c2)) if c1.equals(c2) => {
        // Pair.        
        val cardValue = card_high_value(c1)
        return 1000 * cardValue
      }
      case ("high", (c1, c2)) if card_high_value(c1) > card_high_value(c2) => {
        // High card c1.    
        val cardValue1 = card_high_value(c1)
        val cardValue2 = card_high_value(c2)
        return (20 * cardValue1) + cardValue2
      }
      case ("high", (c1, c2)) => {
        // High card c2. (Should never occur due to card sorting, but cover if it does.)  
        val cardValue1 = card_high_value(c1)
        val cardValue2 = card_high_value(c2)
        return (20 * cardValue2) + cardValue1
      }
      
      case ("low", ("2", "2")) => {
        // Wild pair = low aces.
        val aceValue = card_low_value("A")
        return - (1000 * aceValue)
      }
      case ("low", ("A", "2")) => {
        // Wild treated as two.       
        val cardValue1 = card_low_value("2")
        val cardValue2 = card_low_value("A")
        return - ((20 * cardValue1) + cardValue2)
      }
      case ("low", ("2", "A")) => {
        // Wild treated as two. (Should never occur due to card sorting, but cover if it does.)       
        val cardValue1 = card_low_value("2")
        val cardValue2 = card_low_value("A")
        return - ((20 * cardValue1) + cardValue2)
      }
      case ("low", (c, "2")) => {
        // Wild treated as Ace, so 23 ties with A3.       
        val cardValue1 = card_low_value(c)
        val cardValue2 = card_low_value("A")
        return - ((20 * cardValue1) + cardValue2)
      }
      case ("low", ("2", c)) => {
        // Wild treated as Ace, so 23 ties with A3. (Should never occur due to card sorting, but cover if it does.)      
        val cardValue1 = card_low_value(c)
        val cardValue2 = card_low_value("A")
        return - ((20 * cardValue1) + cardValue2)
      }
      case ("low", (c1, c2)) if c1.equals(c2) => {
        // Low pair.        
        val cardValue = card_low_value(c1)
        return - (1000 * cardValue)
      }
      case ("low", (c1, c2)) if card_low_value(c1) > card_low_value(c2) => {
        // High card c1.      
        val cardValue1 = card_low_value(c1)
        val cardValue2 = card_low_value(c2)
        return - ((20 * cardValue1) + cardValue2)
      }
      case ("low", (c1, c2)) => {
        // High card c2. (Should never occur due to card sorting, but cover if it does.)
        val cardValue1 = card_low_value(c1)
        val cardValue2 = card_low_value(c2)
        return - ((20 * cardValue2) + cardValue1)
      }
      
      case _ => {
        // Safety net.
        return gWorstBetValue
      }
    }
    
    return value
  }

  def game_over(choice: Int): Boolean = {
    val gameIsOver = choice match {
      case 4 => true
      case _ => false
    }
    
    if (gameIsOver) {
      end_of_game
    }
    
    return gameIsOver
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
    losingPlayers foreach (pa => {
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
      text_write("#"); text_write(p.toString()); text_write(" "); text_write(a.toString())
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
        val (c1, c2) = gPlayerHand(p)
        text_cursor(p, 3)
        text_write(c1); text_write(c2)
      }
    })
  }

  def show_players_hands = {
    gPlayerAmountStake.toList.foreach(pa => {
      val (p, a) = pa
      text_cursor(p, 3)
      val (c1, c2) = gPlayerHand(p)
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
      val signText = a match {
        case x if x < 0 => " "
        case _ => " $"
      }
      text_write("#"); text_write(p.toString()); text_write(signText); text_write(a.toString())
      
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
  
  def add_player_text(player: Int, text: String) = {
    gPlayerText(player) += text
    text_cursor(player, 6); text_write(gPlayerText(player))
  }
  
  def new_player_amount_map(initialValue: Int): collection.mutable.Map[Int, Int] = {    
    val amountMap = (1 to gPlayerCount) map(p => (p, initialValue)) toMap
    var mutableMap = collection.mutable.Map(amountMap.toSeq: _*)
    
    return mutableMap
  }

  // TODO: (?) Store player hand as Tuple2[Tuple3[String, Int, Int], Tuple3[String, Int, Int]].
  def new_player_hand_map: collection.mutable.Map[Int, Tuple2[String, String]] = {
    val handMap = (1 to gPlayerCount) map(p => (p, gHandEmpty)) toMap
    var mutableMap = collection.mutable.Map(handMap.toSeq: _*)
    
    return mutableMap
  }

  // TODO: (?) Remove this if/when player hand includes each card as Tuple3[String, Int, Int].
  def card_low_value(card: String): Int = {
    val optionCard = gCardSuit.find {case (c, hi, lo) => c.equals(card)}
    val (c, hi, lo) = optionCard.getOrElse(gCardEmpty, 0, 0)
    
    return lo
  }
  
  // TODO: (?) Remove this if/when player hand includes each card as Tuple3[String, Int, Int].
  def card_high_value(card: String): Int = {
    val optionCard = gCardSuit.find {case (c, hi, lo) => c.equals(card)}
    val (c, hi, lo) = optionCard.getOrElse(gCardEmpty, 0, 0)
    
    return hi
  }
  
  // Maps from "high" vs. "low" to List[Player, Value].
  def new_player_best_hand_map: collection.mutable.Map[String, List[(Int, Int)]] = {
    val emptyHandList = List[(Int, Int)]()
    
    val handListMap = gHighLowList.map(hilo => (hilo, emptyHandList)) toMap
    var mutableMap = collection.mutable.Map(handListMap.toSeq: _*)
    
    return mutableMap
  }
}