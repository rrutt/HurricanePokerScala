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
    val brokePlayerCount = gPlayerAmountStake count { case (p, v) => v <= 0 }
    brokePlayerCount match {
      case 0 => {
        val choice = main_menu
        if (game_over(choice)) {
          return
        }
      }
      case 1 => {
        ask_ok_1(" A player is busted. ")
        end_of_game
        return
      }
      case n => {
        ask_ok_1(" " + n.toString() + " players are busted. ")
        end_of_game
        return
      }
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
        remainingBets match {
          case 0 => {
            // Convert "raise" to "call".
            process_player_bet_action(player, "call", 0, remainingPlayers, remainingBets, totalBet)
          }
          case _ => {
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
        }
      }
      case "fold" => {
        totalBet match {
          case 0 => {
            // Convert "fold" to "call".
            process_player_bet_action(player, "call", 0, remainingPlayers, remainingBets, totalBet)
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

  // Returns (action, betAmount)
  def get_action_bet(mode: String, player: Int, remainingPlayers: Int, remainingBets: Int, totalBet: Int): (String, Int) = {
    val hand = gPlayerHand(player)
    
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
      case "highrise" => {
        hand match {
          case (_, "2") => {
            // Raise on wild.
            return ("raise", 3)
          }
          case (c1, c2) if c1.equals(c2) && (card_high_value(c1) > 11) => {
            // Raise on A, K, Q pair.
            return ("raise", 3)
          }
          case (c1, c2) if c1.equals(c2) => {
            // Raise on any other pair.
            return ("raise", 2)
          }
          case (c1, _) if (card_high_value(c1) > 11) => {
            // Raise on unpaired A, K, Q.
            return ("raise", 1)
          }
          case (c1, _) if (card_high_value(c1) < 10) && (gPlayerAmountDraws(player) == 0) => {
            // Fold on unpaired poor high card if before draw.
            return ("fold", 0)
          }
          case (c1, _) if ((6 to 11) contains card_high_value(c1)) && (gPlayerAmountDraws(player) > 0) => {
            // Fold on unpaired medium high card if after draw.
            return ("fold", 0)
          }
          case (_, _) if (gPlayerAmountDraws(player) == 0) => {
            // Defensive raise if before draw.
            return ("raise", 1)
          }
          case (_, _) if (totalBet > 6) &&  (gPlayerAmountDraws(player) > 0) => {
            // Fold if REALLY BIG bet after draw.
            return ("fold", 0)
          }
          case _ => {
            return ("call", 0)
          }
        }        
      }
      case "pairwise" => {
        hand match {
          case (_, "2") => {
            // Raise on wild.
            return ("raise", 3)
          }
          case (c1, c2) if c1.equals(c2) && (card_high_value(c1) > 11) => {
            // Raise on A, K, Q pair.
            return ("raise", 3)
          }
          case (c1, c2) if c1.equals(c2) => {
            // Raise on any other pair.
            return ("raise", 2)
          }
          case (c1, _) if (card_high_value(c1) > 10) => {
            // Raise on unpaired face card.
            return ("raise", 1)
          }
          case (_, _) if (gPlayerAmountDraws(player) == 0) => {
            // Defensive raise if before draw.
            return ("raise", 1)
          }
          case (_, _) if (totalBet > 2) &&  (gPlayerAmountDraws(player) > 0) => {
            // Fold if big bet after draw.
            return ("fold", 0)
          }
          case _ => {
            return ("call", 0)
          }
        }
      }
      case "lowdown" => {
        hand match {
          case ("A", "2") => {
            // Raise on Acey-Duecy.
            return ("raise", 3)
          }
          case ("A", _) => {
            // Raise on Ace.
            return ("raise", 2)
          }
          case (c1, _) if (card_high_value(c1) < 7) => {
            // Raise on low high card.
            return ("raise", 1)
          }
          case (c1, c2) if (!c1.equals(c2)) && (card_high_value(c2) > 6) && (gPlayerAmountDraws(player) == 0) => {
            // Fold on unpaired high low card before draw.
            return ("fold", 0)
          }
          case (c1, c2) if (!c1.equals(c2)) && (card_high_value(c2) > 6) && (totalBet > 1) && (gPlayerAmountDraws(player) > 0) => {
            // Fold on unpaired high low card after draw and big bet
            return ("fold", 0)
          }
          case _ => {
            return ("call", 0)
          }
        }
      }
      case "hilo" => {
        hand match {
          case ("A", "2") => {
            // Raise on Acey-Duecy.
            return ("raise", 3)
          }
          case ("A", _) => {
            // Raise on Ace.
            return ("raise", 2)
          }
          case (c1, _) if (card_high_value(c1) < 7) => {
            // Raise on low high card.
            return ("raise", 1)
          }
          case (c1, c2) if (!c1.equals(c2)) && ((7 to 9) contains card_high_value(c1)) && ((7 to 9) contains card_high_value(c2)) && (totalBet > 1) => {
            // Fold if unpaired medium cards and big bet.
            return ("fold", 0)
          }
          case (c1, c2) if (!c1.equals(c2)) && ((7 to 9) contains card_high_value(c1)) && ((7 to 9) contains card_high_value(c2)) => {
            // Defensive raise if unpaired medium cards and small bet.
            return ("raise", 1)
          }
          case _ => {
            // Else same as highrise.
            val actionBetAmount = get_action_bet("highrise", player, remainingPlayers, remainingBets, totalBet)
            
            return actionBetAmount
          }
        }
      }
      case "foldout" => {
        hand match {
          case ("A", "2") => {
            // Raise on Acey-Duecy.
            return ("raise", 3)
          }
          case ("A", _) => {
            // Raise on Ace.
            return ("raise", 2)
          }
          case (c1, _) if (card_high_value(c1) < 6) => {
            // Raise on low high card.
            return ("raise", 1)
          }
          case (c1, c2) if (!c1.equals(c2)) && ((6 to 11) contains card_high_value(c1)) && ((6 to 11) contains card_high_value(c2)) => {
            // Fold if unpaired medium cards regardless of bet.
            return ("fold", 0)
          }
          case _ => {
            // Else same as hilo.
            val actionBetAmount = get_action_bet("hilo", player, remainingPlayers, remainingBets, totalBet)
            
            return actionBetAmount
          }
        }
      }
      case _ => {  // "random"
        val index = random_int(5) - 1
        val actionBetAmount = get_action_bet_index(index, remainingBets)
  	    
  	    return actionBetAmount
      }
    }
  }
  
  def get_action_draw(mode: String, player: Int): String = {
    val hand = gPlayerHand(player)
    
    mode match {
      case "human" => {
  	    val action = draw_menu
  	    
  	    return action
      }
      case "checker" => {
        return "keep"
      }
      case "highrise" => {
        hand match {
          case (c1, c2) if c1.equals(c2) => {
            // Keep a pair.
            return "keep"
          }
          case (c1, "2") if (card_high_value(c1) > 10) => {
            // Keep a face card wild pair.
            return "keep"
          }
          case (_, "2") => {
            // Toss "high" card to try for a higher wild pair.
            return "high"
          }
          case _ => {
            // Toss the lowest card.
            return "low"
          }
        }
      }
      case "pairwise" => {
        // Same as highrise.
        val action = get_action_draw("highrise", player)
        
        return action
      }
      case "lowdown" => {
        hand match {
          case ("A", "2") => {
            // Keep Acey-Deucey.
            return "keep"
          }
          case (c1, _) if (card_low_value(c1) < 7) => {
            // Keep low-valued "high" card.
            return "keep"
          }
          case _ => {
            // Toss high card.
            return "high"
          }
        }
      }
      case "hilo" => {
        hand match {
          case ("A", "2") => {
            // Keep Acey-Deucey.
            return "keep"
          }
          case (c1, _) if (card_low_value(c1) < 7) => {
            // Keep low-valued "high" card.
            return "keep"
          }
          case (c1, c2) if (!c1.equals(c2)) && (card_high_value(c1) < 12) && (card_low_value(c2) < 6) => {
            // Toss unpaired poor high card if good low card.
            return "high"
          }
          case _ => {
            // Else same as highrise.
            val action = get_action_draw("highrise", player)

            return action
          }
        }
      }
      case "foldout" => {
        // Same as hilo.
        val action = get_action_draw("hilo", player)

        return action
      }
      case _ => {  // "random"
        val index = random_int(3) - 1
        val action = get_action_draw_index(index)
  	    
  	    return action
      }
    }
  }

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

  def new_player_hand_map: collection.mutable.Map[Int, Tuple2[String, String]] = {
    val handMap = (1 to gPlayerCount) map(p => (p, gHandEmpty)) toMap
    var mutableMap = collection.mutable.Map(handMap.toSeq: _*)
    
    return mutableMap
  }

  def card_low_value(card: String): Int = {
    val optionCard = gCardSuit.find {case (c, hi, lo) => c.equals(card)}
    val (c, hi, lo) = optionCard.getOrElse(gCardEmpty, 0, 0)
    
    return lo
  }
  
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