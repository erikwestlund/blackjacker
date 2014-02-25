## # BlackjackeR: an R-Based Blackjack Simulator
## server.R

library(shiny)

# Card data
diamonds <- rep('D', 13)
hearts <- rep('H', 13)
clubs <- rep('C', 13)
spades <- rep('S', 13)
suits <- c(diamonds, hearts, clubs, spades)

cards <- rep(c(2, 3, 4, 5, 6, 7, 8, 9, 10, 'J', 'Q', 'K', 'A'), 4)
cardValues <- rep(c(2, 3, 4, 5, 6, 7, 8, 9, 10, 10, 10, 10, 'A'), 4)

deck_index <<- matrix(nrow=52, ncol=3)
deck_index[,1] <- cards
deck_index[,2] <- suits
deck_index[,3] <- cardValues

deck <<- seq(1, 52, 1)

## Hand index
dealer_starting_hands <- c(2, 3, 4, 5, 6, 7, 8, 9, 10, 'A')
player_starting_hands <- c(seq(5,20,1), 'A2', 'A3', 'A4', 'A5', 'A6', 'A7', 'A8', 'A9', 'A10', seq(22, 99, 11), 1010, 'AA')

all_hands <- c()
for(p in player_starting_hands){
  for(d in dealer_starting_hands){
    all_hands <- c(all_hands, paste(p, '_', d, sep=''))
  }
}

# columns:
## 1=hand 2=streategy 3=total 4=win 5=loss 6=bets
hand_index <- matrix(nrow=length(all_hands), ncol=9)
hand_index[,1] <- all_hands
colnames(hand_index) <- c('Hand', 'Player Hand', 'Dealer Show Card', 'Strategy', 'Total Hands', 'Wins', 'Losses', 'Bets Won/Lost', 'Win Rate')

hand_index[,5] <- 0
hand_index[,6] <- 0
hand_index[,7] <- 0
hand_index[,8] <- 0
hand_index[,9] <- 0

## Game Functions
# Any function that doesn't need reactive values goes here
get_cards <- function(deck, decks){
  return( rep(deck, decks) )
} 
get_shoe <- function(cards){
  return( sample(cards, length(cards), replace = FALSE, prob = NULL) )
}

move_card_to_back_of_deck <- function(shoe, card_count){
  total_cards <- length(shoe)
  
  # if last card in shoe, shuffle shoe
  if(card_count == total_cards){
    shoe <- get_shoe(shoe)
    card_count <- 1
  } else {
    # add card to end of deck
    last_card <- total_cards + 1
    shoe[last_card] <- shoe[1]
    
    #remove card from front of deck
    shoe <- shoe[-1]
    
    #bump the hand count
    card_count <- card_count + 1
  }
  
  return(list(shoe, card_count))
}

# get hand value... returns two value vector, soft and hard value
get_hand_value <- function(hand){     
  hand_soft_value <- 0
  hand_hard_value <- 0
  for(i in hand){
    card <- i[1]
    value <- deck_index[card,3]
    
    if(value == 'A'){
      soft_value <- 1
      hard_value <- 11
    } else {
      soft_value <- as.numeric(value)
      hard_value <- as.numeric(value)
    }
    
    hand_soft_value <- hand_soft_value + soft_value
    hand_hard_value <- hand_hard_value + hard_value
  }      
  return(c(hand_soft_value, hand_hard_value))
}

# get face cards
get_face_cards <- function(player_hand){
  cards_in_player_hand <- length(player_hand)
  
  player_cards <- ''
  for(i in 1:cards_in_player_hand){
    player_card <- player_hand[i]
    player_card_face <- paste(deck_index[player_card, 1], deck_index[player_card, 2], sep='')
    player_cards <- paste(player_cards, player_card_face)
  }
  
  return(player_cards)
}

# get final hand value
get_final_hand_value <- function(hand_value){
  soft_value <- hand_value[1]
  hard_value <- hand_value[2]
  
  if(hard_value > 21 & soft_value <= 21){
    # play soft value if hard value is a bust and soft value is not
    return(soft_value)
  } else if(soft_value > 21 & hard_value > 21){
    # if both soft and hard value are over 21, report the lower value (is this even possible?)
    return(min(hand_value))
  } else {
    # otherwise, return largest value
    return(max(hand_value))
  }
}

# player stands
player_stand <- function(player_hand_value){
  if(player_hand_value[2] > 21){
    final_value <- player_hand_value[1]
  } else{
    final_value <- player_hand_value[2]
  }
  
  return(final_value)
}

# calculate bets won if no blackjacks
calculate_bets_won <- function(player_final_hand_value, dealer_final_hand_value, double = FALSE){
  if(player_final_hand_value == dealer_final_hand_value){
    # push
    bets_won <- 0
  } else if (player_final_hand_value > 21) {
    # player busts
    bets_won <- -1
  }else if(player_final_hand_value <= 21 & dealer_final_hand_value > 21){
    # dealer busts, player wins
    bets_won <- 1
  } else if(player_final_hand_value > dealer_final_hand_value){
    # winner winner chicken dinner
    bets_won <- 1
  } else {
    # lose
    bets_won <- -1
  }
  
  ifelse(double == TRUE, return(2*bets_won), return(bets_won))
  
}

#updates hand index, returns new hand index
update_hand_index <- function(hand, this_hand_index){
  #player_hand, player_hand_value, player_final_hand_value, dealer_hand, dealer_hand_value, dealer_final_hand_value, shoe, card_count, bets_won, hand, player, dealer, move
  ## 1=hand 2=strategy 3=total 4=win 5=loss 6=bets
  bets_won <- hand[[9]]
  this_hand <- hand[[10]]
  player_hand <- hand[[11]]
  dealer_show_card <- hand[[12]]
  move <- hand[[13]]
  
  
  
  # get hand index row no.
  all_hands <- this_hand_index[,1]
  row_no <- match(this_hand, all_hands)

  this_hand_index[row_no, 2] <- player_hand
  this_hand_index[row_no, 3] <- dealer_show_card
  this_hand_index[row_no, 4] <- move

  this_hand_index[row_no, 5] <- sum(as.numeric(this_hand_index[row_no, 5]), 1)
  
  if(bets_won > 0){
    this_hand_index[row_no, 6] <- sum(as.numeric(this_hand_index[row_no, 6]), 1)
  } else if (bets_won < 0){
    this_hand_index[row_no, 7] <- sum(as.numeric(this_hand_index[row_no, 7]), 1)
  } 
  
  this_hand_index[row_no, 8] <- sum(as.numeric(this_hand_index[row_no, 8]), bets_won)

  this_hand_index[row_no, 9] <- round(as.numeric(this_hand_index[row_no, 6]) / as.numeric(this_hand_index[row_no, 5]), 2)
  
  return(this_hand_index)
}

# Testing vars
#decks <- function(){return(6)}
#soft17 <- function(){return('yes')}
#blackjack_pays <- function(){return(1.5)}
#doubling_allowed <- function(){return('yes')}
#splitdouble_allowed <- function(){return('yes')}
#surrender_allowed <- function(){return('yes')}
#starting_bankroll <- function(){return(2000)}
#wager <- function(){return(25)}
#stop_rule <- function(){return('bust')}
#stop_win <- function(){return(2000)}

#cards <- get_cards(deck, decks())
#shoe <- get_shoe(cards)

shinyServer(
  function(input, output) {
    ########################
    ### GET INPUTS
    ########################    
    
    ########################
    # Strategy
    ########################
    
    strategy_4_2 <- reactive({ input$'4_2' })
    strategy_4_3 <- reactive({ input$'4_3' })
    strategy_4_4 <- reactive({ input$'4_4' })
    strategy_4_5 <- reactive({ input$'4_5' })
    strategy_4_6 <- reactive({ input$'4_6' })
    strategy_4_7 <- reactive({ input$'4_7' })
    strategy_4_8 <- reactive({ input$'4_8' })
    strategy_4_9 <- reactive({ input$'4_9' })
    strategy_4_10 <- reactive({ input$'4_10' })    
    strategy_4_A <- reactive({ input$'4_A' })       
    
    strategy_5_2 <- reactive({ input$'5_2' })
    strategy_5_3 <- reactive({ input$'5_3' })
    strategy_5_4 <- reactive({ input$'5_4' })
    strategy_5_5 <- reactive({ input$'5_5' })
    strategy_5_6 <- reactive({ input$'5_6' })
    strategy_5_7 <- reactive({ input$'5_7' })
    strategy_5_8 <- reactive({ input$'5_8' })
    strategy_5_9 <- reactive({ input$'5_9' })
    strategy_5_10 <- reactive({ input$'5_10' })    
    strategy_5_A <- reactive({ input$'5_A' })       
    
    strategy_6_2 <- reactive({ input$'6_2' })
    strategy_6_3 <- reactive({ input$'6_3' })
    strategy_6_4 <- reactive({ input$'6_4' })
    strategy_6_5 <- reactive({ input$'6_5' })
    strategy_6_6 <- reactive({ input$'6_6' })
    strategy_6_7 <- reactive({ input$'6_7' })
    strategy_6_8 <- reactive({ input$'6_8' })
    strategy_6_9 <- reactive({ input$'6_9' })
    strategy_6_10 <- reactive({ input$'6_10' })    
    strategy_6_A <- reactive({ input$'6_A' })       
    
    strategy_7_2 <- reactive({ input$'7_2' })
    strategy_7_3 <- reactive({ input$'7_3' })
    strategy_7_4 <- reactive({ input$'7_4' })
    strategy_7_5 <- reactive({ input$'7_5' })
    strategy_7_6 <- reactive({ input$'7_6' })
    strategy_7_7 <- reactive({ input$'7_7' })
    strategy_7_8 <- reactive({ input$'7_8' })
    strategy_7_9 <- reactive({ input$'7_9' })
    strategy_7_10 <- reactive({ input$'7_10' })    
    strategy_7_A <- reactive({ input$'7_A' })   
    
    strategy_8_2 <- reactive({ input$'8_2' })
    strategy_8_3 <- reactive({ input$'8_3' })
    strategy_8_4 <- reactive({ input$'8_4' })
    strategy_8_5 <- reactive({ input$'8_5' })
    strategy_8_6 <- reactive({ input$'8_6' })
    strategy_8_7 <- reactive({ input$'8_7' })
    strategy_8_8 <- reactive({ input$'8_8' })
    strategy_8_9 <- reactive({ input$'8_9' })
    strategy_8_10 <- reactive({ input$'8_10' })    
    strategy_8_A <- reactive({ input$'8_A' })    
    
    strategy_9_2 <- reactive({ input$'9_2' })
    strategy_9_3 <- reactive({ input$'9_3' })
    strategy_9_4 <- reactive({ input$'9_4' })
    strategy_9_5 <- reactive({ input$'9_5' })
    strategy_9_6 <- reactive({ input$'9_6' })
    strategy_9_7 <- reactive({ input$'9_7' })
    strategy_9_8 <- reactive({ input$'9_8' })
    strategy_9_9 <- reactive({ input$'9_9' })
    strategy_9_10 <- reactive({ input$'9_10' })    
    strategy_9_A <- reactive({ input$'9_A' })    

    strategy_10_2 <- reactive({ input$'10_2' })
    strategy_10_3 <- reactive({ input$'10_3' })
    strategy_10_4 <- reactive({ input$'10_4' })
    strategy_10_5 <- reactive({ input$'10_5' })
    strategy_10_6 <- reactive({ input$'10_6' })
    strategy_10_7 <- reactive({ input$'10_7' })
    strategy_10_8 <- reactive({ input$'10_8' })
    strategy_10_9 <- reactive({ input$'10_9' })
    strategy_10_10 <- reactive({ input$'10_10' })    
    strategy_10_A <- reactive({ input$'10_A' })    
    
    strategy_11_2 <- reactive({ input$'11_2' })
    strategy_11_3 <- reactive({ input$'11_3' })
    strategy_11_4 <- reactive({ input$'11_4' })
    strategy_11_5 <- reactive({ input$'11_5' })
    strategy_11_6 <- reactive({ input$'11_6' })
    strategy_11_7 <- reactive({ input$'11_7' })
    strategy_11_8 <- reactive({ input$'11_8' })
    strategy_11_9 <- reactive({ input$'11_9' })
    strategy_11_10 <- reactive({ input$'11_10' })    
    strategy_11_A <- reactive({ input$'11_A' })    

    strategy_12_2 <- reactive({ input$'12_2' })
    strategy_12_3 <- reactive({ input$'12_3' })
    strategy_12_4 <- reactive({ input$'12_4' })
    strategy_12_5 <- reactive({ input$'12_5' })
    strategy_12_6 <- reactive({ input$'12_6' })
    strategy_12_7 <- reactive({ input$'12_7' })
    strategy_12_8 <- reactive({ input$'12_8' })
    strategy_12_9 <- reactive({ input$'12_9' })
    strategy_12_10 <- reactive({ input$'12_10' })    
    strategy_12_A <- reactive({ input$'12_A' })    
    
    strategy_13_2 <- reactive({ input$'13_2' })
    strategy_13_3 <- reactive({ input$'13_3' })
    strategy_13_4 <- reactive({ input$'13_4' })
    strategy_13_5 <- reactive({ input$'13_5' })
    strategy_13_6 <- reactive({ input$'13_6' })
    strategy_13_7 <- reactive({ input$'13_7' })
    strategy_13_8 <- reactive({ input$'13_8' })
    strategy_13_9 <- reactive({ input$'13_9' })
    strategy_13_10 <- reactive({ input$'13_10' })    
    strategy_13_A <- reactive({ input$'13_A' })    

    strategy_14_2 <- reactive({ input$'14_2' })
    strategy_14_3 <- reactive({ input$'14_3' })
    strategy_14_4 <- reactive({ input$'14_4' })
    strategy_14_5 <- reactive({ input$'14_5' })
    strategy_14_6 <- reactive({ input$'14_6' })
    strategy_14_7 <- reactive({ input$'14_7' })
    strategy_14_8 <- reactive({ input$'14_8' })
    strategy_14_9 <- reactive({ input$'14_9' })
    strategy_14_10 <- reactive({ input$'14_10' })    
    strategy_14_A <- reactive({ input$'14_A' })    

    strategy_15_2 <- reactive({ input$'15_2' })
    strategy_15_3 <- reactive({ input$'15_3' })
    strategy_15_4 <- reactive({ input$'15_4' })
    strategy_15_5 <- reactive({ input$'15_5' })
    strategy_15_6 <- reactive({ input$'15_6' })
    strategy_15_7 <- reactive({ input$'15_7' })
    strategy_15_8 <- reactive({ input$'15_8' })
    strategy_15_9 <- reactive({ input$'15_9' })
    strategy_15_10 <- reactive({ input$'15_10' })    
    strategy_15_A <- reactive({ input$'15_A' })    
    
    strategy_16_2 <- reactive({ input$'16_2' })
    strategy_16_3 <- reactive({ input$'16_3' })
    strategy_16_4 <- reactive({ input$'16_4' })
    strategy_16_5 <- reactive({ input$'16_5' })
    strategy_16_6 <- reactive({ input$'16_6' })
    strategy_16_7 <- reactive({ input$'16_7' })
    strategy_16_8 <- reactive({ input$'16_8' })
    strategy_16_9 <- reactive({ input$'16_9' })
    strategy_16_10 <- reactive({ input$'16_10' })    
    strategy_16_A <- reactive({ input$'16_A' })    

    strategy_17_2 <- reactive({ input$'17_2' })
    strategy_17_3 <- reactive({ input$'17_3' })
    strategy_17_4 <- reactive({ input$'17_4' })
    strategy_17_5 <- reactive({ input$'17_5' })
    strategy_17_6 <- reactive({ input$'17_6' })
    strategy_17_7 <- reactive({ input$'17_7' })
    strategy_17_8 <- reactive({ input$'17_8' })
    strategy_17_9 <- reactive({ input$'17_9' })
    strategy_17_10 <- reactive({ input$'17_10' })    
    strategy_17_A <- reactive({ input$'17_A' })
  
    strategy_18_2 <- reactive({ input$'18_2' })
    strategy_18_3 <- reactive({ input$'18_3' })
    strategy_18_4 <- reactive({ input$'18_4' })
    strategy_18_5 <- reactive({ input$'18_5' })
    strategy_18_6 <- reactive({ input$'18_6' })
    strategy_18_7 <- reactive({ input$'18_7' })
    strategy_18_8 <- reactive({ input$'18_8' })
    strategy_18_9 <- reactive({ input$'18_9' })
    strategy_18_10 <- reactive({ input$'18_10' })    
    strategy_18_A <- reactive({ input$'18_A' })
    
    strategy_19_2 <- reactive({ input$'19_2' })
    strategy_19_3 <- reactive({ input$'19_3' })
    strategy_19_4 <- reactive({ input$'19_4' })
    strategy_19_5 <- reactive({ input$'19_5' })
    strategy_19_6 <- reactive({ input$'19_6' })
    strategy_19_7 <- reactive({ input$'19_7' })
    strategy_19_8 <- reactive({ input$'19_8' })
    strategy_19_9 <- reactive({ input$'19_9' })
    strategy_19_10 <- reactive({ input$'19_10' })    
    strategy_19_A <- reactive({ input$'19_A' })

    strategy_20_2 <- reactive({ input$'20_2' })
    strategy_20_3 <- reactive({ input$'20_3' })
    strategy_20_4 <- reactive({ input$'20_4' })
    strategy_20_5 <- reactive({ input$'20_5' })
    strategy_20_6 <- reactive({ input$'20_6' })
    strategy_20_7 <- reactive({ input$'20_7' })
    strategy_20_8 <- reactive({ input$'20_8' })
    strategy_20_9 <- reactive({ input$'20_9' })
    strategy_20_10 <- reactive({ input$'20_10' })    
    strategy_20_A <- reactive({ input$'20_A' })
    
    strategy_A2_2 <- reactive({ input$'A2_2' })
    strategy_A2_3 <- reactive({ input$'A2_3' })
    strategy_A2_4 <- reactive({ input$'A2_4' })
    strategy_A2_5 <- reactive({ input$'A2_5' })
    strategy_A2_6 <- reactive({ input$'A2_6' })
    strategy_A2_7 <- reactive({ input$'A2_7' })
    strategy_A2_8 <- reactive({ input$'A2_8' })
    strategy_A2_9 <- reactive({ input$'A2_9' })
    strategy_A2_10 <- reactive({ input$'A2_10' })    
    strategy_A2_A <- reactive({ input$'A2_A' })    

    strategy_A3_2 <- reactive({ input$'A3_2' })
    strategy_A3_3 <- reactive({ input$'A3_3' })
    strategy_A3_4 <- reactive({ input$'A3_4' })
    strategy_A3_5 <- reactive({ input$'A3_5' })
    strategy_A3_6 <- reactive({ input$'A3_6' })
    strategy_A3_7 <- reactive({ input$'A3_7' })
    strategy_A3_8 <- reactive({ input$'A3_8' })
    strategy_A3_9 <- reactive({ input$'A3_9' })
    strategy_A3_10 <- reactive({ input$'A3_10' })    
    strategy_A3_A <- reactive({ input$'A3_A' })    
    
    strategy_A4_2 <- reactive({ input$'A4_2' })
    strategy_A4_3 <- reactive({ input$'A4_3' })
    strategy_A4_4 <- reactive({ input$'A4_4' })
    strategy_A4_5 <- reactive({ input$'A4_5' })
    strategy_A4_6 <- reactive({ input$'A4_6' })
    strategy_A4_7 <- reactive({ input$'A4_7' })
    strategy_A4_8 <- reactive({ input$'A4_8' })
    strategy_A4_9 <- reactive({ input$'A4_9' })
    strategy_A4_10 <- reactive({ input$'A4_10' })    
    strategy_A4_A <- reactive({ input$'A4_A' })    
    
    strategy_A5_2 <- reactive({ input$'A5_2' })
    strategy_A5_3 <- reactive({ input$'A5_3' })
    strategy_A5_4 <- reactive({ input$'A5_4' })
    strategy_A5_5 <- reactive({ input$'A5_5' })
    strategy_A5_6 <- reactive({ input$'A5_6' })
    strategy_A5_7 <- reactive({ input$'A5_7' })
    strategy_A5_8 <- reactive({ input$'A5_8' })
    strategy_A5_9 <- reactive({ input$'A5_9' })
    strategy_A5_10 <- reactive({ input$'A5_10' })    
    strategy_A5_A <- reactive({ input$'A5_A' })    
    
    strategy_A6_2 <- reactive({ input$'A6_2' })
    strategy_A6_3 <- reactive({ input$'A6_3' })
    strategy_A6_4 <- reactive({ input$'A6_4' })
    strategy_A6_5 <- reactive({ input$'A6_5' })
    strategy_A6_6 <- reactive({ input$'A6_6' })
    strategy_A6_7 <- reactive({ input$'A6_7' })
    strategy_A6_8 <- reactive({ input$'A6_8' })
    strategy_A6_9 <- reactive({ input$'A6_9' })
    strategy_A6_10 <- reactive({ input$'A6_10' })    
    strategy_A6_A <- reactive({ input$'A6_A' })    
    
    strategy_A7_2 <- reactive({ input$'A7_2' })
    strategy_A7_3 <- reactive({ input$'A7_3' })
    strategy_A7_4 <- reactive({ input$'A7_4' })
    strategy_A7_5 <- reactive({ input$'A7_5' })
    strategy_A7_6 <- reactive({ input$'A7_6' })
    strategy_A7_7 <- reactive({ input$'A7_7' })
    strategy_A7_8 <- reactive({ input$'A7_8' })
    strategy_A7_9 <- reactive({ input$'A7_9' })
    strategy_A7_10 <- reactive({ input$'A7_10' })    
    strategy_A7_A <- reactive({ input$'A7_A' })    
    
    strategy_A8_2 <- reactive({ input$'A8_2' })
    strategy_A8_3 <- reactive({ input$'A8_3' })
    strategy_A8_4 <- reactive({ input$'A8_4' })
    strategy_A8_5 <- reactive({ input$'A8_5' })
    strategy_A8_6 <- reactive({ input$'A8_6' })
    strategy_A8_7 <- reactive({ input$'A8_7' })
    strategy_A8_8 <- reactive({ input$'A8_8' })
    strategy_A8_9 <- reactive({ input$'A8_9' })
    strategy_A8_10 <- reactive({ input$'A8_10' })    
    strategy_A8_A <- reactive({ input$'A8_A' })    
    
    strategy_A9_2 <- reactive({ input$'A9_2' })
    strategy_A9_3 <- reactive({ input$'A9_3' })
    strategy_A9_4 <- reactive({ input$'A9_4' })
    strategy_A9_5 <- reactive({ input$'A9_5' })
    strategy_A9_6 <- reactive({ input$'A9_6' })
    strategy_A9_7 <- reactive({ input$'A9_7' })
    strategy_A9_8 <- reactive({ input$'A9_8' })
    strategy_A9_9 <- reactive({ input$'A9_9' })
    strategy_A9_10 <- reactive({ input$'A9_10' })    
    strategy_A9_A <- reactive({ input$'A9_A' })
    
    strategy_22_2 <- reactive({ input$'22_2' })
    strategy_22_3 <- reactive({ input$'22_3' })
    strategy_22_4 <- reactive({ input$'22_4' })
    strategy_22_5 <- reactive({ input$'22_5' })
    strategy_22_6 <- reactive({ input$'22_6' })
    strategy_22_7 <- reactive({ input$'22_7' })
    strategy_22_8 <- reactive({ input$'22_8' })
    strategy_22_9 <- reactive({ input$'22_9' })
    strategy_22_10 <- reactive({ input$'22_10' })    
    strategy_22_A <- reactive({ input$'22_A' })
    
    strategy_33_2 <- reactive({ input$'33_2' })
    strategy_33_3 <- reactive({ input$'33_3' })
    strategy_33_4 <- reactive({ input$'33_4' })
    strategy_33_5 <- reactive({ input$'33_5' })
    strategy_33_6 <- reactive({ input$'33_6' })
    strategy_33_7 <- reactive({ input$'33_7' })
    strategy_33_8 <- reactive({ input$'33_8' })
    strategy_33_9 <- reactive({ input$'33_9' })
    strategy_33_10 <- reactive({ input$'33_10' })    
    strategy_33_A <- reactive({ input$'33_A' })
    
    strategy_44_2 <- reactive({ input$'44_2' })
    strategy_44_3 <- reactive({ input$'44_3' })
    strategy_44_4 <- reactive({ input$'44_4' })
    strategy_44_5 <- reactive({ input$'44_5' })
    strategy_44_6 <- reactive({ input$'44_6' })
    strategy_44_7 <- reactive({ input$'44_7' })
    strategy_44_8 <- reactive({ input$'44_8' })
    strategy_44_9 <- reactive({ input$'44_9' })
    strategy_44_10 <- reactive({ input$'44_10' })    
    strategy_44_A <- reactive({ input$'44_A' })
    
    strategy_55_2 <- reactive({ input$'55_2' })
    strategy_55_3 <- reactive({ input$'55_3' })
    strategy_55_4 <- reactive({ input$'55_4' })
    strategy_55_5 <- reactive({ input$'55_5' })
    strategy_55_6 <- reactive({ input$'55_6' })
    strategy_55_7 <- reactive({ input$'55_7' })
    strategy_55_8 <- reactive({ input$'55_8' })
    strategy_55_9 <- reactive({ input$'55_9' })
    strategy_55_10 <- reactive({ input$'55_10' })    
    strategy_55_A <- reactive({ input$'55_A' })
    
    strategy_66_2 <- reactive({ input$'66_2' })
    strategy_66_3 <- reactive({ input$'66_3' })
    strategy_66_4 <- reactive({ input$'66_4' })
    strategy_66_5 <- reactive({ input$'66_5' })
    strategy_66_6 <- reactive({ input$'66_6' })
    strategy_66_7 <- reactive({ input$'66_7' })
    strategy_66_8 <- reactive({ input$'66_8' })
    strategy_66_9 <- reactive({ input$'66_9' })
    strategy_66_10 <- reactive({ input$'66_10' })    
    strategy_66_A <- reactive({ input$'66_A' })
    
    strategy_77_2 <- reactive({ input$'77_2' })
    strategy_77_3 <- reactive({ input$'77_3' })
    strategy_77_4 <- reactive({ input$'77_4' })
    strategy_77_5 <- reactive({ input$'77_5' })
    strategy_77_6 <- reactive({ input$'77_6' })
    strategy_77_7 <- reactive({ input$'77_7' })
    strategy_77_8 <- reactive({ input$'77_8' })
    strategy_77_9 <- reactive({ input$'77_9' })
    strategy_77_10 <- reactive({ input$'77_10' })    
    strategy_77_A <- reactive({ input$'77_A' })
    
    strategy_88_2 <- reactive({ input$'88_2' })
    strategy_88_3 <- reactive({ input$'88_3' })
    strategy_88_4 <- reactive({ input$'88_4' })
    strategy_88_5 <- reactive({ input$'88_5' })
    strategy_88_6 <- reactive({ input$'88_6' })
    strategy_88_7 <- reactive({ input$'88_7' })
    strategy_88_8 <- reactive({ input$'88_8' })
    strategy_88_9 <- reactive({ input$'88_9' })
    strategy_88_10 <- reactive({ input$'88_10' })    
    strategy_88_A <- reactive({ input$'88_A' })
    
    strategy_99_2 <- reactive({ input$'99_2' })
    strategy_99_3 <- reactive({ input$'99_3' })
    strategy_99_4 <- reactive({ input$'99_4' })
    strategy_99_5 <- reactive({ input$'99_5' })
    strategy_99_6 <- reactive({ input$'99_6' })
    strategy_99_7 <- reactive({ input$'99_7' })
    strategy_99_8 <- reactive({ input$'99_8' })
    strategy_99_9 <- reactive({ input$'99_9' })
    strategy_99_10 <- reactive({ input$'99_10' })    
    strategy_99_A <- reactive({ input$'99_A' })
    
    strategy_1010_2 <- reactive({ input$'1010_2' })
    strategy_1010_3 <- reactive({ input$'1010_3' })
    strategy_1010_4 <- reactive({ input$'1010_4' })
    strategy_1010_5 <- reactive({ input$'1010_5' })
    strategy_1010_6 <- reactive({ input$'1010_6' })
    strategy_1010_7 <- reactive({ input$'1010_7' })
    strategy_1010_8 <- reactive({ input$'1010_8' })
    strategy_1010_9 <- reactive({ input$'1010_9' })
    strategy_1010_10 <- reactive({ input$'1010_10' })    
    strategy_1010_A <- reactive({ input$'1010_A' })
    
    strategy_AA_2 <- reactive({ input$'AA_2' })
    strategy_AA_3 <- reactive({ input$'AA_3' })
    strategy_AA_4 <- reactive({ input$'AA_4' })
    strategy_AA_5 <- reactive({ input$'AA_5' })
    strategy_AA_6 <- reactive({ input$'AA_6' })
    strategy_AA_7 <- reactive({ input$'AA_7' })
    strategy_AA_8 <- reactive({ input$'AA_8' })
    strategy_AA_9 <- reactive({ input$'AA_9' })
    strategy_AA_10 <- reactive({ input$'AA_10' })    
    strategy_AA_A <- reactive({ input$'AA_A' })
    
    ########################
    # Games Rules
    ########################
    
    decks <- reactive({ input$decks })
    soft17 <- reactive({ input$soft17 })
    blackjack_pays <- reactive({ input$blackjack_pays_numerator/input$blackjack_pays_denominator })
    doubling_allowed <- reactive({ input$doubling_allowed })
    splitdouble_allowed <- reactive({ input$splitdouble_allowed })
    surrender_allowed <- reactive({ input$surrender_allowed })
    
    ########################
    # Simulation Settings
    ########################
    
    starting_bankroll <- reactive({ input$bankroll })
    wager <- reactive({ input$wager })
    stop_rule <- reactive({ input$stop_rule })
    stop_loss <- reactive({ input$stop_loss })
    stop_hands <- reactive({ input$stop_hands })
    win_loss_lost <- reactive({ input$win_loss_lost })
    win_loss_won <- reactive({ input$win_loss_won })
    
    ########################
    # RUN SIMULATION
    ########################    
    
    # hit
    hit_card <- function(hand, shoe, card_count){
      hand <- c(hand, shoe[1])
      move_card <- move_card_to_back_of_deck(shoe, card_count)
      shoe <- move_card[[1]]
      card_count <- move_card[[2]]      
      
      return(list(hand, shoe, card_count))
    }
    
    # Player: decide whether to hit card
    decide_to_hit_player <- function(dealer_show_card, hand_value){
     
      # after initial decision, we decide only based upon the hard value, unless hard is bust and soft is not
      soft_value <- hand_value[1]
      hard_value <- hand_value[2]
      
      if(hard_value == 21 | soft_value == 21){
        # always stay on 21
        return(FALSE)
      } else if(hard_value > 21 & soft_value > 21){
        # busted, so no more hits
        return(FALSE)
      } else if (hard_value > 21 & soft_value <= 20){
        # hard is bust, but soft is not, decide on soft
        strategy_cards <- soft_value
      } else if(hard_value < 21) {
        strategy_cards <- hard_value
      } else {
        # other situations?  shouldn't exist, but return false
        return(FALSE)
      }
      
      if(strategy_cards <= 4){
        # when splitting aces have to hit... 
        return(TRUE)
      } else{
        player_dealer_combination <- paste('strategy_', strategy_cards, '_', dealer_show_card, sep='')   
        strategy <- get('player_dealer_combination')
        decision <- do.call(strategy, list())        
      } 
      
      
      if(decision == 'S'){
        # stand if      
        hit <- FALSE
      } else {
        # if not stand, hit (equivalent to if decision = Dh, Rh, P, Pd, etc)  
        hit <- TRUE
      } 
      
      return(hit)
    }    
    
    # player: play out hand 
    player_finish_hand <- function(initial_hit, dealer_show_card, player_hand, player_hand_value, shoe, card_count){
      
      hit <- initial_hit
      while(hit == TRUE){
        hit_details <- hit_card(player_hand, shoe, card_count)
        player_hand <- hit_details[[1]]
        shoe <- hit_details[[2]]
        card_count <- hit_details[[3]]
        
        player_hand_value <- get_hand_value(player_hand)
        hit <- decide_to_hit_player(dealer_show_card, player_hand_value)
      }
      
      player_final_hand_value <- get_final_hand_value(player_hand_value)
      return(list(player_hand, player_hand_value, player_final_hand_value, shoe, card_count))
      
    }
        
    decide_to_hit_dealer <- function(hand_value){
      soft_value <- hand_value[1]
      hard_value <- hand_value[2]     
      
      if(soft_value == 17 & soft17() == 'yes'){
        # hit if dealers hits soft 17 and soft hand is 17
        hit <- TRUE
      } else if (soft_value == 17 & soft17() == 'no'){
        # stay if dealer does not hit soft 17 and value is 17
        hit <- FALSE
      } else if(hard_value == 17){
        # dealer always stays on hard 17
        hit <- FALSE
      } else if(soft_value >= 18 & soft_value <= 21){
        # dealer stays on soft 18 to 21
        hit <- FALSE
      } else if(hard_value >= 18 & hard_value <= 21){
        # dealer stays on hard 18 to 21
        hit <- FALSE
      } else if(soft_value < 17 & hard_value > 21) {
        # hit if soft value less than 17 but hard value busted
        hit <- TRUE
      } else if(soft_value < 17 & hard_value < 17) {
        # hit if soft and hard value less than 17
        hit <- TRUE
      } else {
        # otherwise stand
        hit <- FALSE
      }
      
      return(hit)
    }
    

    # dealer plays out hand
    dealer_finish_hand <- function(dealer_hand, dealer_hand_value, shoe, card_count){

      hit <- decide_to_hit_dealer(dealer_hand_value)

      while(hit == TRUE){
        hit_details <- hit_card(dealer_hand, shoe, card_count)
        dealer_hand <- hit_details[[1]]
        shoe <- hit_details[[2]]
        card_count <- hit_details[[3]]
          
        dealer_hand_value <- get_hand_value(dealer_hand)
        hit <- decide_to_hit_dealer(dealer_hand_value)
      }
      
      dealer_final_hand_value <- get_final_hand_value(dealer_hand_value)
      
      return(list(dealer_hand, dealer_hand_value, dealer_final_hand_value, shoe, card_count))
      
    }
    
    # get hand report
    get_hand_report <- function(hand_details){
      player_hand <- hand_details[[1]]
      player_hand_value <- hand_details[[2]]
      player_final_hand_value <- hand_details[[3]]
      dealer_hand <- hand_details[[4]]
      dealer_hand_value <- hand_details[[5]]
      dealer_final_hand_value <- hand_details[[6]]      
      shoe <- hand_details[[7]]
      card_count <- hand_details[[8]]
      bets_won <- hand_details[[9]]
      player_dealer_combination <- hand_details[[10]]
      move <- hand_details[[11]]
      
      player_face_cards <- get_face_cards(player_hand)      
      player_hand_report <- paste('<td><span style="font-family: Menlo, Courier New; font-size: 11px;">', player_face_cards, '</span></td><td>', player_final_hand_value, '</td>', sep='')
      
      dealer_face_cards <- get_face_cards(dealer_hand)      
      dealer_hand_reports <- paste('<td><span style="font-family: Menlo, Courier New; font-size: 11px;">', dealer_face_cards, '</span></td><td>', dealer_final_hand_value, '</td>', sep='')
      
      
      if(bets_won == 0){
        result <- '<td class="push"><span style="color: #ccc; font-weight: bold;">Push</span></td>'
      } else if(bets_won == -0.5){
        result <- '<td class="surrender"><span style="color: #cc3d3d; font-weight: bold;">Surrender</span>'
      } else if (bets_won == 1){
        result <- '<td class="win"><span style="color: #24bb24; font-weight: bold;">Win</span></td>'
      }  else if(bets_won == blackjack_pays()){
        result <- paste('<td class="blackjack"><span style="color: #149614; font-weight: bold;">Blackjack</span></td>', sep='')
      } else if (bets_won == -1 & length(dealer_hand) == 2 & dealer_final_hand_value == 21){
        result <- '<td class="dealer_blackjack"><span style="color: #ff0000; font-weight: bold;">Lose to Blackjack</span></td>'        
      } else if(bets_won == 2){
        result <- '<td class="win"><span style="color: #24bb24; font-weight: bold;">Win Double</span></td>'
      } else if(bets_won == -2){
        result <- '<td class="lose"><span style="color: #ff0000; font-weight: bold;">Lose Double</span></td>'        
      }else {
         result <- '<td class="lose"><span style="color: #ff0000; font-weight: bold;">Lose</span></td>'        
      }
            
      return(paste(result, player_hand_report, dealer_hand_reports, sep=''))
      
    }
        
    # play hand
    #   return wagers won or lost
    play_hand <- function(shoe, card_count){
      # number cards dealt in hand.  count each card.  we'll pop these to the end of the deck when done.
      all_cards <- c()
      player_hand <- c()
      dealer_hand <- c()
      player_card_values <- c()
                  
      # deal initial cards
      for(i in 1:2){
        player_hand <- c(player_hand, shoe[1])
        player_card_values <- c(player_card_values, deck_index[shoe[1], 3])
        move_card <- move_card_to_back_of_deck(shoe, card_count)
        shoe <- move_card[[1]]
        card_count <- move_card[[2]]
        
        dealer_hand <- c(dealer_hand, shoe[1])
        move_card <- move_card_to_back_of_deck(shoe, card_count)
        shoe <- move_card[[1]]
        card_count <- move_card[[2]]
      }
      
      dealer_show_card <- deck_index[dealer_hand[1], 3]
      
      player_hand_value <- get_hand_value(player_hand)
      dealer_hand_value <- get_hand_value(dealer_hand)
      
      if(player_card_values[1] == player_card_values[2]){
        # if cards are the same, concatenate them
        player_initial_two_cards <- paste(player_card_values[1], player_card_values[2], sep='')
      } else if("A" %in% player_card_values == TRUE){
        # if cards contain an Ace, put Ace first, concatenate other card
        if(player_card_values[1] == 'A'){
          player_initial_two_cards <- paste('A', player_card_values[2], sep='')
        } else{
          player_initial_two_cards <- paste('A', player_card_values[1], sep='')
        }
      } else {
        #otherwise, just use the hand value. 
        player_initial_two_cards <- player_hand_value[1]
      }
      
      dealer_show_card <- deck_index[dealer_hand[1], 3]
      initial_hand <- paste(player_initial_two_cards, '_', dealer_show_card, sep='')
      player_dealer_combination <- paste('strategy_', initial_hand, sep='')

      dealer_blackjack <- FALSE
      player_blackjack <- FALSE
      surrender <- FALSE
      double <- FALSE
      split <- FALSE
      
      if(21 %in% dealer_hand_value == TRUE & 21 %in% player_hand_value == TRUE){
        # if dealer has blackjack and player has blackjack, push
        bets_won <- 0
        dealer_blackjack <- TRUE
        player_blackjack <- TRUE
        decision <- ''
      } else if(21 %in% dealer_hand_value == TRUE & 21 %in% player_hand_value == FALSE){
        # if dealer has blackjack and player does not, player loses
        bets_won <- -1
        decision <- ''
      } else if(21 %in% dealer_hand_value == FALSE & 21 %in% player_hand_value == TRUE){
        # if player has blackjack and dealer does not, player wins with blackjack
        bets_won <- blackjack_pays()
        player_blackjack <- TRUE
        decision <- ''
      } else {
        # play out rest of hand
        
        strategy <- get('player_dealer_combination')
        decision <- do.call(strategy, list())
        
        ## Initial check
        if(decision == 'Dh'){
          # double down, else hit          
          if(doubling_allowed() == 'yes'){
            decision <- 'D'        
            double <- TRUE
          } else{
            decision <- 'H'            
          }
        } else if(decision == 'Ds'){
          # double down, else stand
          if(doubling_allowed() == 'yes'){
            decision <- 'D'
            double <- TRUE
          } else{
            decision <- 'S'            
          }          
        } else if(decision == 'Pd'){
          # split and double, else hit
          if(splitdouble_allowed() == 'no'){
            decision <- 'H'
          }
          
         #   double <- TRUE
          #split <- TRUE
        } else if(decision == 'Rh'){
          # surrender, else hit
          if(surrender_allowed() == 'yes'){
            bets_won <- -0.5
            surrender <- TRUE
          } else {
            decision <- 'H'
          }
        } 
        
        if(decision == 'H' | decision == 'P' | decision == 'Pd' | decision == 'D'){
          initial_hit <- TRUE
        } else {
          initial_hit <- FALSE
        }
        
        ## Play the hand out
        if(player_blackjack != TRUE & dealer_blackjack != TRUE & surrender != TRUE){
          if(decision == 'P'){
            # split
            hand_one <- player_hand[1]
            player_hand_value_one <- get_hand_value(hand_one)
            
            hand_two <- player_hand[2]
            player_hand_value_two <- get_hand_value(hand_two)
                                                    
            get_player_final_hand_one <- player_finish_hand(initial_hit, dealer_show_card, hand_one, player_hand_value_one, shoe, card_count)
            player_hand_one <- get_player_final_hand_one[[1]]
            player_hand_value_one <- get_player_final_hand_one[[2]]
            player_final_hand_value_one <- get_player_final_hand_one[[3]]
            shoe <- get_player_final_hand_one[[4]]
            card_count <- get_player_final_hand_one[[5]]   
            
            get_player_final_hand_two <- player_finish_hand(initial_hit, dealer_show_card, hand_two, player_hand_value_two, shoe, card_count)
            player_hand_two <- get_player_final_hand_two[[1]]
            player_hand_value_two <- get_player_final_hand_two[[2]]
            player_final_hand_value_two <- get_player_final_hand_two[[3]]
            shoe <- get_player_final_hand_two[[4]]
            card_count <- get_player_final_hand_two[[5]]   
            
            split <- TRUE
          } else if (decision == 'Pd'){
            # split then double
            hand_one <- player_hand[1]
            player_hand_value_one <- get_hand_value(hand_one)
            
            hand_two <- player_hand[2]
            player_hand_value_two <- get_hand_value(hand_two)
            
            hit_details_one <- hit_card(hand_one, shoe, card_count)
            player_hand_one <- hit_details_one[[1]]
            shoe <- hit_details_one[[2]]
            card_count <- hit_details_one[[3]]            
            player_hand_value_one <- get_hand_value(player_hand_one)  
            player_final_hand_value_one <- get_final_hand_value(player_hand_value_one)            

            hit_details_two <- hit_card(hand_two, shoe, card_count)
            player_hand_two <- hit_details_two[[1]]
            shoe <- hit_details_two[[2]]
            card_count <- hit_details_two[[3]]            
            player_hand_value_two <- get_hand_value(player_hand_two)  
            player_final_hand_value_two <- get_final_hand_value(player_hand_value_two)            
            
            split <- TRUE
            double <- TRUE
            
          } else if (decision == 'D'){
            # double
            hit_details <- hit_card(player_hand, shoe, card_count)
            player_hand <- hit_details[[1]]
            shoe <- hit_details[[2]]
            card_count <- hit_details[[3]]
            
            player_hand_value <- get_hand_value(player_hand)  
            player_final_hand_value <- get_final_hand_value(player_hand_value)
            double <- TRUE
          } else if(decision == 'H'){
            # hit
            get_player_final_hand <- player_finish_hand(initial_hit, dealer_show_card, player_hand, player_hand_value, shoe, card_count)
            player_hand <- get_player_final_hand[[1]]
            player_hand_value <- get_player_final_hand[[2]]
            player_final_hand_value <- get_player_final_hand[[3]]
            shoe <- get_player_final_hand[[4]]
            card_count <- get_player_final_hand[[5]]                                                      
          } 
        }
        
        # Note:  stand: do nothing for player. player_hand and player_hand_value will stand
      }
      
      # player final hand value
      player_final_hand_value <- get_final_hand_value(player_hand_value)
      
      # Play out dealer hand
      if(player_blackjack == TRUE | dealer_blackjack == TRUE){
        dealer_final_hand_value <- get_final_hand_value(dealer_hand_value)
      } else {
        get_dealer_final_hand <- dealer_finish_hand(dealer_hand, dealer_hand_value, shoe, card_count)  
        dealer_hand <- get_dealer_final_hand[[1]]
        dealer_hand_value <- get_dealer_final_hand[[2]]
        dealer_final_hand_value <- get_dealer_final_hand[[3]]
        shoe <- get_dealer_final_hand[[4]]
        card_count <- get_dealer_final_hand[[5]]                
      }
      
      
      # normal scenario, calculate bet win/loss
      if(player_blackjack == FALSE & dealer_blackjack == FALSE & surrender == FALSE){
        if(double == TRUE & split == FALSE){
          bets_won <- calculate_bets_won(player_final_hand_value, dealer_final_hand_value, double = TRUE)
        } else  if(double == FALSE & split == TRUE){
          bets_won_one <- calculate_bets_won(player_final_hand_value_one, dealer_final_hand_value, double = FALSE)
          bets_won_two <- calculate_bets_won(player_final_hand_value_two, dealer_final_hand_value, double = FALSE)
          bets_won <- bets_won_one + bets_won_two
        } else  if(double == TRUE & split == TRUE){
          bets_won_one <- calculate_bets_won(player_final_hand_value_one, dealer_final_hand_value, double = TRUE)
          bets_won_two <- calculate_bets_won(player_final_hand_value_two, dealer_final_hand_value, double = TRUE)
          bets_won <- bets_won_one + bets_won_two
        } else{
          bets_won <- calculate_bets_won(player_final_hand_value, dealer_final_hand_value, double = FALSE)  
        }        
      }  
      
      if(split == TRUE){
        hand_details_one <- list(player_hand_one, player_hand_value_one, player_final_hand_value_one, dealer_hand, dealer_hand_value, dealer_final_hand_value, shoe, card_count, bets_won_one, initial_hand, player_initial_two_cards, dealer_show_card, decision)
        hand_report_one <- get_hand_report(hand_details_one)
        hand_one <- list(hand_details_one, hand_report_one)
        
        hand_details_two <- list(player_hand_two, player_hand_value_two, player_final_hand_value_two, dealer_hand, dealer_hand_value, dealer_final_hand_value, shoe, card_count, bets_won_two, initial_hand, player_initial_two_cards, dealer_show_card,decision)      
        hand_report_two <- get_hand_report(hand_details_two)
        hand_two <- list(hand_details_two, hand_report_two)
        
        hands <- list(hand_one, hand_two)
      } else {
          hand_details <- list(player_hand, player_hand_value, player_final_hand_value, dealer_hand, dealer_hand_value, dealer_final_hand_value, shoe, card_count, bets_won, initial_hand, player_initial_two_cards, dealer_show_card, decision)
          hand_report <- get_hand_report(hand_details)
          hand <- list(hand_details, hand_report)
          
          hands <- list(hand)
      }
      

      return(hands)      
    }
        
    simulation <- function(decks){
      card_count <- 1
      shoe <- get_shoe(get_cards(deck, decks))
      
      bank_roll <- starting_bankroll()
      bank_roll_history <- c()
      wager <- wager()
      bets_won <- 0
      bets_lost <- 0
      money_won <- 0
      
      sim_hand_index <- hand_index
      
      html <- '<table id="hand_report"><tr><td class="hand_no">No.</td><td class="move">Move</td><td class="result">Result</td><td class="player_hand">Player Hand</td><td class="value">Value</td><td class="dealer_hand">Dealer Hand</td><td class="value">Value</td><td class="bets_wonlost">Bets Won</td><td class="bets_wonlost">Bets Lost</td><td class="money">Money Won</td><td class="bankroll">Bank Roll</td></tr>'
      
      hand_count <- 1
      continue <- TRUE
      
      while(continue == TRUE){
        hand <- play_hand(shoe, card_count)
        
        if(length(hand) == 2){
          
          # hand one
          bets_won_one <- hand[[1]][[1]][[9]]
          hand_combination_one <- hand[[1]][[1]][[10]]
          move_made_one <- hand[[1]][[1]][[11]]          
          
          if(bets_won_one > 0){
            bets_won <- bets_won + 1
          } else if(bets_won_one < 0){
            bets_lost <- bets_lost + 1
          }
          
          money_won <- money_won + wager*bets_won_one
          
          if(money_won < 0){
            money_won_class <- 'bank_roll_value_negative'
          } else if(money_won > 0){
            money_won_class <- 'bank_roll_value_positive'
          } else {
            money_won_class <- 'bank_roll_value_neutral'
          }          
          
          bank_roll <- bank_roll + wager*bets_won_one
          bank_roll_history <- c(bank_roll_history, bank_roll)
          
          if(bank_roll < 0){
            bank_roll_class <- 'bank_roll_value_negative'
          } else if(bank_roll > 0){
            bank_roll_class <- 'bank_roll_value_positive'
          } else {
            bank_roll_class <- 'bank_roll_value_neutral'
          }
          
          hand_1_output <- paste('<tr><td>', hand_count, '</td><td class="', move_made_one, '">', move_made_one, '</td>', hand[[1]][[2]], '<td>', bets_won, '</td><td>', bets_lost, '</td><td class="', money_won_class, '">$', money_won, '</td><td class="', bank_roll_class, '">$', bank_roll, '</td></tr>', sep='')
          
          sim_hand_index <- update_hand_index(hand[[1]][[1]], sim_hand_index)
          
          # hand two
          bets_won_two <- hand[[2]][[1]][[9]]
          hand_combination_two <- hand[[2]][[1]][[10]]
          move_made_two <- hand[[2]][[1]][[11]]          
          
          if(bets_won_two > 0){
            bets_won <- bets_won + 1
          } else if(bets_won_two < 0){
            bets_lost <- bets_lost + 1
          }          
          
          money_won <- money_won + wager*bets_won_two
          
          if(money_won < 0){
            money_won_class <- 'bank_roll_value_negative'
          } else if(money_won > 0){
            money_won_class <- 'bank_roll_value_positive'
          } else {
            money_won_class <- 'bank_roll_value_neutral'
          }             
          
          bank_roll <- bank_roll + wager*bets_won_two
          bank_roll_history <- c(bank_roll_history, bank_roll)
          
          if(bank_roll < 0){
            bank_roll_class <- 'bank_roll_value_negative'
          } else if(bank_roll > 0){
            bank_roll_class <- 'bank_roll_value_positive'
          } else {
            bank_roll_class <- 'bank_roll_value_neutral'
          }
          
          hand_2_output <- paste('<tr><td></td><td class="', move_made_two, '">', move_made_two, '</td>', hand[[2]][[2]], '<td>', bets_won, '</td><td>', bets_lost, '</td><td class="', money_won_class, '">$', money_won, '</td><td class="', bank_roll_class, '">$', bank_roll, '</td></tr>', sep='')
          
          output <- paste(hand_1_output, hand_2_output, sep='')
          shoe <- hand[[2]][[1]][[7]]
          card_count <- hand[[2]][[1]][[8]]
          
          sim_hand_index <- update_hand_index(hand[[2]][[1]], sim_hand_index)
          
        } else {
          bets_won_hand <- hand[[1]][[1]][[9]]
          hand_combination <- hand[[1]][[1]][[10]]
          move_made <- hand[[1]][[1]][[11]]
          
          if(bets_won_hand > 0){
            bets_won <- bets_won + 1
          } else if(bets_won_hand < 0){
            bets_lost <- bets_lost + 1
          }          
          
          money_won <- money_won + wager*bets_won_hand
          
          if(money_won < 0){
            money_won_class <- 'bank_roll_value_negative'
          } else if(money_won > 0){
            money_won_class <- 'bank_roll_value_positive'
          } else {
            money_won_class <- 'bank_roll_value_neutral'
          }             
          
          bank_roll <- bank_roll + wager*bets_won_hand
          bank_roll_history <- c(bank_roll_history, bank_roll)
          
          if(bank_roll < 0){
            bank_roll_class <- 'bank_roll_value_negative'
          } else if(bank_roll > 0){
            bank_roll_class <- 'bank_roll_value_positive'
          } else {
            bank_roll_class <- 'bank_roll_value_neutral'
          }
          
          hand_output <- paste('<tr><td>', hand_count, '</td><td class="', move_made, '">', move_made, '</td>', hand[[1]][[2]], '<td>', bets_won, '</td><td>', bets_lost, '</td><td class="', money_won_class, '">$', money_won, '</td><td  class="', bank_roll_class, '">$', bank_roll, '</td></tr>', sep='')
          output <- hand_output
          shoe <- hand[[1]][[1]][[7]]
          card_count <- hand[[1]][[1]][[8]]
          
          sim_hand_index <- update_hand_index(hand[[1]][[1]], sim_hand_index)
          
        }
        
        html <- c(html, paste(output, sep=''))
        
        
        if(stop_rule() == 'after'){
          ifelse(hand_count >= stop_hands(), continue <- FALSE, continue <- TRUE)
        } else if(stop_rule() == 'lost'){
          ifelse(money_won <= -stop_loss(), continue <- FALSE, continue <- TRUE)
        } else if (stop_rule() == 'bust'){
          ifelse(bank_roll <= 0, continue <- FALSE, continue <- TRUE)
        } else if (stop_rule() == 'win_loss'){
          ifelse(money_won <= -win_loss_lost() | money_won >= win_loss_won(), continue <- FALSE, continue <- TRUE)
        }
        
        hand_count <- hand_count + 1
        
      }
      
      sim_hand_index <- sim_hand_index[,-1]
      
      sim_html <- c(html, '</table>')
      return(list(sim_html, data.frame(sim_hand_index)))

    }
    
    simulation_results <- reactive({      
      
      simulation(decks())
    })
    
    ########################
    # OUTPUT
    ########################
    
    output$your_settings <- renderText({
      paste('<strong>Starting Bankroll</strong>: <span class="positive">$', starting_bankroll(), '</span>', sep='')
    })	    

    output$hand_report <- renderText({
      simulation_results()[[1]]
    })

    output$hand_index <- renderDataTable({
      simulation_results()[[2]]
    })

    output$download_hands <- downloadHandler(
      filename = function() { paste('hand_history', Sys.Date(), '.csv', sep='') },
      content = function(file) {
        write.csv(simulation_results()[[2]], file)
      }
    )
  }
)



