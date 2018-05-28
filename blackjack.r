cat("Welcome to blackjack.r\nRun the function blackjack() to begin play.\n")

blackjack <- function() {
    #initial draw
    #six deck
    card_deck <- rep(1:52, 6)
   
    repeat {
        #user cards
        temp_card_sample <- sample_remove(card_deck, 2)
        user_cards <- temp_card_sample$val
        card_deck <- temp_card_sample$vector
        #dealer cards
        temp_card_sample <- sample_remove(card_deck, 2)
        dealer_cards <- temp_card_sample$val
        card_deck <- temp_card_sample$vector

        cat("The dealers card up is the", to_card(dealer_cards[1]), "\n")
        card_deck <- blackjack_game(user_cards, dealer_cards, card_deck)

        if(!continue_game()) {
            break
        }
    }
}

continue_game <- function() {
    user_input <- readline(prompt="Do you want to continue the game? [y/n]: ")
    if (user_input == "y") {
        return(TRUE)
    } else if (user_input == "n") {
        return(FALSE) 
    } else {
        return(continue_game())
    }
}

blackjack_game <- function(user_cards, dealer_cards, card_deck) {
    user_turn <- do_turn(user_cards, card_deck, user_decision, "Your cards are")
    card_deck <- user_turn$card_deck
    if(!is_bust(user_turn$player_cards)) {
        dealer_turn <- do_turn(dealer_cards, card_deck, dealer_decision, "Dealer's cards are")
    }
    game_outcome(user_turn$player_cards, dealer_turn$player_cards)
    invisible(card_deck)
}

game_outcome <- function(user_cards, dealer_cards) {
    if(is_bust(user_cards)) {
        cat("You are bust. You lose.\n")
    } else if (is_bust(dealer_cards)) {
        cat("Dealer is bust. You win.\n")
    } else if (hand_value(user_cards) > hand_value(dealer_cards)) {
        cat("You win.\n")
    } else if (hand_value(user_cards) == hand_value(dealer_cards)) {
        cat("You draw.\n")
    } else {
        cat("You lose.\n")
    }
}

do_turn <- function(player_cards, card_deck, player_decision, greeting) {
    tell_cards(player_cards, greeting)
    while(!is_bust(player_cards) && player_decision(player_cards)) {
        temp_card_sample <- sample_remove(card_deck)
        player_cards <- c(player_cards, temp_card_sample$val)
        card_deck <- temp_card_sample$vector
        tell_cards(player_cards, greeting)
    }

    return(list("player_cards"=player_cards, "card_deck"=card_deck))
}

dealer_decision <- function(dealer_cards) {
    return(hand_value(dealer_cards) < 17)
}

card_value <- function(card_num) {
    val <- card_num%%13 + 1
    #card is a royal
    if (val > 10) {
        return(10)
    }
    #card is an ace
    else if (val == 1) {
        return(11)
    }
    #card is a number
    else {
        return(val)
    }
}

hand_value <- function(card_hand) {
    card_vals <- sapply(card_hand, card_value)
    #reduce any soft aces if we are bust
    while (11 %in% card_vals && sum(card_vals) > 21) {
        card_vals[match(11, card_vals)] <- 1
    }
    return(sum(card_vals))
}

is_bust <- function(card_hand) {
    return(hand_value(card_hand) > 21)
}

tell_cards <- function(user_cards, greeting="Your cards are") {
    cat(greeting, prettify_vec(to_card(user_cards)), "\n")
}

#we take user_cards as an argument to this function to allow generality of decision functions
#   even thought it is not used
user_decision <- function(user_cards = NULL) {
    user_input <- readline(prompt="Do you want to hit or stand? [hit/stand]: ")
    if(user_input == "hit" || user_input == "h") {
        return(TRUE);
    } else if(user_input == "stand" || user_input == "s") {
        return(FALSE)
    } else {
        return(user_decision())
    }
}

sample_remove <- function(vector, num=1) {
    x <- sample(vector, num)
    return(list("val"=x, "vector"=vector[! vector %in% x]))
}

prettify_vec <- function(vec) {
    #we dont want commas if we only have two values
    if (length(vec) <= 2) {
        return(paste(vec, collapse=" and "))
    }
    #first part of the string uses comma seperation, second part uses and
    return(paste(paste(vec[1:length(vec) - 1], collapse=", "), ", and ", vec[length(vec)], sep=""))
}

to_card <- function(num) {
    suite_list <- c("Hearts", "Spades", "Clubs", "Diamonds")
    card_list <- c("Ace", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King")
    suite <- ceiling(num/13)
    card <- num%%13 + 1
    return(paste(card_list[card], "of", suite_list[suite]))
}
