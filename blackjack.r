cat("Welcome to blackjack.r\nRun the function blackjack() to begin play.\n")

blackjack <- function() {
    #initial draw
    card_deck <- 1:52
    temp_card_sample <- sample_remove(card_deck, 2)
    user_cards <- temp_card_sample$val
    card_deck <- temp_card_sample$vector
    tell_cards(user_cards)

    temp_card_sample <- blackjack_turn(user_cards, card_deck)
    user_cards <- temp_card_sample$user_cards
    card_deck <- temp_card_sample$card_deck
}

blackjack_turn <- function(user_cards, card_deck) {
    #did we hit?
    if(turn_decision()) {
        temp_card_sample <- sample_remove(card_deck)
        user_cards <- c(user_cards, temp_card_sample$val)
        card_deck <- temp_card_sample$vector
    }
    tell_cards(user_cards)
    return(list("user_cards"=user_cards, "card_deck"=card_deck))
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

tell_cards <- function(user_cards) {
    cat("Your cards are", prettify_vec(to_card(user_cards)), "\n")
}

turn_decision <- function() {
    user_input <- readline(prompt="Do you want to hit or stand? [hit/stand]: ")
    if(user_input == "hit") {
        return(TRUE);
    } else if(user_input == "stand") {
        return(FALSE)
    } else {
        return(turn_decision())
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
