cat("Welcome to blackjack.r\nRun the function blackjack() to begin play.\n")

blackjack <- function() {
    card_deck <- 1:52
    temp_card_sample <- sample_remove(card_deck, 2)
    user_cards <- temp_card_sample$val
    card_deck <- temp_card_sample$vector
    cat("Your cards are", prettify_vec(to_card(user_cards)), "\n")
    if(turn_decision()) {
        temp_card_sample <- sample_remove(card_deck)
        user_cards <- c(user_cards, temp_card_sample$val)
        card_deck <- temp_card_sample$vector
    }
    cat(prettify_vec(to_card(user_cards)), "\n")
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
    if (length(vec) <= 2) {
        return(paste(vec, collapse=" and "))
    }
    return(paste(paste(vec[1:length(vec) - 1], collapse=", "), ", and ", vec[length(vec)], sep=""))
}

to_card <- function(num) {
    suite_list <- c("Hearts", "Spades", "Clubs", "Diamonds")
    card_list <- c("Ace", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King")
    suite <- ceiling(num/13)
    card <- num%%13 + 1
    return(paste(card_list[card], "of", suite_list[suite]))
}
