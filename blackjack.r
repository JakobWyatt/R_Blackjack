blackjack <- function() {
    card_deck <- 1:52
    temp_card_sample <- sample_remove(card_deck)
    user_cards <- temp_card_sample[[1]]
    card_deck <- temp_card_sample[[2]]
    print(to_card(user_cards))
    print(to_card(card_deck))
}

sample_remove <- function(vector) {
    x <- sample(vector, 1)
    return(list(x, vector[! vector %in% x]))
}

to_card <- function(num) {
    suite_list <- c("Hearts", "Spades", "Clubs", "Diamonds")
    card_list <- c("Ace", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King")
    suite <- ceiling(num/13)
    card <- num%%13 + 1
    return(paste(card_list[card], " of ", suite_list[suite]))
}
