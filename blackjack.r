blackjack <- function() {
    card_deck <- 1:52
    user_cards <- sample(card_deck, 2)
    print(to_card(user_cards))
}

to_card <- function(num) {
    suite_list <- c("Hearts", "Spades", "Clubs", "Diamonds")
    card_list <- c("Ace", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King")
    suite <- ceiling(num/13)
    card <- num%%13 + 1
    return(paste(card_list[card], " of ", suite_list[suite]))
}
