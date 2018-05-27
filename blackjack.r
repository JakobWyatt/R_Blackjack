blackjack <- function() {
    card_deck <- 1:52
    temp_card_sample <- sample_remove(card_deck, 2)
    user_cards <- temp_card_sample$val
    card_deck <- temp_card_sample$vector
    print(to_card(user_cards))
}

sample_remove <- function(vector, num=1) {
    x <- sample(vector, num)
    return(list("val"=x, "vector"=vector[! vector %in% x]))
}

to_card <- function(num) {
    suite_list <- c("Hearts", "Spades", "Clubs", "Diamonds")
    card_list <- c("Ace", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King")
    suite <- ceiling(num/13)
    card <- num%%13 + 1
    return(paste(card_list[card], " of ", suite_list[suite]))
}
