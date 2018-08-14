## Monte Carlo Simulations

B <- 100000
beads <- c("red", "red", "blue", "blue", "blue")
events <- replicate(B, sample(beads,1))

tab <- table(events)
tab

# Combinations and Permutations - Deck of cards
# Define suits, cards, values
suits <- c("Diamonds", "Clubs", "Hearts", "Spades")
cards <- c("Ace", "Deuce", "Three", "Four","Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King")
values <- c(0, 2:9, rep(10, 4))
totalNumOfDecks <- 1

# Build deck, replicated proper number of times
deck <- expand.grid(cards=cards, suits=suits)
#deck$value <- values
#deck <- deck[rep(seq(nrow(deck)), totalNumOfDecks),]
deck <- paste(deck$cards, deck$suits)
deck

kings <- expand.grid(cards = "King", suits=suits)
kings <- paste(kings$cards, kings$suits)
kings

mean(deck %in% kings)


# Permutations
library(gtools)
permutations(5,2)

# Example Phone numbers
all_phone_numbers <- permutations(10, 7, 0:9)
n <- nrow(all_phone_numbers)
index <- sample(n,5)
all_phone_numbers[index,]

# Permutations w deck
hands <- permutations(52, 2, deck)
hands

# Two kings
first_card <- hands[,1]
second_card <- hands[,2]

mean(first_card %in% kings & second_card %in% kings) / mean(first_card %in% kings)

# Combinations
aces <- paste("Ace", suits)

facecard <- c("King", "Queen", "Jack", "Ten")
facecard <- expand.grid(number = facecard, suit = suits)
facecard <- paste(facecard$number, facecard$suit)
facecard

hands <- combinations(52, 2, v = deck)
hands

mean(hands[,1] %in% aces & hands[,2] %in% facecard)

# Birthday Problem, sapply
n <- 50
bdays <- sample(1:365, n, replace = TRUE)
any(duplicated(bdays))

# Loops
n <- seq(1, 60)
compute_prob <- 
  function(n, B=10000){
    results <- replicate(B, {
      bdays <- sample(1:356, n, replace=TRUE)
      any(duplicated((bdays)))
    })
  mean(results)
  }

prob <- sapply(n, compute_prob)
plot(n,prob)

exact_prob <- function(n){
  prob_unique <- seq(365, 365-n+1)/365
  1 - prod(prob_unique)
}

e_prob <- sapply(n, exact_prob)
plot(n,prob)
lines(n, e_prob, col="red")
plot(n, e_prob - prob)
mean(e_prob - prob)

# Accuracy of Monte Carlo
B <- 10^seq(1, 5, len = 100)
compute_prob <- sapply(n=22, B, compute_prob)
plot(B, compute_prob, log = "x")
lines(B, compute_prob)

# Addition rule - Monte Hall
B <- 10000
stick <- replicate(B, {
  doors <- as.character(1:3)
  prize <- sample(c("car", "goat", "goat"))
  prize_door <- doors[prize == "car"]
  my_pick <- sample(doors, 1)
  show <- sample(doors[!doors %in% c(my_pick, prize_door)],1)
  stick <- my_pick
  switch <- doors[!doors%in%c(my_pick, show)]
  switch == prize_door
})

mean(stick)

