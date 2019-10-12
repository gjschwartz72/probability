# Note the edge cases (nPass < 1) or nPass > nCards is taken care of.
# Both cases return 0.

# Function returns the probability of dealy a black jack from the
# standard Eucher deck on the <nPass> deal.
# For example, 
#   P(nPass == 1) = 2 / 24 
#   P(nPass == 2) = (23 / 24) * (2 / 23) = 1/12
probX = function(nCards, nPass){
  if (nPass < 1) return(0)
  if (nPass == 1) {
    return(2 / nCards)
  }
  else if (nPass > 1){
    # Note: when nCards = 2, p == 0 which is expected as
    # this is the number of non-black jacks.
    # It is not possible to deal 24 cards
    p = (nCards - 2) / nCards * probX(nCards - 1, nPass - 1)
    
    if(is.na(p)) p = 0
    return(p)
  }
}

dist = sapply(1:24, FUN = function(n) {probX(24, n)})

player_probs = cbind(dist, 1:length(dist) %% 4)

player_total_probs = aggregate(player_probs[,1] ~ player_probs[,2], FUN = sum)
names(player_total_probs) = c("Player", "Total_Prob")
# In this Scenario, player 0 deals so is least likely to win deal.
#  Player 0 deals first to Player 1, then 2, then 3.  This follows
#  The order of total probability to win the deal.
