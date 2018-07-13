library("cubature")

slotWrap <- function(x){
  slotObjective(x,
                changeK = changeK, changeN = changeN,
                sameK = sameK, sameN = sameN,
                setSize = setSize) *      # likelihood
                dunif(x[1],0,8) *         # prior on k
                dunif(x[2],0,1)           # prior on g
}
slot_margL <- adaptIntegrate(slotWrap,
                             c(0,0),
                             c(8,1))