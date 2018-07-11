#-------------------#
# CATEGORISATION    #
#-------------------#

# Write function to calculate 
# stim x's probability of being categorized as belonging to A

CatA_vals = c(1,2,3,4,5)
CatB_vals = c(7,9,11,13,15)


categ = function(c,x){
  dists_to_A=abs(x-CatA_vals)
  dists_to_B=abs(x-CatB_vals)
  sims_to_A=exp(-c*dists_to_A)
  sims_to_B=exp(-c*dists_to_B)
  prob_A = sum(sims_to_A)/(sum(sims_to_A)+sum(sims_to_B))
  return(prob_A)
}

categ(0.5,2)

values = seq(1,30,.1)

prob_A = NULL 
c = 0.5
for (n in 1:length(values)){
  prob_A[n]=categ(c,values[n])
}

# Plot
plot(prob_A)
