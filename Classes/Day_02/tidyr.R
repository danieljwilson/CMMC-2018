library("tidyverse")

#### GATHER

# Make a fake data frame
widedf <- data.frame(drug_vis=rnorm(10)+1,
                 drug_aud=rnorm(10)+5,
                 plac_vis=rnorm(10)+2,
                 plac_aud=rnorm(10)+3)

longdf <- widedf %>% gather(cellcond, memscore, drug_vis:plac_aud)

#### SEPARATE
findf <- separate(longdf, cellcond, into=c("drug","modality"))

## Exercise: convert drug and modality to factors

## Exercise: calculate mean of memscore for each cell in the 2x2 design

#### UNITE AND SPREAD DO THE OPPOSITE OF SEPARATE AND GATHER, RESPECTIVELY