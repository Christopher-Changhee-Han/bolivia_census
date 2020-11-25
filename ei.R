# note: Elec_si and Elec_no is missing for 2001, disregard it

# Ecological inference package by Gary King
# install.packages("ei")


################################################################################
# Research Question: What caused the near 20% drop in the self-identification of indigenous population from 2001 to 2012 census in Bolivia?

# METHODOLOGY: (talk about assumptions)

# Why not just do a linear regression? it would explain correlation but not causation, and cannot estimate the indigenous population of a group with certain characteristic.

# we want to see if a change in a variable can explain the change in indigenous population

# Use ecological inference method by Gary King to estimate the proportion of indigenous population based on the variable for each of the 339 municipalities.

# Get the weight average as the estimate for the country of Bolivia (because the distribution tends to be bivariate/skewed/overall not very normal)

# Run it 100 times to simulate a density distribution

# Compare the distribution of different levels of the variable to see whether there is a sizable difference

# Then compare the 2001 distributions to 2012 distributions to see whether there is difference in difference (for example: change in indigenous population in urban area from 2001 to 2012 vs. change in indigenous population in rural area from 2001 to 2012) (there is a greater drop in indigenous population in urban areas vs rural, may be interesting to look at)

# Do this for all other variables of interest

library(ei)
load("2001.rdata")
load("2012.rdata")

df_2001$notind <- df_2001$total_pop_15ymas - df_2001$ind_pop
df_2012$notind <- df_2012$total_pop_15ymas - df_2012$ind_pop

############################## Urban/Rural ##################################################

result_urban = array(1:100)
result_rural = array(1:100)
formula= cbind(ind_pop, notind) ~ cbind(urban_15, rural_15)

for(i in 1:100){
  dbuf <- ei(formula = formula, data = as.data.frame(df_2001))
  out <- dbuf$draws$Beta[, seq(1, 1356, 4)]
  out2 <- dbuf$draws$Beta[, seq(2, 1356, 4)]
  result_urban[i] = sum(colMeans(out)*df_2001$total_pop/sum(df_2001$total_pop))
  result_rural[i] = sum(colMeans(out2)*df_2001$total_pop/sum(df_2001$total_pop))
}

df_2012$urban_15 <- round(df_2012$urban/df_2012$total_pop * df_2012$total_pop_15ymas)
df_2012$rural_15 <- round(df_2012$rural/df_2012$total_pop * df_2012$total_pop_15ymas)

result_urban2012 = array(1:100)
result_rural2012 = array(1:100)
formula= cbind(ind_pop, notind) ~ cbind(urban_15, rural_15)

for(i in 1:100){
  dbuf <- ei(formula = formula, data = as.data.frame(df_2012))
  out <- dbuf$draws$Beta[, seq(1, 1356, 4)]
  out2 <- dbuf$draws$Beta[, seq(2, 1356, 4)]
  result_urban2012[i] = sum(colMeans(out)*df_2012$total_pop/sum(df_2012$total_pop))
  result_rural2012[i] = sum(colMeans(out2)*df_2012$total_pop/sum(df_2012$total_pop))
}

plot(density(result_rural2012), xlim = c(0.1, 0.9),col = "blue")
lines(density(result_urban2012), col = "red")
lines(density(result_rural), col = "blue", lty = 2)
lines(density(result_urban), col = "red", lty = 2)
legend("topleft", legend = c("rural", "Urban"), lty = 1, col = c("blue", "red"))

plot(density(result_urban2012 - result_urban))
lines(density(result_rural2012 - result_rural))

t.test(result_urban2012- result_urban, result_rural2012 - result_rural, 
       alternative = "less")
# the decrease in urban indigenous population is significantly greater than the decrease in rural indigenous population.

# check if the proportion of urban population increased from 2001 to 2012
sum(df_2001$urban)/sum(df_2001$total_pop)
sum(df_2012$urban)/sum(df_2012$total_pop)

(mean(result_urban) * sum(df_2001$urban) + mean(result_rural) * sum(df_2001$rural)) / sum(df_2001$total_pop)
(mean(result_urban2012) * sum(df_2012$urban) + mean(result_rural2012) * sum(df_2012$rural)) / sum(df_2012$total_pop)


# about 5% increase from urban population from 2001 to 2012
# Given that people living in urban areas are much less likely to self-identify as indigenous, the migration into urban areas might have played a factor in the decrease in self-identification. However, both urban and rural population showed a decrease from 2001 to 2012, so there may be other factors at play.


############################## Literacy ##################################################
# check literacy as a variable

sum(df_2001$leer_1)/sum(df_2001$leer_1 + df_2001$leer_2 + df_2001$leer_3)
sum(df_2012$leer_1)/sum(df_2012$leer_1 + df_2012$leer_2 + df_2012$leer_3)

# about 7.4% increase in literacy

# check 2001
leer_2001_total_pop <- df_2001$leer_1 + df_2001$leer_2 + df_2001$leer_3
df_2001$leer_1_15 <- round(df_2001$leer_1/leer_2001_total_pop * df_2001$total_pop_15ymas)
df_2001$leer_2_15 <- round(df_2001$leer_2/leer_2001_total_pop * df_2001$total_pop_15ymas)
df_2001$leer_3_15 <- round(df_2001$leer_3/leer_2001_total_pop * df_2001$total_pop_15ymas)

(df_2001$leer_1_15 + df_2001$leer_2_15 + df_2001$leer_3_15) == df_2001$total_pop_15ymas
cbind(df_2001$leer_1_15 + df_2001$leer_2_15 + df_2001$leer_3_15, df_2001$total_pop_15ymas)[1,]
# some totals off by 1, fix this through a loop
for(i in 1:nrow(df_2001)){
  total_leer = df_2001[i, "leer_1_15"] + df_2001[i, "leer_2_15"] + df_2001[i, "leer_3_15"]
  diff_total = df_2001[i, "total_pop_15ymas"] - total_leer
  if(diff_total > 0){
    df_2001[i, "leer_1_15"] <- df_2001[i, "leer_1_15"] + abs(diff_total) # the difference is negligible
  }
  else if (diff_total < 0){
    df_2001[i, "leer_1_15"] <- df_2001[i, "leer_1_15"] - abs(diff_total) # the difference is negligible
  }
}

result_leer_1_2001 = array(1:100)
result_leer_2_2001 = array(1:100)
result_leer_3_2001 = array(1:100)
formula= cbind(ind_pop, notind) ~ cbind(leer_1_15, leer_2_15, leer_3_15)

for(i in 1:100){
  dbuf <- ei(formula = formula, data = as.data.frame(df_2001))
  out <- dbuf$draws$Beta[, seq(1, 2034, 6)]
  out2 <- dbuf$draws$Beta[, seq(2, 2034, 6)]
  out3 <- dbuf$draws$Beta[, seq(3, 2034, 6)]
  
  result_leer_1_2001[i] = sum(colMeans(out)*df_2001$total_pop/sum(df_2001$total_pop))
  result_leer_2_2001[i] = sum(colMeans(out2)*df_2001$total_pop/sum(df_2001$total_pop))
  result_leer_3_2001[i] = sum(colMeans(out3)*df_2001$total_pop/sum(df_2001$total_pop))
}

plot(density(result_leer_1_2001), xlim = c(0.1, 0.9),col = "blue")
lines(density(result_leer_2_2001), col = "red")
lines(density(result_leer_3_2001), col = "black")
legend("topleft", legend = c("Literate", "Illiterate", "No Answer"), lty = 1, col = c("blue", "red", "black"))


# check 2012
leer_2012_total_pop <- df_2012$leer_1 + df_2012$leer_2 + df_2012$leer_3
df_2012$leer_1_15 <- round(df_2012$leer_1/leer_2012_total_pop * df_2012$total_pop_15ymas)
df_2012$leer_2_15 <- round(df_2012$leer_2/leer_2012_total_pop * df_2012$total_pop_15ymas)
df_2012$leer_3_15 <- round(df_2012$leer_3/leer_2012_total_pop * df_2012$total_pop_15ymas)

(df_2012$leer_1_15 + df_2012$leer_2_15 + df_2012$leer_3_15) == df_2012$total_pop_15ymas
cbind(df_2012$leer_1_15 + df_2012$leer_2_15 + df_2012$leer_3_15, df_2012$total_pop_15ymas)[1,]
# some totals off by 1, fix this through a loop
for(i in 1:nrow(df_2012)){
  total_leer = df_2012[i, "leer_1_15"] + df_2012[i, "leer_2_15"] + df_2012[i, "leer_3_15"]
  diff_total = df_2012[i, "total_pop_15ymas"] - total_leer
  if(diff_total > 0){
    df_2012[i, "leer_1_15"] <- df_2012[i, "leer_1_15"] + abs(diff_total) # the difference is negligible
  }
  else if (diff_total < 0){
    df_2012[i, "leer_1_15"] <- df_2012[i, "leer_1_15"] - abs(diff_total) # the difference is negligible
  }
}



result_leer_1_2012 = array(1:100)
result_leer_2_2012 = array(1:100)
result_leer_3_2012 = array(1:100)
formula= cbind(ind_pop, notind) ~ cbind(leer_1_15, leer_2_15, leer_3_15)

for(i in 1:100){
  dbuf <- ei(formula = formula, data = as.data.frame(df_2012))
  out <- dbuf$draws$Beta[, seq(1, 2034, 6)]
  out2 <- dbuf$draws$Beta[, seq(2, 2034, 6)]
  out3 <- dbuf$draws$Beta[, seq(3, 2034, 6)]
  
  result_leer_1_2012[i] = sum(colMeans(out)*df_2012$total_pop/sum(df_2012$total_pop))
  result_leer_2_2012[i] = sum(colMeans(out2)*df_2012$total_pop/sum(df_2012$total_pop))
  result_leer_3_2012[i] = sum(colMeans(out3)*df_2012$total_pop/sum(df_2012$total_pop))
}

plot(density(result_leer_1_2012), xlim = c(0.1, 0.9),col = "blue")
lines(density(result_leer_2_2012), col = "red")
lines(density(result_leer_3_2012), col = "black")
lines(density(result_leer_1_2001), col = "blue", lty = 2)
lines(density(result_leer_2_2001), col = "red", lty = 2)
lines(density(result_leer_3_2001), col = "black", lty = 2)
legend("topleft", legend = c("Literate", "Illiterate", "No Answer"), lty = 1, col = c("blue", "red", "black"))

mean(result_leer_1_2001)
mean(result_leer_2_2001)
mean(result_leer_3_2001)

mean(result_leer_1_2012)
mean(result_leer_2_2012)
mean(result_leer_3_2012)


# literacy doesn't seem to have a significant effect. The means are very similar and the distributions overlap for both 2001 and 2012
############################## Education Level ##################################################
# look at education

sum(df_2001$edu_nivel_low)/sum(df_2001$edu_nivel_low + df_2001$edu_nivel_med + df_2001$edu_nivel_high)
sum(df_2001$edu_nivel_med)/sum(df_2001$edu_nivel_low + df_2001$edu_nivel_med + df_2001$edu_nivel_high)
sum(df_2001$edu_nivel_high)/sum(df_2001$edu_nivel_low + df_2001$edu_nivel_med + df_2001$edu_nivel_high)

sum(df_2012$edu_nivel_low)/sum(df_2012$edu_nivel_low + df_2012$edu_nivel_med + df_2012$edu_nivel_high)
sum(df_2012$edu_nivel_med)/sum(df_2012$edu_nivel_low + df_2012$edu_nivel_med + df_2012$edu_nivel_high)
sum(df_2012$edu_nivel_high)/sum(df_2012$edu_nivel_low + df_2012$edu_nivel_med + df_2012$edu_nivel_high)

edu_nivel_total_pop <- df_2001$edu_nivel_low + df_2001$edu_nivel_med + df_2001$edu_nivel_high
df_2001$edu_nivel_low_15 <- round(df_2001$edu_nivel_low/edu_nivel_total_pop * df_2001$total_pop_15ymas)
df_2001$edu_nivel_med_15 <- round(df_2001$edu_nivel_med/edu_nivel_total_pop * df_2001$total_pop_15ymas)
df_2001$edu_nivel_high_15 <- round(df_2001$edu_nivel_high/edu_nivel_total_pop * df_2001$total_pop_15ymas)

(df_2001$edu_nivel_low_15 + df_2001$edu_nivel_med_15 + df_2001$edu_nivel_high_15) == df_2001$total_pop_15ymas
cbind(df_2001$edu_nivel_low_15 + df_2001$edu_nivel_med_15 + df_2001$edu_nivel_high_15, df_2001$total_pop_15ymas)[1,]
# some totals off by 1, fix this through a loop
for(i in 1:nrow(df_2001)){
  total_leer = df_2001[i, "edu_nivel_low_15"] + df_2001[i, "edu_nivel_med_15"] + df_2001[i, "edu_nivel_high_15"]
  diff_total = df_2001[i, "total_pop_15ymas"] - total_leer
  if(diff_total > 0){
    df_2001[i, "edu_nivel_low_15"] <- df_2001[i, "edu_nivel_low_15"] + abs(diff_total) # the difference is negligible
  }
  else if (diff_total < 0){
    df_2001[i, "edu_nivel_low_15"] <- df_2001[i, "edu_nivel_low_15"] - abs(diff_total) # the difference is negligible
  }
}



result_edu_nivel_low_2001 = array(1:100)
result_edu_nivel_med_2001 = array(1:100)
result_edu_nivel_high_2001 = array(1:100)
formula= cbind(ind_pop, notind) ~ cbind(edu_nivel_low_15, edu_nivel_med_15, edu_nivel_high_15)

for(i in 1:100){
  dbuf <- ei(formula = formula, data = as.data.frame(df_2001))
  out <- dbuf$draws$Beta[, seq(1, 2034, 6)]
  out2 <- dbuf$draws$Beta[, seq(2, 2034, 6)]
  out3 <- dbuf$draws$Beta[, seq(3, 2034, 6)]
  
  result_edu_nivel_low_2001[i] = sum(colMeans(out)*df_2001$total_pop/sum(df_2001$total_pop))
  result_edu_nivel_med_2001[i] = sum(colMeans(out2)*df_2001$total_pop/sum(df_2001$total_pop))
  result_edu_nivel_high_2001[i] = sum(colMeans(out3)*df_2001$total_pop/sum(df_2001$total_pop))
}

plot(density(result_edu_nivel_low_2001), xlim = c(0.1, 0.9),col = "blue")
lines(density(result_edu_nivel_med_2001), col = "red")
lines(density(result_edu_nivel_high_2001), col = "black")
legend("topleft", legend = c("Low", "Medium", "High"), lty = 1, col = c("blue", "red", "black"))


# check 2012
edu_nivel_2012_total_pop <- df_2012$edu_nivel_low + df_2012$edu_nivel_med + df_2012$edu_nivel_high
df_2012$edu_nivel_low_15 <- round(df_2012$edu_nivel_low/edu_nivel_2012_total_pop * df_2012$total_pop_15ymas)
df_2012$edu_nivel_med_15 <- round(df_2012$edu_nivel_med/edu_nivel_2012_total_pop * df_2012$total_pop_15ymas)
df_2012$edu_nivel_high_15 <- round(df_2012$edu_nivel_high/edu_nivel_2012_total_pop * df_2012$total_pop_15ymas)

(df_2012$edu_nivel_low_15 + df_2012$edu_nivel_med_15 + df_2012$edu_nivel_high_15) == df_2012$total_pop_15ymas
cbind(df_2012$edu_nivel_low_15 + df_2012$edu_nivel_med_15 + df_2012$edu_nivel_high_15, df_2012$total_pop_15ymas)[1,]
# some totals off by 1, fix this through a loop
for(i in 1:nrow(df_2012)){
  total_leer = df_2012[i, "edu_nivel_low_15"] + df_2012[i, "edu_nivel_med_15"] + df_2012[i, "edu_nivel_high_15"]
  diff_total = df_2012[i, "total_pop_15ymas"] - total_leer
  if(diff_total > 0){
    df_2012[i, "edu_nivel_low_15"] <- df_2012[i, "edu_nivel_low_15"] + abs(diff_total) # the difference is negligible
  }
  else if (diff_total < 0){
    df_2012[i, "edu_nivel_low_15"] <- df_2012[i, "edu_nivel_low_15"] - abs(diff_total) # the difference is negligible
  }
}

result_edu_nivel_low_2012 = array(1:100)
result_edu_nivel_med_2012 = array(1:100)
result_edu_nivel_high_2012 = array(1:100)
formula= cbind(ind_pop, notind) ~ cbind(edu_nivel_low_15, edu_nivel_med_15, edu_nivel_high_15)

for(i in 1:100){
  dbuf <- ei(formula = formula, data = as.data.frame(df_2012))
  out <- dbuf$draws$Beta[, seq(1, 2034, 6)]
  out2 <- dbuf$draws$Beta[, seq(2, 2034, 6)]
  out3 <- dbuf$draws$Beta[, seq(3, 2034, 6)]
  
  result_edu_nivel_low_2012[i] = sum(colMeans(out)*df_2012$total_pop/sum(df_2012$total_pop))
  result_edu_nivel_med_2012[i] = sum(colMeans(out2)*df_2012$total_pop/sum(df_2012$total_pop))
  result_edu_nivel_high_2012[i] = sum(colMeans(out3)*df_2012$total_pop/sum(df_2012$total_pop))
}

plot(density(result_edu_nivel_low_2012), xlim = c(0.2, 1.0), ylim = c(0, 15),col = "blue")
lines(density(result_edu_nivel_med_2012), col = "red")
lines(density(result_edu_nivel_high_2012), col = "black")
lines(density(result_edu_nivel_low_2001), col = "blue", lty = 2)
lines(density(result_edu_nivel_med_2001), col = "red", lty = 2)
lines(density(result_edu_nivel_high_2001), col = "black", lty = 2)
legend("topleft", legend = c("Low", "Medium", "High"), lty = 1, col = c("blue", "red", "black"))

# As level of education increases, the self-identification of indigenous population decreases in both 2001 and 2012.

# there is about 5.5% increase in population that pursue higher education from 2001 to 2012, 6.9% decrease in low education, and a 1.3% increase in medium education. This may have affected the decrease in indigenous population. 
# However, it's worth noting that the self-identification in all three populations decreased.

############################## Gender ##################################################
# Check gender

result_hombre = array(1:100)
result_mujer = array(1:100)

df_2001$hombre_15 <- round(df_2001$hombre/df_2001$total_pop * df_2001$total_pop_15ymas)
df_2001$mujer_15 <- round(df_2001$mujer/df_2001$total_pop * df_2001$total_pop_15ymas)

formula= cbind(ind_pop, notind) ~ cbind(hombre_15, mujer_15)

for(i in 1:100){
  dbuf <- ei(formula = formula, data = as.data.frame(df_2001))
  out <- dbuf$draws$Beta[, seq(1, 1356, 4)]
  out2 <- dbuf$draws$Beta[, seq(2, 1356, 4)]
  result_hombre[i] = sum(colMeans(out)*df_2001$total_pop/sum(df_2001$total_pop))
  result_mujer[i] = sum(colMeans(out2)*df_2001$total_pop/sum(df_2001$total_pop))
}

df_2012$hombre_15 <- round(df_2012$hombre/df_2012$total_pop * df_2012$total_pop_15ymas)
df_2012$mujer_15 <- round(df_2012$mujer/df_2012$total_pop * df_2012$total_pop_15ymas)

result_hombre2012 = array(1:100)
result_mujer2012 = array(1:100)
formula= cbind(ind_pop, notind) ~ cbind(hombre_15, mujer_15)

for(i in 1:100){
  dbuf <- ei(formula = formula, data = as.data.frame(df_2012))
  out <- dbuf$draws$Beta[, seq(1, 1356, 4)]
  out2 <- dbuf$draws$Beta[, seq(2, 1356, 4)]
  result_hombre2012[i] = sum(colMeans(out)*df_2012$total_pop/sum(df_2012$total_pop))
  result_mujer2012[i] = sum(colMeans(out2)*df_2012$total_pop/sum(df_2012$total_pop))
}

plot(density(result_mujer2012),col = "blue")
lines(density(result_hombre2012), col = "red")
lines(density(result_mujer), col = "blue", lty = 2)
lines(density(result_hombre), col = "red", lty = 2)
legend("topleft", legend = c("mujer", "hombre"), lty = 1, col = c("blue", "red"))

# check if the proportion of hombre population increased from 2001 to 2012
sum(df_2001$hombre)/sum(df_2001$total_pop)
sum(df_2012$hombre)/sum(df_2012$total_pop)

# Gender doesn't seem to be a factor at all. very very even

############################## Age (Young vs. Old) ##################################################
# Check Age
result_edad0a60 = array(1:100)
result_edad60ymas = array(1:100)

df_2001$edad0a60_15 <- round(df_2001$edad0a60/df_2001$total_pop * df_2001$total_pop_15ymas)
df_2001$edad60ymas_15 <- round(df_2001$edad60ymas/df_2001$total_pop * df_2001$total_pop_15ymas)

formula= cbind(ind_pop, notind) ~ cbind(edad0a60_15, edad60ymas_15)

for(i in 1:100){
  dbuf <- ei(formula = formula, data = as.data.frame(df_2001))
  out <- dbuf$draws$Beta[, seq(1, 1356, 4)]
  out2 <- dbuf$draws$Beta[, seq(2, 1356, 4)]
  result_edad0a60[i] = sum(colMeans(out)*df_2001$total_pop/sum(df_2001$total_pop))
  result_edad60ymas[i] = sum(colMeans(out2)*df_2001$total_pop/sum(df_2001$total_pop))
}

df_2012$edad0a60_15 <- round(df_2012$edad0a60/df_2012$total_pop * df_2012$total_pop_15ymas)
df_2012$edad60ymas_15 <- round(df_2012$edad60ymas/df_2012$total_pop * df_2012$total_pop_15ymas)

result_edad0a602012 = array(1:100)
result_edad60ymas2012 = array(1:100)
formula= cbind(ind_pop, notind) ~ cbind(edad0a60_15, edad60ymas_15)

for(i in 1:100){
  dbuf <- ei(formula = formula, data = as.data.frame(df_2012))
  out <- dbuf$draws$Beta[, seq(1, 1356, 4)]
  out2 <- dbuf$draws$Beta[, seq(2, 1356, 4)]
  result_edad0a602012[i] = sum(colMeans(out)*df_2012$total_pop/sum(df_2012$total_pop))
  result_edad60ymas2012[i] = sum(colMeans(out2)*df_2012$total_pop/sum(df_2012$total_pop))
}

plot(density(result_edad60ymas2012), xlim = c(0.1,0.9),col = "blue")
lines(density(result_edad0a602012), col = "red")
lines(density(result_edad60ymas), col = "blue", lty = 2)
lines(density(result_edad0a60), col = "red", lty = 2)
legend("topleft", legend = c("edad60ymas", "edad0a60"), lty = 1, col = c("blue", "red"))

mean(result_edad0a60)
mean(result_edad60ymas)

mean(result_edad0a602012)
mean(result_edad60ymas2012)

# check if the proportion of edad0a60 population increased from 2001 to 2012
sum(df_2001$edad0a60)/sum(df_2001$total_pop)
sum(df_2012$edad0a60)/sum(df_2012$total_pop)

# age is also fairly irrelevant (take it with a grain of salt, the age range here is really broad, and the sample size is highly unequal)

############################## Migration (Born) ##################################################
# Check Migration (nacio_1,2,3) 
result_nacio1 = array(1:100)
result_nacio2 = array(1:100)
result_nacio3 = array(1:100)


df_2001$nacio1_15 <- round(df_2001$nacio_1/df_2001$total_pop * df_2001$total_pop_15ymas)
df_2001$nacio2_15 <- round(df_2001$nacio_2/df_2001$total_pop * df_2001$total_pop_15ymas)
df_2001$nacio3_15 <- round(df_2001$nacio_3/df_2001$total_pop * df_2001$total_pop_15ymas)

formula= cbind(ind_pop, notind) ~ cbind(nacio1_15, nacio2_15, nacio3_15)

# Fix the minor differences in n
for(i in 1:nrow(df_2001)){
  total_leer = df_2001[i, "nacio1_15"] + df_2001[i, "nacio2_15"] + df_2001[i, "nacio3_15"]
  diff_total = df_2001[i, "total_pop_15ymas"] - total_leer
  if(diff_total > 0){
    df_2001[i, "nacio1_15"] <- df_2001[i, "nacio1_15"] + abs(diff_total) # the difference is negligible
  }
  else if (diff_total < 0){
    df_2001[i, "nacio1_15"] <- df_2001[i, "nacio1_15"] - abs(diff_total) # the difference is negligible
  }
}

for(i in 1:100){
  dbuf <- ei(formula = formula, data = as.data.frame(df_2001))
  out <- dbuf$draws$Beta[, seq(1, 2034, 6)]
  out2 <- dbuf$draws$Beta[, seq(2, 2034, 6)]
  out3 <- dbuf$draws$Beta[, seq(3, 2034, 6)]
  result_nacio1[i] = sum(colMeans(out)*df_2001$total_pop/sum(df_2001$total_pop))
  result_nacio2[i] = sum(colMeans(out2)*df_2001$total_pop/sum(df_2001$total_pop))
  result_nacio3[i] = sum(colMeans(out3)*df_2001$total_pop/sum(df_2001$total_pop))
}

df_2012$nacio1_15 <- round(df_2012$nacio_1/df_2012$total_pop * df_2012$total_pop_15ymas)
df_2012$nacio2_15 <- round(df_2012$nacio_2/df_2012$total_pop * df_2012$total_pop_15ymas)
df_2012$nacio3_15 <- round(df_2012$nacio_3/df_2012$total_pop * df_2012$total_pop_15ymas)

result_nacio1_2012 = array(1:100)
result_nacio2_2012 = array(1:100)
result_nacio3_2012 = array(1:100)

formula= cbind(ind_pop, notind) ~ cbind(nacio1_15, nacio2_15, nacio3_15)

# Fix the minor differences in n
for(i in 1:nrow(df_2012)){
  total_leer = df_2012[i, "nacio1_15"] + df_2012[i, "nacio2_15"] + df_2012[i, "nacio3_15"]
  diff_total = df_2012[i, "total_pop_15ymas"] - total_leer
  if(diff_total > 0){
    df_2012[i, "nacio1_15"] <- df_2012[i, "nacio1_15"] + abs(diff_total) # the difference is negligible
  }
  else if (diff_total < 0){
    df_2012[i, "nacio1_15"] <- df_2012[i, "nacio1_15"] - abs(diff_total) # the difference is negligible
  }
}

for(i in 1:100){
  dbuf <- ei(formula = formula, data = as.data.frame(df_2012))
  out <- dbuf$draws$Beta[, seq(1, 2034, 6)]
  out2 <- dbuf$draws$Beta[, seq(2, 2034, 6)]
  out3 <- dbuf$draws$Beta[, seq(3, 2034, 6)]
  result_nacio1_2012[i] = sum(colMeans(out)*df_2012$total_pop/sum(df_2012$total_pop))
  result_nacio2_2012[i] = sum(colMeans(out2)*df_2012$total_pop/sum(df_2012$total_pop))
  result_nacio3_2012[i] = sum(colMeans(out3)*df_2012$total_pop/sum(df_2012$total_pop))
}

plot(density(result_nacio1), xlim = c(0.1,0.9),col = "blue")
lines(density(result_nacio2), col = "red")
lines(density(result_nacio3), col = "black")
lines(density(result_nacio1_2012), col = "blue", lty = 2)
lines(density(result_nacio2_2012), col = "red", lty = 2)
lines(density(result_nacio3_2012), col = "black", lty = 2)


sum(df_2001$nacio_1)/sum(df_2001$total_pop)
sum(df_2001$nacio_2)/sum(df_2001$total_pop)
sum(df_2001$nacio_3)/sum(df_2001$total_pop)

sum(df_2012$nacio_1)/sum(df_2012$total_pop)
sum(df_2012$nacio_2)/sum(df_2012$total_pop)
sum(df_2012$nacio_3)/sum(df_2012$total_pop)

# Population that was born in the same municipality that they live in increased in 2012 (0.73 vs 0.67) 
# Population that was born in a different part of Bolivia decreased in 2012 (0.32 vs 0.25)

mean(result_nacio1)
mean(result_nacio2)
mean(result_nacio3)

mean(result_nacio1_2012)
mean(result_nacio2_2012)
mean(result_nacio3_2012)

# Population that are born in the same municipality identify themselves as indigenous a bit more, although the gap is closed in 2012. This is contradictory to our hypothesis.

############################## Migration (Current Living) ##################################################
# Check Migration (vive_aqui, vive_otro, vive_exterior)
result_vive_aqui = array(1:100)
result_vive_otro = array(1:100)
result_vive_exterior = array(1:100)


df_2001$vive_aqui_15 <- round(df_2001$vive_aqui/df_2001$total_pop * df_2001$total_pop_15ymas)
df_2001$vive_otro_15 <- round(df_2001$vive_otro/df_2001$total_pop * df_2001$total_pop_15ymas)
df_2001$vive_exterior_15 <- round(df_2001$vive_exterior/df_2001$total_pop * df_2001$total_pop_15ymas)

formula= cbind(ind_pop, notind) ~ cbind(vive_aqui_15, vive_otro_15, vive_exterior_15)

# Fix the minor differences in n
for(i in 1:nrow(df_2001)){
  total_leer = df_2001[i, "vive_aqui_15"] + df_2001[i, "vive_otro_15"] + df_2001[i, "vive_exterior_15"]
  diff_total = df_2001[i, "total_pop_15ymas"] - total_leer
  if(diff_total > 0){
    df_2001[i, "vive_aqui_15"] <- df_2001[i, "vive_aqui_15"] + abs(diff_total) # the difference is negligible
  }
  else if (diff_total < 0){
    df_2001[i, "vive_aqui_15"] <- df_2001[i, "vive_aqui_15"] - abs(diff_total) # the difference is negligible
  }
}

for(i in 1:100){
  dbuf <- ei(formula = formula, data = as.data.frame(df_2001))
  out <- dbuf$draws$Beta[, seq(1, 2034, 6)]
  out2 <- dbuf$draws$Beta[, seq(2, 2034, 6)]
  out3 <- dbuf$draws$Beta[, seq(3, 2034, 6)]
  result_vive_aqui[i] = sum(colMeans(out)*df_2001$total_pop/sum(df_2001$total_pop))
  result_vive_otro[i] = sum(colMeans(out2)*df_2001$total_pop/sum(df_2001$total_pop))
  result_vive_exterior[i] = sum(colMeans(out3)*df_2001$total_pop/sum(df_2001$total_pop))
}

df_2012$vive_aqui_15 <- round(df_2012$vive_aqui/df_2012$total_pop * df_2012$total_pop_15ymas)
df_2012$vive_otro_15 <- round(df_2012$vive_otro/df_2012$total_pop * df_2012$total_pop_15ymas)
df_2012$vive_exterior_15 <- round(df_2012$vive_exterior/df_2012$total_pop * df_2012$total_pop_15ymas)

result_vive_aqui_2012 = array(1:100)
result_vive_otro_2012 = array(1:100)
result_vive_exterior_2012 = array(1:100)

formula= cbind(ind_pop, notind) ~ cbind(vive_aqui_15, vive_otro_15, vive_exterior_15)

# Fix the minor differences in n
for(i in 1:nrow(df_2012)){
  total_leer = df_2012[i, "vive_aqui_15"] + df_2012[i, "vive_otro_15"] + df_2012[i, "vive_exterior_15"]
  diff_total = df_2012[i, "total_pop_15ymas"] - total_leer
  if(diff_total > 0){
    df_2012[i, "vive_aqui_15"] <- df_2012[i, "vive_aqui_15"] + abs(diff_total) # the difference is negligible
  }
  else if (diff_total < 0){
    df_2012[i, "vive_aqui_15"] <- df_2012[i, "vive_aqui_15"] - abs(diff_total) # the difference is negligible
  }
}

for(i in 1:100){
  dbuf <- ei(formula = formula, data = as.data.frame(df_2012))
  out <- dbuf$draws$Beta[, seq(1, 2034, 6)]
  out2 <- dbuf$draws$Beta[, seq(2, 2034, 6)]
  out3 <- dbuf$draws$Beta[, seq(3, 2034, 6)]
  result_vive_aqui_2012[i] = sum(colMeans(out)*df_2012$total_pop/sum(df_2012$total_pop))
  result_vive_otro_2012[i] = sum(colMeans(out2)*df_2012$total_pop/sum(df_2012$total_pop))
  result_vive_exterior_2012[i] = sum(colMeans(out3)*df_2012$total_pop/sum(df_2012$total_pop))
}

plot(density(result_vive_aqui), xlim = c(0.1,0.9),col = "blue")
lines(density(result_vive_otro), col = "red")
lines(density(result_vive_exterior), col = "black")
lines(density(result_vive_aqui_2012), col = "blue", lty = 2)
lines(density(result_vive_otro_2012), col = "red", lty = 2)
lines(density(result_vive_exterior_2012), col = "black", lty = 2)


sum(df_2001$vive_aqui)/sum(df_2001$total_pop)
sum(df_2001$vive_otro)/sum(df_2001$total_pop)
sum(df_2001$vive_exterior)/sum(df_2001$total_pop)

sum(df_2012$vive_aqui)/sum(df_2012$total_pop)
sum(df_2012$vive_otro)/sum(df_2012$total_pop)
sum(df_2012$vive_exterior)/sum(df_2012$total_pop)

# Population that currently live here stayed the same

mean(result_vive_aqui)
mean(result_vive_otro)
mean(result_vive_exterior)

mean(result_vive_aqui_2012)
mean(result_vive_otro_2012)
mean(result_vive_exterior_2012)

# The trend of proportion of indigenous population in 2001 reversed for 2012. 
# Take it with grain of salt again because of the gross imbalance of sample size in each group

############################## Employment Status ##################################################
# Check Employment Status
result_obrero = array(1:100)
result_cuenta_propia = array(1:100)
result_empleador = array(1:100)
result_sin_renum = array(1:100)
result_coop = array(1:100)

total_employment = df_2001$ocu_obrero + df_2001$ocu_cuenta_propia + df_2001$ocu_empleador + df_2001$ocu_sin_renum + df_2001$ocu_coop

df_2001$ocu_obrero_15 <- round(df_2001$ocu_obrero/total_employment * df_2001$total_pop_15ymas)
df_2001$ocu_cuenta_propia_15 <- round(df_2001$ocu_cuenta_propia/total_employment * df_2001$total_pop_15ymas)
df_2001$ocu_empleador_15 <- round(df_2001$ocu_empleador/total_employment * df_2001$total_pop_15ymas)
df_2001$ocu_sin_renum_15 <- round(df_2001$ocu_sin_renum/total_employment * df_2001$total_pop_15ymas)
df_2001$ocu_coop_15 <- round(df_2001$ocu_coop/total_employment * df_2001$total_pop_15ymas)

formula= cbind(ind_pop, notind) ~ cbind(ocu_obrero_15, ocu_cuenta_propia_15, ocu_empleador_15, ocu_sin_renum_15, ocu_coop_15)

(df_2001$ocu_obrero_15 + df_2001$ocu_cuenta_propia_15 + df_2001$ocu_empleador_15 + df_2001$ocu_sin_renum_15 + df_2001$ocu_coop_15) == df_2001$total_pop_15ymas
cbind(df_2001$ocu_obrero_15 + df_2001$ocu_cuenta_propia_15 + df_2001$ocu_empleador_15 + df_2001$ocu_sin_renum_15 + df_2001$ocu_coop_15, df_2001$total_pop_15ymas)[1,]

# Fix the minor differences in n
for(i in 1:nrow(df_2001)){
  total_emp = df_2001[i,"ocu_obrero_15"] + df_2001[i, "ocu_cuenta_propia_15"] + df_2001[i,"ocu_empleador_15"] + df_2001[i,"ocu_sin_renum_15"] + df_2001[i, "ocu_coop_15"]
  diff_total = df_2001[i, "total_pop_15ymas"] - total_emp
  if(diff_total > 0){
    df_2001[i, "ocu_obrero_15"] <- df_2001[i, "ocu_obrero_15"] + abs(diff_total) # the difference is negligible
  }
  else if (diff_total < 0){
    df_2001[i, "ocu_obrero_15"] <- df_2001[i, "ocu_obrero_15"] - abs(diff_total) # the difference is negligible
  }
}

for(i in 1:100){
  dbuf <- ei(formula = formula, data = as.data.frame(df_2001))
  out <- dbuf$draws$Beta[, seq(1, 3390, 10)]
  out2 <- dbuf$draws$Beta[, seq(2, 3390, 10)]
  out3 <- dbuf$draws$Beta[, seq(3, 3390, 10)]
  out4 <- dbuf$draws$Beta[, seq(4, 3390, 10)]
  out5 <- dbuf$draws$Beta[, seq(5, 3390, 10)]
  
  result_obrero[i] = sum(colMeans(out)*df_2001$total_pop/sum(df_2001$total_pop))
  result_cuenta_propia[i] = sum(colMeans(out2)*df_2001$total_pop/sum(df_2001$total_pop))
  result_empleador[i] = sum(colMeans(out3)*df_2001$total_pop/sum(df_2001$total_pop))
  result_sin_renum[i] = sum(colMeans(out4)*df_2001$total_pop/sum(df_2001$total_pop))
  result_coop[i] = sum(colMeans(out5)*df_2001$total_pop/sum(df_2001$total_pop))
}

# 2012
result_obrero_2012 = array(1:100)
result_cuenta_propia_2012 = array(1:100)
result_empleador_2012 = array(1:100)
result_sin_renum_2012 = array(1:100)
result_coop_2012 = array(1:100)

total_employment = df_2012$ocu_obrero + df_2012$ocu_cuenta_propia + df_2012$ocu_empleador + df_2012$ocu_sin_renum + df_2012$ocu_coop

df_2012$ocu_obrero_15 <- round(df_2012$ocu_obrero/total_employment * df_2012$total_pop_15ymas)
df_2012$ocu_cuenta_propia_15 <- round(df_2012$ocu_cuenta_propia/total_employment * df_2012$total_pop_15ymas)
df_2012$ocu_empleador_15 <- round(df_2012$ocu_empleador/total_employment * df_2012$total_pop_15ymas)
df_2012$ocu_sin_renum_15 <- round(df_2012$ocu_sin_renum/total_employment * df_2012$total_pop_15ymas)
df_2012$ocu_coop_15 <- round(df_2012$ocu_coop/total_employment * df_2012$total_pop_15ymas)

formula= cbind(ind_pop, notind) ~ cbind(ocu_obrero_15, ocu_cuenta_propia_15, ocu_empleador_15, ocu_sin_renum_15, ocu_coop_15)

(df_2012$ocu_obrero_15 + df_2012$ocu_cuenta_propia_15 + df_2012$ocu_empleador_15 + df_2012$ocu_sin_renum_15 + df_2012$ocu_coop_15) == df_2012$total_pop_15ymas
cbind(df_2012$ocu_obrero_15 + df_2012$ocu_cuenta_propia_15 + df_2012$ocu_empleador_15 + df_2012$ocu_sin_renum_15 + df_2012$ocu_coop_15, df_2012$total_pop_15ymas)[1,]

# Fix the minor differences in n
for(i in 1:nrow(df_2012)){
  total_emp = df_2012[i,"ocu_obrero_15"] + df_2012[i, "ocu_cuenta_propia_15"] + df_2012[i,"ocu_empleador_15"] + df_2012[i,"ocu_sin_renum_15"] + df_2012[i, "ocu_coop_15"]
  diff_total = df_2012[i, "total_pop_15ymas"] - total_emp
  if(diff_total > 0){
    df_2012[i, "ocu_obrero_15"] <- df_2012[i, "ocu_obrero_15"] + abs(diff_total) # the difference is negligible
  }
  else if (diff_total < 0){
    df_2012[i, "ocu_obrero_15"] <- df_2012[i, "ocu_obrero_15"] - abs(diff_total) # the difference is negligible
  }
}

for(i in 1:100){
  dbuf <- ei(formula = formula, data = as.data.frame(df_2012))
  out <- dbuf$draws$Beta[, seq(1, 3390, 10)]
  out2 <- dbuf$draws$Beta[, seq(2, 3390, 10)]
  out3 <- dbuf$draws$Beta[, seq(3, 3390, 10)]
  out4 <- dbuf$draws$Beta[, seq(4, 3390, 10)]
  out5 <- dbuf$draws$Beta[, seq(5, 3390, 10)]
  
  result_obrero_2012[i] = sum(colMeans(out)*df_2012$total_pop/sum(df_2012$total_pop))
  result_cuenta_propia_2012[i] = sum(colMeans(out2)*df_2012$total_pop/sum(df_2012$total_pop))
  result_empleador_2012[i] = sum(colMeans(out3)*df_2012$total_pop/sum(df_2012$total_pop))
  result_sin_renum_2012[i] = sum(colMeans(out4)*df_2012$total_pop/sum(df_2012$total_pop))
  result_coop_2012[i] = sum(colMeans(out5)*df_2012$total_pop/sum(df_2012$total_pop))
}

par(mfrow= c(1,2))
plot(density(result_obrero_2012), xlim = c(0.1,0.9),col = 1)
lines(density(result_cuenta_propia_2012), col = 2)
lines(density(result_empleador_2012), col = 3)
lines(density(result_sin_renum_2012), col = 4)
lines(density(result_coop_2012), col = 5)

plot(density(result_obrero), xlim = c(0.1,0.9),col = 1)
lines(density(result_cuenta_propia), col = 2)
lines(density(result_empleador), col = 3)
lines(density(result_sin_renum), col = 4)
lines(density(result_coop), col = 5)

par(mfrow= c(1,1))

total_emp = df_2001$ocu_obrero + df_2001$ocu_cuenta_propia + df_2001$ocu_empleador + df_2001$ocu_sin_renum + df_2001$ocu_coop 
sum(df_2001$ocu_obrero)/sum(total_emp)
sum(df_2001$ocu_cuenta_propia)/sum(total_emp)
sum(df_2001$ocu_empleador)/sum(total_emp)
sum(df_2001$ocu_sin_renum)/sum(total_emp)
sum(df_2001$ocu_coop)/sum(total_emp)

total_emp_2012 = df_2012$ocu_obrero + df_2012$ocu_cuenta_propia + df_2012$ocu_empleador + df_2012$ocu_sin_renum + df_2012$ocu_coop
sum(df_2012$ocu_obrero)/sum(total_emp_2012)
sum(df_2012$ocu_cuenta_propia)/sum(total_emp_2012)
sum(df_2012$ocu_empleador)/sum(total_emp_2012)
sum(df_2012$ocu_sin_renum)/sum(total_emp_2012)
sum(df_2012$ocu_coop)/sum(total_emp_2012)

# Population that currently live here stayed the same

mean(result_obrero)
mean(result_cuenta_propia)
mean(result_empleador)
mean(result_sin_renum)
mean(result_coop)

mean(result_obrero_2012)
mean(result_cuenta_propia_2012)
mean(result_empleador_2012)
mean(result_sin_renum_2012)
mean(result_coop_2012)

# Doesn't seem to be differences among employment types, plus there wasn't a difference from 2001 to 2012 in the demographics of employment types


############################## Industry ##################################################
# Check Industry

result_trab_1 = array(1:100)
result_trab_2 = array(1:100)
result_trab_3 = array(1:100)
result_trab_4 = array(1:100)
result_trab_5 = array(1:100)
result_trab_6 = array(1:100)
result_trab_7 = array(1:100)
result_trab_8 = array(1:100)
result_trab_9 = array(1:100)
result_trab_10 = array(1:100)
result_trab_11 = array(1:100)
result_trab_12 = array(1:100)
result_trab_13 = array(1:100)
result_trab_14 = array(1:100)

total_trab = df_2001$trab_1 + df_2001$trab_2 + df_2001$trab_3 + df_2001$trab_4 + df_2001$trab_5 + df_2001$trab_6 + df_2001$trab_7 + df_2001$trab_8 + df_2001$trab_9 + df_2001$trab_10 + df_2001$trab_11 + df_2001$trab_12 + df_2001$trab_13 + df_2001$trab_14

df_2001$trab_1_15 <- round(df_2001$trab_1/total_trab * df_2001$total_pop_15ymas)
df_2001$trab_2_15 <- round(df_2001$trab_2/total_trab * df_2001$total_pop_15ymas)
df_2001$trab_3_15 <- round(df_2001$trab_3/total_trab * df_2001$total_pop_15ymas)
df_2001$trab_4_15 <- round(df_2001$trab_4/total_trab * df_2001$total_pop_15ymas)
df_2001$trab_5_15 <- round(df_2001$trab_5/total_trab * df_2001$total_pop_15ymas)
df_2001$trab_6_15 <- round(df_2001$trab_6/total_trab * df_2001$total_pop_15ymas)
df_2001$trab_7_15 <- round(df_2001$trab_7/total_trab * df_2001$total_pop_15ymas)
df_2001$trab_8_15 <- round(df_2001$trab_8/total_trab * df_2001$total_pop_15ymas)
df_2001$trab_9_15 <- round(df_2001$trab_9/total_trab * df_2001$total_pop_15ymas)
df_2001$trab_10_15 <- round(df_2001$trab_10/total_trab * df_2001$total_pop_15ymas)
df_2001$trab_11_15 <- round(df_2001$trab_11/total_trab * df_2001$total_pop_15ymas)
df_2001$trab_12_15 <- round(df_2001$trab_12/total_trab * df_2001$total_pop_15ymas)
df_2001$trab_13_15 <- round(df_2001$trab_13/total_trab * df_2001$total_pop_15ymas)
df_2001$trab_14_15 <- round(df_2001$trab_14/total_trab * df_2001$total_pop_15ymas)

formula= cbind(ind_pop, notind) ~ cbind(trab_1_15, trab_2_15, trab_3_15, trab_4_15, trab_5_15,
                                        trab_6_15, trab_7_15, trab_8_15, trab_9_15, trab_10_15,
                                        trab_11_15, trab_12_15, trab_13_15, trab_14_15
                                        )

(df_2001$trab_1_15 + df_2001$trab_2_15 + df_2001$trab_3_15 + df_2001$trab_4_15 + df_2001$trab_5_15 + df_2001$trab_6_15 + df_2001$trab_7_15 + df_2001$trab_8_15 + df_2001$trab_9_15 + df_2001$trab_10_15 + df_2001$trab_11_15 + df_2001$trab_12_15 + df_2001$trab_13_15 + df_2001$trab_14_15) == df_2001$total_pop_15ymas
cbind(df_2001$ocu_obrero_15 + df_2001$ocu_cuenta_propia_15 + df_2001$ocu_empleador_15 + df_2001$ocu_sin_renum_15 + df_2001$ocu_coop_15, df_2001$total_pop_15ymas)[1,]

# Fix the minor differences in n
for(i in 1:nrow(df_2001)){
  total_tr = df_2001[i,"trab_1_15"] + df_2001[i, "trab_2_15"] + df_2001[i,"trab_3_15"] + df_2001[i,"trab_4_15"] + df_2001[i, "trab_5_15"] + df_2001[i,"trab_6_15"] + df_2001[i, "trab_7_15"] + df_2001[i,"trab_8_15"] + df_2001[i,"trab_9_15"] + df_2001[i, "trab_10_15"] + df_2001[i,"trab_11_15"] + df_2001[i, "trab_12_15"] + df_2001[i,"trab_13_15"] + df_2001[i,"trab_14_15"]
  diff_total = df_2001[i, "total_pop_15ymas"] - total_tr
  if(diff_total > 0){
    df_2001[i, "trab_4_15"] <- df_2001[i, "trab_4_15"] + abs(diff_total) # the difference is negligible
  }
  else if (diff_total < 0){
    df_2001[i, "trab_4_15"] <- df_2001[i, "trab_4_15"] - abs(diff_total) # the difference is negligible
  }
}

for(i in 1:100){
  dbuf <- ei(formula = formula, data = as.data.frame(df_2001))
  out <- dbuf$draws$Beta[, seq(1, 9492, 28)]
  out2 <- dbuf$draws$Beta[, seq(2, 9492, 28)]
  out3 <- dbuf$draws$Beta[, seq(3, 9492, 28)]
  out4 <- dbuf$draws$Beta[, seq(4, 9492, 28)]
  out5 <- dbuf$draws$Beta[, seq(5, 9492, 28)]
  out6 <- dbuf$draws$Beta[, seq(6, 9492, 28)]
  out7 <- dbuf$draws$Beta[, seq(7, 9492, 28)]
  out8 <- dbuf$draws$Beta[, seq(8, 9492, 28)]
  out9 <- dbuf$draws$Beta[, seq(9, 9492, 28)]
  out10 <- dbuf$draws$Beta[, seq(10, 9492, 28)]
  out11 <- dbuf$draws$Beta[, seq(11, 9492, 28)]
  out12 <- dbuf$draws$Beta[, seq(12, 9492, 28)]
  out13 <- dbuf$draws$Beta[, seq(13, 9492, 28)]
  out14 <- dbuf$draws$Beta[, seq(14, 9492, 28)]
  
  result_trab_1[i] = sum(colMeans(out)*df_2001$total_pop/sum(df_2001$total_pop))
  result_trab_2[i] = sum(colMeans(out2)*df_2001$total_pop/sum(df_2001$total_pop))
  result_trab_3[i] = sum(colMeans(out3)*df_2001$total_pop/sum(df_2001$total_pop))
  result_trab_4[i] = sum(colMeans(out4)*df_2001$total_pop/sum(df_2001$total_pop))
  result_trab_5[i] = sum(colMeans(out5)*df_2001$total_pop/sum(df_2001$total_pop))
  result_trab_6[i] = sum(colMeans(out6)*df_2001$total_pop/sum(df_2001$total_pop))
  result_trab_7[i] = sum(colMeans(out7)*df_2001$total_pop/sum(df_2001$total_pop))
  result_trab_8[i] = sum(colMeans(out8)*df_2001$total_pop/sum(df_2001$total_pop))
  result_trab_9[i] = sum(colMeans(out9)*df_2001$total_pop/sum(df_2001$total_pop))
  result_trab_10[i] = sum(colMeans(out10)*df_2001$total_pop/sum(df_2001$total_pop))
  result_trab_11[i] = sum(colMeans(out11)*df_2001$total_pop/sum(df_2001$total_pop))
  result_trab_12[i] = sum(colMeans(out12)*df_2001$total_pop/sum(df_2001$total_pop))
  result_trab_13[i] = sum(colMeans(out13)*df_2001$total_pop/sum(df_2001$total_pop))
  result_trab_14[i] = sum(colMeans(out14)*df_2001$total_pop/sum(df_2001$total_pop))
}

#2012

result_trab_1_2012 = array(1:100)
result_trab_2_2012 = array(1:100)
result_trab_3_2012 = array(1:100)
result_trab_4_2012 = array(1:100)
result_trab_5_2012 = array(1:100)
result_trab_6_2012 = array(1:100)
result_trab_7_2012 = array(1:100)
result_trab_8_2012 = array(1:100)
result_trab_9_2012 = array(1:100)
result_trab_10_2012 = array(1:100)
result_trab_11_2012 = array(1:100)
result_trab_12_2012 = array(1:100)
result_trab_13_2012 = array(1:100)
result_trab_14_2012 = array(1:100)

total_trab = df_2012$trab_1 + df_2012$trab_2 + df_2012$trab_3 + df_2012$trab_4 + df_2012$trab_5 + df_2012$trab_6 + df_2012$trab_7 + df_2012$trab_8 + df_2012$trab_9 + df_2012$trab_10 + df_2012$trab_11 + df_2012$trab_12 + df_2012$trab_13 + df_2012$trab_14

df_2012$trab_1_15 <- round(df_2012$trab_1/total_trab * df_2012$total_pop_15ymas)
df_2012$trab_2_15 <- round(df_2012$trab_2/total_trab * df_2012$total_pop_15ymas)
df_2012$trab_3_15 <- round(df_2012$trab_3/total_trab * df_2012$total_pop_15ymas)
df_2012$trab_4_15 <- round(df_2012$trab_4/total_trab * df_2012$total_pop_15ymas)
df_2012$trab_5_15 <- round(df_2012$trab_5/total_trab * df_2012$total_pop_15ymas)
df_2012$trab_6_15 <- round(df_2012$trab_6/total_trab * df_2012$total_pop_15ymas)
df_2012$trab_7_15 <- round(df_2012$trab_7/total_trab * df_2012$total_pop_15ymas)
df_2012$trab_8_15 <- round(df_2012$trab_8/total_trab * df_2012$total_pop_15ymas)
df_2012$trab_9_15 <- round(df_2012$trab_9/total_trab * df_2012$total_pop_15ymas)
df_2012$trab_10_15 <- round(df_2012$trab_10/total_trab * df_2012$total_pop_15ymas)
df_2012$trab_11_15 <- round(df_2012$trab_11/total_trab * df_2012$total_pop_15ymas)
df_2012$trab_12_15 <- round(df_2012$trab_12/total_trab * df_2012$total_pop_15ymas)
df_2012$trab_13_15 <- round(df_2012$trab_13/total_trab * df_2012$total_pop_15ymas)
df_2012$trab_14_15 <- round(df_2012$trab_14/total_trab * df_2012$total_pop_15ymas)

formula= cbind(ind_pop, notind) ~ cbind(trab_1_15, trab_2_15, trab_3_15, trab_4_15, trab_5_15,
                                        trab_6_15, trab_7_15, trab_8_15, trab_9_15, trab_10_15,
                                        trab_11_15, trab_12_15, trab_13_15, trab_14_15
)

(df_2012$trab_1_15 + df_2012$trab_2_15 + df_2012$trab_3_15 + df_2012$trab_4_15 + df_2012$trab_5_15 + df_2012$trab_6_15 + df_2012$trab_7_15 + df_2012$trab_8_15 + df_2012$trab_9_15 + df_2012$trab_10_15 + df_2012$trab_11_15 + df_2012$trab_12_15 + df_2012$trab_13_15 + df_2012$trab_14_15) == df_2012$total_pop_15ymas
cbind(df_2012$ocu_obrero_15 + df_2012$ocu_cuenta_propia_15 + df_2012$ocu_empleador_15 + df_2012$ocu_sin_renum_15 + df_2012$ocu_coop_15, df_2012$total_pop_15ymas)[1,]

# Fix the minor differences in n
for(i in 1:nrow(df_2012)){
  total_tr = df_2012[i,"trab_1_15"] + df_2012[i, "trab_2_15"] + df_2012[i,"trab_3_15"] + df_2012[i,"trab_4_15"] + df_2012[i, "trab_5_15"] + df_2012[i,"trab_6_15"] + df_2012[i, "trab_7_15"] + df_2012[i,"trab_8_15"] + df_2012[i,"trab_9_15"] + df_2012[i, "trab_10_15"] + df_2012[i,"trab_11_15"] + df_2012[i, "trab_12_15"] + df_2012[i,"trab_13_15"] + df_2012[i,"trab_14_15"]
  diff_total = df_2012[i, "total_pop_15ymas"] - total_tr
  if(diff_total > 0){
    df_2012[i, "trab_4_15"] <- df_2012[i, "trab_4_15"] + abs(diff_total) # the difference is negligible
  }
  else if (diff_total < 0){
    df_2012[i, "trab_4_15"] <- df_2012[i, "trab_4_15"] - abs(diff_total) # the difference is negligible
  }
}

for(i in 1:100){
  dbuf <- ei(formula = formula, data = as.data.frame(df_2012))
  out <- dbuf$draws$Beta[, seq(1, 9492, 28)]
  out2 <- dbuf$draws$Beta[, seq(2, 9492, 28)]
  out3 <- dbuf$draws$Beta[, seq(3, 9492, 28)]
  out4 <- dbuf$draws$Beta[, seq(4, 9492, 28)]
  out5 <- dbuf$draws$Beta[, seq(5, 9492, 28)]
  out6 <- dbuf$draws$Beta[, seq(6, 9492, 28)]
  out7 <- dbuf$draws$Beta[, seq(7, 9492, 28)]
  out8 <- dbuf$draws$Beta[, seq(8, 9492, 28)]
  out9 <- dbuf$draws$Beta[, seq(9, 9492, 28)]
  out10 <- dbuf$draws$Beta[, seq(10, 9492, 28)]
  out11 <- dbuf$draws$Beta[, seq(11, 9492, 28)]
  out12 <- dbuf$draws$Beta[, seq(12, 9492, 28)]
  out13 <- dbuf$draws$Beta[, seq(13, 9492, 28)]
  out14 <- dbuf$draws$Beta[, seq(14, 9492, 28)]
  
  result_trab_1_2012[i] = sum(colMeans(out)*df_2012$total_pop/sum(df_2012$total_pop))
  result_trab_2_2012[i] = sum(colMeans(out2)*df_2012$total_pop/sum(df_2012$total_pop))
  result_trab_3_2012[i] = sum(colMeans(out3)*df_2012$total_pop/sum(df_2012$total_pop))
  result_trab_4_2012[i] = sum(colMeans(out4)*df_2012$total_pop/sum(df_2012$total_pop))
  result_trab_5_2012[i] = sum(colMeans(out5)*df_2012$total_pop/sum(df_2012$total_pop))
  result_trab_6_2012[i] = sum(colMeans(out6)*df_2012$total_pop/sum(df_2012$total_pop))
  result_trab_7_2012[i] = sum(colMeans(out7)*df_2012$total_pop/sum(df_2012$total_pop))
  result_trab_8_2012[i] = sum(colMeans(out8)*df_2012$total_pop/sum(df_2012$total_pop))
  result_trab_9_2012[i] = sum(colMeans(out9)*df_2012$total_pop/sum(df_2012$total_pop))
  result_trab_10_2012[i] = sum(colMeans(out10)*df_2012$total_pop/sum(df_2012$total_pop))
  result_trab_11_2012[i] = sum(colMeans(out11)*df_2012$total_pop/sum(df_2012$total_pop))
  result_trab_12_2012[i] = sum(colMeans(out12)*df_2012$total_pop/sum(df_2012$total_pop))
  result_trab_13_2012[i] = sum(colMeans(out13)*df_2012$total_pop/sum(df_2012$total_pop))
  result_trab_14_2012[i] = sum(colMeans(out14)*df_2012$total_pop/sum(df_2012$total_pop))
}


result_2001_trab <- c(mean(result_trab_1), mean(result_trab_2), mean(result_trab_3), mean(result_trab_4),
                      mean(result_trab_5), mean(result_trab_6), mean(result_trab_7), mean(result_trab_8),
                      mean(result_trab_9), mean(result_trab_10), mean(result_trab_11), mean(result_trab_12),
                      mean(result_trab_13), mean(result_trab_14))
result_2012_trab <- c(mean(result_trab_1_2012), mean(result_trab_2_2012), mean(result_trab_3_2012), mean(result_trab_4_2012),
                      mean(result_trab_5_2012), mean(result_trab_6_2012), mean(result_trab_7_2012), mean(result_trab_8_2012),
                      mean(result_trab_9_2012), mean(result_trab_10_2012), mean(result_trab_11_2012), mean(result_trab_12_2012),
                      mean(result_trab_13_2012), mean(result_trab_14_2012))

cbind(result_2001_trab, result_2012_trab)

save(result_2001_trab, file = "trab_2001.rdata")
save(result_2012_trab, file = "trab_2001.rdata")

# proportion of indigenous population decreased pretty much across the board except for mining, which is interesting



############################## Language ##################################################
# Check language (BIG ISSUE: MULTIPLE RESPONSES ALLOWED)

result_castellano = array(1:100)
result_quechua = array(1:100)
result_guarani = array(1:100)
result_aymara = array(1:100)
result_otro = array(1:100)
result_extranjero = array(1:100)

df_2001$civ_soltero_15 <- round(df_2001$civ_soltero/df_2001$total_pop * df_2001$total_pop_15ymas)
df_2001$civ_casado_15 <- round(df_2001$civ_casado/df_2001$total_pop * df_2001$total_pop_15ymas)
df_2001$civ_conviviente_15 <- round(df_2001$civ_conviviente/df_2001$total_pop * df_2001$total_pop_15ymas)
df_2001$civ_separado_15 <- round(df_2001$civ_separado/df_2001$total_pop * df_2001$total_pop_15ymas)
df_2001$civ_divorciado_15 <- round(df_2001$civ_divorciado/df_2001$total_pop * df_2001$total_pop_15ymas)
df_2001$idi_extranjero_15 <- round(df_2001$idi_extranjero/df_2001$total_pop * df_2001$total_pop_15ymas)


formula= cbind(ind_pop, notind) ~ cbind(civ_soltero_15, civ_casado_15, civ_conviviente_15, civ_separado_15, civ_divorciado_15, idi_extranjero_15)

for(i in 1:nrow(df_2001)){
  total_idi = df_2001[i,"civ_soltero_15"] + df_2001[i, "civ_casado_15"] + df_2001[i,"civ_conviviente_15"] + df_2001[i,"civ_separado_15"] + df_2001[i, "civ_divorciado_15"] + df_2001[i,"idi_extranjero_15"]
  diff_total = df_2001[i, "total_pop_15ymas"] - total_idi
  if(diff_total > 0){
    df_2001[i, "civ_soltero_15"] <- df_2001[i, "civ_soltero_15"] + abs(diff_total) # the difference is negligible
  }
  else if (diff_total < 0){
    df_2001[i, "civ_soltero_15"] <- df_2001[i, "civ_soltero_15"] - abs(diff_total) # the difference is negligible
  }
}

for(i in 1:100){
  dbuf <- ei(formula = formula, data = as.data.frame(df_2001))
  out <- dbuf$draws$Beta[, seq(1, 4068, 12)]
  out2 <- dbuf$draws$Beta[, seq(2, 4068, 12)]
  out3 <- dbuf$draws$Beta[, seq(3, 4068, 12)]
  out4 <- dbuf$draws$Beta[, seq(4, 4068, 12)]
  out5 <- dbuf$draws$Beta[, seq(5, 4068, 12)]
  out6 <- dbuf$draws$Beta[, seq(6, 4068, 12)]
  
  result_castellano[i] = sum(colMeans(out)*df_2001$total_pop/sum(df_2001$total_pop))
  result_quechua[i] = sum(colMeans(out2)*df_2001$total_pop/sum(df_2001$total_pop))
  result_guarani[i] = sum(colMeans(out3)*df_2001$total_pop/sum(df_2001$total_pop))
  result_aymara[i] = sum(colMeans(out4)*df_2001$total_pop/sum(df_2001$total_pop))
  result_otro[i] = sum(colMeans(out5)*df_2001$total_pop/sum(df_2001$total_pop))
  result_extranjero[i] = sum(colMeans(out6)*df_2001$total_pop/sum(df_2001$total_pop))
}



############################## Relationship Status ##################################################
# Check relationship status

result_soltero = array(1:100)
result_casado = array(1:100)
result_conviviente = array(1:100)
result_separado = array(1:100)
result_divorciado = array(1:100)
result_viudo = array(1:100)

formula= cbind(ind_pop, notind) ~ cbind(civ_soltero, civ_casado, civ_conviviente, civ_separado, civ_divorciado, civ_viudo)

for(i in 1:nrow(df_2001)){
  total_civ = df_2001[i, "civ_soltero"] + df_2001[i, "civ_casado"] + df_2001[i, "civ_conviviente"] + df_2001[i, "civ_separado"] + df_2001[i, "civ_divorciado"] + df_2001[i, "civ_viudo"]
  diff_total = df_2001[i, "total_pop_15ymas"] - total_civ
  if(diff_total > 0){
    df_2001[i, "civ_soltero"] <- df_2001[i, "civ_soltero"] + abs(diff_total) # the difference is negligible
  }
  else if (diff_total < 0){
    df_2001[i, "civ_soltero"] <- df_2001[i, "civ_soltero"] - abs(diff_total) # the difference is negligible
  }
}

for(i in 1:100){
  dbuf <- ei(formula = formula, data = as.data.frame(df_2001))
  out <- dbuf$draws$Beta[, seq(1, 4068, 12)]
  out2 <- dbuf$draws$Beta[, seq(2, 4068, 12)]
  out3 <- dbuf$draws$Beta[, seq(3, 4068, 12)]
  out4 <- dbuf$draws$Beta[, seq(4, 4068, 12)]
  out5 <- dbuf$draws$Beta[, seq(5, 4068, 12)]
  out6 <- dbuf$draws$Beta[, seq(6, 4068, 12)]
  
  result_soltero[i] = sum(colMeans(out)*df_2001$total_pop/sum(df_2001$total_pop))
  result_casado[i] = sum(colMeans(out2)*df_2001$total_pop/sum(df_2001$total_pop))
  result_conviviente[i] = sum(colMeans(out3)*df_2001$total_pop/sum(df_2001$total_pop))
  result_separado[i] = sum(colMeans(out4)*df_2001$total_pop/sum(df_2001$total_pop))
  result_divorciado[i] = sum(colMeans(out5)*df_2001$total_pop/sum(df_2001$total_pop))
  result_viudo[i] = sum(colMeans(out6)*df_2001$total_pop/sum(df_2001$total_pop))
}

result_soltero_2012 = array(1:100)
result_casado_2012 = array(1:100)
result_conviviente_2012 = array(1:100)
result_separado_2012 = array(1:100)
result_divorciado_2012 = array(1:100)
result_viudo_2012 = array(1:100)

formula= cbind(ind_pop, notind) ~ cbind(civ_soltero, civ_casado, civ_conviviente, civ_separado, civ_divorciado, civ_viudo)

for(i in 1:nrow(df_2012)){
  total_civ = df_2012[i, "civ_soltero"] + df_2012[i, "civ_casado"] + df_2012[i, "civ_conviviente"] + df_2012[i, "civ_separado"] + df_2012[i, "civ_divorciado"] + df_2012[i, "civ_viudo"]
  diff_total = df_2012[i, "total_pop_15ymas"] - total_civ
  if(diff_total > 0){
    df_2012[i, "civ_soltero"] <- df_2012[i, "civ_soltero"] + abs(diff_total) # the difference is negligible
  }
  else if (diff_total < 0){
    df_2012[i, "civ_soltero"] <- df_2012[i, "civ_soltero"] - abs(diff_total) # the difference is negligible
  }
}

for(i in 1:100){
  dbuf <- ei(formula = formula, data = as.data.frame(df_2012))
  out <- dbuf$draws$Beta[, seq(1, 4068, 12)]
  out2 <- dbuf$draws$Beta[, seq(2, 4068, 12)]
  out3 <- dbuf$draws$Beta[, seq(3, 4068, 12)]
  out4 <- dbuf$draws$Beta[, seq(4, 4068, 12)]
  out5 <- dbuf$draws$Beta[, seq(5, 4068, 12)]
  out6 <- dbuf$draws$Beta[, seq(6, 4068, 12)]
  
  result_soltero_2012[i] = sum(colMeans(out)*df_2012$total_pop/sum(df_2012$total_pop))
  result_casado_2012[i] = sum(colMeans(out2)*df_2012$total_pop/sum(df_2012$total_pop))
  result_conviviente_2012[i] = sum(colMeans(out3)*df_2012$total_pop/sum(df_2012$total_pop))
  result_separado_2012[i] = sum(colMeans(out4)*df_2012$total_pop/sum(df_2012$total_pop))
  result_divorciado_2012[i] = sum(colMeans(out5)*df_2012$total_pop/sum(df_2012$total_pop))
  result_viudo_2012[i] = sum(colMeans(out6)*df_2012$total_pop/sum(df_2012$total_pop))
}

rel_result_2001 <- c(mean(result_soltero), mean(result_casado), mean(result_conviviente), 
                     mean(result_separado), mean(result_divorciado), mean(result_viudo))

rel_result_2012 <- c(mean(result_soltero_2012), mean(result_casado_2012), mean(result_conviviente_2012), 
                     mean(result_separado_2012), mean(result_divorciado_2012), mean(result_viudo_2012))
change_rel_2001 <- c(sum(df_2001$civ_soltero)/sum(df_2001$total_pop_15ymas), 
                sum(df_2001$civ_casado)/sum(df_2001$total_pop_15ymas),
                sum(df_2001$civ_conviviente)/sum(df_2001$total_pop_15ymas),
                sum(df_2001$civ_separado)/sum(df_2001$total_pop_15ymas),
                sum(df_2001$civ_divorciado)/sum(df_2001$total_pop_15ymas),
                sum(df_2001$civ_viudo)/sum(df_2001$total_pop_15ymas)
                )
change_rel_2012 <- c(sum(df_2012$civ_soltero)/sum(df_2012$total_pop_15ymas), 
                     sum(df_2012$civ_casado)/sum(df_2012$total_pop_15ymas),
                     sum(df_2012$civ_conviviente)/sum(df_2012$total_pop_15ymas),
                     sum(df_2012$civ_separado)/sum(df_2012$total_pop_15ymas),
                     sum(df_2012$civ_divorciado)/sum(df_2012$total_pop_15ymas),
                     sum(df_2012$civ_viudo)/sum(df_2012$total_pop_15ymas)
)
cbind(rel_result_2001, rel_result_2012, change_rel_2001, change_rel_2012)

# went down across the board, no significant change
# single population went up, married population went down from 2001 to 2012

################################################################################
# Check Birth documentation
result_reg_civil_si = array(1:100)
result_reg_civil_no = array(1:100)
result_reg_civil_se = array(1:100)

df_2001$reg_civil_si_15 <- round(df_2001$reg_civil_si/df_2001$total_pop * df_2001$total_pop_15ymas)
df_2001$reg_civil_no_15 <- round(df_2001$reg_civil_no/df_2001$total_pop * df_2001$total_pop_15ymas)
df_2001$reg_civil_se_15 <- round(df_2001$reg_civil_se/df_2001$total_pop * df_2001$total_pop_15ymas)

formula= cbind(ind_pop, notind) ~ cbind(reg_civil_si_15, reg_civil_no_15, reg_civil_se_15)

for(i in 1:nrow(df_2001)){
  total_reg = df_2001[i, "reg_civil_si_15"] + df_2001[i, "reg_civil_no_15"] + df_2001[i, "reg_civil_se_15"]
  diff_total = df_2001[i, "total_pop_15ymas"] - total_reg
  if(diff_total > 0){
    df_2001[i, "reg_civil_si_15"] <- df_2001[i, "reg_civil_si_15"] + abs(diff_total) # the difference is negligible
  }
  else if (diff_total < 0){
    df_2001[i, "reg_civil_si_15"] <- df_2001[i, "reg_civil_si_15"] - abs(diff_total) # the difference is negligible
  }
}

for(i in 1:100){
  dbuf <- ei(formula = formula, data = as.data.frame(df_2001))
  out <- dbuf$draws$Beta[, seq(1, 2034, 6)]
  out2 <- dbuf$draws$Beta[, seq(2, 2034, 6)]
  out3 <- dbuf$draws$Beta[, seq(3, 2034, 6)]
  result_reg_civil_si[i] = sum(colMeans(out)*df_2001$total_pop/sum(df_2001$total_pop))
  result_reg_civil_no[i] = sum(colMeans(out2)*df_2001$total_pop/sum(df_2001$total_pop))
  result_reg_civil_se[i] = sum(colMeans(out3)*df_2001$total_pop/sum(df_2001$total_pop))
}

# 2012

result_reg_civil_si_2012 = array(1:100)
result_reg_civil_no_2012 = array(1:100)
result_reg_civil_se_2012 = array(1:100)

df_2012$reg_civil_si_15 <- round(df_2012$reg_civil_si/df_2012$total_pop * df_2012$total_pop_15ymas)
df_2012$reg_civil_no_15 <- round(df_2012$reg_civil_no/df_2012$total_pop * df_2012$total_pop_15ymas)
df_2012$reg_civil_se_15 <- round(df_2012$reg_civil_se/df_2012$total_pop * df_2012$total_pop_15ymas)

formula= cbind(ind_pop, notind) ~ cbind(reg_civil_si_15, reg_civil_no_15, reg_civil_se_15)

for(i in 1:nrow(df_2012)){
  total_reg = df_2012[i, "reg_civil_si_15"] + df_2012[i, "reg_civil_no_15"] + df_2012[i, "reg_civil_se_15"]
  diff_total = df_2012[i, "total_pop_15ymas"] - total_reg
  if(diff_total > 0){
    df_2012[i, "reg_civil_si_15"] <- df_2012[i, "reg_civil_si_15"] + abs(diff_total) # the difference is negligible
  }
  else if (diff_total < 0){
    df_2012[i, "reg_civil_si_15"] <- df_2012[i, "reg_civil_si_15"] - abs(diff_total) # the difference is negligible
  }
}

for(i in 1:100){
  dbuf <- ei(formula = formula, data = as.data.frame(df_2012))
  out <- dbuf$draws$Beta[, seq(1, 2034, 6)]
  out2 <- dbuf$draws$Beta[, seq(2, 2034, 6)]
  out3 <- dbuf$draws$Beta[, seq(3, 2034, 6)]
  result_reg_civil_si_2012[i] = sum(colMeans(out)*df_2012$total_pop/sum(df_2012$total_pop))
  result_reg_civil_no_2012[i] = sum(colMeans(out2)*df_2012$total_pop/sum(df_2012$total_pop))
  result_reg_civil_se_2012[i] = sum(colMeans(out3)*df_2012$total_pop/sum(df_2012$total_pop))
}

reg_result_2001 <- c(mean(result_reg_civil_si), mean(result_reg_civil_no), mean(result_reg_civil_se))

reg_result_2012 <- c(mean(result_reg_civil_si_2012), mean(result_reg_civil_no_2012), mean(result_reg_civil_se_2012))

change_reg_2001 <- c(sum(df_2001$reg_civil_si_15)/sum(df_2001$total_pop_15ymas), 
                     sum(df_2001$reg_civil_no_15)/sum(df_2001$total_pop_15ymas),
                     sum(df_2001$reg_civil_se_15)/sum(df_2001$total_pop_15ymas)
)
change_reg_2012 <- c(sum(df_2012$reg_civil_si_15)/sum(df_2012$total_pop_15ymas), 
                     sum(df_2012$reg_civil_no_15)/sum(df_2012$total_pop_15ymas),
                     sum(df_2012$reg_civil_se_15)/sum(df_2012$total_pop_15ymas)
)
cbind(reg_result_2001, reg_result_2012, change_reg_2001, change_reg_2012)

# the difference seems neglible but more people did register in 2012 compared to 2001 (about 7%)


################################################################################
# Check Housing

result_viv_casa = array(1:100)
result_viv_dep = array(1:100)
result_viv_cuarto = array(1:100)
result_viv_improv = array(1:100)
result_viv_local_no = array(1:100)

vivienda_total = df_2001$viv_casa + df_2001$viv_dep + df_2001$viv_cuarto + df_2001$viv_improv + df_2001$viv_local_no

df_2001$viv_casa_15 <- round(df_2001$viv_casa/vivienda_total * df_2001$total_pop_15ymas)
df_2001$viv_dep_15 <- round(df_2001$viv_dep/vivienda_total * df_2001$total_pop_15ymas)
df_2001$viv_cuarto_15 <- round(df_2001$viv_cuarto/vivienda_total * df_2001$total_pop_15ymas)
df_2001$viv_improv_15 <- round(df_2001$viv_improv/vivienda_total * df_2001$total_pop_15ymas)
df_2001$viv_local_no_15 <- round(df_2001$viv_local_no/vivienda_total * df_2001$total_pop_15ymas)


formula= cbind(ind_pop, notind) ~ cbind(viv_casa_15, viv_dep_15, viv_cuarto_15,
                                        viv_improv_15, viv_local_no_15)

for(i in 1:nrow(df_2001)){
  total_viv = df_2001[i, "viv_casa_15"] + df_2001[i, "viv_dep_15"] + df_2001[i, "viv_cuarto_15"] + df_2001[i, "viv_improv_15"] + df_2001[i, "viv_local_no_15"]
  diff_total = df_2001[i, "total_pop_15ymas"] - total_viv
  if(diff_total > 0){
    df_2001[i, "viv_casa_15"] <- df_2001[i, "viv_casa_15"] + abs(diff_total) # the difference is negligible
  }
  else if (diff_total < 0){
    df_2001[i, "viv_casa_15"] <- df_2001[i, "viv_casa_15"] - abs(diff_total) # the difference is negligible
  }
}

for(i in 1:100){
  dbuf <- ei(formula = formula, data = as.data.frame(df_2001))
  out <- dbuf$draws$Beta[, seq(1, 3390, 10)]
  out2 <- dbuf$draws$Beta[, seq(2, 3390, 10)]
  out3 <- dbuf$draws$Beta[, seq(3, 3390, 10)]
  out4 <- dbuf$draws$Beta[, seq(4, 3390, 10)]
  out5 <- dbuf$draws$Beta[, seq(5, 3390, 10)]
  
  result_viv_casa[i] = sum(colMeans(out)*df_2001$total_pop/sum(df_2001$total_pop))
  result_viv_dep[i] = sum(colMeans(out2)*df_2001$total_pop/sum(df_2001$total_pop))
  result_viv_cuarto[i] = sum(colMeans(out3)*df_2001$total_pop/sum(df_2001$total_pop))
  result_viv_improv[i] = sum(colMeans(out4)*df_2001$total_pop/sum(df_2001$total_pop))
  result_viv_local_no[i] = sum(colMeans(out5)*df_2001$total_pop/sum(df_2001$total_pop))
}

# 2012

result_viv_casa_2012 = array(1:100)
result_viv_dep_2012 = array(1:100)
result_viv_cuarto_2012 = array(1:100)
result_viv_improv_2012 = array(1:100)
result_viv_local_no_2012 = array(1:100)

vivienda_total_2012 = df_2012$viv_casa + df_2012$viv_dep + df_2012$viv_cuarto + df_2012$viv_improv + df_2012$viv_local_no

df_2012$viv_casa_15 <- round(df_2012$viv_casa/vivienda_total_2012 * df_2012$total_pop_15ymas)
df_2012$viv_dep_15 <- round(df_2012$viv_dep/vivienda_total_2012 * df_2012$total_pop_15ymas)
df_2012$viv_cuarto_15 <- round(df_2012$viv_cuarto/vivienda_total_2012 * df_2012$total_pop_15ymas)
df_2012$viv_improv_15 <- round(df_2012$viv_improv/vivienda_total_2012 * df_2012$total_pop_15ymas)
df_2012$viv_local_no_15 <- round(df_2012$viv_local_no/vivienda_total_2012 * df_2012$total_pop_15ymas)


formula= cbind(ind_pop, notind) ~ cbind(viv_casa_15, viv_dep_15, viv_cuarto_15,
                                        viv_improv_15, viv_local_no_15)

for(i in 1:nrow(df_2012)){
  total_viv = df_2012[i, "viv_casa_15"] + df_2012[i, "viv_dep_15"] + df_2012[i, "viv_cuarto_15"] + df_2012[i, "viv_improv_15"] + df_2012[i, "viv_local_no_15"]
  diff_total = df_2012[i, "total_pop_15ymas"] - total_viv
  if(diff_total > 0){
    df_2012[i, "viv_casa_15"] <- df_2012[i, "viv_casa_15"] + abs(diff_total) # the difference is negligible
  }
  else if (diff_total < 0){
    df_2012[i, "viv_casa_15"] <- df_2012[i, "viv_casa_15"] - abs(diff_total) # the difference is negligible
  }
}

for(i in 1:100){
  dbuf <- ei(formula = formula, data = as.data.frame(df_2012))
  out <- dbuf$draws$Beta[, seq(1, 3390, 10)]
  out2 <- dbuf$draws$Beta[, seq(2, 3390, 10)]
  out3 <- dbuf$draws$Beta[, seq(3, 3390, 10)]
  out4 <- dbuf$draws$Beta[, seq(4, 3390, 10)]
  out5 <- dbuf$draws$Beta[, seq(5, 3390, 10)]
  
  result_viv_casa_2012[i] = sum(colMeans(out)*df_2012$total_pop/sum(df_2012$total_pop))
  result_viv_dep_2012[i] = sum(colMeans(out2)*df_2012$total_pop/sum(df_2012$total_pop))
  result_viv_cuarto_2012[i] = sum(colMeans(out3)*df_2012$total_pop/sum(df_2012$total_pop))
  result_viv_improv_2012[i] = sum(colMeans(out4)*df_2012$total_pop/sum(df_2012$total_pop))
  result_viv_local_no_2012[i] = sum(colMeans(out5)*df_2012$total_pop/sum(df_2012$total_pop))
}

viv_result_2001 <- c(mean(result_viv_casa), mean(result_viv_dep), mean(result_viv_cuarto),
                     mean(result_viv_improv), mean(result_viv_local_no)
                     )

viv_result_2012 <- c(mean(result_viv_casa_2012), mean(result_viv_dep_2012), mean(result_viv_cuarto_2012),
                     mean(result_viv_improv_2012), mean(result_viv_local_no_2012)
)

change_viv_2001 <- c(sum(df_2001$viv_casa_15)/sum(df_2001$total_pop_15ymas), 
                     sum(df_2001$viv_dep_15)/sum(df_2001$total_pop_15ymas),
                     sum(df_2001$viv_cuarto_15)/sum(df_2001$total_pop_15ymas),
                     sum(df_2001$viv_improv_15)/sum(df_2001$total_pop_15ymas),
                     sum(df_2001$viv_local_no_15)/sum(df_2001$total_pop_15ymas)
)
change_viv_2012 <- c(sum(df_2012$viv_casa_15)/sum(df_2012$total_pop_15ymas), 
                     sum(df_2012$viv_dep_15)/sum(df_2012$total_pop_15ymas),
                     sum(df_2012$viv_cuarto_15)/sum(df_2012$total_pop_15ymas),
                     sum(df_2012$viv_improv_15)/sum(df_2012$total_pop_15ymas),
                     sum(df_2012$viv_local_no_15)/sum(df_2012$total_pop_15ymas)
)

cbind(viv_result_2001, viv_result_2012, change_viv_2001, change_viv_2012)

# Except for improvised housing, all other indigenous proportions went down. However, there's very little change in the composition of housing from 2001 to 2012, so housing is a non-factor.
