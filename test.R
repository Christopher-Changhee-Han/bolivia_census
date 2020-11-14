# Code file containing all the random trials and errors

#### random code ####
# Overall Table with Ethnic Group, Age, Gender, Population (no other factors)
f_2001 <- read_xls("2001/2001_ind_edad_fem.xls") %>% get_summary() %>% mutate(gender = "F")
m_2001 <- read_xls("2001/2001_ind_edad_masc.xls") %>% get_summary() %>% mutate(gender = "M")

f_m_2001 <- rbind(f_2001, m_2001)

not_ind <- f_2001[grep("NINGUNO", f_2001$ethnic_group),]
ind <- f_2001[-grep("NINGUNO", f_2001$ethnic_group),]
ind$population <- as.numeric(ind$population)
f_ind_2001 <- tapply(ind$population, ind$age, sum)/(tapply(ind$population, ind$age, sum) + as.numeric(not_ind$population))

not_indm <- m_2001[grep("NINGUNO", m_2001$ethnic_group),]
indm <- m_2001[-grep("NINGUNO", m_2001$ethnic_group),]
indm$population <- as.numeric(indm$population)
m_ind_2001 <- tapply(indm$population, indm$age, sum)/(tapply(indm$population, indm$age, sum) + as.numeric(not_indm$population))


blue <- rgb(0, 0, 1, alpha=0.5)
red <- rgb(1, 0, 0, alpha=0.5)

barplot(f_ind_2001, col = blue, main = "Age vs. Proportion of Self-Identified Indigenous Population (Bolivia, 2001 Census)")
barplot(m_ind_2001, col = red, add = TRUE)
legend("topleft", legend = c("Female", "Male"), col = c(blue, red), fill = c(blue,red))

# attempt at sanity check to see if i sum up all the data from caebl (17) it will match up the overall female data
sum = 0
for (i in 1:17){
  temp <- read_xls(paste("2001/2001_ind_edad_fem_caebl=" , i, ".xls", sep ="")) %>% get_summary() %>% mutate(gender = "F") %>% filter(ethnic_group == "QUECHUA") 
  temp$population <- as.numeric(temp$population)
  quechua_15 = as.numeric(temp[temp$age == "15", "population"])
  ifelse(is.na(quechua_15), 0, quechua_15)
  sum = sum + ifelse(is.na(quechua_15), 0, quechua_15)
}

#### Test 2001 read in functions ####
source("2001_functions.R")
source("packages.R")


f_1 <- read_xls("2001/2001_ind_edad_fem_p36=1.xls")
f_2 <- read_xls("2001/2001_ind_edad_fem_p36=2.xls")
f_9 <- read_xls("2001/2001_ind_edad_fem_p36=9.xls")

m_1 <- read_xls("2001/2001_ind_edad_masc_p36=1.xls")
m_2 <- read_xls("2001/2001_ind_edad_masc_p36=2.xls")
m_9 <- read_xls("2001/2001_ind_edad_masc_p36=9.xls")

mun_f_1 <- get_mun_2001(f_1)
mun_f_2 <- get_mun_2001(f_2)
mun_f_9 <- get_mun_2001(f_9)

mun_m_1 <- get_mun_2001(m_1)
mun_m_2 <- get_mun_2001(m_2)
mun_m_9 <- get_mun_2001(m_9)


# check to see if the 2001 get_mun code is robust
# verdict: everything seems to work as intended, good work
f_overall <- read_xls("2001/2001_ind_edad_fem.xls")
mun_f_overall <- get_mun_2001(f_overall)
f_caebl1 <- read_xls("2001/2001_ind_edad_fem_caebl=1.xls")
mun_f_caebl1 <- get_mun_2001(f_caebl1)