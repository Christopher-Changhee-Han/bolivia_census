setwd("~/bolivia_census")
source("packages.R")

#### make the functions replicable later

###############################################################################
###############################################################################
#### aggregate 2001 data to the municipal level ####
# poblacion
rawdata2001 <- read_xlsx("data/2001.xlsx", sheet = 1)

id_map <- rawdata2001 %>% 
  select(c(1,3,5,2,6)) %>% 
  mutate(id = paste(idep,ipro,isec, sep = "")) %>%
  select(-c(1,2,3)) %>%
  relocate(id, .before = Departamento) %>%
  rename(departamento = Departamento,
         municipio = Municipio) %>%
  distinct()

id_map[id_map$id == "020205", 2] = "La Paz"
id_map[id_map$id == "020205", 3] = "Santiago de Huata"
id_map <- id_map %>% drop_na(departamento, municipio) 

cleaned_municipio <- str_extract(id_map$municipio,  "(?<=\\().+?(?=\\))") 
id_map$municipio[!is.na(cleaned_municipio)] <- cleaned_municipio[!is.na(cleaned_municipio)]
id_map$departamento <- stri_trans_general(str = id_map$departamento, id = "Latin-ASCII")
id_map$municipio <- stri_trans_general(str = id_map$municipio, id = "Latin-ASCII")

id_map <- arrange(id_map, municipio, departamento)
id_map[c(62,193,216,237,247,254,292,304,317,319), "municipio"] <- c("Chiquihuta", "Puerto Carabuco", "San Pedro de Buena Vista", "San Jose de Chiquitos","San Pedro de Curahuara","Santa Ana de Yacuma","Toco","Humanata","Villa de Sacaca","Villa Nueva (Loma Alta)")
id_map <- arrange(id_map, municipio, departamento)

poblacion_2001 <- rawdata2001 %>% 
  select(c(2,6,10:23, 36:45, 49:51, 69:73, 91:103)) %>% 
  group_by(Departamento, Municipio) %>% 
  rename(departamento = Departamento, municipio = Municipio) %>%
  summarise_all(sum)

cleaned_municipio <- str_extract(poblacion_2001$municipio,  "(?<=\\().+?(?=\\))") 
poblacion_2001$municipio[!is.na(cleaned_municipio)] <- cleaned_municipio[!is.na(cleaned_municipio)]
poblacion_2001$departamento <- stri_trans_general(str = poblacion_2001$departamento, id = "Latin-ASCII")
poblacion_2001$municipio <- stri_trans_general(str = poblacion_2001$municipio, id = "Latin-ASCII")
poblacion_2001 <- arrange(poblacion_2001, municipio)
poblacion_2001$departamento[339] <- "La Paz"
poblacion_2001$municipio[339] <- "Santiago de Huata"
poblacion_2001 <- arrange(poblacion_2001, municipio)

poblacion_2001[c(62,193,216,237,247,254,292,304,317,319), "municipio"] <- c("Chiquihuta", "Puerto Carabuco", "San Pedro de Buena Vista", "San Jose de Chiquitos","San Pedro de Curahuara","Santa Ana de Yacuma","Toco","Humanata","Villa de Sacaca","Villa Nueva (Loma Alta)")

# vivienda

rawdata2001 <- read_xlsx("data/2001.xlsx", sheet = 3)

vivienda_2001 <- rawdata2001 %>% 
  select(c(2,6,12:16, 54:55)) %>% 
  group_by(Departamento, Municipio) %>% 
  rename(departamento = Departamento, municipio = Municipio) %>%
  summarise_all(sum)

cleaned_municipio <- str_extract(vivienda_2001$municipio,  "(?<=\\().+?(?=\\))") 
vivienda_2001$municipio[!is.na(cleaned_municipio)] <- cleaned_municipio[!is.na(cleaned_municipio)]
vivienda_2001$departamento <- stri_trans_general(str = vivienda_2001$departamento, id = "Latin-ASCII")
vivienda_2001$departamento <- str_to_title(vivienda_2001$departamento)
vivienda_2001$municipio <- stri_trans_general(str = vivienda_2001$municipio, id = "Latin-ASCII")
vivienda_2001 <- arrange(vivienda_2001, municipio)
vivienda_2001$municipio[341] <- "Santiago de Huata"
vivienda_2001 <- arrange(vivienda_2001, municipio)[1:339,]

vivienda_2001[c(62,193,216,237,247,254,292,304,317,319), "municipio"] <- c("Chiquihuta", "Puerto Carabuco", "San Pedro de Buena Vista", "San Jose de Chiquitos","San Pedro de Curahuara","Santa Ana de Yacuma","Toco","Humanata","Villa de Sacaca","Villa Nueva (Loma Alta)")

rm(rawdata2001)

df_2001 <- id_map %>%
  inner_join(poblacion_2001, by = c("departamento", "municipio")) %>%
  inner_join(vivienda_2001, by = c("departamento", "municipio"))

###############################################################################
###############################################################################
#### aggregate 2012 data to the municipal level ####

#### ind_pop ####
# Pertenencia a algun Pueblo.xlsx
pueblo <- data.frame()
for (i in 1:9){
  temp <- read_xlsx("data/Pertenencia a algun Pueblo.xlsx", sheet = i, skip = 2) %>% 
    select(c(1,3,5)) %>% 
    group_by(...1, ...3) %>% # group by departamento and municipio
    summarise_all(sum) %>% 
    drop_na(...3)
  pueblo <- rbind(pueblo, temp)
}
names(pueblo) <- c("departamento", "municipio", "ind_pop")
pueblo$departamento <- stri_trans_general(str = pueblo$departamento, id = "Latin-ASCII")
pueblo$municipio <- stri_trans_general(str = pueblo$municipio, id = "Latin-ASCII")

rm(temp)

#### total_pop, hombre, mujer, reg_civil_si, reg_civil_no, reg_civil_se ####
# Sexo_InscripcionRegistroCivil_TenenciaCedula.xlsx
sexo_reg_civil <- data.frame()
for (i in 1:9){
  temp <- read_xlsx("data/Sexo_InscripcionRegistroCivil_TenenciaCedula.xlsx", sheet = i, skip = 2) %>% 
    select(c(1,3,5,6,7,9,10,11)) %>% 
    group_by(...1, ...3) %>% # group by departamento and municipio
    summarise_all(sum) %>%
    drop_na(...3)
  sexo_reg_civil <- rbind(sexo_reg_civil, temp)
}
names(sexo_reg_civil) <- c("departamento", "municipio", "total_pop",
                           "hombre", "mujer", "reg_civil_si", "reg_civil_no", "reg_civil_se")
sexo_reg_civil$departamento <- stri_trans_general(str = sexo_reg_civil$departamento, id = "Latin-ASCII")
sexo_reg_civil$municipio <- stri_trans_general(str = sexo_reg_civil$municipio, id = "Latin-ASCII")
rm(temp)

#### edad0a4 - edad90ymas ####
# Edad_Quinquenal.xlsx

edad <- data.frame()
for (i in 1:9){
  temp <- read_xlsx("data/Edad_Quinquenal.xlsx", sheet = i, skip = 2) %>% 
    select(c(1,3,6:24)) %>% 
    group_by(...1, ...3) %>% # group by departamento and municipio
    summarise_all(sum) %>%
    drop_na(...3)
  edad <- rbind(edad, temp)
}
names(edad) <- c("departamento", "municipio", 
                           "edad0a4", "edad5a9", "edad10a14", "edad15a19", "edad20a24",
                           "edad25a29", "edad30a34", "edad35a39", "edad40a44", "edad45a49",
                           "edad50a54", "edad55a59", "edad60a64", "edad65a69", "edad70a74",
                           "edad75a79", "edad80a84", "edad85a89", "edad90ymas"
                           )
edad$departamento <- stri_trans_general(str = edad$departamento, id = "Latin-ASCII")
edad$municipio <- stri_trans_general(str = edad$municipio, id = "Latin-ASCII")

proportion_15ymas <- rowSums(edad[,6:21])/rowSums(edad[,3:21]) #get proportion of 15 years and older
pueblo$ind_pop <- round(pueblo$ind_pop * proportion_15ymas) # adjust the 2012 ind pop (2001 is 15 years and older)

rm(temp)

#### ed_pub, ... , ed_se, idi_castellano, ... , idi_no ####
# AsisteEscolar_Idioma que Aprendio en la Ninez.xlsxW

educacion <- data.frame()
for (i in c(1:3, 5:9)){
  temp <- read_xlsx("data/AsisteEscolar_Idioma que Aprendio en la Ninez.xlsx", sheet = i, skip = 2) %>% 
    select(c(1,3,6,7,9, 12:18, 20)) %>% 
    group_by(...1, ...3) %>% # group by departamento and municipio
    summarise_all(sum) %>%
    drop_na(...3)
  educacion <- rbind(educacion, temp)
}
# note: Oruro only has 19 columns (missing column 17)
temp <- read_xlsx("data/AsisteEscolar_Idioma que Aprendio en la Ninez.xlsx", sheet = 4, skip = 2) %>% 
  select(c(1,3,6,7,9, 12:17,19)) %>% 
  group_by(...1, ...3) %>% # group by departamento and municipio
  summarise_all(sum) %>%
  drop_na(...3)
educacion <- rbind(educacion, temp)

# resolve the mismatch in the column names
educacion[is.na(educacion[,3]),3] <- educacion[is.na(educacion[,3]), 14]
educacion <- educacion %>% select(c(1:13))

names(educacion) <- c("departamento", "municipio",
                      "ed_pub", "ed_priv", "ed_no", # education variables
                      "idi_castellano", "idi_quechua", "idi_guarani", "idi_aymara", "idi_otro_oficial", "idi_otro", "idi_extranjero", "idi_no" # first language learned variables
                      )
educacion$departamento <- stri_trans_general(str = educacion$departamento, id = "Latin-ASCII")
educacion$municipio <- stri_trans_general(str = educacion$municipio, id = "Latin-ASCII")

educacion <- educacion %>% group_by(municipio) %>% 
  mutate(idi_otro = sum(idi_otro_oficial, idi_otro)) %>% 
  select(-10)

#### vive_####
# DondeNacio_DondeViveHabitual_DondeVivia hace 5anos.xlsx

vive <- data.frame()
for (i in 1:9){
  temp <- read_xlsx("data/DondeNacio_DondeViveHabitual_DondeVivia hace 5anos.xlsx", sheet = i, skip = 2) %>% 
    select(c(1,3,10:12)) %>% 
    group_by(...1, ...3) %>% # group by departamento and municipio
    summarise_all(sum) %>% 
    drop_na(...3)
  vive <- rbind(vive, temp)
}
names(vive) <- c("departamento", "municipio", "vive_aqui", "vive_otro", "vive_exterior")
vive$departamento <- stri_trans_general(str = vive$departamento, id = "Latin-ASCII")
vive$municipio <- stri_trans_general(str = vive$municipio, id = "Latin-ASCII")

rm(temp)

#### civ_soltero, ... , civ_viudo, ocu_obrero, ... , ocu_se ####
# EstadoCivil_CategoriaOcupacional.xlsx


est_civil <- data.frame()
for (i in 1:9){
  temp <- read_xlsx("data/EstadoCivil_CategoriaOcupacional.xlsx", sheet = i, skip = 2) %>% 
    select(c(1,3,6:11, 13:16, 18)) %>% 
    group_by(...1, ...3) %>% # group by departamento and municipio
    summarise_all(sum) %>% 
    drop_na(...3)
  est_civil <- rbind(est_civil, temp)
}
names(est_civil) <- c("departamento", "municipio", 
                      "civ_soltero", "civ_casado", "civ_conviviente", 
                      "civ_separado", "civ_divorciado", "civ_viudo",
                      "ocu_obrero", "ocu_cuenta_propia", "ocu_empleador", 
                      "ocu_sin_renum","ocu_coop"
                      ) #civ = civil status, ocu = ocupation status
est_civil$departamento <- stri_trans_general(str = est_civil$departamento, id = "Latin-ASCII")
est_civil$municipio <- stri_trans_general(str = est_civil$municipio, id = "Latin-ASCII")

rm(temp)


#### viv_casa, ... , viv_colectiva####
# TipoVivienda_Ocupacion vivienda.xlsx

vivienda <- data.frame()
for (i in 1:9){
  temp <- read_xlsx("data/TipoVivienda_Ocupacion vivienda.xlsx", sheet = i, skip = 2) %>% 
    select(c(1,3,6:10)) %>% 
    group_by(...1, ...3) %>% # group by departamento and municipio
    summarise_all(sum) %>% 
    drop_na(...3)
  vivienda <- rbind(vivienda, temp)
}
names(vivienda) <- c("departamento", "municipio", 
                 "viv_casa", "viv_dep", "viv_cuarto", "viv_improv",
                 "viv_local_no"
                 )
vivienda$departamento <- stri_trans_general(str = vivienda$departamento, id = "Latin-ASCII")
vivienda$municipio <- stri_trans_general(str = vivienda$municipio, id = "Latin-ASCII")

rm(temp)



#### energi

#### elec_pub, ... , elec_no ####
# Servicios_Basicos
energia <- data.frame()
for (i in 1:9){
  temp <- read_xlsx("data/Servicios_Basicos.xlsx", sheet = i, skip = 2) %>% 
    select(c(1,3,21:25)) %>%
    group_by(...1, ...3) %>% # group by departamento and municipio
    summarise_all(sum) %>% 
    drop_na(...3)
  energia <- rbind(energia, temp)
}
names(energia) <- c("departamento", "municipio", 
                    "elec_pub", "elec_propia", "elec_solar", "elec_otra", "elec_no")
energia$departamento <- stri_trans_general(str = energia$departamento, id = "Latin-ASCII")
energia$municipio <- stri_trans_general(str = energia$municipio, id = "Latin-ASCII")
energia <- energia %>% group_by(municipio) %>% 
  mutate(elec_si = sum(c_across(elec_pub:elec_otra))) %>% 
  select(-(3:6)) %>%
  relocate(elec_si, .before = elec_no)

rm(temp)


#### urban ####

urban <- data.frame()

departamento_names <- c("BOLIVIA", "CHUQUISACA", "LA PAZ", "COCHABAMBA", 
                        "ORURO", "POTOSI", "TARIJA", "SANTA CRUZ",
                        "BENI", "PANDO")

urban <- read_xls("data/2012.xls") %>% 
  select(c(1,11,12)) %>% 
  rename(municipio = 1,
         urban = ...11,
         rural = ...12) %>%
  drop_na(1, rural) %>% 
  mutate(municipio = stri_trans_general(str = municipio, id = "Latin-ASCII"),
         urban = as.numeric(urban),
         rural = as.numeric(rural)) %>%
  filter(!(municipio %in% departamento_names)) %>%
  arrange(municipio)



#### join together all tables for 2012 ####
df_2012 <- pueblo %>% 
  inner_join(sexo_reg_civil, by = c("departamento", "municipio")) %>%
  inner_join(edad, by = c("departamento", "municipio")) %>%
  inner_join(educacion, by = c("departamento", "municipio")) %>%
  inner_join(energia, by = c("departamento", "municipio")) %>%
  inner_join(est_civil, by = c("departamento", "municipio")) %>%
  inner_join(vive, by = c("departamento", "municipio")) %>%
  inner_join(vivienda, by = c("departamento", "municipio")) %>%
  arrange(municipio)

# some manual edits for irregularities
df_2012[177, "municipio"] <- "Soracachi"
urban[193, "municipio"] <- "Puerto Carabuco"
urban[216, "municipio"] <- "San Pedro De Buena Vista"

df_2012 <- arrange(df_2012, municipio)
urban <- arrange(urban, municipio)
df_2012 <- cbind(df_2012, urban[,2:3]) # manually merging columns because of irregularities in municipio columns

# Fixing irregularities in the municipio names

df_2012[c(62,83,197,220,265,269), "municipio"] <- c("Chiquihuta", "Coro Coro", "Puerto Gonzales Moreno", "Salinas de Garcia Mendoza","El Sena", "Sipe Sipe")

df_2012 <- arrange(df_2012, municipio, departamento)

df_2012 <- id_map %>% 
  inner_join(df_2012, by= c("departamento", "municipio"))

###############################################################################
###############################################################################
# aggregate 0 to 60, 60 and up age groups for consistency with 2001 census
# for 2001, calculate the ind_pop

df_2001 <- df_2001 %>%
  group_by(departamento, municipio) %>%
  mutate(edad0a60 = sum(c_across(p29_1:p29_5)),
         total_pop_15ymas = sum(c_across(p29_3:p29_6)),               
         ind_pop = sum(c_across(p49_1:p49_6))) %>%
  select(-c(9:13, 42:48)) %>%
  relocate(edad0a60, .before = p29_6) %>%
  rename(edad60ymas = p29_6)

df_2012 <- df_2012 %>% 
  group_by(departamento, municipio) %>%
  mutate(edad0a60 = sum(c_across(edad0a4:edad55a59)),
         edad60ymas = sum(c_across(edad60a64:edad90ymas)),
         total_pop_15ymas = sum(c_across(edad15a19:edad90ymas))
         ) %>%
  select(-(11:29)) %>% 
  relocate(edad0a60, edad60ymas, .before = ed_pub) 

# get the population number from percentages for urban and rural
df_2012 <- df_2012 %>% group_by(departamento, municipio) %>% mutate(urban = total_pop * urban/100,
                                                              rural = total_pop * rural/100)

# rename df_2001 variables
df_2001 <- df_2001 %>% rename(urban = Area_u,
                              rural = Area_r,
                              hombre = p28_1,
                              mujer = p28_2,
                              total_pop = p29_t,
                              reg_civil_si = p30_1,
                              reg_civil_no = p30_2,
                              reg_civil_se = p30_3,
                              idi_quechua = p32_1,
                              idi_aymara = p32_2,
                              idi_castellano = p32_3,
                              idi_guarani = p32_4,
                              idi_extranjero = p32_5,
                              idi_no = p32_6,
                              idi_otro = p32_7,
                              vive_aqui = p33_1,
                              vive_otro = p33_2,
                              vive_exterior = p33_3,
                              ed_no = p37_1,
                              ed_pub = p37_2,
                              ed_priv = p37_3,
                              ocu_obrero = p46_3,
                              ocu_cuenta_propia = p46_4,
                              ocu_empleador = p46_5,
                              ocu_coop = p46_6,
                              ocu_sin_renum = p46_7,
                              civ_soltero = p48_1,
                              civ_casado = p48_2,
                              civ_conviviente = p48_3,
                              civ_separado = p48_4,
                              civ_divorciado = p48_5,
                              civ_viudo = p48_6,
                              viv_casa = v04_1,
                              viv_dep = v04_2,
                              viv_cuarto = v04_3,
                              viv_improv = v04_4,
                              viv_local_no = v04_5,
                              elec_si = v15_1,
                              elec_no = v15_2
                              )

df_2001 <- df_2001[, names(df_2012)] # to get it in the same column order as df_2012

# use id_map to add the area codes, save 2001 and 2012 with the area codes,
# go extract the area codes from get_2001 function and get_2012 function to merge the 2 together

df_2001 <- arrange(df_2001, id)
df_2012 <- arrange(df_2012, id)
id_map <- arrange(id_map, id)

save(df_2001, file = "2001.rdata")
save(df_2012, file = "2012.rdata")
save(id_map, file = "id_map.rdata")

###############################################################################
###############################################################################
#### add additional variables from queried tables from Dr. Poertner ####

rm(list = ls())
setwd("~/bolivia_census")
load("2001.rdata")
load("2012.rdata")
load("id_map.rdata")


###############################################################################
# 2001
source("packages.R")
source("2001_functions.R")

setwd("data/2001_others/")

#### job ####

# female job
myFiles <- list.files(pattern = "2001_ind_edad_fem_caebl=.*xls")

caeblf = data.frame(id = character(0))
for (i in 1:length(myFiles)){
  temp <- read_xls(myFiles[i]) %>% get_2001()
  names(temp)[2] <-paste("trab_", i, sep = "")
  caeblf <- caeblf %>% right_join(temp, by = "id")
}

# male job

myFiles <- list.files(pattern = "2001_ind_edad_masc_caebl=.*xls")

caeblm = data.frame(id = character(0))
for (i in 1:length(myFiles)){
  temp <- read_xls(myFiles[i]) %>% get_2001()
  names(temp)[2] <-paste("trab_", i, sep = "")
  caeblm <- caeblm %>% right_join(temp, by = "id")
}

job_2001 <- bind_rows(caeblf, caeblm) %>%
  group_by(id) %>%
  summarise_all(sum)
  

rm(caeblf, caeblm)

# join job into 2001 data
df_2001 <- df_2001 %>% inner_join(job_2001, by = "id")

#### education ####

# female education
myFiles <- list.files(pattern = "2001_ind_edad_fem_p39niv=.*xls")

eduf = data.frame(id = character(0))
for (i in 1:length(myFiles)){
  temp <- read_xls(myFiles[i]) %>% get_2001()
  names(temp)[2] <-paste("edu_nivel_", i, sep = "")
  eduf <- eduf %>% right_join(temp, by = "id")
}
eduf <- eduf %>% group_by(id) %>% mutate(edu_nivel_low = sum(c_across(edu_nivel_1:edu_nivel_2)),
                                         edu_nivel_med = sum(c_across(edu_nivel_3:edu_nivel_7)),
                                         edu_nivel_high = sum(c_across(edu_nivel_8:edu_nivel_13))
                                         ) %>% select(-(2:14))

# male education

myFiles <- list.files(pattern = "2001_ind_edad_masc_p39niv=.*xls")

edum = data.frame(id = character(0))
for (i in 1:length(myFiles)){
  temp <- read_xls(myFiles[i]) %>% get_2001()
  names(temp)[2] <-paste("edu_nivel_", i, sep = "")
  edum <- edum %>% right_join(temp, by = "id")
}
edum <- edum %>% group_by(id) %>% mutate(edu_nivel_low = sum(c_across(edu_nivel_1:edu_nivel_2)),
                                         edu_nivel_med = sum(c_across(edu_nivel_3:edu_nivel_7)),
                                         edu_nivel_high = sum(c_across(edu_nivel_8:edu_nivel_13))
                                          ) %>% select(-(2:14))
edu_2001 <- bind_rows(eduf, edum) %>%
  group_by(id) %>%
  summarise_all(sum)

rm(eduf, edum)

# join education level into 2001 data
df_2001 <- df_2001 %>% inner_join(edu_2001, by = "id")

#### illiteracy ####

# male illiteracy
myFiles <- list.files(pattern = "2001_ind_edad_fem_p36=.*xls")

illif = data.frame(id = character(0))
for (i in 1:length(myFiles)){
  temp <- read_xls(myFiles[i]) %>% get_2001()
  names(temp)[2] <-paste("leer_", i, sep = "")
  illif <- illif %>% right_join(temp, by = "id")
}


myFiles <- list.files(pattern = "2001_ind_edad_masc_p36=.*xls")

illim = data.frame(id = character(0))
for (i in 1:length(myFiles)){
  temp <- read_xls(myFiles[i]) %>% get_2001()
  names(temp)[2] <-paste("leer_", i, sep = "")
  illim <- illim %>% right_join(temp, by = "id")
}

illi_2001 <- bind_rows(illif, illim) %>%
  group_by(id) %>%
  summarise_all(sum)

rm(illif, illim)

# join education level into 2001 data
df_2001 <- df_2001 %>% inner_join(illi_2001, by = "id")

#### place of birth ####

# male place of birth
myFiles <- list.files(pattern = "2001_ind_edad_fem_p34a=.*xls")

naciof = data.frame(id = character(0))
for (i in 1:length(myFiles)){
  temp <- read_xls(myFiles[i]) %>% get_2001()
  names(temp)[2] <-paste("nacio_", i, sep = "")
  naciof <- naciof %>% right_join(temp, by = "id")
}


myFiles <- list.files(pattern = "2001_ind_edad_masc_p34a=.*xls")

naciom = data.frame(id = character(0))
for (i in 1:length(myFiles)){
  temp <- read_xls(myFiles[i]) %>% get_2001()
  names(temp)[2] <-paste("nacio_", i, sep = "")
  naciom <- naciom %>% right_join(temp, by = "id")
}

nacio_2001 <- bind_rows(naciof, naciom) %>%
  group_by(id) %>%
  summarise_all(sum)

rm(naciof, naciom)

# join education level into 2001 data
df_2001 <- df_2001 %>% inner_join(nacio_2001, by = "id")
setwd("~/bolivia_census")
save(df_2001, file= "2001.rdata")
###############################################################################

rm(list = ls())
setwd("~/bolivia_census")
load("2012.rdata")

# 2012
source("packages.R")
source("2012_functions.R")

setwd("~/bolivia_census/data/2012_others/")
#### job ####

# female job
myFiles <- list.files(pattern = "2012_ind_edad_fem_p44=.*xls")

caeblf = data.frame(id = character(0))
for (i in c(2:6,8,9,7, 10:14,16, 15, 1)){
  temp <- read_xls(myFiles[i]) %>% get_2012()
  names(temp)[2] <-paste("trab_", i, sep = "")
  caeblf <- caeblf %>% right_join(temp, by = "id")
}

# male job

myFiles <- list.files(pattern = "2012_ind_edad_masc_p44=.*xls")

caeblm = data.frame(id = character(0))
for (i in c(2:6,8,9,7, 10:14,16, 15, 1)){
  temp <- read_xls(myFiles[i]) %>% get_2012()
  names(temp)[2] <-paste("trab_", i, sep = "")
  caeblm <- caeblm %>% right_join(temp, by = "id")
}

job_2012 <- bind_rows(caeblf, caeblm) %>%
  group_by(id) %>%
  summarise_all(sum)
job_2012[,11] <- job_2012[,11] + job_2012[,12] # refer to documentation
job_2012[,16] <- job_2012[,16] + job_2012[,17]
job_2012 <- job_2012[,-c(12,17)]
names(job_2012) <- c("id", "trab_1", "trab_2", "trab_3", "trab_4", "trab_5", 
                     "trab_6", "trab_7", "trab_8", "trab_9", "trab_10", 
                     "trab_11", "trab_12", "trab_13", "trab_14")

rm(caeblf, caeblm)

# join job into 2012 data
df_2012 <- df_2012 %>% inner_join(job_2012, by = "id")

#### education ####

# female education
myFiles <- list.files(pattern = "2012_ind_edad_fem_p37a_nivelnue=.*xls")

eduf = data.frame(id = character(0))
for (i in c(1,11,12,13,2,3:10)){
  temp <- read_xls(myFiles[i]) %>% get_2012()
  names(temp)[2] <-paste("edu_nivel_", i, sep = "")
  eduf <- eduf %>% right_join(temp, by = "id")
}

# male education

myFiles <- list.files(pattern = "2012_ind_edad_masc_p37a_nivelnue=.*xls")

edum = data.frame(id = character(0))
for (i in c(1,11,12,13,2,3:10)){
  temp <- read_xls(myFiles[i]) %>% get_2012()
  names(temp)[2] <-paste("edu_nivel_", i, sep = "")
  edum <- edum %>% right_join(temp, by = "id")
}

edu_2012 <- bind_rows(eduf, edum) %>%
  group_by(id) %>%
  summarise_all(sum)

edu_2012[,2] <- rowSums(edu_2012[,2:4])
edu_2012[,5] <- rowSums(edu_2012[,5:6])
edu_2012[,7] <- rowSums(edu_2012[,7:14])
edu_2012 <- edu_2012[,-c(3,4,6,8:14)]
names(edu_2012) <- c("id", "edu_nivel_low", "edu_nivel_med", "edu_nivel_high")


rm(eduf, edum)

# join education level into 2012 data
df_2012 <- df_2012 %>% inner_join(edu_2012, by = "id")

#### illiteracy ####

# male illiteracy
myFiles <- list.files(pattern = "2012_ind_edad_fem_p35=.*xls")

illif = data.frame(id = character(0))
for (i in 1:length(myFiles)){
  temp <- read_xls(myFiles[i]) %>% get_2012()
  names(temp)[2] <-paste("leer_", i, sep = "")
  illif <- illif %>% right_join(temp, by = "id")
}


myFiles <- list.files(pattern = "2012_ind_edad_masc_p35=.*xls")

illim = data.frame(id = character(0))
for (i in 1:length(myFiles)){
  temp <- read_xls(myFiles[i]) %>% get_2012()
  names(temp)[2] <-paste("leer_", i, sep = "")
  illim <- illim %>% right_join(temp, by = "id")
}

illi_2012 <- bind_rows(illif, illim) %>%
  group_by(id) %>%
  summarise_all(sum)

rm(illif, illim)

# join education level into 2012 data
df_2012 <- df_2012 %>% inner_join(illi_2012, by = "id")

#### place of birth ####

# male place of birth
myFiles <- list.files(pattern = "2012_ind_edad_fem_p32a=.*xls")

naciof = data.frame(id = character(0))
for (i in 1:length(myFiles)){
  temp <- read_xls(myFiles[i]) %>% get_2012()
  names(temp)[2] <-paste("nacio_", i, sep = "")
  naciof <- naciof %>% right_join(temp, by = "id")
}


myFiles <- list.files(pattern = "2012_ind_edad_masc_p32a=.*xls")

naciom = data.frame(id = character(0))
for (i in 1:length(myFiles)){
  temp <- read_xls(myFiles[i]) %>% get_2012()
  names(temp)[2] <-paste("nacio_", i, sep = "")
  naciom <- naciom %>% right_join(temp, by = "id")
}

nacio_2012 <- bind_rows(naciof, naciom) %>%
  group_by(id) %>%
  summarise_all(sum)

rm(naciof, naciom)

# join place of birth into 2012 data
df_2012 <- df_2012 %>% inner_join(nacio_2012, by = "id")
setwd("~/bolivia_census")
save(df_2012, file = "2012.rdata")


write.csv(df_2001, "clean2001.csv")
write.csv(df_2012, "clean2012.csv")
write.csv(id_map, "id_map.csv")
