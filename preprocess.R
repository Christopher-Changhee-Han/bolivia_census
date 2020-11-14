source("packages.R")

#### aggregate 2001 data to the municipal level ####
# poblacion
rawdata2001 <- read_xlsx("data/2001.xlsx", sheet = 1)

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
rm(rawdata2001)

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
vivienda_2001$municipio <- stri_trans_general(str = vivienda_2001$municipio, id = "Latin-ASCII")
vivienda_2001 <- arrange(vivienda_2001, municipio)[1:339,]
rm(rawdata2001)

df_2001 <- cbind(poblacion_2001, vivienda_2001[3:ncol(vivienda_2001)])

save(df_2001, file = "2001.rdata")

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

rm(temp)

#### ed_pub, ... , ed_se, idi_castellano, ... , idi_no ####
# AsisteEscolar_Idioma que Aprendio en la Ninez.xlsxW

educacion <- data.frame()
for (i in c(1:3, 5:9)){
  temp <- read_xlsx("data/AsisteEscolar_Idioma que Aprendio en la Ninez.xlsx", sheet = i, skip = 2) %>% 
    select(c(1,3,6:10, 12:20)) %>% 
    group_by(...1, ...3) %>% # group by departamento and municipio
    summarise_all(sum) %>%
    drop_na(...3)
  educacion <- rbind(educacion, temp)
}
# note: Oruro only has 19 columns (missing column 17)
temp <- read_xlsx("data/AsisteEscolar_Idioma que Aprendio en la Ninez.xlsx", sheet = 4, skip = 2) %>% 
  select(c(1,3,6:10, 12:19)) %>% 
  group_by(...1, ...3) %>% # group by departamento and municipio
  summarise_all(sum) %>%
  drop_na(...3)
educacion <- rbind(educacion, temp)

# resolve the mismatch in the column names
educacion[is.na(educacion[,3]),3] <- educacion[is.na(educacion[,3]), 17]
educacion[is.na(educacion[,15]),15] <- educacion[is.na(educacion[,15]), 18]
educacion <- educacion %>% select(c(1:16))

names(educacion) <- c("departamento", "municipio",
                      "ed_pub", "ed_priv", "ed_conv", "ed_no", "ed_se", # education variables
                      "idi_castellano", "idi_quechua", "idi_guarani", "idi_aymara", "idi_otro_oficial", "idi_otro", "idi_extranjero", "idi_se", "idi_no" # first language learned variables
                      )
educacion$departamento <- stri_trans_general(str = educacion$departamento, id = "Latin-ASCII")
educacion$municipio <- stri_trans_general(str = educacion$municipio, id = "Latin-ASCII")

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
    select(c(1,3,6:11, 13:19)) %>% 
    group_by(...1, ...3) %>% # group by departamento and municipio
    summarise_all(sum) %>% 
    drop_na(...3)
  est_civil <- rbind(est_civil, temp)
}
names(est_civil) <- c("departamento", "municipio", 
                      "civ_soltero", "civ_casado", "civ_conviviente", 
                      "civ_separado", "civ_divorciado", "civ_viudo",
                      "ocu_obrero", "ocu_cuenta_propia", "ocu_empleador", 
                      "ocu_sin_renum", "ocu_del_hogar", "ocu_coop", "ocu_se"
                      ) #civ = civil status, ocu = ocupation status
est_civil$departamento <- stri_trans_general(str = est_civil$departamento, id = "Latin-ASCII")
est_civil$municipio <- stri_trans_general(str = est_civil$municipio, id = "Latin-ASCII")

rm(temp)


#### viv_casa, ... , viv_colectiva####
# TipoVivienda_Ocupacion vivienda.xlsx

vivienda <- data.frame()
for (i in 1:9){
  temp <- read_xlsx("data/TipoVivienda_Ocupacion vivienda.xlsx", sheet = i, skip = 2) %>% 
    select(c(1,3,6:11)) %>% 
    group_by(...1, ...3) %>% # group by departamento and municipio
    summarise_all(sum) %>% 
    drop_na(...3)
  vivienda <- rbind(vivienda, temp)
}
names(vivienda) <- c("departamento", "municipio", 
                 "viv_casa", "viv_dep", "viv_cuarto", "viv_improv",
                 "viv_local_no", "viv_colectiva")
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
rm(temp)


#### urban ####

urban <- data.frame()

departamento_names <- c("BOLIVIA", "CHUQUISACA", "LA PAZ", "COCHABAMBA", 
                        "ORURO", "POTOSI", "TARIJA", "SANTA CRUZ",
                        "BENI", "PANDO")

urban <- read_xls("data/2012.xls") %>% 
  select(c(1,11,12)) %>% 
  rename(municipio = `BOLIVIA: INDICADORES DE POBLACIÓN, SEGÚN  DEPARTAMENTO Y MUNICIPIO, CENSO 2012`,
         urban = ...11,
         rural = ...12) %>%
  drop_na(1, rural) %>% 
  mutate(municipio = stri_trans_general(str = municipio, id = "Latin-ASCII")) %>%
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

cbind(df_2012$municipio, urban$municipio) #sanity check to see if the municipios match up

df_2012 <- cbind(df_2012, urban[,2:3]) # manually merging columns because of irregularities in municipio columns
save(df_2012, file = "2012.rdata")
load("2012.rdata")



#### test ####

load("2001.rdata")
load("2012.rdata")

# To do: 
df_2001$municipio == df_2012$municipio # fix the irregularities

# rename the variables in 2001 to match up 2012
# figure out and format the variables in the correct format for 2001 and 2012
