# Bolivia Census
Analysis of Bolivia Census data from 2001 and 2012

## Variables and Preprocessing

### Data Aggregation

I obtained data from Instituto Nacional de Estadistica ([INE](https://www.ine.gob.bo/)) to extract relevant data for 2001 and 2012 census, then extracted additional variables from the files provided to add to the tables.

**Files used for 2001:**

- 2001.xlsx
- 2001_ind_edad_fem_caebl=(3 ~ 17).xls
- 2001_ind_edad_fem_p34a=(1 ~ 3).xls
- 2001_ind_edad_fem_p36=(1,2,9).xls
- 2001_ind_edad_fem_p39niv=(11 ~ 23).xls
- 2001_ind_edad_masc_caebl=(3 ~ 17).xls
- 2001_ind_edad_masc_p34a=(1 ~ 3).xls
- 2001_ind_edad_masc_p36=(1,2,9).xls
- 2001_ind_edad_masc_p39niv=(11 ~ 23).xls

**Files used for 2012:**

- 2012.xlsx 
- Pertenencia a algun Pueblo.xlsx 
- Sexo_InscripcionRegistroCivil_TenenciaCedula.xlsx 
- Edad_Quinquenal.xlsx 
- AsisteEscolar_Idioma que Aprendio en la Ninez.xlsx 
- DondeNacio_DondeViveHabitual_DondeVivia hace 5anos.xlsx 
- EstadoCivil_CategoriaOcupacional.xlsx 
- TipoVivienda_Ocupacion vivienda.xlsx 
- 2012_ind_edad_fem_p32a=(1,2,3).xls  
- 2012_ind_edad_fem_p35=(1,2,9).xls 
- 2012_ind_edad_fem_p37a_nivelnue=(1,2,3, 9 ~ 18).xls 
- 2012_ind_edad_fem_p44=(2 ~ 12,15,16,17,20,21).xls
- 2012_ind_edad_masc_p32a=(1,2,3).xls  
- 2012_ind_edad_masc_p35=(1,2,9).xls 
- 2012_ind_edad_masc_p37a_nivelnue=(1,2,3, 9 ~ 18).xls 
- 2012_ind_edad_masc_p44=(2 ~ 12,15,16,17,20,21).xls 

Note: Changed the name of file 2012_ind_edad_masc_p44=21(1) -> 2012_ind_edad_fem_p44=21 after cross validating the error.

### Data Cleaning and Merging

File: preprocess.R  

**go through the general steps in preprocess.R**


### Variables

**Identification**  

- id - Unique 6 digit code for every municipality    
- departamento - Name of department    
- municipio - Name of municipality    

**Population Summary**

- ind_pop - Self-identified indigenous population over 15 years of age  
- total_pop - Total population  
- total_pop_15ymas - Total population of 15 years or older  
- hombre - Males  
- mujer - Females 
- urban - Urban population  
- rural - Rural population  

**Birth**

- reg_civil_si - Birth documented in registry office
- reg_civil_no - Birth not documented in registry office
- reg_civil_se - Birth documentation not specified

**Age**

- edad0a60 - Age 0 to 60
- edad60ymas - Age 60 or more

**Education**

- ed_pub - Enrolled in public education 
- ed_priv - Enrolled in private education 
- ed_no - Not enrolled in school  
- edu_nivel_low - Preschool level 
- edu_nivel_med - School (Elementary, Middle, High School)  
- edu_nivel_high - Higher education (University, Doctorate, Technical, Occupational, Military, Political School)  
- leer_1 - Literate 
- leer_2 - Illiiterate  
- leer_3 - Did not specificy literacy 

**Language**

- idi_castellano - Speaks Spanish 
- idi_quechua - Speaks Quechua  
- idi_guarani - Speaks Guarani  
- idi_aymara - Speaks Aymara  
- idi_otro - Speaks other indigenous language 
- idi_extranjero - Speaks a foreign language  

**Relationship Status**

- civ_soltero - Single  
- civ_casado - Married  
- civ_conviviente - Has a partner 
- civ_separado - Separated  
- civ_divorciado - Divorced 
- civ_viudo - Widowed 


**Migration**

- nacio_1 - Born here 
- nacio_2 - Born in other area of Bolivia 
- nacio_3 - Born in a foreign country 
- vive_aqui - Currently lives here  
- vive_otro - Currently lives in other area of Bolivia  
- vive_exterior - Currently lives in other country  

**Housing**

- viv_casa - Lives in a house 
- viv_dep - Lives in an apartment 
- viv_cuarto - Cuarto o habitacion suelta 
- viv_improv - Improvised or mobile housing 
- viv_local_no - Unmarked housing 
- elec_si - Has electricity in home 
- elec_no - No electricity in home  

Note: elec_si and elec_no data is missing from 2001 table

**Industry**

- trab_1 - Finance  
- trab_2 - Real Estate  
- trab_3 - Public Administration, Defense, National Security  
- trab_4 - Education  
- trab_5 - Social and Health Services 
- trab_6 - Domestic Work  
- trab_7 - International Organization 
- trab_8 - Mining 
- trab_9 - Manufacturing  
- trab_10 - Utilities (Electricity, Gas, Water) 
- trab_11 - Construction  
- trab_12 - Wholesale and Retail Trade  
- trab_13 - Hotel and Restaurant  
- trab_14 - Transportation, Storage, Communication  

**Employment Type**

- ocu_obrero - Employed
- ocu_cuenta_propia - Self-Employed
- ocu_empleador - Employer
- ocu_sin_renum - Working without pay
- ocu_coop - Worker cooperative



