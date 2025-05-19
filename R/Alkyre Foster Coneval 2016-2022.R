library(tidyverse)
library(openxlsx)
library(readr)
library(tidyr)
library(data.table)


# Directorio
setwd("C:/Users/leca_/OneDrive/Documentos/Codiguito/Alkire-Foster Coneval 2016-2022/R")

# Estimación de Pobreza Multidimensional a través del Método de Alkyre-Foster (2016-2022)

###################################
## Indicador de Rezago Educativo ##
###################################
# Cargar Bases
poblacion16 <- fread("poblacion_16.csv") %>% rename_all(tolower)
poblacion18 <- fread("poblacion_18.csv") %>% rename_all(tolower)
poblacion20 <- fread("poblacion_20.csv") %>% rename_all(tolower)
poblacion22 <- fread("poblacion_22.csv") %>% rename_all(tolower)

# Año
poblacion16[, Año := 2016]
poblacion18[, Año := 2018]
poblacion20[, Año := 2020]
poblacion22[, Año := 2022]

# Lista Bases de Datos Educación
bases_ic_rezedu <- c("poblacion16","poblacion18","poblacion20","poblacion22")

for (base_ic_rezedu in bases_ic_rezedu) { 
  df <- get(base_ic_rezedu)
  # Filtar la base de datos
  df <- df %>% filter(!(between(parentesco,400,499) | between(parentesco,700,799)))
  # Identificadores
  df <- df %>% mutate(idhogar = paste0(folioviv,foliohog),
                      idpersona = paste0(folioviv,foliohog,numren))
  # Año de nacimiento
  df <- df %>% mutate(año_nacimiento = case_when(!is.na(edad) ~ Año - edad))
  # Inasistencia a la escuela
  df <- df %>% mutate(inasis_escuela = case_when(asis_esc == 1 ~ 0,
                                                 asis_esc == 2 ~ 1))
  # Nivel Educativo
  df <- df %>% mutate(niveledu = ifelse(between(nivelaprob,0,1) |
                            (nivelaprob == 2 & gradoaprob < 6), 0,NA))
  df <- df %>% mutate(niveledu = ifelse((nivelaprob == 2 & gradoaprob == 6) | 
                                       ((nivelaprob == 5 | nivelaprob == 6) & antec_esc == 1 & gradoaprob < 3) |
                                         nivelaprob == 3 & gradoaprob < 3, 1, niveledu))
  df <- df %>% mutate(niveledu = ifelse((nivelaprob == 3 & gradoaprob == 3) |
                                        (nivelaprob == 4 & gradoaprob < 3) |
                                        (nivelaprob == 5 & antec_esc == 1 & gradoaprob >= 3) |
                                        (nivelaprob == 5 & antec_esc == 2 & gradoaprob < 3) |
                                        (nivelaprob == 6 & antec_esc == 1 & gradoaprob >= 3) |
                                        (nivelaprob == 6 & antec_esc == 2 & gradoaprob < 3), 2, niveledu))
  df <- df %>% mutate(niveledu = ifelse((nivelaprob == 4 & gradoaprob >= 3) |
                                        (nivelaprob == 5 & antec_esc == 2 & gradoaprob >= 3) |
                                        (nivelaprob == 5 & antec_esc >= 3 & !is.na(antec_esc)) |
                                        (nivelaprob == 6 & antec_esc >= 3 & !is.na(antec_esc)) |
                                        (nivelaprob == 6 & antec_esc == 2 & gradoaprob >= 3) |
                                        (nivelaprob >= 7 & !is.na(nivelaprob)), 3, niveledu))
  # Indicador de Carencia de Rezago Educativo
  df <- df %>% mutate(ic_rezagoeducativo = case_when(between(edad, 3, 21) & niveledu < 3 & inasis_escuela == 1 & año_nacimiento >= 1998 ~ 1,
                                                     edad >= 22 & año_nacimiento >= 1998 & niveledu < 3 ~ 1,
                                                     edad >= 16 & año_nacimiento < 1982 & niveledu == 0 ~ 1,
                                                     between(año_nacimiento, 1982, 1997) & edad >= 16 & niveledu < 2 ~ 1,
                                                     between(edad, 0, 2) ~ 0,
                                                     año_nacimiento >= 1998 & between(edad, 3, 21) & inasis_escuela == 0 ~ 0,
                                                     niveledu == 3 ~ 0,
                                                     edad >= 16 & between(año_nacimiento, 1982, 1997) & (niveledu>=2 & !is.na(niveledu)) ~ 0,
                                                     edad >= 16 & año_nacimiento <= 1981 & (niveledu>=1 & !is.na(niveledu)) ~ 0))  
  # Hablante de Lengua Indígena
  df$hli <- ifelse(df$hablaind == 1 & df$edad >= 3, 1, ifelse(df$hablaind == 2 & df$edad >= 3, 0, NA))
  # Mantener variables
  df <- df %>% select(idhogar, Año, idpersona, año_nacimiento, inasis_escuela, niveledu, ic_rezagoeducativo, hli, sexo, edad)

  assign(base_ic_rezedu, df)
}

# Cambiar el nombre de las bases
ic_rezedu16 = poblacion16
ic_rezedu18 = poblacion18
ic_rezedu20 = poblacion20
ic_rezedu22 = poblacion22

# Exportar a csv
fwrite(ic_rezedu16, "Indicador de Carencia de Rezago Educativo 2016.csv")
fwrite(ic_rezedu18, "Indicador de Carencia de Rezago Educativo 2018.csv")
fwrite(ic_rezedu20, "Indicador de Carencia de Rezago Educativo 2020.csv")
fwrite(ic_rezedu22, "Indicador de Carencia de Rezago Educativo 2022.csv")

###############################################################
## Indicador de Carencia por Acceso a los Servicios de Salud ##
###############################################################

# Crear la base de datos de personas ocupadas
ocupados16 <- fread("trabajos_16.csv") %>% rename_all(tolower)
ocupados18 <- fread("trabajos_18.csv") %>% rename_all(tolower)
ocupados20 <- fread("trabajos_20.csv") %>% rename_all(tolower)
ocupados22 <- fread("trabajos_22.csv") %>% rename_all(tolower)
# Lista de Bases de personas ocupados
bases_ocupados <- c("ocupados16","ocupados18","ocupados20","ocupados22")
# Bucle
for (base_ocupados in bases_ocupados) {
  df <- get(base_ocupados)
  # Identificador del Hogar
  df <- df %>% mutate(idhogar = paste0(folioviv,foliohog))
  # Identificador de la Persona
  df <- df %>% mutate(idpersona = paste0(folioviv,foliohog,numren))
  # Tipo de trabajo
  df <- df %>% mutate(tipo_trabajo = case_when(subor == 1 ~ 1,
                                              (subor == 2 & indep == 1 & tiene_suel == 1) | (subor == 2 & indep == 2 & pago == 1) ~ 2,
                                              (subor == 2 & indep == 1 & tiene_suel == 2) | (subor == 2 & indep == 2 & (between(pago, 2, 3))) ~ 3,
                                              TRUE ~ NA_real_))
  # Variable de ocupación
  df <- df %>% mutate(ocupa = case_when(id_trabajo == 1 ~ 1,
                                        id_trabajo == 2 ~ 0,
                                        TRUE ~ NA_real_))
  # Mantener variables
  df <- df %>% select(folioviv,foliohog,numren, idhogar, idpersona, id_trabajo, tipo_trabajo, ocupa)
  # Convertir datos de formato ancho a largo
  df <- df %>% pivot_wider(id_cols = c(folioviv,foliohog,numren),names_from = id_trabajo,values_from = c(tipo_trabajo,ocupa))
  df <- df %>% mutate(ocupa_2 = case_when(ocupa_2 == 0 ~ 1,
                                          TRUE ~ 0))
  # Trabajo
  df <- df %>% mutate(trabajo = 1)
  # Identificador de la persona
  df <- df %>% mutate(idpersona = paste0(folioviv,foliohog,numren))
  assign(base_ocupados, df)
}

# Crear las bases de datos para salud
poblacion16_salud <- fread("poblacion_16.csv") %>% rename_all(tolower)
poblacion18_salud <- fread("poblacion_18.csv") %>% rename_all(tolower)
poblacion20_salud <- fread("poblacion_20.csv") %>% rename_all(tolower)
poblacion22_salud <- fread("poblacion_22.csv") %>% rename_all(tolower)

# Generar lista de las bases de salud
bases_salud <- c("poblacion16_salud","poblacion18_salud","poblacion20_salud","poblacion22_salud")
# Bucle
for (base_salud in bases_salud) {
  df <- get(base_salud)
  # Filtrar por parentesco
  df <- df %>% filter(!(between(parentesco, 400,499) | between(parentesco, 700, 799)))
  # Identificadores
  df <- df %>% mutate(idhogar = paste0(folioviv,foliohog),
                      idpersona = paste0(folioviv,foliohog,numren))
  assign(base_salud, df)
}

# Unir bases 
ic_asalud16 <- full_join(ocupados16,poblacion16_salud,by="idpersona")
ic_asalud18 <- full_join(ocupados18,poblacion18_salud,by="idpersona")
ic_asalud20 <- full_join(ocupados20,poblacion20_salud,by="idpersona")
ic_asalud22 <- full_join(ocupados22,poblacion22_salud,by="idpersona")

# Lista de bases ic_asalud
bases_ic_asalud <- c("ic_asalud16","ic_asalud18","ic_asalud20","ic_asalud22")

# Bucle
for (base_ic_asalud in bases_ic_asalud) {
  df = get(base_ic_asalud)
  # Población Económicamente Activa
  df <- df %>% mutate(pea = case_when(trabajo == 1 & (edad >= 16 & !is.na(edad)) ~ 1,
                                      (act_pnea1 == 1 | act_pnea2 == 1) & (edad >= 16 & !is.na(edad)) ~ 2,
                                      (edad >= 16 & !is.na(edad)) & ((act_pnea1 != 1 | is.na(act_pnea1)) & (act_pnea2 != 1 | is.na(act_pnea2))) & ((act_pnea1 >= 2 & act_pnea1 <= 6) | (act_pnea2 >= 2 & act_pnea2 <= 6)) ~ 0))
  # Tipo de Trabajo 1
  df <- df %>% mutate(tipo_trabajo_1 = case_when(pea == 1 ~ tipo_trabajo_1,
                                                 pea %in% c(0,2) ~ NA,
                                                 is.na(pea) ~ NA,
                                                 TRUE ~ tipo_trabajo_1))
  # Tipo de Trabajo 2
  df <- df %>% mutate(tipo_trabajo_2 = case_when(pea == 1 ~ tipo_trabajo_2,
                                                 pea %in% c(0,2) ~ NA,
                                                 is.na(pea) ~ NA,
                                                 TRUE ~ tipo_trabajo_2))
  # Servicios Médicos por Prestación Laboral
  # Como ocupación principal
  df <- df %>% mutate(smedlprinc = case_when(ocupa_1 == 1 & atemed == 1 & (inst_1 == 1 | inst_2 == 2 | inst_3 == 3 | inst_4 == 4) & inscr_1 == 1 ~ 1,
                                             ocupa_1 == 1 ~ 0,
                                             TRUE ~ NA))
  # Como ocupación secundaria
  df <- df %>% mutate(smedlsec = case_when(ocupa_2 == 1 & atemed == 1 & (inst_1 == 1 | inst_2 == 2 |inst_3 == 3 | inst_4 == 4) & inscr_1 == 1 ~ 1,
                                           ocupa_2 == 1 ~ 0,
                                           TRUE ~ NA))
  # Servicios médicos de contratacion voluntaria
  df <- df %>% mutate(smedcontvol = case_when(atemed == 1 & (inst_1 == 1 | inst_2 == 2 | inst_3 == 3 |inst_4 == 4) & inscr_6 == 6 & (edad >= 12 & !is.na(edad)) ~ 1,
                                              edad >= 12 & !is.na(edad) ~ 0,
                                              TRUE ~ NA))
  # Acceso directo al servicio de salud
  df <- df %>% mutate(smeddirecto = case_when(tipo_trabajo_1 == 1 & smedlprinc == 1 ~ 1,
                                              tipo_trabajo_1 == 2 & (smedlprinc == 1 | smedcontvol == 1) ~ 1,
                                              tipo_trabajo_1 == 3 & (smedlprinc == 1 | smedcontvol == 1) ~ 1,
                                              tipo_trabajo_2 == 1 & smedlsec == 1 ~ 1,
                                              tipo_trabajo_2 == 2 & (smedlsec == 1 | smedcontvol == 1) ~ 1,
                                              tipo_trabajo_2 == 3 & (smedlsec == 1 | smedcontvol == 1) ~ 1,
                                              TRUE ~ 0))
  # Pariente
  df <- df %>% mutate(pariente = case_when(between(parentesco,100,199) ~ 1,
                                           between(parentesco,200,299) ~ 2,
                                           between(parentesco,300,399) ~ 3,
                                           parentesco == 601 ~ 4,
                                           parentesco == 615 ~ 5,
                                           TRUE ~ 0))
  # Inasistencia a la escuela
  df <- df %>% mutate(inasis_escuela = case_when(asis_esc == 1 ~ 0,
                                                 asis_esc == 2 ~ 1,
                                                 TRUE ~ NA))
  # ¿El miembro cuenta con acceso directo a la salud por medio de la relacion del parentesco?
  df <- df %>% mutate(jefe = case_when(pariente == 1 & smeddirecto == 1 & (((inst_2 == 2 | inst_3 == 3) & inscr_6 == 6) & (is.na(inst_1) & is.na(inst_4) & is.na(inst_6)) & (is.na(inscr_1) & is.na(inscr_2) & is.na(inscr_3) & is.na(inscr_4) & is.na(inscr_5) & is.na(inscr_7))) ~ NA,
                                       pariente == 1 & smeddirecto == 1 ~ 1,
                                       TRUE ~ NA))
    
  df <- as.data.table(df)[, jefe_sa := sum(jefe, na.rm = TRUE), by = idhogar] %>%
    mutate(jefe_sa = ifelse(jefe_sa > 0, 1, jefe_sa))
  # Conyuge
  df <- df %>% mutate(conyuge = case_when(pariente == 2 & smeddirecto == 1 & (((inst_2 == 2 | inst_3 == 3) & inscr_6 == 6) & (is.na(inst_1) & is.na(inst_4) & is.na(inst_6)) & (is.na(inscr_1) & is.na(inscr_2) & is.na(inscr_3) & is.na(inscr_4) & is.na(inscr_5) & is.na(inscr_7))) ~ NA,
                                          pariente == 2 & smeddirecto == 1 ~ 1,
                                          TRUE ~ NA))
  df <- as.data.table(df)[, conyuge_sa := sum(conyuge, na.rm=TRUE), by = idhogar] %>% 
    mutate(conyuge_sa = ifelse(conyuge_sa > 0, 1, conyuge_sa))
  # Hijo
  df <- df %>% mutate(hijo = case_when(pariente == 3 & smeddirecto == 1 & (((inst_2 == 2 | inst_3 == 3) & inscr_6 == 6) & (is.na(inst_1) & is.na(inst_4) & is.na(inst_6)) & (is.na(inscr_1) & is.na(inscr_2) & is.na(inscr_3) & is.na(inscr_4) & is.na(inscr_5) & is.na(inscr_7))) ~ NA,
                                       pariente == 3 & smeddirecto == 1 ~ 1,
                                       TRUE ~ NA))
  df <- as.data.table(df)[, hijo_sa := sum(hijo, na.rm = TRUE), by = idhogar] %>%
    mutate(hijo_sa = ifelse(hijo_sa > 0, 1, hijo_sa))
    assign(base_ic_asalud, df)
}

# Bases Seguro Popular (2016-2028)
bases_ic_asalud_segpop <- c("ic_asalud16","ic_asalud18")
# Bases Insabi (2020-2022)
bases_ic_asalud_insabi <- c("ic_asalud20","ic_asalud22")

# Bucle (Seguro Popular)
for (base_ic_asalud_segpop in bases_ic_asalud_segpop) {
  df = get(base_ic_asalud_segpop)
  df <- df %>% mutate(s_salud = case_when(atemed == 1 & (inst_1 == 1 | inst_2 == 2 | inst_3 == 3 |inst_4 == 4) & (inscr_3 == 3 | inscr_4 == 4 | inscr_6 == 6 | inscr_7 == 7) ~ 1,
                                          !is.na(segpop) & !is.na(atemed) ~ 0,
                                          TRUE ~ NA))
  # Indicador de Carencia de acceso a la salud
  df <- df %>% mutate(ic_asalud = case_when(smeddirecto == 1 ~ 0,
                                            pariente == 1 & conyuge_sa == 1 ~ 0,
                                            pariente == 1 & pea == 0 & hijo_sa == 1 ~ 0,
                                            pariente == 2 & jefe_sa == 1 ~ 0,
                                            pariente == 2 & pea == 0 & hijo_sa == 1 ~ 0,
                                            pariente == 3 & edad < 16 & jefe_sa == 1 ~ 0,
                                            pariente == 3 & edad < 16 & conyuge_sa == 1 ~ 0,
                                            pariente == 3 & between(edad, 16, 25) & inasis_escuela == 0 & jefe_sa == 1 ~ 0,
                                            pariente == 3 & between(edad, 16, 25) & inasis_escuela == 0 & conyuge_sa == 1 ~ 0,
                                            pariente == 4 & pea == 0 & jefe_sa == 1 ~ 0,
                                            pariente == 5 & pea == 0 & conyuge_sa == 1 ~ 0,
                                            s_salud == 1 ~ 0,
                                            segpop == 1 | (segpop == 2 & atemed == 1 & (inst_1 == 1 | inst_2 == 2 |inst_3 == 3 | inst_4 == 4 | inst_5 == 5 | inst_6 == 6)) | segvol_2 == 2 ~ 0,
                                            TRUE ~ 1))
  # Personas con discapacidad
  df <- df %>% mutate(discap = case_when(disc1 == 8 ~ 0,
                                         between(disc1, 1, 7) ~ 1,
                                         between(disc2, 2, 7) ~ 1,
                                         between(disc3, 3, 7) ~ 1,
                                         between(disc4, 4, 7) ~ 1,
                                         between(disc5, 5, 7) ~ 1,
                                         between(disc6, 6, 7) ~ 1,
                                         disc7 == 7 ~ 1,
                                         TRUE ~ NA))
  df <- df %>% select(idhogar,idpersona, sexo, segpop, atemed, starts_with("inst_"),
                     starts_with("inscr_"), pea,edad,act_pnea1,act_pnea2,smedlprinc,smedlsec,
                     smedcontvol,smeddirecto,pariente,inasis_escuela,jefe,jefe_sa,conyuge,conyuge_sa,hijo,hijo_sa,
                     s_salud,ic_asalud,discap)
  assign(base_ic_asalud_segpop, df)
}

table(ic_asalud16$discap)


for (base_ic_asalud_insabi in bases_ic_asalud_insabi) {
  df = get(base_ic_asalud_insabi)
  df <- df %>% mutate(s_salud = case_when(!is.na(pop_insabi) & !is.na(atemed) ~ 0,
                                          atemed == 1 & (inst_1 == 1 | inst_2 == 2 | inst_3 == 3 |inst_4 == 4) & (inscr_3 == 3 | inscr_4 == 4 | inscr_6 == 6 | inscr_7 == 7) ~ 1,
                                          TRUE ~ NA))
  # Indicador de Carencia de acceso a la salud
  df <- df %>% mutate(ic_asalud = case_when(smeddirecto == 1 ~ 0,
                                            pariente == 1 & conyuge == 1 ~ 0,
                                            pariente == 1 & pea == 0 & hijo_sa == 1 ~ 0,
                                            pariente == 2 & jefe_sa == 1 ~ 0,
                                            pariente == 2 & pea == 0 & hijo_sa == 1 ~ 0,
                                            pariente == 3 & edad< 16 & jefe_sa == 1 ~ 0,
                                            pariente == 3 & edad < 16 & conyuge_sa == 1 ~ 0,
                                            pariente == 3 & between(edad, 16, 25) & inasis_escuela == 0 & jefe_sa == 1 ~ 0,
                                            pariente == 3 & between(edad, 16, 25) & inasis_escuela == 0 & conyuge_sa == 1 ~ 0,
                                            pariente == 4 & pea == 0 & jefe_sa == 1 ~ 0,
                                            pariente == 5 & pea == 0 & conyuge_sa == 1 ~ 0,
                                            s_salud == 1 ~ 0,
                                            pop_insabi == 1 | (pop_insabi == 2 & atemed == 1 & (inst_1 == 1 | inst_2 == 2 |inst_3 == 3 | inst_4 == 4 | inst_5 == 5 | inst_6 == 6)) | segvol_2 == 2 ~ 0,
                                            TRUE ~ 1))
  # Personas con discapacidad
  df <- df %>% mutate(discap = case_when(between(disc_camin, 3, 4) ~ 0,
                                         between(disc_ver, 3, 4) ~ 0,
                                         between(disc_brazo, 3, 4) ~ 0,
                                         between(disc_apren, 3, 4) ~ 0,
                                         between(disc_oir, 3, 4) ~ 0,
                                         between(disc_vest, 3, 4) ~ 0,
                                         between(disc_habla, 3, 4) ~ 0,
                                         between(disc_acti, 3, 4) ~ 0,
                                         between(disc_camin, 1, 2) ~ 1,
                                         between(disc_ver, 1, 2) ~ 1,
                                         between(disc_brazo, 1, 2) ~ 1,
                                         between(disc_apren, 1, 2) ~ 1,
                                         between(disc_oir, 1, 2) ~ 1,
                                         between(disc_vest, 1, 2) ~ 1,
                                         between(disc_habla, 1, 2) ~ 1,
                                         between(disc_acti, 1, 2) ~ 1,
                                         TRUE ~ NA))
  df <- df %>% select(idhogar,idpersona, sexo, pop_insabi, atemed, starts_with("isnt_"),
                     starts_with("inscr_"), pea,edad,act_pnea1,act_pnea2,smedlprinc,smedlsec,
                     smedcontvol,smeddirecto,pariente,inasis_escuela,jefe,jefe_sa,conyuge,conyuge_sa,hijo,hijo_sa,
                     s_salud,ic_asalud,discap)
  assign(base_ic_asalud_insabi, df)
}

fwrite(ic_asalud16, "Indicador de Carencia por Acceso a los Servicios de Salud 2016.csv")
fwrite(ic_asalud18, "Indicador de Carencia por Acceso a los Servicios de Salud 2018.csv")
fwrite(ic_asalud20, "Indicador de Carencia por Acceso a los Servicios de Salud 2020.csv")
fwrite(ic_asalud22, "Indicador de Carencia por Acceso a los Servicios de Salud 2022.csv")

#################################################################
## Indicador de Carencia por Calidad y Espacios de la Vivienda ##
#################################################################

# Cargar bases (Vivienda)
vivienda16 <- fread("viviendas_16.csv") %>% rename_all(tolower)
vivienda18 <- fread("viviendas_18.csv") %>% rename_all(tolower)
vivienda20 <- fread("viviendas_20.csv") %>% rename_all(tolower)
vivienda22 <- fread("viviendas_22.csv") %>% rename_all(tolower)
# Cargar bases (Concentrado del hogar)
concentradohogar16 <- fread("concentradohogar_16.csv") %>% rename_all(tolower)
concentradohogar18 <- fread("concentradohogar_18.csv") %>% rename_all(tolower)
concentradohogar20 <- fread("concentradohogar_20.csv") %>% rename_all(tolower)
concentradohogar22 <- fread("concentradohogar_22.csv") %>% rename_all(tolower)

# Unir bases
ic_cev16 <- full_join(vivienda16,concentradohogar16,by="folioviv")
ic_cev18 <- full_join(vivienda18,concentradohogar18,by="folioviv")
ic_cev20 <- full_join(vivienda20,concentradohogar20,by="folioviv")
ic_cev22 <- full_join(vivienda22,concentradohogar22,by="folioviv")

# Lista de bases 
bases_ic_cev <- c("ic_cev16","ic_cev18","ic_cev20","ic_cev22")
# Bucle
for (base_ic_cev in bases_ic_cev) {
  df <- get(base_ic_cev)
  # Identificador del Hogar
  df <- df %>% mutate(idhogar = paste0(folioviv,foliohog))
  # Pisos
  df <- df %>% mutate(mat_pisos = as.numeric(mat_pisos)) 
  df <- df %>% mutate(icv_pisos = case_when(mat_pisos == 1 ~ 1,
                                            between(mat_pisos, 2, 3) ~ 0,
                                            TRUE ~ NA_real_))
  # Techos
  df <- df %>% mutate(icv_techos = case_when(between(mat_techos, 1, 2) ~ 1,
                                             TRUE ~ 0))
  # Muros
  df <- df %>% mutate(icv_muros = case_when(mat_pared < 6 ~ 1,
                                            mat_pared >= 6 ~ 0,
                                            TRUE ~ NA_real_))
  # Indice de hacinamiento
  df <- as.data.table(df)[, indicehacinamiento := tot_resid/num_cuarto]
  df <- df %>% mutate(icv_hac = case_when(indicehacinamiento <= 2.5 ~ 0,
                                          indicehacinamiento > 2.5 ~ 1,
                                          TRUE ~ NA_real_))
  # Indicador de Carencia por Calidad y Espacios de la Vivienda
  df <- df %>% mutate(iccv = case_when(is.na(icv_pisos) | is.na(icv_techos) | is.na(icv_muros) | is.na(icv_hac) ~ NA_real_,
                                       icv_pisos == 1 | icv_techos == 1 | icv_muros == 1 | icv_hac == 1 ~ 1,
                                       icv_pisos == 0 & icv_techos == 0 & icv_muros == 0 & icv_hac == 0 ~ 0,
                                       TRUE ~ NA_real_))
  # Mantener variables
  df <- df %>% select(idhogar,icv_pisos,icv_techos,icv_muros,indicehacinamiento,icv_hac,
                      iccv)

  assign(base_ic_cev, df)
}

# Exportar bases
fwrite(ic_cev16,"Indicador de Carencia por Calidad y Espacios de la Vivienda 2016.csv")
fwrite(ic_cev18,"Indicador de Carencia por Calidad y Espacios de la Vivienda 2018.csv")
fwrite(ic_cev20,"Indicador de Carencia por Calidad y Espacios de la Vivienda 2020.csv")
fwrite(ic_cev22,"Indicador de Carencia por Calidad y Espacios de la Vivienda 2022.csv")

#############################################################################
## Indicador de Carencia por Acceso a los Servicios Básicos en la Vivienda ##
#############################################################################

# Unir bases de vivienda y concentrado del hogar
ic_sbv16 <- full_join(vivienda16,concentradohogar16,by="folioviv")
ic_sbv18 <- full_join(vivienda18,concentradohogar18,by="folioviv")
ic_sbv20 <- full_join(vivienda20,concentradohogar20,by="folioviv")
ic_sbv22 <- full_join(vivienda22,concentradohogar22,by="folioviv")

# Listas de bases
bases_ic_sbv <- c("ic_sbv16","ic_sbv18","ic_sbv20","ic_sbv22")
bases_ic_sbv_procaptar <- c("ic_sbv18","ic_sbv20","ic_sbv22")

# Bucle general
for (base_ic_sbv in bases_ic_sbv) {
  df <- get(base_ic_sbv)
  # Identificador
  df <- df %>% mutate(idhogar = paste0(folioviv,foliohog))
  # Indicador de carencia de disposicón de agua
  df <- df %>% mutate(ic_agua = case_when(disp_agua >= 3 ~ 1,
                                          disp_agua <= 2 ~ 0,
                                          TRUE ~ NA_real_))
  # Indicador de carencia por servicio de drenaje
  df <- df %>% mutate(ic_drenaje = case_when(drenaje <= 2 ~ 0,
                                             drenaje >= 3 ~ 1,
                                             TRUE ~ NA_real_))
  # Indicador de Carencia por servicios de electricidad
  df <- df %>% mutate(ic_electricidad = case_when(disp_elect <= 4 ~ 0,
                                                  disp_elect == 5 ~ 1,
                                                  TRUE ~ NA_real_))
  # Indicador de Carencia por combustible para Cocinar
  df <- df %>% mutate(ic_combustible = case_when(between(combustible,3,6) ~ 0,
                                              between(combustible,1,2) & estufa_chi == 1 ~ 0,
                                              between(combustible,1,2) & estufa_chi == 2 ~ 1,
                                              TRUE ~ NA_real_))
  # Indicador de Carencia por Acceso a Servicios Básicos en la Vivienda
  df <- df %>% mutate(ic_sbv = case_when(ic_agua == 1 | ic_drenaje == 1 | ic_electricidad == 1 | ic_combustible == 1 ~ 1,
                                         ic_agua == 0 & ic_drenaje == 0 & ic_electricidad == 0 & ic_combustible == 0 ~ 0,
                                         is.na(ic_agua) | is.na(ic_drenaje) | is.na(ic_electricidad) | is.na(ic_combustible) ~ NA_real_,
                                         TRUE ~ NA_real_))
  assign(base_ic_sbv, df)
}

# Bucle Programa Procaptar
for (base_ic_sbv_procaptar in bases_ic_sbv_procaptar) {
  df = get(base_ic_sbv_procaptar)
  df <- df %>% mutate(ic_agua = ifelse(procaptar == 1 & disp_agua == 4, 0, ic_agua))
  assign(base_ic_sbv_procaptar, df)
}

# Mantener variables
for (base_ic_sbv in bases_ic_sbv) {
  df = get(base_ic_sbv)
  df <- df %>% select(idhogar, ic_agua, ic_drenaje, ic_electricidad, ic_combustible, ic_sbv)
  assign(base_ic_sbv,df)
}

# Exportar bases
fwrite(ic_sbv16, "Indicador de Carencia por Acceso a Servicios Básicos en la Vivienda 2016.csv")
fwrite(ic_sbv18, "Indicador de Carencia por Acceso a Servicios Básicos en la Vivienda 2018.csv")
fwrite(ic_sbv20, "Indicador de Carencia por Acceso a Servicios Básicos en la Vivienda 2020.csv")
fwrite(ic_sbv22, "Indicador de Carencia por Acceso a Servicios Básicos en la Vivienda 2022.csv")


###############################################################################
## Indicador de Carencia por Acceso a la Alimentación Nutritiva y de Calidad ##
###############################################################################

# Bases menores de edad
menores16 <- fread("poblacion_16.csv") %>% rename_all(tolower)
menores18 <- fread("poblacion_18.csv") %>% rename_all(tolower)
menores20 <- fread("poblacion_20.csv") %>% rename_all(tolower)
menores22 <- fread("poblacion_22.csv") %>% rename_all(tolower)

# Lista de Bases de Menores de Edad
bases_menores <- c("menores16","menores18","menores20","menores22")
# Bucle
for (base_menores in bases_menores) {
  df <- get(base_menores)
  # Filtar por parentesco
  df <- df %>% filter(!(between(parentesco,400,499) | between(parentesco,700,799)))
  # Identificador del Hogar
  df <- df %>% mutate(idhogar = paste0(folioviv,foliohog))
  # Menores
  df <- df %>% mutate(menores = ifelse(between(edad, 0, 17), 1, NA_real_))
  # Cantidad de menores por familia
  df <- df %>% 
    group_by(idhogar) %>%
    summarise(suma_menores = sum(menores,na.rm = TRUE))
  # Identificador de menores
  df <- df %>% mutate(idmenores = case_when(suma_menores >= 1 & !is.na(suma_menores) ~ 1,
                                            suma_menores == 0 ~ 0,
                                            TRUE ~ NA_real_))
  assign(base_menores, df)
}

# Bases Hogares
hogares_menores16 <- fread("hogares_16.csv") %>% rename_all(tolower)
hogares_menores18 <- fread("hogares_18.csv") %>% rename_all(tolower)
hogares_menores20 <- fread("hogares_20.csv") %>% rename_all(tolower)
hogares_menores22 <- fread("hogares_22.csv") %>% rename_all(tolower)

# Lista hogares de los menores
bases_hogares_menores <- c("hogares_menores16","hogares_menores18","hogares_menores20","hogares_menores22")
# Crear el Identificador del Hogar
for (base_hogares_menores in bases_hogares_menores) {
  df <- get(base_hogares_menores)
  # Identificador del Hogar
  df <- df %>% mutate(idhogar = paste0(folioviv,foliohog))
  assign(base_hogares_menores, df)
}

# Unir las bases de menores con las de hogares
ic_ali16 <- full_join(menores16,hogares_menores16,by="idhogar")
ic_ali18 <- full_join(menores18,hogares_menores18,by="idhogar")
ic_ali20 <- full_join(menores20,hogares_menores20,by="idhogar")
ic_ali22 <- full_join(menores22,hogares_menores22,by="idhogar")

# Lista bases ic_ali
bases_ic_ali <- c("ic_ali16","ic_ali18","ic_ali20","ic_ali22")
# Bucle
for (base_ic_ali in bases_ic_ali) {
  df <- get(base_ic_ali)
  # Hogares sin menores
  df <- df %>% mutate(i_alim1ad = ifelse(acc_alim4 == 1, 1, 0))
  df <- df %>% mutate(i_alim2ad = ifelse(acc_alim5 == 1, 1, 0))
  df <- df %>% mutate(i_alim3ad = ifelse(acc_alim6 == 1, 1, 0))
  df <- df %>% mutate(i_alim4ad = ifelse(acc_alim2 == 1, 1, 0))
  df <- df %>% mutate(i_alim5ad = ifelse(acc_alim7 == 1, 1, 0))
  df <- df %>% mutate(i_alim6ad = ifelse(acc_alim8 == 1, 1, 0))
  # Hogares con menores
  df <- df %>% mutate(i_alim7men = ifelse(acc_alim11 == 1, 1, 0))
  df <- df %>% mutate(i_alim8men = ifelse(acc_alim12 == 1, 1, 0))
  df <- df %>% mutate(i_alim9men = ifelse(acc_alim13 == 1, 1, 0))
  df <- df %>% mutate(i_alim10men = ifelse(acc_alim14 == 1, 1, 0))
  df <- df %>% mutate(i_alim11men = ifelse(acc_alim15 == 1, 1, 0))
  df <- df %>% mutate(i_alim12men = ifelse(acc_alim16 == 1, 1, 0))
  # Escala de Hogares sin menores
  df <- df %>% mutate(tot_ia1 = ifelse(idmenores == 0, rowSums(select(., i_alim1ad,i_alim2ad,i_alim3ad,i_alim4ad,i_alim5ad,i_alim6ad), na.rm = TRUE), NA_real_))
  # Escala de hogares con menores
  df <- df %>% mutate(tot_ia2 = ifelse(idmenores == 1, rowSums(select(., i_alim1ad,i_alim2ad,i_alim3ad,i_alim4ad,i_alim5ad,i_alim6ad,i_alim7men,i_alim8men,i_alim9men,i_alim10men,i_alim11men,i_alim12men), na.rm = TRUE), NA_real_))
  # Inseguridad Alimentaria
  df <- df %>% mutate(i_alimentaria = case_when(tot_ia1 == 0 | tot_ia2 == 0 ~ 0,
                                                between(tot_ia1,1,2) | between(tot_ia2,1,3) ~ 1,
                                                between(tot_ia1,3,4) | between(tot_ia2,4,7) ~ 2,
                                                between(tot_ia1,5,6) | between(tot_ia2,8,12) & !is.na(tot_ia2) ~ 3,
                                                TRUE ~ NA_real_))
  # Indice de Carencia Alimentaria
  df <- df %>% mutate(ic_alimentaria = case_when(between(i_alimentaria,2,3) ~ 1,
                                                 between(i_alimentaria,0,1) ~ 0,
                                                 TRUE ~ NA_real_))
  # Consumo de alimentos ponderados
  df <- df %>% mutate(cpond1 = pmax(alim17_1,alim17_2))
  df <- df %>% mutate(cpond1 = cpond1 * 2)
  df <- df %>% mutate(cpond3 = alim17_3 * 1)
  df <- df %>% mutate(cpond4 = alim17_4 * 1)
  df <- df %>% mutate(cpond5 = pmax(alim17_5,alim17_6,alim17_7))
  df <- df %>% mutate(cpond5 = cpond5 * 4)
  df <- df %>% mutate(cpond8 = alim17_8 * 3)
  df <- df %>% mutate(cpond9 = alim17_9 * 4)
  df <- df %>% mutate(cpond10 = alim17_10 * 0.5)
  df <- df %>% mutate(cpond11 = alim17_11 * 0.5)
  df <- df %>% mutate(cpond12 = alim17_12 * 0)
  # Suma ponderada
  df <- df %>% mutate(tot_cpond = rowSums(select(., cpond1,cpond3,cpond4,cpond5,
                                                 cpond8,cpond9,cpond10,cpond11,cpond12), na.rm=TRUE))
  # Dieta Consumida por Hogares
  df <- df %>% mutate(dch = case_when(between(tot_cpond,0,28) ~ 1,
                                      tot_cpond > 28 & tot_cpond <= 42 ~ 2,
                                      tot_cpond > 42 & !is.na(tot_cpond) ~ 3,
                                      TRUE ~ NA))
  # Limitacion en el consumo de alimentos
  df <- df %>% mutate(lca = ifelse(dch <= 2, 1, 0))
  # Indicador de Carencia por Acceso a la Alimentación Nutritica y de Calidad
  df <- df %>% mutate(ic_ali_nc = case_when(lca == 1 | ic_alimentaria == 1 & !is.na(lca) & !is.na(ic_alimentaria) ~ 1,
                                            lca == 0 & ic_alimentaria == 0 ~ 0,
                                            TRUE ~ NA_real_))
  # Mantener variables importantes
  df <- df %>% select(idhogar,suma_menores,idmenores,starts_with("i_alim"),starts_with("tot_ia"),
                      i_alimentaria,ic_alimentaria,starts_with("cpond"),tot_cpond,dch,lca,ic_ali_nc)
  assign(base_ic_ali, df)
}

table(ic_ali16$ic_ali_nc)



# Exportar Bases
fwrite(ic_ali16,"Indicador de Carencia por Acceso a la Alimentación Nutritiva y de Calidad 2016.csv")
fwrite(ic_ali18,"Indicador de Carencia por Acceso a la Alimentación Nutritiva y de Calidad 2018.csv")
fwrite(ic_ali20,"Indicador de Carencia por Acceso a la Alimentación Nutritiva y de Calidad 2020.csv")
fwrite(ic_ali22,"Indicador de Carencia por Acceso a la Alimentación Nutritiva y de Calidad 2022.csv")


############################################################
## Indicador de Carencia por Acceso a la Seguridad Social ##
############################################################

# Cargar bases de trabajo
prestaciones16 <- fread("trabajos_16.csv") %>% rename_all(tolower)
prestaciones18 <- fread("trabajos_18.csv") %>% rename_all(tolower)
prestaciones20 <- fread("trabajos_20.csv") %>% rename_all(tolower)
prestaciones22 <- fread("trabajos_22.csv") %>% rename_all(tolower)

# Lista prestaciones
bases_prestaciones <- c("prestaciones16","prestaciones18","prestaciones20","prestaciones22")
# Bucle
for (base_prestaciones in bases_prestaciones) {
  df <- get(base_prestaciones)
  # Tipo de trabajo
  df <- df %>% mutate(tipo_trabajo = case_when(subor == 1 ~ 1,
                                               subor == 2 & indep == 1 & tiene_suel == 1 ~ 2,
                                               subor == 2 & indep == 2 & pago == 1 ~ 2,
                                               subor == 2 & indep == 1 & tiene_suel == 2 ~ 3,
                                               subor == 2 & indep == 2 & between(pago,2,3) ~ 3,
                                               TRUE ~ NA_real_))
  # Identificador del trabajo
  df <- df %>% mutate(ocupa = case_when(id_trabajo == 2 ~ 0,
                                        id_trabajo == 1 ~ 1,
                                        TRUE ~ NA_real_))
  if (base_prestaciones == "prestaciones16") {
    df <- df %>% mutate(aforelaboral = case_when(is.na(pres_14) ~ 0,
                                                 pres_14 == 14 ~ 1,
                                                 TRUE ~ NA_real_))
  } else {
    df <- df %>% mutate(aforelaboral = case_when(is.na(pres_8) ~ 0,
                                                 pres_8 == 8 ~ 1,
                                                 TRUE ~ NA_real_))
  }
  # Mantener Variables
  df <- df %>% select(folioviv,foliohog,numren,id_trabajo,tipo_trabajo,aforelaboral,ocupa)
  # Reformatear la tabla
  df <- df %>% 
    pivot_wider(id_cols = c(folioviv,foliohog,numren),names_from = id_trabajo,values_from = c(tipo_trabajo,aforelaboral,ocupa))
  # Ocupa 2 
  df <- df %>% mutate(ocupa_2 = ifelse(ocupa_2 == 0, 1, 0))
  # Población Trabajadora
  df <- df %>% mutate(trabajo = 1)
  # Identificador de la persona
  df <- df %>% mutate(idpersona = paste0(folioviv,foliohog,numren))
  assign(base_prestaciones, df)
}

# Creación de las bases de pensiones
pensiones16 <- fread("ingresos_16.csv") %>% rename_all(tolower)
pensiones18 <- fread("ingresos_18.csv") %>% rename_all(tolower)
pensiones20 <- fread("ingresos_20.csv") %>% rename_all(tolower)
pensiones22 <- fread("ingresos_22.csv") %>% rename_all(tolower)

# Años
pensiones16[, año := 2016]
pensiones18[, año := 2018]
pensiones20[, año := 2020]
pensiones22[, año := 2022]

# Deflactor
deflactor_pensiones <- fread("deflactores_pensiones.csv") %>% rename_all(tolower)
# Mes 1 
deflactor_pensiones_mes1 <- deflactor_pensiones %>% rename(mes_1 = mes,
                                                           deflactor_mes1 = deflactor)
# Mes_2
deflactor_pensiones_mes2 <- deflactor_pensiones %>% rename(mes_2 = mes,
                                                           deflactor_mes2 = deflactor)
# Mes 3
deflactor_pensiones_mes3 <- deflactor_pensiones %>% rename(mes_3 = mes,
                                                           deflactor_mes3 = deflactor)
# Mes 4
deflactor_pensiones_mes4 <- deflactor_pensiones %>% rename(mes_4 = mes,
                                                           deflactor_mes4 = deflactor)
# Mes 5
deflactor_pensiones_mes5 <- deflactor_pensiones %>% rename(mes_5 = mes,
                                                           deflactor_mes5 = deflactor)
# Mes 6
deflactor_pensiones_mes6 <- deflactor_pensiones %>% rename(mes_6 = mes,
                                                           deflactor_mes6 = deflactor)

# Unir deflactor con las bases de pensiones
# 2016
pensiones16 <- left_join(pensiones16,deflactor_pensiones_mes1,by = c("mes_1","año"))
pensiones16 <- left_join(pensiones16,deflactor_pensiones_mes2,by = c("mes_2","año"))
pensiones16 <- left_join(pensiones16,deflactor_pensiones_mes3,by = c("mes_3","año"))
pensiones16 <- left_join(pensiones16,deflactor_pensiones_mes4,by = c("mes_4","año"))
pensiones16 <- left_join(pensiones16,deflactor_pensiones_mes5,by = c("mes_5","año"))
pensiones16 <- left_join(pensiones16,deflactor_pensiones_mes6,by = c("mes_6","año"))
# 2018
pensiones18 <- left_join(pensiones18,deflactor_pensiones_mes1,by = c("mes_1","año"))
pensiones18 <- left_join(pensiones18,deflactor_pensiones_mes2,by = c("mes_2","año"))
pensiones18 <- left_join(pensiones18,deflactor_pensiones_mes3,by = c("mes_3","año"))
pensiones18 <- left_join(pensiones18,deflactor_pensiones_mes4,by = c("mes_4","año"))
pensiones18 <- left_join(pensiones18,deflactor_pensiones_mes5,by = c("mes_5","año"))
pensiones18 <- left_join(pensiones18,deflactor_pensiones_mes6,by = c("mes_6","año"))
# 2020
pensiones20 <- left_join(pensiones20,deflactor_pensiones_mes1,by = c("mes_1","año"))
pensiones20 <- left_join(pensiones20,deflactor_pensiones_mes2,by = c("mes_2","año"))
pensiones20 <- left_join(pensiones20,deflactor_pensiones_mes3,by = c("mes_3","año"))
pensiones20 <- left_join(pensiones20,deflactor_pensiones_mes4,by = c("mes_4","año"))
pensiones20 <- left_join(pensiones20,deflactor_pensiones_mes5,by = c("mes_5","año"))
pensiones20 <- left_join(pensiones20,deflactor_pensiones_mes6,by = c("mes_6","año"))
# 2022
pensiones22 <- left_join(pensiones22,deflactor_pensiones_mes1,by = c("mes_1","año"))
pensiones22 <- left_join(pensiones22,deflactor_pensiones_mes2,by = c("mes_2","año"))
pensiones22 <- left_join(pensiones22,deflactor_pensiones_mes3,by = c("mes_3","año"))
pensiones22 <- left_join(pensiones22,deflactor_pensiones_mes4,by = c("mes_4","año"))
pensiones22 <- left_join(pensiones22,deflactor_pensiones_mes5,by = c("mes_5","año"))
pensiones22 <- left_join(pensiones22,deflactor_pensiones_mes6,by = c("mes_6","año"))

# Lista bases pensiones
bases_pensiones <- c("pensiones16","pensiones18","pensiones20","pensiones22")
# Bucle
for (base_pensiones in bases_pensiones) {
  df <- get(base_pensiones)
  # Filtrar
  if (base_pensiones %in% c("pensiones16","pensiones18")) {
    df <- df %>% filter(clave %in% c("P032","P033","P044","P045"))
  } else {
    df <- df %>% filter(clave %in% c("P032","P033","P104","P045"))
  }
  # Identificador de Persona
  df <- df %>% mutate(idpersona = paste0(folioviv,foliohog,numren))
  # Ingresos deflactado (1-6)
  df <- df %>% mutate(ingresod1 = (ing_1/deflactor_mes1)*100,
                      ingresod2 = (ing_2/deflactor_mes2)*100,
                      ingresod3 = (ing_3/deflactor_mes3)*100,
                      ingresod4 = (ing_4/deflactor_mes4)*100,
                      ingresod5 = (ing_5/deflactor_mes5)*100,
                      ingresod6 = (ing_6/deflactor_mes6)*100)
  # Ingreso a través de las Pensiones
  df <- df %>% mutate(ingreso_pensiones = ifelse(clave %in% c("P032","P033"), rowMeans(select(., ingresod1,
                                                                                              ingresod2,ingresod3,ingresod4,ingresod5,ingresod6), na.rm=TRUE), NA_real_),
                      ingreso_pensiones = ifelse(is.na(ingreso_pensiones), 0, ingreso_pensiones))
  # Ingreso PAM
  df <- df %>% mutate(ingreso_pam = ifelse(clave %in% c("P044","P045","P104"), rowMeans(select(., ingresod1,
                                                                                               ingresod2,ingresod3,ingresod4,ingresod5,ingresod6), na.rm = TRUE), NA_real_),
                      ingreso_pam = ifelse(is.na(ingreso_pam), 0, ingreso_pam))
  # Transformar las bases
  df <- df %>% 
    group_by(idpersona) %>% 
    summarise(suma_ingreso_pensiones = sum(ingreso_pensiones, na.rm=TRUE),
              suma_ingreso_pam = sum(ingreso_pam, na.rm = TRUE))
  assign(base_pensiones, df)
}


# Bases Seguridad social
poblacion_seguridadsocial16 <- fread("poblacion_16.csv") %>% rename_all(tolower)
poblacion_seguridadsocial18 <- fread("poblacion_18.csv") %>% rename_all(tolower)
poblacion_seguridadsocial20 <- fread("poblacion_20.csv") %>% rename_all(tolower)
poblacion_seguridadsocial22 <- fread("poblacion_22.csv") %>% rename_all(tolower)

# Lista seguridad social 
bases_seguridadsocial <- c("poblacion_seguridadsocial16","poblacion_seguridadsocial18","poblacion_seguridadsocial20","poblacion_seguridadsocial22")
# Bucle
for (base_seguridadsocial in bases_seguridadsocial) {
  df <- get(base_seguridadsocial)
  # Mantener parentesco
  df <- df %>% filter(!(between(parentesco,400,499) | between(parentesco,700,799)))
  # Identificador de la persona
  df <- df %>% mutate(idpersona = paste0(folioviv,foliohog,numren))
  assign(base_seguridadsocial, df)
}

# Lineas de pobreza extrema (rural y urbana)
linea_pobreza_extrema <- data.frame(
  "año" = c(2016,2018,2020,2022),
  "lpobrezaeurbana" = c(1348.81,1521.44,1688.57,2042.89),
  "lpobrezaerural" = c(1015.44,1145.5,1287.59,1566.95)
)

# Unir las bases de prestaciones y pensiones
# 2016
ic_segsoc16 <- full_join(prestaciones16,pensiones16,by="idpersona")
ic_segsoc16 <- full_join(ic_segsoc16,poblacion_seguridadsocial16,by="idpersona")
# 2018
ic_segsoc18 <- full_join(prestaciones18,pensiones18,by="idpersona")
ic_segsoc18 <- full_join(ic_segsoc18,poblacion_seguridadsocial18,by="idpersona")
# 2020
ic_segsoc20 <- full_join(prestaciones20,pensiones20,by="idpersona")
ic_segsoc20 <- full_join(ic_segsoc20,poblacion_seguridadsocial20,by="idpersona")
# 2022
ic_segsoc22 <- full_join(prestaciones22,pensiones22,by="idpersona")
ic_segsoc22 <- full_join(ic_segsoc22,poblacion_seguridadsocial22,by="idpersona")

# Año
ic_segsoc16$año <- 2016
ic_segsoc18$año <- 2018
ic_segsoc20$año <- 2020
ic_segsoc22$año <- 2022

# Unir ic_segsoc con la base de lineas de pobreza
ic_segsoc16 <- left_join(ic_segsoc16,linea_pobreza_extrema,by="año")
ic_segsoc18 <- left_join(ic_segsoc18,linea_pobreza_extrema,by="año")
ic_segsoc20 <- left_join(ic_segsoc20,linea_pobreza_extrema,by="año")
ic_segsoc22 <- left_join(ic_segsoc22,linea_pobreza_extrema,by="año")

# Lista ic_segsoc
bases_ic_segsoc <- c("ic_segsoc16","ic_segsoc18","ic_segsoc20","ic_segsoc22")
# Bucle
for (base_ic_segsoc in bases_ic_segsoc) {
  df <- get(base_ic_segsoc)
  # Otros Nucleos Familiares<
  if (base_ic_segsoc %in% c("ic_segsoc16","ic_segsoc18")) {
    df <- df %>% mutate(s_salud = case_when(atemed == 1 & (inst_1 == 1 | inst_2 == 2 | inst_3 == 3 | inst_4 == 4) & (inscr_3 == 3 | inscr_4 == 4 | inscr_6 == 6 | inscr_7 == 7) ~ 1,
                                            !is.na(segpop) & !is.na(atemed) ~ 0,
                                            TRUE ~ NA_real_))
  } else {
    df <- df %>% mutate(s_salud = case_when(atemed == 1 & (inst_1 == 1 | inst_2 == 2 | inst_3 == 3 | inst_4 == 4) & (inscr_3 == 3 | inscr_4 == 4 | inscr_6 == 6 | inscr_7 == 7) ~ 1,
                                            !is.na(pop_insabi) & !is.na(atemed) ~ 0,
                                            TRUE ~ NA_real_))
  }
  # linea de pobreza por programa de adultos mayores
  df <- df %>% mutate(lpobreza_pam = (lpobrezaeurbana + lpobrezaerural)/2)
  # Programa de Adultos Mayores
  df <- df %>% mutate(pam = case_when(edad >= 65 & !is.na(edad) ~ 0,
                                      (edad >= 65 & !is.na(edad)) & suma_ingreso_pam >= lpobreza_pam & !is.na(suma_ingreso_pam) ~ 1,
                                      TRUE ~ NA))
  # Identificador del hogar
  df <- df %>% mutate(idhogar = paste0(folioviv.x,foliohog.x,numren.x))
  # Población Económicamente actuva
  df <- df %>% mutate(pea = case_when(trabajo == 1 & (edad >= 16 & !is.na(edad)) ~ 1,
                                      (act_pnea1 == 1 | act_pnea2 == 1) & (edad >= 16 & !is.na(edad)) ~ 2,
                                      (edad >= 16 & !is.na(edad)) & ((act_pnea1 != 1 | is.na(act_pnea1)) & (act_pnea2 != 1 | is.na(act_pnea2))) & ((act_pnea1 >= 2 & act_pnea1 <= 6) | (act_pnea2 >= 2 & act_pnea2 <= 6)) ~ 0))
  # Tipo de Trabajo 1
  df <- df %>% mutate(tipo_trabajo_1 = case_when(pea == 1 ~ tipo_trabajo_1,
                                                 pea %in% c(0,2) ~ NA,
                                                 is.na(pea) ~ NA,
                                                 TRUE ~ tipo_trabajo_1))
  # Tipo de Trabajo 2
  df <- df %>% mutate(tipo_trabajo_2 = case_when(pea == 1 ~ tipo_trabajo_2,
                                                 pea %in% c(0,2) ~ NA,
                                                 is.na(pea) ~ NA,
                                                 TRUE ~ tipo_trabajo_2))
  # Jubilados
  df <- df %>% mutate(jubilados = case_when(trabajo_mp == 2 & (act_pnea1 == 2 | act_pnea2 == 2) ~ 1,
                                            suma_ingreso_pensiones > 0 & !is.na(suma_ingreso_pensiones) ~ 1,
                                            inscr_2 == 2 ~ 1,
                                            TRUE ~ 0))
  # Servicios Médicos por Prestación Laboral
  # Como ocupación principal
  df <- df %>% mutate(smedlprinc = case_when(ocupa_1 == 1 & atemed == 1 & (inst_1 == 1 | inst_2 == 2 | inst_3 == 3 | inst_4 == 4) & inscr_1 == 1 ~ 1,
                                             ocupa_1 == 1 ~ 0,
                                             TRUE ~ NA))
  # Como ocupación secundaria
  df <- df %>% mutate(smedlsec = case_when(ocupa_2 == 1 & atemed == 1 & (inst_1 == 1 | inst_2 == 2 |inst_3 == 3 | inst_4 == 4) & inscr_1 == 1 ~ 1,
                                           ocupa_2 == 1 ~ 0,
                                           TRUE ~ NA))
  # Servicios médicos de contratacion voluntaria
  df <- df %>% mutate(smedcontvol = case_when(atemed == 1 & (inst_1 == 1 | inst_2 == 2 | inst_3 == 3 |inst_4 == 4) & inscr_6 == 6 & (edad >= 12 & !is.na(edad)) ~ 1,
                                              edad >= 12 & !is.na(edad) ~ 0,
                                              TRUE ~ NA))
  # Ahorro para el retiro o pension para la vejez (SAR o Afore)
  df <- df %>% mutate(aforecv = case_when(is.na(segvol_1) & (edad >= 12 & !is.na(edad)) ~ 0,
                                          segvol_1 == 1 & (edad >= 12 & !is.na(edad)) ~ 1,
                                          TRUE ~ NA_real_))
  # Acceso directo a la seguridad social
  df <- df %>% mutate(ss_directo = case_when(tipo_trabajo_1 == 1 & smedlprinc == 1 ~ 1,
                                             tipo_trabajo_1 == 2 & ((smedlprinc == 1 | smedcontvol == 1) & (aforelaboral_1 == 1 | aforecv == 1)) ~ 1,
                                             tipo_trabajo_1 == 3 & ((smedlprinc == 1 | smedcontvol == 1) & aforecv == 1) ~ 1,
                                             tipo_trabajo_2 == 1 & smedlsec == 1 ~ 1,
                                             tipo_trabajo_2 == 2 & ((smedlsec == 1 | smedcontvol == 1) & (aforelaboral_2 == 1 | aforecv == 1)) ~ 1,
                                             tipo_trabajo_2 == 3 & ((smedlsec == 1 | smedcontvol == 1) & aforecv == 1) ~ 1,
                                             jubilados == 1 ~ 1,
                                             TRUE ~ 0))
  # Pariente
  df <- df %>% mutate(pariente = case_when(between(parentesco,100,199) ~ 1,
                                           between(parentesco,200,299) ~ 2,
                                           between(parentesco,300,399) ~ 3,
                                           parentesco == 601 ~ 4,
                                           parentesco == 615 ~ 5,
                                           TRUE ~ 0))
  # Inasistencia a la escuela
  df <- df %>% mutate(inasis_escuela = case_when(asis_esc == 1 ~ 0,
                                                 asis_esc == 2 ~ 1,
                                                 TRUE ~ NA))
  # ¿El miembro cuenta con acceso directo a la salud por medio de la relacion del parentesco?
  df <- df %>% mutate(jefe = case_when(pariente == 1 & ss_directo == 1 & (((inst_2 == 2 | inst_3 == 3) & inscr_6 == 6) & (is.na(inst_1) & is.na(inst_4) & is.na(inst_6)) & (is.na(inscr_1) & is.na(inscr_2) & is.na(inscr_3) & is.na(inscr_4) & is.na(inscr_5) & is.na(inscr_7))) ~ NA,
                                       pariente == 1 & ss_directo == 1 ~ 1,
                                       TRUE ~ NA))
  
  df <- as.data.table(df)[, jefe_ss := sum(jefe, na.rm = TRUE), by = idhogar] %>%
    mutate(jefe_ss = ifelse(jefe_ss > 0, 1, jefe_ss))
  # Conyuge
  df <- df %>% mutate(conyuge = case_when(pariente == 2 & ss_directo == 1 & (((inst_2 == 2 | inst_3 == 3) & inscr_6 == 6) & (is.na(inst_1) & is.na(inst_4) & is.na(inst_6)) & (is.na(inscr_1) & is.na(inscr_2) & is.na(inscr_3) & is.na(inscr_4) & is.na(inscr_5) & is.na(inscr_7))) ~ NA,
                                          pariente == 2 & ss_directo == 1 ~ 1,
                                          TRUE ~ NA))
  df <- as.data.table(df)[, conyuge_ss := sum(conyuge, na.rm=TRUE), by = idhogar] %>% 
    mutate(conyuge_ss = ifelse(conyuge_ss > 0, 1, conyuge_ss))
  # Hijo
  df <- df %>% mutate(hijo = case_when(pariente == 3 & ss_directo == 1 & (((inst_2 == 2 | inst_3 == 3) & inscr_6 == 6) & (is.na(inst_1) & is.na(inst_4) & is.na(inst_6)) & (is.na(inscr_1) & is.na(inscr_2) & is.na(inscr_3) & is.na(inscr_4) & is.na(inscr_5) & is.na(inscr_7))) ~ NA,
                                       pariente == 3 & ss_directo == 1 ~ 1,
                                       TRUE ~ NA))
  df <- as.data.table(df)[, hijo_ss := sum(hijo, na.rm = TRUE), by = idhogar] %>%
    mutate(hijo_ss = ifelse(hijo_ss > 0, 1, hijo_ss))
  # Indicador de Carencia por Acceso a la Seguridad Social
  df <- df %>% mutate(ic_seguridadsocial = case_when(ss_directo == 1 ~ 0,
                                                     pariente == 1 & conyuge_ss == 1 ~ 0,
                                                     pariente == 1 & pea == 0 & hijo_ss == 1 ~ 0,
                                                     pariente == 2 & jefe_ss == 1 ~ 1,
                                                     pariente == 2 & pea == 0 & hijo_ss == 1 ~ 0,
                                                     pariente == 3 & edad < 16 & jefe_ss == 1 ~ 0,
                                                     pariente == 3 & edad < 16 & conyuge_ss == 1 ~ 0,
                                                     pariente == 3 & between(edad, 16,25) & inasis_escuela == 0 & jefe_ss == 1 ~ 0,
                                                     pariente == 3 & between(edad, 16,25) & inasis_escuela == 0 & conyuge_ss == 1 ~ 0,
                                                     pariente == 4 & pea == 0 & jefe_ss == 1 ~ 0,
                                                     pariente == 5 & pea == 0 & conyuge_ss == 1 ~ 0,
                                                     s_salud == 1 ~ 0,
                                                     pam == 1 ~ 0,
                                                     TRUE ~ 1))
  # Mantener variables
  df <- df %>% select(tipo_trabajo_1,tipo_trabajo_2,aforelaboral_1,aforelaboral_2,ocupa_1,ocupa_2,trabajo,idpersona,sexo,
                      edad,parentesco,act_pnea1,act_pnea2,idhogar,año,s_salud,pam,pea,jubilados,smedlprinc,smedlsec,smedcontvol,
                      aforecv,ss_directo,pariente,inasis_escuela,jefe,jefe_ss,conyuge,conyuge_ss,hijo,hijo_ss,ic_seguridadsocial)
  assign(base_ic_segsoc, df)
}

# Exportar bases
fwrite(ic_segsoc16, "Indicador de Carencia por Acceso a la Seguridad Social 2016.csv")
fwrite(ic_segsoc18, "Indicador de Carencia por Acceso a la Seguridad Social 2018.csv")
fwrite(ic_segsoc20, "Indicador de Carencia por Acceso a la Seguridad Social 2020.csv")
fwrite(ic_segsoc22, "Indicador de Carencia por Acceso a la Seguridad Social 2022.csv")









