library(tidyverse)
library(openxlsx)
library(readr)
library(tidyr)
library(data.table)


# Directorio
setwd("C:/Users/leca_/OneDrive/Documentos/Codiguito/Alkire-Foster Coneval 2016-2022/R")

# Estimación de Pobreza Multidimensional a través del Método de Alkyre-Foster (2016-2022)
# Indicador de Rezago Educativo
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
fwrite(poblacion16, "Indicador de Carencia de Rezago Educativo 2016.csv")
fwrite(poblacion18, "Indicador de Carencia de Rezago Educativo 2018.csv")
fwrite(ic_rezedu20, "Indicador de Carencia de Rezago Educativo 2020.csv")
fwrite(ic_rezedu22, "Indicador de Carencia de Rezago Educativo 2022.csv")


## Indicador de Carencia por Acceso a los Servicios de Salud ##
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
  
  
  
  
  
  
  
    assign(base_ic_asalud, df)
}

table(ic_asalud16$pea)



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
df <- df %>% mutate(smedcontvol = case_when(edad >= 12 & !is.na(edad) ~ 0,
                                            atemed == 1 & (inst_1 == 1 | inst_2 == 2 | inst_3 == 3 |inst_4 == 4) & inscr_6 == 6 & (edad >= 12 & !is.na(edad)) ~ 1,
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
df <- df %>% 
  group_by("idhogar") %>% 
  mutate(jefe_sa = sum(jefe,na.rm = TRUE)) %>%
  ungroup()
# Conyuge
df <- df %>% mutate(conyuge = case_when(pariente == 2 & smeddirecto == 1 & (((inst_2 == 2 | inst_3 == 3) & inscr_6 == 6) & (is.na(inst_1) & is.na(inst_4) & is.na(inst_6)) & (is.na(inscr_1) & is.na(inscr_2) & is.na(inscr_3) & is.na(inscr_4) & is.na(inscr_5) & is.na(inscr_7))) ~ NA,
                                        pariente == 2 & smeddirecto == 1 ~ 1,
                                        TRUE ~ NA))
df <- df %>% 
  group_by("idhogar") %>%
  mutate(conyuge_sa = sum(conyuge,na.rm = TRUE)) %>%
  ungroup()
# Hijo
df <- df %>% mutate(hijo = case_when(pariente == 3 & smeddirecto == 1 & (((inst_2 == 2 | inst_3 == 3) & inscr_6 == 6) & (is.na(inst_1) & is.na(inst_4) & is.na(inst_6)) & (is.na(inscr_1) & is.na(inscr_2) & is.na(inscr_3) & is.na(inscr_4) & is.na(inscr_5) & is.na(inscr_7))) ~ NA,
                                     pariente == 3 & smeddirecto == 1 ~ 1,
                                     TRUE ~ NA))
df <- df %>% 
  group_by("idhogar") %>%
  mutate(hijo_sa = sum(hijo,na.rm = TRUE)) %>%
  ungroup()
df <- df %>% mutate(hijo_sa = case_when(hijo_sa >= 1 & !is.na(hijo_sa) ~ 1,
                                        TRUE ~ hijo_sa))  



table(ic_asalud16$pea)

sum(is.na(ic_asalud16$act_pnea1))

ic_asalud16$pea = NA_integer_

aver <- ic_asalud16 %>% 
  filter(!is.na(edad), edad >= 16,
         act_pnea1 != 1, act_pnea2 != 1,
         (act_pnea1 >= 2 & act_pnea1 <= 6) | act_pnea2 == 6,
         is.na(pea)) %>%
  nrow()








# Bases Seguro Popular (2016-2028)
bases_ic_asalud_segpop <- c("ic_asalud16","ic_asalud18")
# Bases Insabi (2020-2022)
bases_ic_asalud_insabi <- c("ic_asalud20","ic_asalud22")

# Bucle (Seguro Popular)
for (base_ic_asalud_segpop in bases_ic_asalud_segpop) {
  df = get(base_ic_asalud_segpop)
  df <- df %>% mutate(s_salud = case_when(!is.na(segpop) & !is.na(atemed) ~ 0,
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
                                            seg_pop == 1 | (seg_pop == 2 & atemed == 1 & (inst_1 == 1 | inst_2 == 2 |inst_3 == 3 | inst_4 == 4 | inst_5 == 5 | inst_6 == 6)) | segvol_2 == 2 ~ 0,
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
  assign(base_ic_asalud_segpop, df)
}

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
                                         between(dis_oir, 3, 4) ~ 0,
                                         between(disc_vest, 3, 4) ~ 0,
                                         between(disc_habla, 3, 4) ~ 0,
                                         between(disc_acti, 3, 4) ~ 0,
                                         between(disc_camin, 1, 2) ~ 1,
                                         between(disc_ver, 1, 2) ~ 1,
                                         between(disc_brazo, 1, 2) ~ 1,
                                         between(disc_apren, 1, 2) ~ 1,
                                         between(dis_oir, 1, 2) ~ 1,
                                         between(disc_vest, 1, 2) ~ 1,
                                         between(disc_habla, 1, 2) ~ 1,
                                         between(disc_acti, 1, 2) ~ 1,
                                         TRUE ~ NA))
  assign(base_ic_asalud_insabi, df)
}

 








typeof(ic_asalud16$inscr_1)





