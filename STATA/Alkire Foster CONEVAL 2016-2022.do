*Método de Alkire-Foster con los indicadores de CONEVAL 2016-2022
*Este do-file está programado para que corra de una al momento de presionar el botón de ejecutar. En caso de comprobar, solamente se cambia el directorio y se colocan en este los siguientes bases de datos de la ENIGH:
*concentradohogar*
*gastoshogar*
*gastospersona*
*Hogares*
*ingresos*
*poblacion*
*trabajos*
*viviendas*


*Directorio*
cd "C:\Users\leca_\OneDrive\Documentos\Asistente de Investigación Maestría Economia\Bases"
*Introducir año para cada variable
*2016*
use poblacion16.dta,clear
gen año=2016
label var año "Año de la encuesta"
save ic_rezedu16.dta,replace
*2018*
use poblacion18.dta,clear
gen año=2018
label var año "Año de la encuesta"
save ic_rezedu18.dta,replace
*2020*
use poblacion20.dta,clear
gen año=2020
label var año "Año de la encuesta"
save ic_rezedu20.dta,replace
*2022
use poblacion22.dta,clear
gen año=2022
label var año "Año de la encuesta"
save ic_rezedu22.dta,replace

**********************************************
******Indicador de Rezago Educativo***********
**********************************************
set more off



local flist ic_rezedu16.dta ic_rezedu18.dta ic_rezedu20.dta ic_rezedu22.dta

foreach file in `flist' {
        use `file'		
rename *, lower
egen idhogar=concat(folioviv foliohog)
egen idpersona=concat(folioviv foliohog numren)
label var idhogar "Identificador del hogar"
label var idpersona "Identificador de la persona"
*delimitar por poblacion objetivo
drop if parentesco>="400" & parentesco<"500"
drop if parentesco>="700" & parentesco<"800"
*Año de Nacimiento
gen año_nacimiento=año-edad if edad!=.
label var edad "Edad reportada al momento de la entrevista"
label var año_nacimiento "Año de nacimiento"
*Inasistencia a la escuela
gen inasis_escuela=.
replace inasis_escuela=0 if asis_esc=="1"
replace inasis_escuela=1 if asis_esc=="2"
label var inasis_escuela "Inasistencia a la escuela"
label define inasis_escuela 0 "Sí asiste" 1 "No asiste"
label values inasis_escuela inasis_escuela
*Nivel educativo
gen niveledu=.
replace niveledu=0 if nivelaprob=="0" |nivelaprob=="1" |(nivelaprob=="2" & gradoaprob<"6")
replace niveledu=1 if (nivelaprob=="2" & gradoaprob=="6") |((nivelaprob=="5" | nivelaprob=="6") & antec_esc=="1" & gradoaprob<"3") |(nivelaprob=="3" & gradoaprob<"3")
replace niveledu=2 if nivelaprob=="3" & gradoaprob=="3" 
replace niveledu=2 if nivelaprob=="4" & gradoaprob<"3"
replace niveledu=2 if nivelaprob=="5" & antec_esc=="1" & gradoaprob>="3"
replace niveledu=2 if nivelaprob=="5" & antec_esc=="2" & gradoaprob<"3"
replace niveledu=2 if nivelaprob=="6" & antec_esc=="1" & gradoaprob>="3"
replace niveledu=2 if nivelaprob=="6" & antec_esc=="2" & gradoaprob<"3"
replace niveledu=3 if nivelaprob=="4" & gradoaprob>="3"
replace niveledu=3 if nivelaprob=="5" & antec_esc=="2" & gradoaprob>="3"
replace niveledu=3 if nivelaprob=="5" & antec_esc>="3" & antec_esc!="."
replace niveledu=3 if nivelaprob=="6" & antec_esc>="3" & antec_esc!="."
replace niveledu=3 if nivelaprob=="6" & antec_esc=="2" & gradoaprob>="3"
replace niveledu=3 if nivelaprob>="7" & nivelaprob!="."
label var niveledu "Nivel Educativo"
label define niveledu 0 "Con primaria incompleta o menos" 1 "Primaria completa o secundaria incompleta" 2 "Secundaria completa o media superior" 3 "Media superior completa o mayor"
label value niveledu niveledu
*Rezagos educativos 
gen rezagoeducativo=.
replace rezagoeducativo=1 if (edad>2 & edad<22) & niveledu<3 & inasis_escuela==1
replace rezagoeducativo=1 if edad>=22 & año_nacimiento>=1998 & niveledu<3 
replace rezagoeducativo=1 if edad>=16 & año_nacimiento<1982 & niveledu==0
replace rezagoeducativo=1 if (año_nacimiento>=1982 & año_nacimiento<=1997) & edad>=16 & niveledu<2
replace rezagoeducativo=0 if edad>=0 & edad<=2 
replace rezagoeducativo=0 if año_nacimiento>=1998 & (edad>=3 & edad<=21) & inasis_esc==0 
replace rezagoeducativo=0 if niveledu==3
replace rezagoeducativo=0 if edad>=16 & (año_nacimiento>=1982 & año_nacimiento<=1997) & (niveledu>=2 & niveledu!=.)
replace rezagoeducativo=0 if edad>=16 & año_nacimiento<=1981 & (niveledu>=1 & niveledu!=.)
label var rezagoeducativo "Indicador de rezago educativo"
label define rezagoeducativo 0 "No tiene rezago educativo" 1 "Tiene rezago educativo"
label value rezagoeducativo rezagoeducativo
*Hablante de lengua indígena;
gen hli=.
replace hli=1 if hablaind=="1" & edad>=3
replace hli=0 if hablaind=="2" & edad>=3
label var hli "Hablante de lengua indígena"
label define hli 0 "No habla lengua indígena" 1 "Habla lengua indígena"
label value hli hli
*sort
sort idhogar idpersona
save, replace
clear
}


**************************************************************************
******Indicador de carencia por acceso a los servicios de salud***********
**************************************************************************

**2016**
*Base trabajos
use trabajos16.dta,clear
egen idhogar=concat(folioviv foliohog)
egen idpersona=concat(folioviv foliohog numren)
label var idhogar "Identificador del hogar"
label var idpersona "Identificador de la persona"
*tipo de trabajo 
gen tipo_trabajo=.
replace tipo_trabajo=1 if subor=="1"
replace tipo_trabajo=2 if (subor=="2" & indep=="1" & tiene_suel=="1") |(subor=="2" & indep=="2" & pago=="1")
replace tipo_trabajo=3 if (subor=="2" & indep=="1" & tiene_suel=="2") |(subor=="2" & indep=="2" & (pago=="2" |pago=="3"))
label var tipo_trabajo ""
*trabajo principal
destring id_trabajo,replace
recode id_trabajo (1=1) (2=0), gen (ocupa)
*distincion de prestaciones en trabajo principal y secundario
keep folioviv foliohog numren idhogar idpersona id_trabajo tipo_trabajo ocupa
reshape wide tipo_trabajo ocupa,i(folioviv foliohog numren) j(id_trabajo)
recode ocupa2 (0=1) (.=0)
label var tipo_trabajo1 "Tipo de trabajo 1"
label var tipo_trabajo2 "Tipo de trabajo 2"
label var ocupa1 "Ocupacion principal"
label var ocupa2 "Ocupacion secundaria"
label define ocupa 0 "Sin ocupacion secundaria" 1 "Con ocupacion secundaria"
label value ocupa2 ocupa
*poblacion trabajadora
gen trabajo=1
label var trabajo "Poblacion con al menos un empleo"
keep folioviv foliohog numren trabajo tipo_trabajo* ocupa* idpersona
save ocupados16.dta,replace

*base poblacion
use poblacion16.dta,
egen idhogar=concat(folioviv foliohog)
egen idpersona=concat(folioviv foliohog numren)
label var idhogar "Identificador del hogar"
label var idpersona "Identificador de la persona"
drop if parentesco>="400" & parentesco<"500"
drop if parentesco>="700" & parentesco<"800"
*merge con base ocupados*
merge 1:m idpersona using ocupados16.dta
drop _merge
save ic_asalud16.dta,replace

**2018**
*base trabajos
use trabajos18.dta,clear
egen idhogar=concat(folioviv foliohog)
egen idpersona=concat(folioviv foliohog numren)
label var idhogar "Identificador del hogar"
label var idpersona "Identificador de la persona"
*tipo de trabajo 
gen tipo_trabajo=.
replace tipo_trabajo=1 if subor=="1"
replace tipo_trabajo=2 if (subor=="2" & indep=="1" & tiene_suel=="1") |(subor=="2" & indep=="2" & pago=="1")
replace tipo_trabajo=3 if (subor=="2" & indep=="1" & tiene_suel=="2") |(subor=="2" & indep=="2" & (pago=="2" |pago=="3"))
label var tipo_trabajo ""
*trabajo principal
destring id_trabajo,replace
recode id_trabajo (1=1) (2=0), gen (ocupa)
*distincion de prestaciones en trabajo principal y secundario
keep folioviv foliohog numren idhogar idpersona id_trabajo tipo_trabajo ocupa
reshape wide tipo_trabajo ocupa,i(folioviv foliohog numren) j(id_trabajo)
recode ocupa2 (0=1) (.=0)
label var tipo_trabajo1 "Tipo de trabajo 1"
label var tipo_trabajo2 "Tipo de trabajo 2"
label var ocupa1 "Ocupacion principal"
label var ocupa2 "Ocupacion secundaria"
label define ocupa 0 "Sin ocupacion secundaria" 1 "Con ocupacion secundaria"
label value ocupa2 ocupa
*poblacion trabajadora
gen trabajo=1
label var trabajo "Poblacion con al menos un empleo"
keep folioviv foliohog numren trabajo tipo_trabajo* ocupa* idpersona
save ocupados18.dta,replace
*base poblacion
use poblacion18.dta,
egen idhogar=concat(folioviv foliohog)
egen idpersona=concat(folioviv foliohog numren)
label var idhogar "Identificador del hogar"
label var idpersona "Identificador de la persona"
drop if parentesco>="400" & parentesco<"500"
drop if parentesco>="700" & parentesco<"800"
*merge con base ocupados*
merge 1:m idpersona using ocupados18.dta
drop _merge
save ic_asalud18.dta,replace

**2020**
*base trabajos
use trabajos20.dta,clear
egen idhogar=concat(folioviv foliohog)
egen idpersona=concat(folioviv foliohog numren)
label var idhogar "Identificador del hogar"
label var idpersona "Identificador de la persona"
*tipo de trabajo 
gen tipo_trabajo=.
replace tipo_trabajo=1 if subor=="1"
replace tipo_trabajo=2 if (subor=="2" & indep=="1" & tiene_suel=="1") |(subor=="2" & indep=="2" & pago=="1")
replace tipo_trabajo=3 if (subor=="2" & indep=="1" & tiene_suel=="2") |(subor=="2" & indep=="2" & (pago=="2" |pago=="3"))
label var tipo_trabajo ""
*trabajo principal
destring id_trabajo,replace
recode id_trabajo (1=1) (2=0), gen (ocupa)
*distincion de prestaciones en trabajo principal y secundario
keep folioviv foliohog numren idhogar idpersona id_trabajo tipo_trabajo ocupa
reshape wide tipo_trabajo ocupa,i(folioviv foliohog numren) j(id_trabajo)
recode ocupa2 (0=1) (.=0)
label var tipo_trabajo1 "Tipo de trabajo 1"
label var tipo_trabajo2 "Tipo de trabajo 2"
label var ocupa1 "Ocupacion principal"
label var ocupa2 "Ocupacion secundaria"
label define ocupa 0 "Sin ocupacion secundaria" 1 "Con ocupacion secundaria"
label value ocupa2 ocupa
*poblacion trabajadora
gen trabajo=1
label var trabajo "Poblacion con al menos un empleo"
keep folioviv foliohog numren trabajo tipo_trabajo* ocupa* idpersona
save ocupados20.dta,replace
*base poblacion
use poblacion20.dta,
egen idhogar=concat(folioviv foliohog)
egen idpersona=concat(folioviv foliohog numren)
label var idhogar "Identificador del hogar"
label var idpersona "Identificador de la persona"
drop if parentesco>="400" & parentesco<"500"
drop if parentesco>="700" & parentesco<"800"
*merge con base ocupados*
merge 1:m idpersona using ocupados20.dta
drop _merge
save ic_asalud20.dta,replace

**2022**
*base trabajos
use trabajos22.dta,clear
egen idhogar=concat(folioviv foliohog)
egen idpersona=concat(folioviv foliohog numren)
label var idhogar "Identificador del hogar"
label var idpersona "Identificador de la persona"
*tipo de trabajo 
gen tipo_trabajo=.
replace tipo_trabajo=1 if subor=="1"
replace tipo_trabajo=2 if (subor=="2" & indep=="1" & tiene_suel=="1") |(subor=="2" & indep=="2" & pago=="1")
replace tipo_trabajo=3 if (subor=="2" & indep=="1" & tiene_suel=="2") |(subor=="2" & indep=="2" & (pago=="2" |pago=="3"))
label var tipo_trabajo ""
*trabajo principal
destring id_trabajo,replace
recode id_trabajo (1=1) (2=0), gen (ocupa)
*distincion de prestaciones en trabajo principal y secundario
keep folioviv foliohog numren idhogar idpersona id_trabajo tipo_trabajo ocupa
reshape wide tipo_trabajo ocupa,i(folioviv foliohog numren) j(id_trabajo)
recode ocupa2 (0=1) (.=0)
label var tipo_trabajo1 "Tipo de trabajo 1"
label var tipo_trabajo2 "Tipo de trabajo 2"
label var ocupa1 "Ocupacion principal"
label var ocupa2 "Ocupacion secundaria"
label define ocupa 0 "Sin ocupacion secundaria" 1 "Con ocupacion secundaria"
label value ocupa2 ocupa
*poblacion trabajadora
gen trabajo=1
label var trabajo "Poblacion con al menos un empleo"
keep folioviv foliohog numren trabajo tipo_trabajo* ocupa* idpersona
save ocupados22.dta,replace

*base poblacion
use poblacion22.dta,
egen idhogar=concat(folioviv foliohog)
egen idpersona=concat(folioviv foliohog numren)
label var idhogar "Identificador del hogar"
label var idpersona "Identificador de la persona"
drop if parentesco>="400" & parentesco<"500"
drop if parentesco>="700" & parentesco<"800"
*merge con base ocupados*
merge 1:m idpersona using ocupados22.dta
drop _merge
save ic_asalud22.dta,replace

*loops
*2016-2018
*Seguro Popular
set more off 

local flist ic_asalud16.dta ic_asalud18.dta 

foreach file in `flist' {
	use `file'
rename *, lower	
*Poblacion economicamente activa
gen pea=.
replace pea=1 if trabajo==1 & (edad>=16 & edad!=.)
replace pea=2 if (act_pnea1=="1" |act_pnea2=="1") & (edad>=16 & edad!=.)
replace pea=0 if (edad>=16 & edad!=.) & (act_pnea1!="1" & act_pnea2!="1") & ((act_pnea1>="2" & act_pnea1<="6") | (act_pnea2>="2" & act_pnea2=="6")) & pea==.
label var pea "Poblacion economicamente activa"
label define pea 0 "PNEA" 1 "PEA:ocupado" 2 "PEA:desocupado"
label value pea pea
*tipo de trabajo
replace tipo_trabajo1=tipo_trabajo1 if pea==1
replace tipo_trabajo1=. if (pea==0 |pea==2)
replace tipo_trabajo1=. if pea==.
label define tipo_trabajo 1 "Depende de un patrón, jefe o superior" 2 "No depende de un jefe y recibe o tiene asignado un sueldo" 3 "No depende de un jefe y no recibe o no tiene asignado o un sueldo"
label value tipo_trabajo1 tipo_trabajo
replace tipo_trabajo2=tipo_trabajo2 if pea==1
replace tipo_trabajo2=. if (pea==0 |pea==2)
replace tipo_trabajo2=. if pea==.
label value tipo_trabajo2 tipo_trabajo
*servicios medicos por prestacion laboral
*Como ocupacion principal
gen smedlprinc=.
replace smedlprinc=0 if ocupa1==1
replace smedlprinc=1 if ocupa1==1 & atemed=="1" & (inst_1=="1" |inst_2=="2" |inst_3=="3" |inst_4=="4") & (inscr_1=="1")
label var smedlprinc "Servicios medicos por prestacion laboral en ocupacion principal"
label define cmedica 0 "Sin servicios medicos" 1 "Con servicios medicos"
label value smedlprinc cmedica
*con ocupacion secundaria
gen smedlsec=.
replace smedlsec=0 if ocupa2==1
replace smedlsec=1 if ocupa2==1 & atemed=="1" & (inst_1=="1" |inst_2=="2" |inst_3=="3" |inst_4=="4") & (inscr_1=="1")
label var smedlsec "Servicios medicos por prestacion laboral en ocupacion secundaria"
label value smedlsec cmedica
*servicios medicos de contratacion voluntaria*
gen smedcontvol=.
replace smedcontvol=0 if (edad>=12 & edad!=.)
replace smedcontvol=1 if atemed=="1" & (inst_1=="1" |inst_2=="2" |inst_3=="3" |inst_4=="4") & (inscr_6=="6") & (edad>=12 & edad!=.)
label var smedcontvol "Servicios medicos de contratacion voluntaria"
label define cuenta 0 "No cuenta" 1 "Sí cuenta"
label value smedcontvol cuenta
*Acceso directo al servicio de salud**
gen smeddirecto=0
replace smeddirecto=1 if tipo_trabajo1==1 & smedlprinc==1
replace smeddirecto=1 if tipo_trabajo1==2 & (smedlprinc==1 |smedcontvol==1)
replace smeddirecto=1 if tipo_trabajo1==3 & (smedlprinc==1 |smedcontvol==1)
replace smeddirecto=1 if tipo_trabajo2==1 & smedlsec==1
replace smeddirecto=1 if tipo_trabajo2==2 & (smedlsec==1 |smedcontvol==1)
replace smeddirecto=1 if tipo_trabajo2==3 & (smedlsec==1 |smedcontvol==1)
label var smeddirecto "Acceso directo a servicios de salud"
label define smeddirecto 0 "Sin acceso" 1 "Con acceso"
label value smeddirecto smeddirecto
*Servicios de salud por relaciones familiares
gen pariente=0
replace pariente=1 if (parentesco>="100" & parentesco<"200")
replace pariente=2 if (parentesco>="200" & parentesco<"300")
replace pariente=3 if (parentesco>="300" & parentesco<"400")
replace pariente=4 if parentesco=="601"
replace pariente=5 if parentesco=="615"
label var pariente "Integrantes que tienen acceso por otros miembros"
label define pariente 0 "Sin parentesco directo" 1 "Jefe o jefa de hogar" 2 "Conyuge del jefe/a" 3 "Hijo de jefe/a" 4 "Padre o madre del jefe/a" 5 "Suegro del jefe/a"
label value pariente pariente
*Inasistencia a la escuela*
gen inasis_escuela=.
replace inasis_escuela=0 if asis_esc=="1"
replace inasis_escuela=1 if asis_esc=="2"
label var inasis_escuela "Inasistencia a la escuela"
label define inasis_escuela 0 "Sí asiste" 1 "No asiste"
label value inasis_escuela inasis_escuela
	**¿El miembro cuenta con acceso directo a la salud por medio de la relacion del parentesco?**
*jefe de hogar
gen jefe=1 if pariente==1 & smeddirecto==1
replace jefe=. if pariente==1 & smeddirecto==1 & (((inst_2=="2" | inst_3=="3") & inscr_6=="6") & (inst_1==" " & inst_4==" " & inst_6==" ") & (inscr_1==" " & inscr_2==" " & inscr_3==" " & inscr_4==" " & inscr_5==" " & inscr_7==" "))
egen jefe_sa=sum(jefe), by(idhogar)
label var jefe_sa "Acceso directo a los servicios d esalud de la jefatura de hogar"
label value jefe_sa cuenta
*conyuge
gen conyuge=1 if pariente==2 & smeddirecto==1
replace conyuge=. if pariente==2 & smeddirecto==1 & (((inst_2=="2" | inst_3=="3") & inscr_6=="6") & (inst_1==" " & inst_4==" " & inst_6==" ") & (inscr_1==" " & inscr_2==" " & inscr_3==" " & inscr_4==" " & inscr_5==" " & inscr_7==" "))
egen conyuge_sa=sum(conyuge),by(idhogar)
replace conyuge_sa=1 if conyuge_sa>=1 & conyuge_sa!=.
label var conyuge_sa "Acceso directo a los servicios de salud del conyuge de la jefatura de hogar"
label value conyuge_sa cuenta
*hijo
gen hijo=1 if pariente==3 & smeddirecto==1
replace hijo=. if pariente==3 & smeddirecto==1 & (((inst_2=="2" | inst_3=="3") & inscr_6=="6") & (inst_1==" " & inst_4==" " & inst_6==" ") & (inscr_1==" " & inscr_2==" " & inscr_3==" " & inscr_4==" " & inscr_5==" " & inscr_7==" "))
egen hijo_sa=sum(hijo),by(idhogar)
replace hijo_sa=1 if hijo_sa>=1 & hijo_sa!=.
label var hijo_sa "Acceso directo a los servicios de salud de hijos(as) de la jefatura de hogar"
label value hijo_sa cuenta




*otros nucleos familiares
gen s_salud=.
replace s_salud=0 if segpop!=" " & atemed!=" "
replace s_salud=1 if atemed=="1" & (inst_1=="1" |inst_2=="2" |inst_3=="3" |inst_4=="4") & (inscr_3=="3" |inscr_4=="4" |inscr_6=="6" |inscr_7=="7")
label var s_salud "Servicios medicos por otros nucleos familiares o por contratacion propia"
label value s_salud cuenta




*Indicador de carencia por servicios de salud*
gen ic_asalud=1
replace ic_asalud=0 if smeddirecto==1
replace ic_asalud=0 if pariente==1 & conyuge_sa==1
replace ic_asalud=0 if pariente==1 & pea==0 & hijo_sa==1
replace ic_asalud=0 if pariente==2 & jefe_sa==1
replace ic_asalud=0 if pariente==2 & pea==0 & hijo_sa==1
replace ic_asalud=0 if pariente==3 & edad<16 & jefe_sa==1
replace ic_asalud=0 if pariente==3 & edad<16 & conyuge_sa==1
replace ic_asalud=0 if pariente==3 & (edad>=16 & edad<=25) & inasis_escuela==0 & jefe_sa==1
replace ic_asalud=0 if pariente==3 & (edad>=16 & edad<=25) & inasis_escuela==0 & conyuge==1
replace ic_asalud=0 if pariente==4 & pea==0 & jefe_sa==1
replace ic_asalud=0 if pariente==5 & pea==0 & conyuge_sa==1
replace ic_asalud=0 if s_salud==1
replace ic_asalud=0 if segpop=="1" | (segpop=="2" & atemed=="1" & (inst_1=="1" |inst_2=="2" | inst_3=="3" | inst_4=="4" | inst_5=="5" | inst_6=="6")) | segvol_2=="2"


label var ic_asalud "Indicador de carencia por acceso a servicios de salud"
label define ic_asalud 0 "No presenta carencia" 1 "Presenta carencia"
label value ic_asalud ic_asalud	
*Personas con discapacidad
gen discap=.
replace discap=0 if disc1=="8"
replace discap=1 if (disc1=="1" |disc1=="2" |disc1=="3" |disc1=="4" |disc1=="5" |disc1=="6" |disc1=="7")
replace discap=1 if (disc2=="2" |disc2=="3" |disc2=="4" |disc2=="5" |disc2=="6" |disc2=="7")
replace discap=1 if (disc3=="3" |disc3=="4" |disc3=="5" |disc3=="6" |disc3=="7")
replace discap=1 if (disc4=="4" |disc4=="5" |disc4=="6" |disc4=="7")
replace discap=1 if (disc5=="5" |disc5=="6" |disc5=="7")
replace discap=1 if (disc6=="6" |disc6=="7")
replace discap=1 if disc7=="7"	
save, replace
clear	
}

*2020-2022
*Insabi
set more off 

local flist ic_asalud20.dta ic_asalud22.dta

foreach file in `flist' {
	use `file'
rename *, lower	
*Poblacion economicamente activa
gen pea=.
replace pea=1 if trabajo==1 & (edad>=16 & edad!=.)
replace pea=2 if (act_pnea1=="1" |act_pnea2=="1") & (edad>=16 & edad!=.)
replace pea=0 if (edad>=16 & edad!=.) & (act_pnea1!="1" & act_pnea2!="1") & ((act_pnea1>="2" & act_pnea1<="6") | (act_pnea2>="2" & act_pnea2=="6")) & pea==.
label var pea "Poblacion economicamente activa"
label define pea 0 "PNEA" 1 "PEA:ocupado" 2 "PEA:desocupado"
label value pea pea
*tipo de trabajo
replace tipo_trabajo1=tipo_trabajo1 if pea==1
replace tipo_trabajo1=. if (pea==0 |pea==2)
replace tipo_trabajo1=. if pea==.
label define tipo_trabajo 1 "Depende de un patrón, jefe o superior" 2 "No depende de un jefe y recibe o tiene asignado un sueldo" 3 "No depende de un jefe y no recibe o no tiene asignado o un sueldo"
label value tipo_trabajo1 tipo_trabajo
replace tipo_trabajo2=tipo_trabajo2 if pea==1
replace tipo_trabajo2=. if (pea==0 |pea==2)
replace tipo_trabajo2=. if pea==.
label value tipo_trabajo2 tipo_trabajo
*servicios medicos por prestacion laboral
*Como ocupacion principal
gen smedlprinc=.
replace smedlprinc=0 if ocupa1==1
replace smedlprinc=1 if ocupa1==1 & atemed=="1" & (inst_1=="1" |inst_2=="2" |inst_3=="3" |inst_4=="4") & (inscr_1=="1")
label var smedlprinc "Servicios medicos por prestacion laboral en ocupacion principal"
label define cmedica 0 "Sin servicios medicos" 1 "Con servicios medicos"
label value smedlprinc cmedica
*con ocupacion secundaria
gen smedlsec=.
replace smedlsec=0 if ocupa2==1
replace smedlsec=1 if ocupa2==1 & atemed=="1" & (inst_1=="1" |inst_2=="2" |inst_3=="3" |inst_4=="4") & (inscr_1=="1")
label var smedlsec "Servicios medicos por prestacion laboral en ocupacion secundaria"
label value smedlsec cmedica
*servicios medicos de contratacion voluntaria*
gen smedcontvol=.
replace smedcontvol=0 if (edad>=12 & edad!=.)
replace smedcontvol=1 if atemed=="1" & (inst_1=="1" |inst_2=="2" |inst_3=="3" |inst_4=="4") & (inscr_6=="6") & (edad>=12 & edad!=.)
label var smedcontvol "Servicios medicos de contratacion voluntaria"
label define cuenta 0 "No cuenta" 1 "Sí cuenta"
label value smedcontvol cuenta
*Acceso directo al servicio de salud**
gen smeddirecto=0
replace smeddirecto=1 if tipo_trabajo1==1 & smedlprinc==1
replace smeddirecto=1 if tipo_trabajo1==2 & (smedlprinc==1 |smedcontvol==1)
replace smeddirecto=1 if tipo_trabajo1==3 & (smedlprinc==1 |smedcontvol==1)
replace smeddirecto=1 if tipo_trabajo2==1 & smedlsec==1
replace smeddirecto=1 if tipo_trabajo2==2 & (smedlsec==1 |smedcontvol==1)
replace smeddirecto=1 if tipo_trabajo2==3 & (smedlsec==1 |smedcontvol==1)
label var smeddirecto "Acceso directo a servicios de salud"
label define smeddirecto 0 "Sin acceso" 1 "Con acceso"
label value smeddirecto smeddirecto
*Servicios de salud por relaciones familiares
gen pariente=0
replace pariente=1 if (parentesco>="100" & parentesco<"200")
replace pariente=2 if (parentesco>="200" & parentesco<"300")
replace pariente=3 if (parentesco>="300" & parentesco<"400")
replace pariente=4 if parentesco=="601"
replace pariente=5 if parentesco=="615"
label var pariente "Integrantes que tienen acceso por otros miembros"
label define pariente 0 "Sin parentesco directo" 1 "Jefe o jefa de hogar" 2 "Conyuge del jefe/a" 3 "Hijo de jefe/a" 4 "Padre o madre del jefe/a" 5 "Suegro del jefe/a"
label value pariente pariente
*Inasistencia a la escuela*
gen inasis_escuela=.
replace inasis_escuela=0 if asis_esc=="1"
replace inasis_escuela=1 if asis_esc=="2"
label var inasis_escuela "Inasistencia a la escuela"
label define inasis_escuela 0 "Sí asiste" 1 "No asiste"
label value inasis_escuela inasis_escuela
**¿El miembro cuenta con acceso directo a la salud por medio de la relacion del parentesco?**
*jefe de hogar
gen jefe=1 if pariente==1 & smeddirecto==1
replace jefe=. if pariente==1 & smeddirecto==1 & (((inst_2=="2" | inst_3=="3") & inscr_6=="6") & (inst_1==" " & inst_4==" " & inst_6==" ") & (inscr_1==" " & inscr_2==" " & inscr_3==" " & inscr_4==" " & inscr_5==" " & inscr_7==" "))
egen jefe_sa=sum(jefe), by(idhogar)
label var jefe_sa "Acceso directo a los servicios d esalud de la jefatura de hogar"
label value jefe_sa cuenta
*conyuge
gen conyuge=1 if pariente==2 & smeddirecto==1
replace conyuge=. if pariente==2 & smeddirecto==1 & (((inst_2=="2" | inst_3=="3") & inscr_6=="6") & (inst_1==" " & inst_4==" " & inst_6==" ") & (inscr_1==" " & inscr_2==" " & inscr_3==" " & inscr_4==" " & inscr_5==" " & inscr_7==" "))
egen conyuge_sa=sum(conyuge),by(idhogar)
replace conyuge_sa=1 if conyuge_sa>=1 & conyuge_sa!=.
label var conyuge_sa "Acceso directo a los servicios de salud del conyuge de la jefatura de hogar"
label value conyuge_sa cuenta
*hijo
gen hijo=1 if pariente==3 & smeddirecto==1
replace hijo=. if pariente==3 & smeddirecto==1 & (((inst_2=="2" | inst_3=="3") & inscr_6=="6") & (inst_1==" " & inst_4==" " & inst_6==" ") & (inscr_1==" " & inscr_2==" " & inscr_3==" " & inscr_4==" " & inscr_5==" " & inscr_7==" "))
egen hijo_sa=sum(hijo),by(idhogar)
replace hijo_sa=1 if hijo_sa>=1 & hijo_sa!=.
label var hijo_sa "Acceso directo a los servicios de salud de hijos(as) de la jefatura de hogar"
label value hijo_sa cuenta



*otros nucleos familiares
gen s_salud=.
replace s_salud=0 if pop_insabi!=" " & atemed!=" "
replace s_salud=1 if atemed=="1" & (inst_1=="1" |inst_2=="2" |inst_3=="3" |inst_4=="4") & (inscr_3=="3" |inscr_4=="4" |inscr_6=="6" |inscr_7=="7")
label var s_salud "Servicios medicos por otros nucleos familiares o por contratacion propia"
label value s_salud cuenta









*Indicador de carencia por servicios de salud*
gen ic_asalud=1
replace ic_asalud=0 if smeddirecto==1
replace ic_asalud=0 if pariente==1 & conyuge_sa==1
replace ic_asalud=0 if pariente==1 & pea==0 & hijo_sa==1
replace ic_asalud=0 if pariente==2 & jefe_sa==1
replace ic_asalud=0 if pariente==2 & pea==0 & hijo_sa==1
replace ic_asalud=0 if pariente==3 & edad<16 & jefe_sa==1
replace ic_asalud=0 if pariente==3 & edad<16 & conyuge_sa==1
replace ic_asalud=0 if pariente==3 & (edad>=16 & edad<=25) & inasis_escuela==0 & jefe_sa==1
replace ic_asalud=0 if pariente==3 & (edad>=16 & edad<=25) & inasis_escuela==0 & conyuge==1
replace ic_asalud=0 if pariente==4 & pea==0 & jefe_sa==1
replace ic_asalud=0 if pariente==5 & pea==0 & conyuge_sa==1
replace ic_asalud=0 if s_salud==1
replace ic_asalud=0 if pop_insabi=="1" | (pop_insabi=="2" & atemed=="1" & (inst_1=="1" |inst_2=="2" | inst_3=="3" | inst_4=="4" | inst_5=="5" | inst_6=="6")) | segvol_2=="2"
label var ic_asalud "Indicador de carencia por acceso a servicios de salud"
label define ic_asalud 0 "No presenta carencia" 1 "Presenta carencia"
label value ic_asalud ic_asalud	

*Población con presencia de discapacidad, sea física o mental
gen discap=.
replace discap=0 if disc_camin=="3" | disc_camin=="4"
replace discap=0 if disc_ver=="3" | disc_ver=="4"
replace discap=0 if disc_brazo=="3" | disc_brazo=="4"
replace discap=0 if disc_apren=="3" | disc_apren=="4"
replace discap=0 if disc_oir=="3" | disc_oir=="4"
replace discap=0 if disc_vest=="3" | disc_vest=="4"
replace discap=0 if disc_habla=="3" | disc_habla=="4"
replace discap=0 if disc_acti=="3" | disc_acti=="4"
replace discap=1 if (disc_camin=="1" | disc_camin=="2")
replace discap=1 if (disc_ver=="1" | disc_ver=="2")
replace discap=1 if (disc_brazo=="1" | disc_brazo=="2")
replace discap=1 if (disc_apren=="1" | disc_apren=="2")
replace discap=1 if (disc_oir=="1" | disc_oir=="2")
replace discap=1 if (disc_vest=="1" | disc_vest=="2")
replace discap=1 if (disc_habla=="1" | disc_habla=="2")
replace discap=1 if (disc_acti=="1" | disc_acti=="2")

label var discap "Población con presencia de discapacidad física o mental"
label define discap  0 "Sin presencia de discapacidad" 1 "Con presencia de discapacidad"
label value discap discap
save, replace
clear	
}

*************************************************************************
*******Indicador de carencia por calidad y espacios de la vivienda*******
*************************************************************************

*2016*
use viviendas16.dta,clear
sort folioviv
save,replace
use concentradohogar16.dta,clear
sort folioviv
merge m:1 folioviv using viviendas16.dta
keep if _merge==3
drop _merge
gen año=2016
label var año "Año de la encuesta"
egen idhogar=concat(folioviv foliohog)
label var idhogar "Identificador del hogar"
save ic_cev16.dta,replace
*2018
use viviendas18.dta,clear
sort folioviv
save,replace
use concentradohogar18.dta,clear
merge m:1 folioviv using viviendas18.dta
keep if _merge==3
drop _merge
gen año=2018
label var año "Año de la encuesta"
egen idhogar=concat(folioviv foliohog)
label var idhogar "Identificador del hogar"
save ic_cev18.dta,replace
*2020
use viviendas20.dta,clear
sort folioviv
save,replace
use concentradohogar20.dta,clear
merge m:1 folioviv using viviendas20.dta
keep if _merge==3
drop _merge
gen año=2020
label var año "Año de la encuesta"
egen idhogar=concat(folioviv foliohog)
label var idhogar "Identificador del hogar"
save ic_cev20.dta,replace
*2022
use viviendas22.dta,clear
sort folioviv
save,replace
use concentradohogar22.dta,clear
merge m:1 folioviv using viviendas22.dta
keep if _merge==3
drop _merge
gen año=2022
label var año "Año de la encuesta"
egen idhogar=concat(folioviv foliohog)
label var idhogar "Identificador del hogar"
save ic_cev22.dta,replace

set more off

local flist ic_cev16.dta ic_cev18.dta ic_cev20.dta ic_cev22.dta 

foreach file in `flist' {
	use `file'
rename *, lower	
*pisos*
gen icv_pisos=.
replace icv_pisos=1 if mat_pisos=="1"
replace icv_pisos=0 if mat_pisos=="2" |mat_pisos=="3"
label var icv_pisos "Indicador de carencia por material de piso de la vivienda"
label define icv_pisos 0 "Sin carencia" 1 "Con carencia"
label value icv_pisos icv_pisos
*techos*
gen icv_techos=0
replace icv_techos=1 if mat_techos=="01" |mat_techos=="02"
label var icv_techos "Indicador de carencia por material de techos de vivienda"
label define icv_techos 0 "Sin carencia" 1 "Con carencia"
label value icv_techos icv_techos
*muros*
gen icv_muros=.
replace icv_muros=1 if mat_pared<"6"
replace icv_muros=0 if mat_pared>="6"
label var icv_muros "Indicador de carencia por material de muros de la vivienda"
label define icv_muros 0 "Sin carencia" 1 "Con carencia"
label value icv_muros icv_muros
*Indice de hacinamiento
gen indicehacinamiento=tot_resid/num_cuarto
gen icv_hac=.
label var indicehacinamiento "Indice de hacinamiento"
replace icv_hac=0 if indicehacinamiento<=2.5
replace icv_hac=1 if indicehacinamiento>2.5 & indicehacinamiento!=.
label var icv_hac "Indicador de carencia por hacinamiento en la vivienda"
label define icv_hac 0 "Sin carencia" 1 "Con carencia"
label value icv_hac icv_hac
*Indicador*
gen iccv=.
replace iccv=1 if icv_pisos==1 |icv_techos==1 |icv_muros==1 |icv_hac==1
replace iccv=0 if icv_pisos==0 & icv_techos==0 & icv_muros==0 & icv_hac==0
replace iccv=. if icv_pisos==. |icv_techos==. |icv_muros==. |icv_hac==.
label var iccv "Indicador de carencia por calidad y espacios de la vivienda"
label define iccv 0 "Sin carencia" 1 "Con carencia"
label value iccv iccv
*sort
sort idhogar 
save, replace
clear	
}

***************************************************************************
**Indicador de Carencia por Acceso a los servicios basicos en la vivienda**
***************************************************************************

*2016*
use viviendas16.dta,clear
sort folioviv
save,replace
use concentradohogar16.dta,clear
sort folioviv
merge m:1 folioviv using viviendas16.dta
keep if _merge==3
drop _merge
gen año=2016
label var año "Año de la encuesta"
egen idhogar=concat(folioviv foliohog)
label var idhogar "Identificador del hogar"
gen ic_agua=.
save ic_sbv16.dta,replace


*2018
use viviendas18.dta,clear
sort folioviv
save,replace
use concentradohogar18.dta,clear
merge m:1 folioviv using viviendas18.dta
keep if _merge==3
drop _merge
gen año=2018
label var año "Año de la encuesta"
egen idhogar=concat(folioviv foliohog)
label var idhogar "Identificador del hogar"
gen ic_agua=.
replace ic_agua=0 if procaptar==1 & disp_agua=="4"
save ic_sbv18.dta,replace

*2020
use viviendas20.dta,clear
sort folioviv
save,replace
use concentradohogar20.dta,clear
merge m:1 folioviv using viviendas20.dta
keep if _merge==3
drop _merge
gen año=2020
label var año "Año de la encuesta"
egen idhogar=concat(folioviv foliohog)
label var idhogar "Identificador del hogar"
gen ic_agua=.
replace ic_agua=0 if procaptar==1 & disp_agua=="4"
save ic_sbv20.dta,replace

*2022
use viviendas22.dta,clear
sort folioviv
save,replace
use concentradohogar22.dta,clear
merge m:1 folioviv using viviendas22.dta
keep if _merge==3
drop _merge
gen año=2022
label var año "Año de la encuesta"
egen idhogar=concat(folioviv foliohog)
label var idhogar "Identificador del hogar"
gen ic_agua=.
replace ic_agua=0 if procaptar==1 & disp_agua=="4"
save ic_sbv22.dta,replace

set more off

local flist ic_sbv16.dta ic_sbv18.dta ic_sbv20.dta ic_sbv22.dta 

foreach file in `flist' {
	use `file'
rename *, lower	
*Servicio de agua potable
replace ic_agua=1 if disp_agua>="3"
replace ic_agua=0 if disp_agua<="2"
label var ic_agua "Indicador de carencia por acceso al agua"
label define caren 0 "Sin carencia" 1 "Con carencia"
label value ic_agua caren
*Drenaje*
gen ic_drenaje=.
replace ic_drenaje=0 if drenaje<="2"
replace ic_drenaje=1 if drenaje>="3"
label var ic_drenaje "Indicador de carencia por servicio de drenaje"
label value ic_drenaje caren
*Energia Eléctrica
gen ic_electricidad=.
replace ic_electricidad=0 if disp_elect<="4"
replace ic_electricidad=1 if disp_elect=="5"
label var ic_electricidad "Indicador de carencia por servicios de electricidad"
label value ic_electricidad caren
*Combustible para cocinar*
gen ic_combustible=.
replace ic_combustible=0 if combustible>"2" & combustible<="6"
replace ic_combustible=0 if (combustible=="1" |combustible=="2") & estufa=="1"
replace ic_combustible=1 if (combustible=="1" |combustible=="2") & estufa=="2"
label var ic_combustible "indicador de Carencia por combustible para cocinar"
label value ic_combustible caren
*indicador*
gen ic_sbv=.
replace ic_sbv=1 if (ic_agua==1 | ic_drenaje==1 |ic_electricidad==1 |ic_combustible==1)
replace ic_sbv=0 if ic_agua==0 & ic_drenaje==0 & ic_electricidad==0 & ic_combustible==0
replace ic_sbv=. if (ic_agua==. | ic_drenaje==. |ic_electricidad==. |ic_combustible==.)
label var ic_sbv "Indicador de carencia por acceso a servicios básicos en la vivienda"
label value ic_sbv caren
*sort
sort idhogar 
save, replace
clear	
}

*****************************************************************************
**Indicador de carencia por acceso a la alimentacion nutritiva y de calidad**
*****************************************************************************

**2016**
*base poblacion
use poblacion16.dta,clear
drop if parentesco>="400" & parentesco<"500"
drop if parentesco>="700" & parentesco<"800"
egen idhogar=concat(folioviv foliohog)
label var idhogar "Identificador del hogar"
*menores
gen menores=1 if edad>=0 & edad<=17
collapse (sum) menores,by(idhogar)
gen idmenores=.
replace idmenores=1 if menores>=1 & menores!=.
replace idmenores=0 if menores==0
label var idmenores "Hogares con poblacion de 0 a 17 años de edad"
label define idmenores 0 "Sin poblacion de 0 a 17 años" 1 "Con poblacion de 0 a 17 años"
label value idmenores idmenores
save menores16.dta,replace
*base hogares
use hogares16.dta,clear 
egen idhogar=concat(folioviv foliohog)
merge m:1 idhogar using menores16.dta
keep if _merge==3
drop _merge
label var idhogar "Identificador del hogar"
save ic_ali16.dta,replace

**2018**
use poblacion18.dta,clear
drop if parentesco>="400" & parentesco<"500"
drop if parentesco>="700" & parentesco<"800"
egen idhogar=concat(folioviv foliohog)
label var idhogar "Identificador del hogar"
*menores
gen menores=1 if edad>=0 & edad<=17
collapse (sum) menores,by(idhogar)
gen idmenores=.
replace idmenores=1 if menores>=1 & menores!=.
replace idmenores=0 if menores==0
label var idmenores "Hogares con poblacion de 0 a 17 años de edad"
label define idmenores 0 "Sin poblacion de 0 a 17 años" 1 "Con poblacion de 0 a 17 años"
label value idmenores idmenores
save menores18.dta,replace
*base hogares
use hogares18.dta,clear 
egen idhogar=concat(folioviv foliohog)
merge m:1 idhogar using menores18.dta
keep if _merge==3
drop _merge
label var idhogar "Identificador del hogar"
save ic_ali18.dta,replace

**2020**
use poblacion20.dta,clear
drop if parentesco>="400" & parentesco<"500"
drop if parentesco>="700" & parentesco<"800"
egen idhogar=concat(folioviv foliohog)
label var idhogar "Identificador del hogar"
*menores
gen menores=1 if edad>=0 & edad<=17
collapse (sum) menores,by(idhogar)
gen idmenores=.
replace idmenores=1 if menores>=1 & menores!=.
replace idmenores=0 if menores==0
label var idmenores "Hogares con poblacion de 0 a 17 años de edad"
label define idmenores 0 "Sin poblacion de 0 a 17 años" 1 "Con poblacion de 0 a 17 años"
label value idmenores idmenores
save menores20.dta,replace
*base hogares
use hogares20.dta,clear 
egen idhogar=concat(folioviv foliohog)
merge m:1 idhogar using menores20.dta
keep if _merge==3
drop _merge
label var idhogar "Identificador del hogar"
save ic_ali20.dta,replace

**2022**
use poblacion22.dta,clear
drop if parentesco>="400" & parentesco<"500"
drop if parentesco>="700" & parentesco<"800"
egen idhogar=concat(folioviv foliohog)
label var idhogar "Identificador del hogar"
*menores
gen menores=1 if edad>=0 & edad<=17
collapse (sum) menores,by(idhogar)
gen idmenores=.
replace idmenores=1 if menores>=1 & menores!=.
replace idmenores=0 if menores==0
label var idmenores "Hogares con poblacion de 0 a 17 años de edad"
label define idmenores 0 "Sin poblacion de 0 a 17 años" 1 "Con poblacion de 0 a 17 años"
label value idmenores idmenores
save menores22.dta,replace
*base hogares
use hogares22.dta,clear 
egen idhogar=concat(folioviv foliohog)
merge m:1 idhogar using menores22.dta
keep if _merge==3
drop _merge
label var idhogar "Identificador del hogar"
save ic_ali22.dta,replace

set more off

local flist ic_ali16.dta ic_ali18.dta ic_ali20.dta ic_ali22.dta

foreach file in `flist' {
	use `file'
rename *, lower		
*Generar los indices de carencias de alimentacion
*Grado de Inseguridad Alimentaria
destring acc_alim*,replace
*Hogares sin menores
recode acc_alim4 (2=0) (1=1) (.=0), gen (i_alim1ad)
recode acc_alim5 (2=0) (1=1) (.=0), gen (i_alim2ad)
recode acc_alim6 (2=0) (1=1) (.=0), gen (i_alim3ad)
recode acc_alim2 (2=0) (1=1) (.=0), gen (i_alim4ad)
recode acc_alim7 (2=0) (1=1) (.=0), gen (i_alim5ad)
recode acc_alim8 (2=0) (1=1) (.=0), gen (i_alim6ad)
*Hogares con menores
recode acc_alim11 (2=0) (1=1) (.=0), gen (i_alim7men)
recode acc_alim12 (2=0) (1=1) (.=0), gen (i_alim8men)
recode acc_alim13 (2=0) (1=1) (.=0), gen (i_alim9men)
recode acc_alim14 (2=0) (1=1) (.=0), gen (i_alim10men)
recode acc_alim15 (2=0) (1=1) (.=0), gen (i_alim11men)
recode acc_alim16 (2=0) (1=1) (.=0), gen (i_alim12men)
label var i_alim1ad "Algun adulto tuvo una alimentacion basada en muy poca variedad"
label define respuesta 0 "No" 1 "Sí"
label value i_alim1ad respuesta
label var i_alim2ad "Algun adulto dejó de desayunar, comer o cenar"
label value i_alim2ad respuesta
label var i_alim3ad "Algun adulto comió menos de lo que debía"
label value i_alim3ad respuesta
label var i_alim4ad "El hogar se quedó sin comida"
label value i_alim4ad respuesta
label var i_alim5ad "Algún adulto sintió hambre pero no comió"
label value i_alim5ad respuesta
label var i_alim6ad "Algun adulto solo comió una vez al dia o dejó de comer todo un dia"
label value i_alim6ad respuesta
label var i_alim7men "Alguien de 0 a 17 años tuvo una alimentacion basada en muy poca variedad de alimentos"
label value i_alim7men respuesta
label var i_alim8men "Alguien de 0 a 17 años comió menos de lo que debía"
label value i_alim8men respuesta
label var i_alim9men "Se tuvo que disminuir la cantidad servida en las comidas a alguien de 0 a 17 años"
label value i_alim9men respuesta
label var i_alim10men "Alguien de 0 a 17 años sintió hambre pero no comió"
label value i_alim10men respuesta
label var i_alim11men "Alguien de 0 a 17 años se acostó con hambre"
label value i_alim11men respuesta
label var i_alim12men "Alguien de 0 a 17 años comió una vez al dia o dejó de comer todo un día"
label value i_alim12men respuesta
*Escala de hogares sin menores*
gen tot_ia1=i_alim1ad+i_alim2ad+i_alim3ad+i_alim4ad+i_alim5ad+i_alim6ad if idmenores==0
label var tot_ia1 "Escala de inseguridad aliemntaria para hogares sin menores de edad"
*Escala de hogares con menores*
gen tot_ia2=i_alim1ad+i_alim2ad+i_alim3ad+i_alim4ad+i_alim5ad+i_alim6ad+i_alim7men+i_alim8men+i_alim9men+i_alim10men+i_alim11men+i_alim12men if idmenores==1
label var tot_ia2 "Escala de inseguridad aliemntaria para hogares con menores de edad"
*Inseguridad Alimentaria
gen i_alimentaria=.
replace i_alimentaria=0 if tot_ia1==0 |tot_ia2==0
replace i_alimentaria=1 if (tot_ia1==1 | tot_ia1==2) |(tot_ia2==1 |tot_ia2==2 |tot_ia2==3)
replace i_alimentaria=2 if (tot_ia1==3 |tot_ia1==4) |(tot_ia2==4 |tot_ia2==5 |tot_ia2==6 |tot_ia2==7)
replace i_alimentaria=3 if (tot_ia1==5 |tot_ia1==6) |(tot_ia2==8 |tot_ia2==9 |tot_ia2==10 |tot_ia2==11 |tot_ia2==12) & tot_ia2!=.
label var i_alimentaria "Grado de Inseguridad Alimentaria"
label define i_alimentaria 0 "Seguridad alimentaria" 1 "Inseguridad alimentaria breve" 2 "Inseguridad alimentaria moderada" 3 "Inseguridad alimentaria severa"
label value i_alimentaria i_alimentaria
*Indice de carencia alimentaria
gen ic_alimentaria=.
replace ic_alimentaria=1 if i_alimentaria==2 |i_alimentaria==3
replace ic_alimentaria=0 if i_alimentaria==0 |i_alimentaria==1
label var ic_alimentaria "Indicador de carencia por acceso a la alimentacion"
label define ic_alimentaria 0 "Sin carencia" 1 "Con carencia"
label value ic_alimentaria ic_alimentaria
*consumo de alimentos ponderados
egen cpond1=rowmax(alim17_1 alim17_2)
replace cpond=cpond*2
gen cpond3=1*alim17_3
gen cpond4=1*alim17_4
egen cpond5=rowmax(alim17_5 alim17_6 alim17_7)
replace cpond5=cpond5*4
gen cpond8=3*alim17_8
gen cpond9=4*alim17_9
gen cpond10=0.5*alim17_10
gen cpond11=0.5*alim17_11
gen cpond12=0*alim17_12
*suma ponderada
gen tot_cpond=cpond1+cpond3+cpond4+cpond5+cpond8+cpond9+cpond10+cpond11+cpond12
*dieta consumida por los hogares
gen dch=.
replace dch=1 if tot_cpond >=0 & tot_cpond<=28
replace dch=2 if tot_cpond>28 & tot_cpond<=42
replace dch=3 if tot_cpond>42 & tot_cpond!=.
label var dch "Dieta consumida en los hogares"
label define dch 1 "Pobre" 2 "Limítrofe" 3 "Aceptable"
label value dch dch
*limitacion en el consumo de alimentos (lca)
gen lca=.
replace lca=1 if dch==1 |dch==2
replace lca=0 if dch==3
label var lca "Limitación en el consumo de alimentos"
label define lca 0 "No limitado" 1 "Limitado"
label value lca lca
**Indice final**
*Indicador de carencia por acceso a la alimentacion nutritica y de calidad
gen ic_ali_nc=.
replace ic_ali_nc=1 if (lca==1 |ic_alimentaria==1) & (lca!=. & ic_alimentaria!=.)
replace ic_ali_nc=0 if lca==0 & ic_alimentaria==0
label var ic_ali_nc "Indicador de carencia por acceso a la alimentacion nutritiva y de calidad"
label define ic_ali_nc 0 "Sin carencia" 1 "Con carencia"
label value ic_ali_nc ic_ali_nc		
save, replace
clear	
}

************************************************************
***Indicador de Carencia por acceso a la Seguridad Social***
************************************************************


**2016**
use trabajos16.dta,replace
*tipo de trabajador*
gen tipo_trabajo=.
replace tipo_trabajo=1 if subor=="1"
replace tipo_trabajo=2 if subor=="2" & indep=="1" & tiene_suel=="1"
replace tipo_trabajo=2 if subor=="2" & indep=="2" & pago=="1"
replace tipo_trabajo=3 if subor=="2" & indep=="1" & tiene_suel=="2"
replace tipo_trabajo=3 if subor=="2" & indep=="2" & (pago=="2" |pago=="3")
*afore,sar
gen aforelaboral=.
replace aforelaboral=0 if pres_14==" " 
replace aforelaboral=1 if pres_14=="14"
*ocupacion principal o secundaria
gen ocupa=.
replace ocupa=0 if id_trabajo=="2"
replace ocupa=1 if id_trabajo=="1"
keep folioviv foliohog numren id_trabajo tipo_trabajo aforelaboral ocupa
destring id_trabajo,replace
reshape wide tipo_trabajo aforelaboral ocupa,i(folioviv foliohog numren) j(id_trabajo)
recode ocupa2 (0=1) (.=0)
*label
label var tipo_trabajo1 "Tipo de trabajo 1"
label var tipo_trabajo2 "Tipo de trabajo 2"
label var aforelaboral1 "SAR o Afore (ocupación principal)"
label var aforelaboral2 "SAR o Afore (ocupación secundaria)"
label var ocupa1 "Ocupacion principal"
label var ocupa2 "Ocupación secundaria"
label define cuenta 0 "No cuenta" 1 "Sí cuenta"
label value aforelaboral1 cuenta
label value aforelaboral2 cuenta
label define ocupas 0 "Sin ocupacion secundaria" 1 "Con ocupacion secundaria"
label value ocupa2 ocupas
*poblacion trabajadora
gen trabajo=1
label var trabajo "Población con al menos un empleo"
*mantener variables
keep folioviv foliohog numren trabajo tipo_trabajo* aforelaboral* ocupa* 
egen idhogar=concat(folioviv foliohog)
label var idhogar "Identificador del hogar"
egen idpersona=concat(folioviv foliohog numren)
label var idpersona "Identificador de la persona"
save prestaciones16.dta,replace

**Base ingresos**
use ingresos16.dta,clear
egen idhogar=concat(folioviv foliohog)
label var idhogar "Identificador del hogar"
egen idpersona=concat(folioviv foliohog numren)
label var idpersona "Identificador de la persona"
*mantener ingresos por jubilaciones, pensiones y programas de adultos mayores
keep if clave=="P032" |clave=="P033" |clave=="P044" |clave=="P045"
*Deflactar*
*La base es el mes de julio de 2023
gen ingresod1=(ing_1/69.5144952171)*100 if mes_1=="07"
replace ingresod1=(ing_1/69.7104240356)*100 if mes_1=="08"
replace ingresod1=(ing_1/70.1361027189)*100 if mes_1=="09"
replace ingresod1=(ing_1/70.5617814021)*100 if mes_1=="10"

gen ingresod2=(ing_2/69.3337275570)*100 if mes_2=="06"
replace ingresod2=(ing_2/69.5144952171)*100 if mes_2=="07"
replace ingresod2=(ing_2/69.7104240356)*100 if mes_2=="08"
replace ingresod2=(ing_2/70.1361027189)*100 if mes_2=="09"

gen ingresod3=(ing_3/69.2573386427)*100 if mes_3=="05"
replace ingresod3=(ing_3/69.3337275570)*100 if mes_3=="06"
replace ingresod3=(ing_3/69.5144952171)*100 if mes_3=="07"
replace ingresod3=(ing_3/69.7104240356)*100 if mes_3=="08"

gen ingresod4=(ing_4/69.5675592721)*100 if mes_4=="04"
replace ingresod4=(ing_4/69.2573386427)*100 if mes_4=="05"
replace ingresod4=(ing_4/69.3337275570)*100 if mes_4=="06"
replace ingresod4=(ing_4/69.5144952171)*100 if mes_4=="07"

gen ingresod5=(ing_5/69.7885623145)*100 if mes_5=="03"
replace ingresod5=(ing_5/69.5675592721)*100 if mes_5=="04"
replace ingresod5=(ing_5/69.2573386427)*100 if mes_5=="05"
replace ingresod5=(ing_5/69.3337275570)*100 if mes_5=="06"

gen ingresod6=(ing_6/69.6859329333)*100 if mes_6=="02"
replace ingresod6=(ing_6/69.7885623145)*100 if mes_6=="03"
replace ingresod6=(ing_6/69.5675592721)*100 if mes_6=="04"
replace ingresod6=(ing_6/69.2573386427)*100 if mes_6=="05"

egen ingreso_pensiones=rowmean(ingresod1 ingresod2 ingresod3 ingresod4 ingresod5 ingresod6) if clave=="P032" |clave=="P033"
replace ingreso_pensiones=0 if ingreso_pensiones==.
egen ingreso_pam=rowmean(ingresod1 ingresod2 ingresod3 ingresod4 ingresod5 ingresod6) if clave=="P044" |clave=="P045"
replace ingreso_pam=0 if ingreso_pam==.
label var ingreso_pensiones "Ingreso promedio mensual por jubilaciones y pensiones"
label var ingreso_pam "Ingreso promedio mensual por programas de adultos mayores"
collapse (sum) ingreso_pensiones ingreso_pam,by(idpersona)
save pensiones16.dta,replace

*poblacion
use poblacion16.dta,clear
egen idhogar=concat(folioviv foliohog)
label var idhogar "Identificador del hogar"
egen idpersona=concat(folioviv foliohog numren)
label var idpersona "Identificador de la persona"
drop if parentesco>="400" & parentesco<"500"
drop if parentesco>="700" & parentesco<"800"
merge 1:m idpersona using prestaciones16.dta
drop _merge
merge 1:m idpersona using pensiones16.dta
drop _merge
gen año=2016
*otros nucleos familiares
gen s_salud=.
replace s_salud=0 if segpop!=" " & atemed!=" " 
replace s_salud=1 if atemed=="1" & (inst_1=="1" |inst_2=="2" |inst_3=="3" |inst_4=="4") & (inscr_3=="3" |inscr_4=="4" |inscr_6=="6" |inscr_7=="7")
*valor monetario de las lineas de pobreza extrema por ingresos rural y urbana para el mes de agosto de 2022
scalar lpobrezaeurbana16=1348.81
scalar lpobrezaerural16=1015.44
scalar lpobreza_pam16=(lpobrezaeurbana16+lpobrezaerural16)/2 
gen pam=.
replace pam=0 if (edad>=65 & edad!=.)
replace pam=1 if (edad>=65 & edad!=.) & ingreso_pam>=lpobreza_pam16 & ingreso_pam!=.
label var pam "Programa de adultos mayores"
label define pam 0 "No recibe" 1 "Recibe"
label value pam pam
save ic_segsoc16.dta,replace

**2018**
use trabajos18.dta,replace
**tipo de trabajador**
gen tipo_trabajo=.
replace tipo_trabajo=1 if subor=="1"
replace tipo_trabajo=2 if subor=="2" & indep=="1" & tiene_suel=="1"
replace tipo_trabajo=2 if subor=="2" & indep=="2" & pago=="1"
replace tipo_trabajo=3 if subor=="2" & indep=="1" & tiene_suel=="2"
replace tipo_trabajo=3 if subor=="2" & indep=="2" & (pago=="2" |pago=="3")
*afore,sar
gen aforelaboral=.
replace aforelaboral=0 if pres_8==" " 
replace aforelaboral=1 if pres_8=="08" 
*ocupacion principal o secundaria
gen ocupa=.
replace ocupa=0 if id_trabajo=="2"
replace ocupa=1 if id_trabajo=="1"
destring id_trabajo,replace
keep folioviv foliohog numren id_trabajo tipo_trabajo aforelaboral ocupa
reshape wide tipo_trabajo aforelaboral ocupa,i(folioviv foliohog numren) j(id_trabajo)
recode ocupa2 (0=1) (.=0)
*label
label var tipo_trabajo1 "Tipo de trabajo 1"
label var tipo_trabajo2 "Tipo de trabajo 2"
label var aforelaboral1 "SAR o Afore (ocupación principal)"
label var aforelaboral2 "SAR o Afore (ocupación secundaria)"
label var ocupa1 "Ocupacion principal"
label var ocupa2 "Ocupación secundaria"
label define cuenta 0 "No cuenta" 1 "Sí cuenta"
label value aforelaboral1 cuenta
label value aforelaboral2 cuenta
label define ocupas 0 "Sin ocupacion secundaria" 1 "Con ocupacion secundaria"
label value ocupa2 ocupas
*poblacion trabajadora
gen trabajo=1
label var trabajo "Población con al menos un empleo"
*mantener variables
keep folioviv foliohog numren trabajo tipo_trabajo* aforelaboral* ocupa* 
egen idhogar=concat(folioviv foliohog)
label var idhogar "Identificador del hogar"
egen idpersona=concat(folioviv foliohog numren)
label var idpersona "Identificador de la persona"	
save prestaciones18.dta,replace

*ingresos*
use ingresos18.dta,clear
egen idhogar=concat(folioviv foliohog)
label var idhogar "Identificador del hogar"
egen idpersona=concat(folioviv foliohog numren)
label var idpersona "Identificador de la persona"
*mantener ingresos por jubilaciones, pensiones y programas de adultos mayores
keep if clave=="P032" |clave=="P033" |clave=="P044" |clave=="P045"
*Deflactar*
*La base es el mes de julio de 2023
gen ingresod1=(ing_1/77.5499092652)*100 if mes_1=="07"
replace ingresod1=(ing_1/78.0023596622)*100 if mes_1=="08"
replace ingresod1=(ing_1/78.3322466468)*100 if mes_1=="09"
replace ingresod1=(ing_1/78.7382016890)*100 if mes_1=="10"

gen ingresod2=(ing_2/77.1364761331)*100 if mes_2=="06"
replace ingresod2=(ing_2/77.5499092652)*100 if mes_2=="07"
replace ingresod2=(ing_2/78.0023596622)*100 if mes_2=="08"
replace ingresod2=(ing_2/78.3322466468)*100 if mes_2=="09"

gen ingresod3=(ing_3/76.8396672978)*100 if mes_3=="05"
replace ingresod3=(ing_3/77.1364761331)*100 if mes_3=="06"
replace ingresod3=(ing_3/77.5499092652)*100 if mes_3=="07"
replace ingresod3=(ing_3/78.0023596622)*100 if mes_3=="08"

gen ingresod4=(ing_4/76.9644552953)*100 if mes_4=="04"
replace ingresod4=(ing_4/76.8396672978)*100 if mes_4=="05"
replace ingresod4=(ing_4/77.1364761331)*100 if mes_4=="06"
replace ingresod4=(ing_4/77.5499092652)*100 if mes_4=="07"

gen ingresod5=(ing_5/77.2262768416)*100 if mes_5=="03"
replace ingresod5=(ing_5/76.9644552953)*100 if mes_5=="04"
replace ingresod5=(ing_5/76.8396672978)*100 if mes_5=="05"
replace ingresod5=(ing_5/77.1364761331)*100 if mes_5=="06"

gen ingresod6=(ing_6/76.9772839680)*100 if mes_6=="02"
replace ingresod6=(ing_6/77.2262768416)*100 if mes_6=="03"
replace ingresod6=(ing_6/76.9644552953)*100 if mes_6=="04"
replace ingresod6=(ing_6/76.8396672978)*100 if mes_6=="05"

egen ingreso_pensiones=rowmean(ingresod1 ingresod2 ingresod3 ingresod4 ingresod5 ingresod6) if clave=="P032" |clave=="P033"
replace ingreso_pensiones=0 if ingreso_pensiones==.
egen ingreso_pam=rowmean(ingresod1 ingresod2 ingresod3 ingresod4 ingresod5 ingresod6) if clave=="P044" |clave=="P045"
replace ingreso_pam=0 if ingreso_pam==.
label var ingreso_pensiones "Ingreso promedio mensual por jubilaciones y pensiones"
label var ingreso_pam "Ingreso promedio mensual por programas de adultos mayores"
collapse (sum) ingreso_pensiones ingreso_pam,by(idpersona)
save pensiones18.dta,replace

*base de datos poblacion
use poblacion18.dta,clear
egen idhogar=concat(folioviv foliohog)
label var idhogar "Identificador del hogar"
egen idpersona=concat(folioviv foliohog numren)
label var idpersona "Identificador de la persona"
drop if parentesco>="400" & parentesco<"500"
drop if parentesco>="700" & parentesco<"800"
*merge las bases pensiones y prestaciones
merge 1:m idpersona using prestaciones18.dta
drop _merge
merge 1:m idpersona using pensiones18.dta
drop _merge
gen año=2018
*otros nucleos familiares
gen s_salud=.
replace s_salud=0 if segpop!=" " & atemed!=" " 
replace s_salud=1 if atemed=="1" & (inst_1=="1" |inst_2=="2" |inst_3=="3" |inst_4=="4") & (inscr_3=="3" |inscr_4=="4" |inscr_6=="6" |inscr_7=="7")
*valor monetario de las lineas de pobreza extrema por ingresos rural y urbana para el mes de julio de 2022
scalar lpobrezaeurbana18=1521.44
scalar lpobrezaerural18=1145.50
scalar lpobreza_pam18=(lpobrezaeurbana18+lpobrezaerural18)/2 
gen pam=.
replace pam=0 if (edad>=65 & edad!=.)
replace pam=1 if (edad>=65 & edad!=.) & ingreso_pam>=lpobreza_pam18 & ingreso_pam!=.
label var pam "Programa de adultos mayores"
label define pam 0 "No recibe" 1 "Recibe"
label value pam pam
save ic_segsoc18.dta,replace

**2020**
use trabajos20.dta,replace
**tipo de trabajador**
gen tipo_trabajo=.
replace tipo_trabajo=1 if subor=="1"
replace tipo_trabajo=2 if subor=="2" & indep=="1" & tiene_suel=="1"
replace tipo_trabajo=2 if subor=="2" & indep=="2" & pago=="1"
replace tipo_trabajo=3 if subor=="2" & indep=="1" & tiene_suel=="2"
replace tipo_trabajo=3 if subor=="2" & indep=="2" & (pago=="2" |pago=="3")
*afore,sar
gen aforelaboral=.
replace aforelaboral=0 if pres_8==" " 
replace aforelaboral=1 if pres_8=="08" 
*ocupacion principal o secundaria
gen ocupa=.
replace ocupa=0 if id_trabajo=="2"
replace ocupa=1 if id_trabajo=="1"
destring id_trabajo,replace
keep folioviv foliohog numren id_trabajo tipo_trabajo aforelaboral ocupa
reshape wide tipo_trabajo aforelaboral ocupa,i(folioviv foliohog numren) j(id_trabajo)
recode ocupa2 (0=1) (.=0)
*label
label var tipo_trabajo1 "Tipo de trabajo 1"
label var tipo_trabajo2 "Tipo de trabajo 2"
label var aforelaboral1 "SAR o Afore (ocupación principal)"
label var aforelaboral2 "SAR o Afore (ocupación secundaria)"
label var ocupa1 "Ocupacion principal"
label var ocupa2 "Ocupación secundaria"
label define cuenta 0 "No cuenta" 1 "Sí cuenta"
label value aforelaboral1 cuenta
label value aforelaboral2 cuenta
label define ocupas 0 "Sin ocupacion secundaria" 1 "Con ocupacion secundaria"
label value ocupa2 ocupas
*poblacion trabajadora
gen trabajo=1
label var trabajo "Población con al menos un empleo"
*mantener variables
keep folioviv foliohog numren trabajo tipo_trabajo* aforelaboral* ocupa* 
egen idhogar=concat(folioviv foliohog)
label var idhogar "Identificador del hogar"
egen idpersona=concat(folioviv foliohog numren)
label var idpersona "Identificador de la persona"	
save prestaciones20.dta,replace

*ingresos*
use ingresos20.dta,clear
egen idhogar=concat(folioviv foliohog)
label var idhogar "Identificador del hogar"
egen idpersona=concat(folioviv foliohog numren)
label var idpersona "Identificador de la persona"
*mantener ingresos por jubilaciones, pensiones y programas de adultos mayores
keep if clave=="P032" |clave=="P033" |clave=="P104" |clave=="P045"
*Deflactar*
*La base es el mes de julio de 2023
gen ingresod1=(ing_1/83.3985345256)*100 if mes_1=="07"
replace ingresod1=(ing_1/83.7268691008)*100 if mes_1=="08"
replace ingresod1=(ing_1/83.9185916542)*100 if mes_1=="09"
replace ingresod1=(ing_1/84.4308867362)*100 if mes_1=="10"

gen ingresod2=(ing_2/82.8544150522)*100 if mes_2=="06"
replace ingresod2=(ing_2/83.3985345256)*100 if mes_2=="07"
replace ingresod2=(ing_2/83.7268691008)*100 if mes_2=="08"
replace ingresod2=(ing_2/83.9185916542)*100 if mes_2=="09"

gen ingresod3=(ing_3/82.4034401391)*100 if mes_3=="05"
replace ingresod3=(ing_3/82.8544150522)*100 if mes_3=="06"
replace ingresod3=(ing_3/83.3985345256)*100 if mes_3=="07"
replace ingresod3=(ing_3/83.7268691008)*100 if mes_3=="08"

gen ingresod4=(ing_4/82.0875248385)*100 if mes_4=="04"
replace ingresod4=(ing_4/82.4034401391)*100 if mes_4=="05"
replace ingresod4=(ing_4/82.8544150522)*100 if mes_4=="06"
replace ingresod4=(ing_4/83.3985345256)*100 if mes_4=="07"

gen ingresod5=(ing_5/82.9281544958)*100 if mes_5=="03"
replace ingresod5=(ing_5/82.0875248385)*100 if mes_5=="04"
replace ingresod5=(ing_5/82.4034401391)*100 if mes_5=="05"
replace ingresod5=(ing_5/82.8544150522)*100 if mes_5=="06"

gen ingresod6=(ing_6/82.9677409339)*100 if mes_6=="02"
replace ingresod6=(ing_6/82.9281544958)*100 if mes_6=="03"
replace ingresod6=(ing_6/82.0875248385)*100 if mes_6=="04"
replace ingresod6=(ing_6/82.4034401391)*100 if mes_6=="05"

egen ingreso_pensiones=rowmean(ingresod1 ingresod2 ingresod3 ingresod4 ingresod5 ingresod6) if clave=="P032" |clave=="P033"
replace ingreso_pensiones=0 if ingreso_pensiones==.
egen ingreso_pam=rowmean(ingresod1 ingresod2 ingresod3 ingresod4 ingresod5 ingresod6) if clave=="P104" |clave=="P045"
replace ingreso_pam=0 if ingreso_pam==.
label var ingreso_pensiones "Ingreso promedio mensual por jubilaciones y pensiones"
label var ingreso_pam "Ingreso promedio mensual por programas de adultos mayores"
collapse (sum) ingreso_pensiones ingreso_pam,by(idpersona)
save pensiones20.dta,replace

*base de datos poblacion
use poblacion20.dta,clear
egen idhogar=concat(folioviv foliohog)
label var idhogar "Identificador del hogar"
egen idpersona=concat(folioviv foliohog numren)
label var idpersona "Identificador de la persona"
drop if parentesco>="400" & parentesco<"500"
drop if parentesco>="700" & parentesco<"800"
*merge las bases pensiones y prestaciones
merge 1:m idpersona using prestaciones20.dta
drop _merge
merge 1:m idpersona using pensiones20.dta
drop _merge
gen año=2020
*otros nucleos familiares
gen s_salud=.
replace s_salud=0 if pop_insabi!=" " & atemed!=" " 
replace s_salud=1 if atemed=="1" & (inst_1=="1" |inst_2=="2" |inst_3=="3" |inst_4=="4") & (inscr_3=="3" |inscr_4=="4" |inscr_6=="6" |inscr_7=="7")
*valor monetario de las lineas de pobreza extrema por ingresos rural y urbana para el mes de julio de 2022
scalar lpobrezaeurbana20=1688.57
scalar lpobrezaerural20=1287.59
scalar lpobreza_pam20=(lpobrezaeurbana20+lpobrezaerural20)/2 
gen pam=.
replace pam=0 if (edad>=65 & edad!=.)
replace pam=1 if (edad>=65 & edad!=.) & ingreso_pam>=lpobreza_pam20 & ingreso_pam!=.
label var pam "Programa de adultos mayores"
label define pam 0 "No recibe" 1 "Recibe"
label value pam pam
save ic_segsoc20.dta,replace


**2022**
use trabajos22.dta,replace
**tipo de trabajador**
gen tipo_trabajo=.
replace tipo_trabajo=1 if subor=="1"
replace tipo_trabajo=2 if subor=="2" & indep=="1" & tiene_suel=="1"
replace tipo_trabajo=2 if subor=="2" & indep=="2" & pago=="1"
replace tipo_trabajo=3 if subor=="2" & indep=="1" & tiene_suel=="2"
replace tipo_trabajo=3 if subor=="2" & indep=="2" & (pago=="2" |pago=="3")
*afore,sar
gen aforelaboral=.
replace aforelaboral=0 if pres_8==" " 
replace aforelaboral=1 if pres_8=="08" 
*ocupacion principal o secundaria
gen ocupa=.
replace ocupa=0 if id_trabajo=="2"
replace ocupa=1 if id_trabajo=="1"
destring id_trabajo,replace
keep folioviv foliohog numren id_trabajo tipo_trabajo aforelaboral ocupa
reshape wide tipo_trabajo aforelaboral ocupa,i(folioviv foliohog numren) j(id_trabajo)
recode ocupa2 (0=1) (.=0)
*label
label var tipo_trabajo1 "Tipo de trabajo 1"
label var tipo_trabajo2 "Tipo de trabajo 2"
label var aforelaboral1 "SAR o Afore (ocupación principal)"
label var aforelaboral2 "SAR o Afore (ocupación secundaria)"
label var ocupa1 "Ocupacion principal"
label var ocupa2 "Ocupación secundaria"
label define cuenta 0 "No cuenta" 1 "Sí cuenta"
label value aforelaboral1 cuenta
label value aforelaboral2 cuenta
label define ocupas 0 "Sin ocupacion secundaria" 1 "Con ocupacion secundaria"
label value ocupa2 ocupas
*poblacion trabajadora
gen trabajo=1
label var trabajo "Población con al menos un empleo"
*mantener variables
keep folioviv foliohog numren trabajo tipo_trabajo* aforelaboral* ocupa* 
egen idhogar=concat(folioviv foliohog)
label var idhogar "Identificador del hogar"
egen idpersona=concat(folioviv foliohog numren)
label var idpersona "Identificador de la persona"
save prestaciones22.dta,replace

*ingresos*
use ingresos22.dta,clear
egen idhogar=concat(folioviv foliohog)
label var idhogar "Identificador del hogar"
egen idpersona=concat(folioviv foliohog numren)
label var idpersona "Identificador de la persona"
*mantener ingresos por jubilaciones, pensiones y programas de adultos mayores
keep if clave=="P032" |clave=="P033" |clave=="P104" |clave=="P045"
*Deflactar*
*La base es el mes de julio de 2023
gen ingresod1=(ing_1/95.4328117238)*100 if mes_1=="07"
replace ingresod1=(ing_1/96.0964667163)*100 if mes_1=="08"
replace ingresod1=(ing_1/96.6925919026)*100 if mes_1=="09"
replace ingresod1=(ing_1/97.2398161947)*100 if mes_1=="10"

gen ingresod2=(ing_2/94.7311227024)*100 if mes_2=="06"
replace ingresod2=(ing_2/95.4328117238)*100 if mes_2=="07"
replace ingresod2=(ing_2/96.0964667163)*100 if mes_2=="08"
replace ingresod2=(ing_2/96.6925919026)*100 if mes_2=="09"

gen ingresod3=(ing_3/93.9378415301)*100 if mes_3=="05"
replace ingresod3=(ing_3/94.7311227024)*100 if mes_3=="06"
replace ingresod3=(ing_3/95.4328117238)*100 if mes_3=="07"
replace ingresod3=(ing_3/96.0964667163)*100 if mes_3=="08"

gen ingresod4=(ing_4/93.7725099354)*100 if mes_4=="04"
replace ingresod4=(ing_4/93.9378415301)*100 if mes_4=="05"
replace ingresod4=(ing_4/94.7311227024)*100 if mes_4=="06"
replace ingresod4=(ing_4/95.4328117238)*100 if mes_4=="07"

gen ingresod5=(ing_5/93.2679769001)*100 if mes_5=="03"
replace ingresod5=(ing_5/93.7725099354)*100 if mes_5=="04"
replace ingresod5=(ing_5/93.9378415301)*100 if mes_5=="05"
replace ingresod5=(ing_5/94.7311227024)*100 if mes_5=="06"

gen ingresod6=(ing_6/92.3536077993)*100 if mes_6=="02"
replace ingresod6=(ing_6/93.2679769001)*100 if mes_6=="03"
replace ingresod6=(ing_6/93.7725099354)*100 if mes_6=="04"
replace ingresod6=(ing_6/93.9378415301)*100 if mes_6=="05"

egen ingreso_pensiones=rowmean(ingresod1 ingresod2 ingresod3 ingresod4 ingresod5 ingresod6) if clave=="P032" |clave=="P033"
replace ingreso_pensiones=0 if ingreso_pensiones==.
egen ingreso_pam=rowmean(ingresod1 ingresod2 ingresod3 ingresod4 ingresod5 ingresod6) if clave=="P104" |clave=="P045"
replace ingreso_pam=0 if ingreso_pam==.
label var ingreso_pensiones "Ingreso promedio mensual por jubilaciones y pensiones"
label var ingreso_pam "Ingreso promedio mensual por programas de adultos mayores"
collapse (sum) ingreso_pensiones ingreso_pam,by(idpersona)
save pensiones22.dta,replace

*base de datos poblacion
use poblacion22.dta,clear
egen idhogar=concat(folioviv foliohog)
label var idhogar "Identificador del hogar"
egen idpersona=concat(folioviv foliohog numren)
label var idpersona "Identificador de la persona"
drop if parentesco>="400" & parentesco<"500"
drop if parentesco>="700" & parentesco<"800"
*merge las bases pensiones y prestaciones
merge 1:m idpersona using prestaciones22.dta
drop _merge
merge 1:m idpersona using pensiones22.dta
drop _merge
gen año=2022
*otros nucleos familiares
gen s_salud=.
replace s_salud=0 if pop_insabi!=" " & atemed!=" " 
replace s_salud=1 if atemed=="1" & (inst_1=="1" |inst_2=="2" |inst_3=="3" |inst_4=="4") & (inscr_3=="3" |inscr_4=="4" |inscr_6=="6" |inscr_7=="7")
**valor monetario de las lineas de pobreza extrema por ingresos rural y urbana para el mes de agosto julio de 2022
scalar lpobrezaeurbana22=2042.89 
scalar lpobrezaerural22=1566.95 
scalar lpobreza_pam22=(lpobrezaeurbana22+lpobrezaerural22)/2 
gen pam=.
replace pam=0 if (edad>=65 & edad!=.)
replace pam=1 if (edad>=65 & edad!=.) & ingreso_pam>=lpobreza_pam22 & ingreso_pam!=.
label var pam "Programa de adultos mayores"
label define pam 0 "No recibe" 1 "Recibe"
label value pam pam
save ic_segsoc22.dta,replace

*loop
set more off

local flist ic_segsoc16.dta ic_segsoc18.dta ic_segsoc20.dta ic_segsoc22.dta

foreach file in `flist' {
	use `file'
rename *, lower
*PEA
gen pea=.
replace pea=1 if trabajo==1 & (edad>=16 & edad!=.)
replace pea=2 if (act_pnea1=="1" |act_pnea2=="1") & (edad>=16 & edad!=.)
replace pea=0 if (edad>=16 & edad!=.) & (act_pnea1!="1" & act_pnea2!="1") & ((act_pnea1>="2" & act_pnea1<="6") | (act_pnea2>="2" & act_pnea2=="6")) & pea==.
label var pea "Poblacion economicamente activa"
label define pea 0 "PNEA" 1 "PEA:ocupado" 2 "PEA:desocupado"
label value pea pea
*tipo de trabajo
replace tipo_trabajo1=tipo_trabajo1 if pea==1
replace tipo_trabajo1=. if (pea==0 |pea==2)
replace tipo_trabajo1=. if pea==.
label define tipo_trabajo 1 "Depende de un patrón, jefe o superior" 2 "No depende de un jefe y recibe o tiene asignado un sueldo" 3 "No depende de un jefe y no recibe o no tiene asignado o un sueldo"
label value tipo_trabajo1 tipo_trabajo
replace tipo_trabajo2=tipo_trabajo2 if pea==1
replace tipo_trabajo2=. if (pea==0 |pea==2)
replace tipo_trabajo2=. if pea==.
label value tipo_trabajo2 tipo_trabajo
*jubilados o pensionados
gen jubilados=0
replace jubilados=1 if trabajo_mp=="2" & (act_pnea1=="2" |act_pnea2=="2")
replace jubilados=1 if ingreso_pensiones>0 & ingreso_pensiones!=.
replace jubilados=1 if inscr_2=="2"
label var jubilados "Poblacion pensionada o jubilada"
label define jubilados 0 "Poblacion no pensionada o jubilada" 1 "Poblacion pensionada o jubilada"
label value jubilados jubilados
**Prestaciones basicas**
*prestaciones laborales (servicios medicos)
*con ocupacion principal
gen smedlprinc=.
replace smedlprinc=0 if ocupa1==1
replace smedlprinc=1 if ocupa1==1 & atemed=="1" & (inst_1=="1" |inst_2=="2" |inst_3=="3" |inst_4=="4") & (inscr_1=="1")
label var smedlprinc "Servicios medicos por prestacion laboral en ocupacion principal"
label define cmedica 0 "Sin servicios medicos" 1 "Con servicios medicos"
label value smedlprinc cmedica
*con ocupacion secundaria
gen smedlsec=.
replace smedlsec=0 if ocupa2==1
replace smedlsec=1 if ocupa2==1 & atemed=="1" & (inst_1=="1" |inst_2=="2" |inst_3=="3" |inst_4=="4") & (inscr_1=="1")
label var smedlsec "Servicios medicos por prestacion laboral en ocupacion secundaria"
label value smedlsec cmedica
*Contratacion voluntaria: servicios medicos y ahorro para el retiro o pension para la vejez (SAR o Afore)
*servicios medicos*
gen smedcontvol=.
replace smedcontvol=0 if (edad>=12 & edad!=.)
replace smedcontvol=1 if atemed=="1" & (inst_1=="1" |inst_2=="2" |inst_3=="3" |inst_4=="4") & (inscr_6=="6") & (edad>=12 & edad!=.)
label var smedcontvol "Servicios medicos de contratacion voluntaria"
label value smedcontvol cuenta
*Ahorro para el retiro o pension para la vejez (SAR o Afore)
gen aforecv=.
replace aforecv=0 if segvol_1==" " & (edad>=12 & edad!=.)
replace aforecv=1 if segvol_1=="1" & (edad>=12 & edad!=.)
label var aforecv "SAR o Afore"
label value aforecv cuenta
*Acceso directo a la seguridad social*
gen ss_directo=0
replace ss_directo=1 if tipo_trabajo1==1 & smedlprinc==1
replace ss_directo=1 if tipo_trabajo1==2 & ((smedlprinc==1 |smedcontvol==1) & (aforelaboral1==1 |aforecv==1))
replace ss_directo=1 if tipo_trabajo1==3 & ((smedlprinc==1 |smedcontvol==1) & aforecv==1)
replace ss_directo=1 if tipo_trabajo2==1 & smedlsec==1
replace ss_directo=1 if tipo_trabajo2==2 & ((smedlsec==1 |smedcontvol==1) & (aforelaboral2==1 |aforecv==1))
replace ss_directo=1 if tipo_trabajo2==3 & ((smedlsec==1 |smedcontvol==1) & aforecv==1)
replace ss_directo=1 if jubilados==1
label var ss_directo "Acceso direcot a la seguridad social"
label define ss_directo 0 "Sin acceso" 1 "Con acceso"
label value ss_directo ss_directo
*Nucleos familiares
gen pariente=0
replace pariente=1 if (parentesco>="100" & parentesco<"200")
replace pariente=2 if (parentesco>="200" & parentesco<"300")
replace pariente=3 if (parentesco>="300" & parentesco<"400")
replace pariente=4 if parentesco=="601"
replace pariente=5 if parentesco=="615"
label var pariente "Integrantes que tienen acceso por otros miembros"
label define pariente 0 "Sin parentesco directo" 1 "Jefe o jefa de hogar" 2 "Conyuge del jefe/a" 3 "Hijo de jefe/a" 4 "Padre o madre del jefe/a" 5 "Suegro del jefe/a"
label value pariente pariente
*Inasistencia a la escuela*
gen inasis_escuela=.
replace inasis_escuela=0 if asis_esc=="1"
replace inasis_escuela=1 if asis_esc=="2"
label var inasis_escuela "Inasistencia a la escuela"
label define inasis_escuela 0 "Sí asiste" 1 "No asiste"
label value inasis_escuela inasis_escuela
**¿El miembro cuenta con acceso directo a la salud por medio de la relacion del parentesco?**
*jefe de hogar
gen jefe=1 if pariente==1 & ss_directo==1
replace jefe=. if pariente==1 & ss_directo==1 & (((inst_2=="2" | inst_3=="3") & inscr_6=="6") & (inst_1==" " & inst_4==" " & inst_6==" ") & (inscr_1==" " & inscr_2==" " & inscr_3==" " & inscr_4==" " & inscr_5==" " & inscr_7==" "))
egen jefe_ss=sum(jefe), by(idhogar)
label var jefe_ss "Acceso directo a la seguridad social de la jefatura de hogar"
label value jefe_ss cuenta
*conyuge
gen conyuge=1 if pariente==2 & ss_directo==1
replace conyuge=. if pariente==2 & ss_directo==1 & (((inst_2=="2" | inst_3=="3") & inscr_6=="6") & (inst_1==" " & inst_4==" " & inst_6==" ") & (inscr_1==" " & inscr_2==" " & inscr_3==" " & inscr_4==" " & inscr_5==" " & inscr_7==" "))
egen conyuge_ss=sum(conyuge),by(idhogar)
replace conyuge_ss=1 if conyuge_ss>=1 & conyuge_ss!=.
label var conyuge_ss "Acceso directo a la seguridad social del conyuge de la jefatura de hogar"
label value conyuge_ss cuenta
*hijo
gen hijo=1 if pariente==3 & ss_directo==1 & jubilados==0
replace hijo=1 if pariente==3 & ss_directo==1 & jubilados==1 & (edad>25 & edad!=.)
replace hijo=. if pariente==3 & ss_directo==1 & (((inst_2=="2" | inst_3=="3") & inscr_6=="6") & (inst_1==" " & inst_4==" " & inst_6==" ") & (inscr_1==" " & inscr_2==" " & inscr_3==" " & inscr_4==" " & inscr_5==" " & inscr_7==" "))
egen hijo_ss=sum(hijo),by(idhogar)
replace hijo_ss=1 if hijo_ss>=1 & hijo_ss!=.
label var hijo_ss "Acceso directo a la seguridad social de hijos(as) de la jefatura de hogar"
label value hijo_ss cuenta
*otros nucleos familiares
label var s_salud "Servicios medicos por otros nucleos familiares o por contratacion propia"
label value s_salud cuenta
*Indicador de carencia por acceso a la seguridad social*
gen ic_seguridadsocial=1
replace ic_seguridadsocial=0 if ss_directo==1
replace ic_seguridadsocial=0 if pariente==1 & conyuge_ss==1
replace ic_seguridadsocial=0 if pariente==1 & pea==0 & hijo_ss==1
replace ic_seguridadsocial=0 if pariente==2 & jefe_ss==1
replace ic_seguridadsocial=0 if pariente==2 & pea==0 & hijo_ss==1
replace ic_seguridadsocial=0 if pariente==3 & edad<16 & jefe_ss==1
replace ic_seguridadsocial=0 if pariente==3 & edad<16 & conyuge_ss==1
replace ic_seguridadsocial=0 if pariente==3 & (edad>=16 & edad<=25) & inasis_escuela==0 & jefe_ss==1
replace ic_seguridadsocial=0 if pariente==3 & (edad>=16 & edad<=25) & inasis_escuela==0 & conyuge_ss==1 
replace ic_seguridadsocial=0 if pariente==4 & pea==0 & jefe_ss==1
replace ic_seguridadsocial=0 if pariente==5 & pea==0 & conyuge_ss==1
replace ic_seguridadsocial=0 if s_salud==1
replace ic_seguridadsocial=0 if pam==1
label var ic_seguridadsocial "Indicador de carencia por acceso a la seguridad social"
label define ic_seguridadsocial 0 "No presenta carencia" 1 "Presenta carencia"
label value ic_seguridadsocial ic_seguridadsocial
save,replace
clear
}

************************************
****Bienestar economico (ingresos)**
************************************

********
**2016**
********
*Ingreso Monetario*
use trabajos16.dta,clear
keep folioviv foliohog numren id_trabajo pres_8 
destring pres_8 id_trabajo,replace
reshape wide pres_8,i(folioviv foliohog numren) j(id_trabajo)
egen idhogar=concat(folioviv foliohog)
label var idhogar "Identificador del Hogar"
egen idpersona=concat(folioviv foliohog numren)
label var idpersona "Identificador de la persona"
gen trabajo=1
label var trabajo "Poblacion con al menos un empleo"
gen aguinaldo1=0
replace aguinaldo1=1 if pres_81==8
gen aguinaldo2=0
replace aguinaldo2=1 if pres_82==8
label var aguinaldo1 "Aguinaldo por trabajo principal"
label var aguinaldo2 "Aguinaldo por trabajo secundario"
label define aguinaldo 0 "No recibe aguinaldo" 1 "Recibe aguinaldo"
label value aguinaldo1 aguinaldo
label value aguinaldo2 aguinaldo
save aguinaldo16.dta,replace


**base ingresos
use ingresos16.dta,clear
egen idhogar=concat(folioviv foliohog)
label var idhogar "Identificador del Hogar"
egen idpersona=concat(folioviv foliohog numren)
label var idpersona "Identificador de la persona"
*merge con aguinaldos
merge m:1 idpersona using aguinaldo16.dta
drop _merge
*delimitar
drop if clave=="P009" & aguinaldo1!=1
drop if clave=="P016" & aguinaldo2!=1
*deflactar el ingreso de los hogares a precios de agosto de 2023. 
gen ingresod1=(ing_1/69.5144952171)*100 if mes_1=="07"
replace ingresod1=(ing_1/69.7104240356)*100 if mes_1=="08"
replace ingresod1=(ing_1/70.1361027189)*100 if mes_1=="09"
replace ingresod1=(ing_1/70.5617814021)*100 if mes_1=="10"

gen ingresod2=(ing_2/69.3337275570)*100 if mes_2=="06"
replace ingresod2=(ing_2/69.5144952171)*100 if mes_2=="07"
replace ingresod2=(ing_2/69.7104240356)*100 if mes_2=="08"
replace ingresod2=(ing_2/70.1361027189)*100 if mes_2=="09"

gen ingresod3=(ing_3/69.2573386427)*100 if mes_3=="05"
replace ingresod3=(ing_3/69.3337275570)*100 if mes_3=="06"
replace ingresod3=(ing_3/69.5144952171)*100 if mes_3=="07"
replace ingresod3=(ing_3/69.7104240356)*100 if mes_3=="08"

gen ingresod4=(ing_4/69.5675592721)*100 if mes_4=="04"
replace ingresod4=(ing_4/69.2573386427)*100 if mes_4=="05"
replace ingresod4=(ing_4/69.3337275570)*100 if mes_4=="06"
replace ingresod4=(ing_4/69.5144952171)*100 if mes_4=="07"

gen ingresod5=(ing_5/69.7885623145)*100 if mes_5=="03"
replace ingresod5=(ing_5/69.5675592721)*100 if mes_5=="04"
replace ingresod5=(ing_5/69.2573386427)*100 if mes_5=="05"
replace ingresod5=(ing_5/69.3337275570)*100 if mes_5=="06"

gen ingresod6=(ing_6/69.6859329333)*100 if mes_6=="02"
replace ingresod6=(ing_6/69.7885623145)*100 if mes_6=="03"
replace ingresod6=(ing_6/69.5675592721)*100 if mes_6=="04"
replace ingresod6=(ing_6/69.2573386427)*100 if mes_6=="05"

*deflactar los repartos de utilidades y los aguinaldos                  
replace ingresod1=((ing_1/69.2573386427)*100)/12 if clave=="P008" |clave=="P015"
replace ingresod1=((ing_1/69.1185557295)*100)/12 if clave=="P009" |clave=="P016"
recode ingresod1 ingresod2 ingresod3 ingresod4 ingresod5 ingresod6 (0=.) 
 
*ingreso promedio mensual
egen ingresopromediodeflac=rowmean(ingresod1 ingresod2 ingresod3 ingresod4 ingresod5 ingresod6)
*ingreso promedio monetario
gen ingresomonetariop=ingresopromediodeflac if clave>="P001" & clave<="P009" |clave>="P011" & clave<="P016" |clave>="P018" & clave<="P048" |clave>="P067" & clave<="P081" |clave>="P101" & clave<="P108"
*gen ingreso laboral promedio*
gen ingresolaboralp=ingresopromediodeflac if clave>="P001" & clave<="P009" |clave>="P011" & clave<="P016" |clave>="P018" & clave<="P022" |clave>="P067" & clave<="P081"
*ingreso por rentas*
gen ingresorentasp=ingresopromediodeflac if clave>="P023" & clave<="P031"
*ingreso por transferencias
gen ingresotransferenciasp=ingresopromediodeflac if clave>="P032" & clave<="P048" |clave>="P101" & clave<="P108"
*collapse
collapse (sum) ingresopromediodeflac ingresomonetariop ingresolaboralp ingresorentasp ingresotransferenciasp,by(idhogar)
label var ingresopromediodeflac "Ingreso corriente total del hogar"
label var ingresomonetariop "Ingreso corriente monetario del hogar"
label var ingresolaboralp "Ingreso corriente monetario laboral"
label var ingresorentasp "Ingreso corriente monetario por rentas"
label var ingresotransferenciasp "ingreso corriente monetario por transferencias"
save ingresodeflactado16.dta,replace

*Ingreso no monetario*
use gastoshogar16.dta,clear
gen base=1
append using gastospersona16.dta
recode base (.=2)
replace frecuencia=frec_rem if base==2
label var base "Origen del monto obtenido"
label define base 1 "Monto del Hogar" 2 "Monto de la persona"
label value base base
egen idhogar=concat(folioviv foliohog)
label var idhogar "Identificador del Hogar"
egen idpersona=concat(folioviv foliohog numren)
label var idpersona "Identificador de la persona"
**decena de levantamiento**
gen d_lev=substr(folioviv,8,1)
label var d_lev "Decena de levantamiento"
destring d_lev,replace
**deflactores**
*alimentos semanal (sin incluir las bebidas alcoholicas)
scalar dalimentosw716=60.257290193385
scalar dalimentosw816=60.345045794565
scalar dalimentosw916=61.358436274152
scalar dalimentosw1016=61.548417815005
scalar dalimentosw1116=61.860230270263
*bebidas alcoholicas y tabaco
scalar dbatw716=66.622326449046
scalar dbatw816=66.890921457696
scalar dbatw916=67.337383771693
scalar dbatw1016=67.478857478539
scalar dbatw1116=67.566509666476
*Ropa,vestido, calzado y accesorios
scalar drvcat516=78.634856674676
scalar drvcat616=78.856313253458
scalar drvcat716=79.161157803264
scalar drvcat816=79.692926995525
*vivienda
scalar dviviendam716=83.73028951
scalar dviviendam816=83.58556161
scalar dviviendam916=83.40325242
scalar dviviendam1016=84.70420435
scalar dviviendam1116=86.58246869
*accesorios y articulos de limpieza para el hogar (mensual)
scalar daalhm716=64.68058321
scalar daalhm816=65.09134345
scalar daalhm916=65.36215677
scalar daalhm1016=65.45830351
scalar daalhm1116=65.66074581
*accesorios y articulos de limpieza para el hogar (trimestral)
scalar daalht516=64.652273333999
scalar daalht616=64.834239943238
scalar daalht716=65.044694475646
scalar daalht816=65.303934576480
*muebles y aparatos domesticos (semestral)
scalar dmads216=89.53280143
scalar dmads316=89.31899024
scalar dmads416=89.15081464
scalar dmads516=89.0346129
*salud (trimestral)
scalar dsaludt516=70.626987591248
scalar dsaludt616=70.811017118143
scalar dsaludt716=70.994858090195
scalar dsaludt816=71.192652120639
*transporte publico urbano (semanal)
scalar dtpurbanow716=75.572503306773
scalar dtpurbanow816=75.586392202929
scalar dtpurbanow916=75.665498524510
scalar dtpurbanow1016=75.813445461817
scalar dtpurbanow1116=75.893155648448
*transporte (mensual)
scalar dtransportem716=65.64377426
scalar dtransportem816=66.25487137
scalar dtransportem916=66.58024069
scalar dtransportem1016=66.90046176
scalar dtransportem1116=67.16353726
*transporte (semestral)
scalar dtransportes216=64.59396055
scalar dtransportes316=64.90748889
scalar dtransportes416=65.2718991
scalar dtransportes516=65.71155953
*educacion y esparcimiento 
scalar deducacionyespm716=77.87597033
scalar deducacionyespm816=77.89337451
scalar deducacionyespm916=78.89766015
scalar deducacionyespm1016=78.91957653
scalar deducacionyespm1116=79.13036049
*accesorios y cuidados del vestido (mensual)
scalar dacvm716=76.64940547
scalar dacvm816=77.24063198
scalar dacvm916=77.46623157
scalar dacvm1016=77.47012122
scalar dacvm1116=77.68210704
*accesorios y cuidados del vestido (trimestral)
scalar dacvt516=76.574421699848
scalar dacvt616=76.856205095585
scalar dacvt716=77.118756342379
scalar dacvt816=77.392328258792
*inpc semestral
scalar dinpcs216=69.52460266
scalar dinpcs316=69.52868451
scalar dinpcs416=69.58660791
scalar dinpcs516=69.7523116
*regalos y especie
gen gastonomonetario=gas_nm_tri/3
label var gastonomonetario "Gasto no monetario mensual"
gen especie=1 if tipo_gasto=="G4"
gen regalos=1 if tipo_gasto=="G5"
replace regalos=1 if tipo_gasto=="G6"
drop if tipo_gasto=="G2" |tipo_gasto=="G3" |tipo_gasto=="G7"
label var especie "Gasto no monetario por remuneraciones en especie"
label var regalos "Gasto no monetario por regalos de otro hogar y transferencias de instituciones"
*control de la frecuencia de los regalos recibidos en el hogar
drop if ((frecuencia>="5" & frecuencia<="6") |frecuencia==" " |frecuencia=="0") & base==1 & tipo_gasto=="G5"
drop if (frecuencia=="9" |frecuencia=="") & base==2 & tipo_gasto=="G5"
**Hora de deflactar
*Gasto no monetario en alimentos deflactado
gen gnm_alimentos=gastonomonetario if (clave>="A001" & clave<="A222") |(clave>="A242" & clave<="A247")
replace gnm_alimentos=(gnm_alimentos/dalimentosw816)*100 if d_lev==1
replace gnm_alimentos=(gnm_alimentos/dalimentosw816)*100 if d_lev==2
replace gnm_alimentos=(gnm_alimentos/dalimentosw816)*100 if d_lev==3
replace gnm_alimentos=(gnm_alimentos/dalimentosw916)*100 if d_lev==4
replace gnm_alimentos=(gnm_alimentos/dalimentosw916)*100 if d_lev==5
replace gnm_alimentos=(gnm_alimentos/dalimentosw916)*100 if d_lev==6
replace gnm_alimentos=(gnm_alimentos/dalimentosw1016)*100 if d_lev==7
replace gnm_alimentos=(gnm_alimentos/dalimentosw1016)*100 if d_lev==8
replace gnm_alimentos=(gnm_alimentos/dalimentosw1016)*100 if d_lev==9
replace gnm_alimentos=(gnm_alimentos/dalimentosw1116)*100 if d_lev==0
label var gnm_alimentos "Gasto no monetario en alimentos deflactado"
*Gasto no monetario en alcohol y tabaco deflactado
gen gnm_alcoholytabaco=gastonomonetario if clave>="A223" & clave<="A241"
replace gnm_alcoholytabaco=(gnm_alcoholytabaco/dbatw816)*100 if d_lev==1
replace gnm_alcoholytabaco=(gnm_alcoholytabaco/dbatw816)*100 if d_lev==2
replace gnm_alcoholytabaco=(gnm_alcoholytabaco/dbatw816)*100 if d_lev==3
replace gnm_alcoholytabaco=(gnm_alcoholytabaco/dbatw916)*100 if d_lev==4
replace gnm_alcoholytabaco=(gnm_alcoholytabaco/dbatw916)*100 if d_lev==5
replace gnm_alcoholytabaco=(gnm_alcoholytabaco/dbatw916)*100 if d_lev==6
replace gnm_alcoholytabaco=(gnm_alcoholytabaco/dbatw1016)*100 if d_lev==7
replace gnm_alcoholytabaco=(gnm_alcoholytabaco/dbatw1016)*100 if d_lev==8
replace gnm_alcoholytabaco=(gnm_alcoholytabaco/dbatw1016)*100 if d_lev==9
replace gnm_alcoholytabaco=(gnm_alcoholytabaco/dbatw1116)*100 if d_lev==0
label var gnm_alcoholytabaco "Gasto no monetario en alcohol y tabaco deflactado"
*Gasto no monetario en vestido y calzado deflactado
gen gnm_vestidoycalzado=gastonomonetario if (clave>="H001" & clave<="H122") |clave=="H136"
replace gnm_vestidoycalzado=(gnm_vestidoycalzado/drvcat516)*100 if d_lev==1
replace gnm_vestidoycalzado=(gnm_vestidoycalzado/drvcat516)*100 if d_lev==2
replace gnm_vestidoycalzado=(gnm_vestidoycalzado/drvcat616)*100 if d_lev==3
replace gnm_vestidoycalzado=(gnm_vestidoycalzado/drvcat616)*100 if d_lev==4
replace gnm_vestidoycalzado=(gnm_vestidoycalzado/drvcat616)*100 if d_lev==5
replace gnm_vestidoycalzado=(gnm_vestidoycalzado/drvcat716)*100 if d_lev==6
replace gnm_vestidoycalzado=(gnm_vestidoycalzado/drvcat716)*100 if d_lev==7
replace gnm_vestidoycalzado=(gnm_vestidoycalzado/drvcat716)*100 if d_lev==8
replace gnm_vestidoycalzado=(gnm_vestidoycalzado/drvcat816)*100 if d_lev==9
replace gnm_vestidoycalzado=(gnm_vestidoycalzado/drvcat816)*100 if d_lev==0
label var gnm_vestidoycalzado "Gasto no monetario en vestido y calzado deflactado"
*Gasto no monetario en viviendas deflactado
gen gnm_viviendas=gastonomonetario if (clave>="G001" & clave<="G016") |(clave>="R001" & clave<="R004") |clave=="R013"
replace gnm_viviendas=(gnm_viviendas/dviviendam716)*100 if d_lev==1
replace gnm_viviendas=(gnm_viviendas/dviviendam716)*100 if d_lev==2
replace gnm_viviendas=(gnm_viviendas/dviviendam816)*100 if d_lev==3
replace gnm_viviendas=(gnm_viviendas/dviviendam816)*100 if d_lev==4
replace gnm_viviendas=(gnm_viviendas/dviviendam816)*100 if d_lev==5
replace gnm_viviendas=(gnm_viviendas/dviviendam916)*100 if d_lev==6
replace gnm_viviendas=(gnm_viviendas/dviviendam916)*100 if d_lev==7
replace gnm_viviendas=(gnm_viviendas/dviviendam916)*100 if d_lev==8
replace gnm_viviendas=(gnm_viviendas/dviviendam1016)*100 if d_lev==9
replace gnm_viviendas=(gnm_viviendas/dviviendam1016)*100 if d_lev==0
label var gnm_viviendas "Gasto no monetario en viviendas deflactado"
*Gasto no monetario en Articulos de limpieza deflactado
gen gnm_alimpieza=gastonomonetario if clave>="C001" & clave<="C024"
replace gnm_alimpieza=(gnm_alimpieza/daalhm716)*100 if d_lev==1
replace gnm_alimpieza=(gnm_alimpieza/daalhm716)*100 if d_lev==2
replace gnm_alimpieza=(gnm_alimpieza/daalhm816)*100 if d_lev==3
replace gnm_alimpieza=(gnm_alimpieza/daalhm816)*100 if d_lev==4
replace gnm_alimpieza=(gnm_alimpieza/daalhm816)*100 if d_lev==5
replace gnm_alimpieza=(gnm_alimpieza/daalhm916)*100 if d_lev==6
replace gnm_alimpieza=(gnm_alimpieza/daalhm916)*100 if d_lev==7
replace gnm_alimpieza=(gnm_alimpieza/daalhm916)*100 if d_lev==8
replace gnm_alimpieza=(gnm_alimpieza/daalhm1016)*100 if d_lev==9
replace gnm_alimpieza=(gnm_alimpieza/daalhm1016)*100 if d_lev==0
label var gnm_alimpieza "Gasto no monetario en accesorios y articulos de limpieza"
*Gasto no monetario en cristaleria y blancos deflactado
gen gnm_cristaleria=gastonomonetario if clave>="I001" & clave<="I026"
replace gnm_cristaleria=(gnm_cristaleria/daalht516)*100 if d_lev==1
replace gnm_cristaleria=(gnm_cristaleria/daalht516)*100 if d_lev==2
replace gnm_cristaleria=(gnm_cristaleria/daalht616)*100 if d_lev==3
replace gnm_cristaleria=(gnm_cristaleria/daalht616)*100 if d_lev==4
replace gnm_cristaleria=(gnm_cristaleria/daalht616)*100 if d_lev==5
replace gnm_cristaleria=(gnm_cristaleria/daalht716)*100 if d_lev==6
replace gnm_cristaleria=(gnm_cristaleria/daalht716)*100 if d_lev==7
replace gnm_cristaleria=(gnm_cristaleria/daalht716)*100 if d_lev==8
replace gnm_cristaleria=(gnm_cristaleria/daalht816)*100 if d_lev==9
replace gnm_cristaleria=(gnm_cristaleria/daalht816)*100 if d_lev==0
label var gnm_cristaleria "Gasto no monetario en cristaleria y blancos deflactado"
*Gasto no monetario en enseres domesticos y muebles deflactado
gen gnm_edomesticos=gastonomonetario if clave>="K001" & clave<="K037"
replace gnm_edomesticos=(gnm_edomesticos/dmads216)*100 if d_lev==1
replace gnm_edomesticos=(gnm_edomesticos/dmads216)*100 if d_lev==2
replace gnm_edomesticos=(gnm_edomesticos/dmads316)*100 if d_lev==3
replace gnm_edomesticos=(gnm_edomesticos/dmads316)*100 if d_lev==4
replace gnm_edomesticos=(gnm_edomesticos/dmads316)*100 if d_lev==5
replace gnm_edomesticos=(gnm_edomesticos/dmads416)*100 if d_lev==6
replace gnm_edomesticos=(gnm_edomesticos/dmads416)*100 if d_lev==7
replace gnm_edomesticos=(gnm_edomesticos/dmads416)*100 if d_lev==8
replace gnm_edomesticos=(gnm_edomesticos/dmads516)*100 if d_lev==9
replace gnm_edomesticos=(gnm_edomesticos/dmads516)*100 if d_lev==0
label var gnm_edomesticos "Gasto no monetario en enseres domesticos y muebles deflactado"
*Gasto no monetario en salud deflactado
gen gnm_salud=gastonomonetario if clave>="J001" & clave<="J072"
replace gnm_salud=(gnm_salud/dsaludt516)*100 if d_lev==1
replace gnm_salud=(gnm_salud/dsaludt516)*100 if d_lev==2
replace gnm_salud=(gnm_salud/dsaludt616)*100 if d_lev==3
replace gnm_salud=(gnm_salud/dsaludt616)*100 if d_lev==4
replace gnm_salud=(gnm_salud/dsaludt616)*100 if d_lev==5
replace gnm_salud=(gnm_salud/dsaludt716)*100 if d_lev==6
replace gnm_salud=(gnm_salud/dsaludt716)*100 if d_lev==7
replace gnm_salud=(gnm_salud/dsaludt716)*100 if d_lev==8
replace gnm_salud=(gnm_salud/dsaludt816)*100 if d_lev==9
replace gnm_salud=(gnm_salud/dsaludt816)*100 if d_lev==0
label var gnm_salud "Gasto no monetario en salud deflactado"
*Gasto no monetario en transporte publico deflactado
gen gnm_transportepublico=gastonomonetario if clave>="B001" & clave<="B007"
replace gnm_transportepublico=(gnm_transportepublico/dtpurbanow816)*100 if d_lev==1
replace gnm_transportepublico=(gnm_transportepublico/dtpurbanow816)*100 if d_lev==2
replace gnm_transportepublico=(gnm_transportepublico/dtpurbanow816)*100 if d_lev==3
replace gnm_transportepublico=(gnm_transportepublico/dtpurbanow916)*100 if d_lev==4
replace gnm_transportepublico=(gnm_transportepublico/dtpurbanow916)*100 if d_lev==5
replace gnm_transportepublico=(gnm_transportepublico/dtpurbanow916)*100 if d_lev==6
replace gnm_transportepublico=(gnm_transportepublico/dtpurbanow1016)*100 if d_lev==7
replace gnm_transportepublico=(gnm_transportepublico/dtpurbanow1016)*100 if d_lev==8
replace gnm_transportepublico=(gnm_transportepublico/dtpurbanow1016)*100 if d_lev==9
replace gnm_transportepublico=(gnm_transportepublico/dtpurbanow1116)*100 if d_lev==0
label var gnm_transportepublico "Gasto no monetario en transporte publico deflactado"
*Gasto no monetario en transporte foraneo deflactado*
gen gnm_transporteforaneo=gastonomonetario if (clave>="M001" & clave<="M018") |(clave>="F007" & clave<="F014")
replace gnm_transporteforaneo=(gnm_transporteforaneo/dtransportes216)*100 if d_lev==1
replace gnm_transporteforaneo=(gnm_transporteforaneo/dtransportes216)*100 if d_lev==2
replace gnm_transporteforaneo=(gnm_transporteforaneo/dtransportes316)*100 if d_lev==3
replace gnm_transporteforaneo=(gnm_transporteforaneo/dtransportes316)*100 if d_lev==4
replace gnm_transporteforaneo=(gnm_transporteforaneo/dtransportes316)*100 if d_lev==5
replace gnm_transporteforaneo=(gnm_transporteforaneo/dtransportes416)*100 if d_lev==6
replace gnm_transporteforaneo=(gnm_transporteforaneo/dtransportes416)*100 if d_lev==7
replace gnm_transporteforaneo=(gnm_transporteforaneo/dtransportes416)*100 if d_lev==8
replace gnm_transporteforaneo=(gnm_transporteforaneo/dtransportes516)*100 if d_lev==9
replace gnm_transporteforaneo=(gnm_transporteforaneo/dtransportes516)*100 if d_lev==0
label var gnm_transporteforaneo "Gasto no monetario en transporte foraneo deflactado"
*Gasto no monetario en comunicaciones deflactado*
gen gnm_comunicaciones=gastonomonetario if (clave>="F001" & clave<="F006") |(clave>="R005" & clave<="R008") |(clave>="R010" & clave<="R011")
replace gnm_comunicaciones=(gnm_comunicaciones/dtransportem716)*100 if d_lev==1
replace gnm_comunicaciones=(gnm_comunicaciones/dtransportem716)*100 if d_lev==2
replace gnm_comunicaciones=(gnm_comunicaciones/dtransportem816)*100 if d_lev==3
replace gnm_comunicaciones=(gnm_comunicaciones/dtransportem816)*100 if d_lev==4
replace gnm_comunicaciones=(gnm_comunicaciones/dtransportem816)*100 if d_lev==5
replace gnm_comunicaciones=(gnm_comunicaciones/dtransportem916)*100 if d_lev==6
replace gnm_comunicaciones=(gnm_comunicaciones/dtransportem916)*100 if d_lev==7
replace gnm_comunicaciones=(gnm_comunicaciones/dtransportem916)*100 if d_lev==8
replace gnm_comunicaciones=(gnm_comunicaciones/dtransportem1016)*100 if d_lev==9
replace gnm_comunicaciones=(gnm_comunicaciones/dtransportem1016)*100 if d_lev==0
label var gnm_comunicaciones "Gasto no monetario en comunicaciones deflactado"
*Gasto no monetario en educacion y recreacion deflectado
gen gnm_educacionyrecreacion=gastonomonetario if (clave>="E001" & clave<="E034") |(clave>="H134" & clave<="H135") |(clave>="L001" & clave<="L029") |(clave>="N003" & clave<="N005") |clave=="R009"
replace gnm_educacionyrecreacion=(gnm_educacionyrecreacion/deducacionyespm716)*100 if d_lev==1
replace gnm_educacionyrecreacion=(gnm_educacionyrecreacion/deducacionyespm716)*100 if d_lev==2
replace gnm_educacionyrecreacion=(gnm_educacionyrecreacion/deducacionyespm816)*100 if d_lev==3
replace gnm_educacionyrecreacion=(gnm_educacionyrecreacion/deducacionyespm816)*100 if d_lev==4
replace gnm_educacionyrecreacion=(gnm_educacionyrecreacion/deducacionyespm816)*100 if d_lev==5
replace gnm_educacionyrecreacion=(gnm_educacionyrecreacion/deducacionyespm916)*100 if d_lev==6
replace gnm_educacionyrecreacion=(gnm_educacionyrecreacion/deducacionyespm916)*100 if d_lev==7
replace gnm_educacionyrecreacion=(gnm_educacionyrecreacion/deducacionyespm916)*100 if d_lev==8
replace gnm_educacionyrecreacion=(gnm_educacionyrecreacion/deducacionyespm1016)*100 if d_lev==9
replace gnm_educacionyrecreacion=(gnm_educacionyrecreacion/deducacionyespm1016)*100 if d_lev==0
label var gnm_educacionyrecreacion "Gasto no monetario en educacion y recreacion deflactado"
*Gasto no monetario en Educacion basica deflactado*
gen gnm_educacionbasica=gastonomonetario if (clave>="E002" & clave<="E003") |(clave>="H134" & clave<="H135")
replace gnm_educacionbasica=(gnm_educacionbasica/deducacionyespm716)*100 if d_lev==1
replace gnm_educacionbasica=(gnm_educacionbasica/deducacionyespm716)*100 if d_lev==2
replace gnm_educacionbasica=(gnm_educacionbasica/deducacionyespm816)*100 if d_lev==3
replace gnm_educacionbasica=(gnm_educacionbasica/deducacionyespm816)*100 if d_lev==4
replace gnm_educacionbasica=(gnm_educacionbasica/deducacionyespm816)*100 if d_lev==5
replace gnm_educacionbasica=(gnm_educacionbasica/deducacionyespm916)*100 if d_lev==6
replace gnm_educacionbasica=(gnm_educacionbasica/deducacionyespm916)*100 if d_lev==7
replace gnm_educacionbasica=(gnm_educacionbasica/deducacionyespm916)*100 if d_lev==8
replace gnm_educacionbasica=(gnm_educacionbasica/deducacionyespm1016)*100 if d_lev==9
replace gnm_educacionbasica=(gnm_educacionbasica/deducacionyespm1016)*100 if d_lev==0
label var gnm_educacionbasica "Gasto no monetario en educacion basica deflactado"
*Gasto no monetario en cuidado personal deflactado*
gen gnm_cuidadopersonal=gastonomonetario if (clave>="D001" & clave<="D026") |clave=="H132"
replace gnm_cuidadopersonal=(gnm_cuidadopersonal/dacvm716)*100 if d_lev==1
replace gnm_cuidadopersonal=(gnm_cuidadopersonal/dacvm716)*100 if d_lev==2
replace gnm_cuidadopersonal=(gnm_cuidadopersonal/dacvm816)*100 if d_lev==3
replace gnm_cuidadopersonal=(gnm_cuidadopersonal/dacvm816)*100 if d_lev==4
replace gnm_cuidadopersonal=(gnm_cuidadopersonal/dacvm816)*100 if d_lev==5
replace gnm_cuidadopersonal=(gnm_cuidadopersonal/dacvm916)*100 if d_lev==6
replace gnm_cuidadopersonal=(gnm_cuidadopersonal/dacvm916)*100 if d_lev==7
replace gnm_cuidadopersonal=(gnm_cuidadopersonal/dacvm916)*100 if d_lev==8
replace gnm_cuidadopersonal=(gnm_cuidadopersonal/dacvm1016)*100 if d_lev==9
replace gnm_cuidadopersonal=(gnm_cuidadopersonal/dacvm1016)*100 if d_lev==0
label var gnm_cuidadopersonal "Gasto no monetario en cuidado personal deflactado"
*Gasto no monetario en accesorios personales deflactado*
gen gnm_accesoriospersonales=gastonomonetario if (clave>="H123" & clave<="H131") |clave=="H133"
replace gnm_accesoriospersonales=(gnm_accesoriospersonales/dacvt516)*100 if d_lev==1
replace gnm_accesoriospersonales=(gnm_accesoriospersonales/dacvt516)*100 if d_lev==2
replace gnm_accesoriospersonales=(gnm_accesoriospersonales/dacvt616)*100 if d_lev==3
replace gnm_accesoriospersonales=(gnm_accesoriospersonales/dacvt616)*100 if d_lev==4
replace gnm_accesoriospersonales=(gnm_accesoriospersonales/dacvt616)*100 if d_lev==5
replace gnm_accesoriospersonales=(gnm_accesoriospersonales/dacvt716)*100 if d_lev==6
replace gnm_accesoriospersonales=(gnm_accesoriospersonales/dacvt716)*100 if d_lev==7
replace gnm_accesoriospersonales=(gnm_accesoriospersonales/dacvt716)*100 if d_lev==8
replace gnm_accesoriospersonales=(gnm_accesoriospersonales/dacvt816)*100 if d_lev==9
replace gnm_accesoriospersonales=(gnm_accesoriospersonales/dacvt816)*100 if d_lev==0
label var gnm_accesoriospersonales "Gasto no monetario en accesorios personales deflactado"
*gasto no monetario en otros gastos y transferencias deflactado*
gen gnm_otrosgastos=gastonomonetario if (clave>="N001" & clave<="N002") |(clave>="N006" & clave<="N016") |(clave>="T901" & clave<="T915") |clave=="R012"
replace gnm_otrosgastos=(gnm_otrosgastos/dinpcs216)*100 if d_lev==1
replace gnm_otrosgastos=(gnm_otrosgastos/dinpcs216)*100 if d_lev==2
replace gnm_otrosgastos=(gnm_otrosgastos/dinpcs316)*100 if d_lev==3
replace gnm_otrosgastos=(gnm_otrosgastos/dinpcs316)*100 if d_lev==4
replace gnm_otrosgastos=(gnm_otrosgastos/dinpcs316)*100 if d_lev==5
replace gnm_otrosgastos=(gnm_otrosgastos/dinpcs416)*100 if d_lev==6
replace gnm_otrosgastos=(gnm_otrosgastos/dinpcs416)*100 if d_lev==7
replace gnm_otrosgastos=(gnm_otrosgastos/dinpcs416)*100 if d_lev==8
replace gnm_otrosgastos=(gnm_otrosgastos/dinpcs516)*100 if d_lev==9
replace gnm_otrosgastos=(gnm_otrosgastos/dinpcs516)*100 if d_lev==0
label var gnm_otrosgastos "Gasto no monetario en otros gastos y transferencias deflactado"
*Gasto no monetario en regalos otoragdos deflactado
gen gnm_regalosotorgados=gastonomonetario if (clave>="T901" & clave<="T915") |clave=="N013"
replace gnm_regalosotorgados=(gnm_regalosotorgados/dinpcs216)*100 if d_lev==1
replace gnm_regalosotorgados=(gnm_regalosotorgados/dinpcs216)*100 if d_lev==2
replace gnm_regalosotorgados=(gnm_regalosotorgados/dinpcs316)*100 if d_lev==3
replace gnm_regalosotorgados=(gnm_regalosotorgados/dinpcs316)*100 if d_lev==4
replace gnm_regalosotorgados=(gnm_regalosotorgados/dinpcs316)*100 if d_lev==5
replace gnm_regalosotorgados=(gnm_regalosotorgados/dinpcs416)*100 if d_lev==6
replace gnm_regalosotorgados=(gnm_regalosotorgados/dinpcs416)*100 if d_lev==7
replace gnm_regalosotorgados=(gnm_regalosotorgados/dinpcs416)*100 if d_lev==8
replace gnm_regalosotorgados=(gnm_regalosotorgados/dinpcs516)*100 if d_lev==9
replace gnm_regalosotorgados=(gnm_regalosotorgados/dinpcs516)*100 if d_lev==0
label var gnm_regalosotorgados "Gasto no monetario en regalos otorgados deflactado"

save ingresonomonetariodeflac16.dta,replace

*En especie
use ingresonomonetariodeflac16.dta,clear
keep if especie==1
collapse (sum) gnm_*,by(idhogar)
rename gnm_alcoholytabaco gnm_alcoholytabacoespecie
rename gnm_alimentos gnm_alimentosespecie
rename gnm_vestidoycalzado gnm_vestidoycalzadoespecie
rename gnm_viviendas gnm_viviendasespecie
rename gnm_alimpieza gnm_alimpiezaespecie
rename gnm_cristaleria gnm_cristaleriaespecie
rename gnm_edomesticos gnm_edomesticosespecie
rename gnm_salud gnm_saludespecie
rename gnm_transportepublico gnm_transportepublicoespecie
rename gnm_transporteforaneo gnm_transporteforaneoespecie
rename gnm_comunicaciones gnm_comunicacionesespecie
rename gnm_educacionyrecreacion gnm_educacionyrecreacionespecie
rename gnm_educacionbasica gnm_educacionbasicaespecie
rename gnm_cuidadopersonal gnm_cuidadopersonalespecie
rename gnm_accesoriospersonales gnm_accesoriospersonalesespecie
rename gnm_otrosgastos gnm_otrosgastosespecie
rename gnm_regalosotorgados gnm_regalosotorgadosespecie
sort idhogar
save especiedeflac16.dta,replace
*En regalos
use ingresonomonetariodeflac16.dta,clear
keep if regalos==1
collapse (sum) gnm_*,by(idhogar)

rename gnm_alcoholytabaco gnm_alcoholytabacoregalos
rename gnm_alimentos gnm_alimentosregalos
rename gnm_vestidoycalzado gnm_vestidoycalzadoregalos
rename gnm_viviendas gnm_viviendasregalos
rename gnm_alimpieza gnm_alimpiezaregalos
rename gnm_cristaleria gnm_cristaleriaregalos
rename gnm_edomesticos gnm_edomesticosregalos
rename gnm_salud gnm_saludregalos
rename gnm_transportepublico gnm_transportepublicoregalos
rename gnm_transporteforaneo gnm_transporteforaneoregalos
rename gnm_comunicaciones gnm_comunicacionesregalos
rename gnm_educacionyrecreacion gnm_educacionyrecreacionregalos
rename gnm_educacionbasica gnm_educacionbasicaregalos
rename gnm_cuidadopersonal gnm_cuidadopersonalregalos
rename gnm_accesoriospersonales gnm_accesoriospersonalesregalos
rename gnm_otrosgastos gnm_otrosgastosregalos
rename gnm_regalosotorgados gnm_regalosotorgadosregalos

save regalosdeflac16.dta,replace

**Ingreso corriente total
*concentrado del hogar
use concentradohogar16.dta,clear
egen idhogar=concat(folioviv foliohog)
label var idhogar "Identificador del Hogar"
keep idhogar tam_loc factor tot_integ est_dis upm ubica_geo
*merge con las otras bases*
merge 1:m idhogar using ingresodeflactado16.dta
drop _merge
merge 1:m idhogar using especiedeflac16.dta
drop _merge
merge 1:m idhogar using regalosdeflac16.dta
drop _merge
*Localidad rural
gen rural=. 
replace rural=0 if tam_loc<="3"
replace rural=1 if tam_loc=="4"
label var rural "Localidad rural"
label define rural 0 "Urbano" 1 "Rural"
label value rural rural
*Suma de los pagos en especie y regalos
egen pago_especie=rsum(gnm_alcoholytabacoespecie gnm_alimentosespecie gnm_vestidoycalzadoespecie gnm_viviendasespecie gnm_alimpiezaespecie gnm_cristaleriaespecie gnm_edomesticosespecie gnm_saludespecie gnm_transportepublicoespecie gnm_transporteforaneoespecie gnm_comunicacionesespecie gnm_educacionyrecreacionespecie gnm_educacionbasicaespecie gnm_cuidadopersonalespecie gnm_accesoriospersonalesespecie gnm_otrosgastosespecie gnm_regalosotorgadosespecie) 
egen pago_regalos=rsum(gnm_alcoholytabacoregalos gnm_alimentosregalos gnm_vestidoycalzadoregalos gnm_viviendasregalos gnm_alimpiezaregalos gnm_cristaleriaregalos gnm_edomesticosregalos gnm_saludregalos gnm_transportepublicoregalos gnm_transporteforaneoregalos gnm_comunicacionesregalos gnm_educacionyrecreacionregalos gnm_educacionbasicaregalos gnm_cuidadopersonalregalos gnm_accesoriospersonalesregalos gnm_otrosgastosregalos gnm_regalosotorgadosregalos)
egen inomonetario=rsum(pago_especie pago_regalos)
egen icorrientetotal=rsum(ingresomonetariop inomonetario)
label var pago_especie "Ingreso corriente no monetario pago especie"
label var pago_regalos "Ingreso corriente no monetario regalos especie"
label var inomonetario "Suma del ingreso corriente no monetario"
label var icorrientetotal "Ingreso corriente total"

save ingresocorrientetotal16.dta,replace

*tamaño del hogar*
use poblacion16.dta,replace
egen idhogar=concat(folioviv foliohog)
label var idhogar "Identificador del Hogar"
drop if parentesco>="400" & parentesco<"500"
drop if parentesco>="700" & parentesco<"800"
*total de los integrantes del hogar*
gen integrante=1
egen tot_integrantes=sum(integrante),by(idhogar)
*collapse
collapse (max) tot_integrantes,by(idhogar)
save tamañohogar16.dta,replace
**Bienestar economico*
use ingresocorrientetotal16.dta,clear
*merge con tamaño de hogar en escala
merge 1:m idhogar using tamañohogar16.dta
drop _merge
*per capita*
gen ictpercapita=icorrientetotal/tot_integrantes
label var ictpercapita "Ingreso corriente total per capita"
*linea de pobreza por ingresos
*julio 2016
scalar lpi_rural16=2019.01
scalar lpi_urbano16=2893.10
gen pobres_lpi=.
replace pobres_lpi=1 if (ictpercapita<lpi_urbano16 & rural==0)
replace pobres_lpi=1 if (ictpercapita<lpi_rural16 & rural==1)
replace pobres_lpi=0 if (ictpercapita>=lpi_urbano16 & rural==0) & ictpercapita!=.
replace pobres_lpi=0 if (ictpercapita>=lpi_rural16 & rural==1) & ictpercapita!=.
label var pobres_lpi "Poblacion con ingreso menor o igual a la linea de pobreza por ingresos"
label define pobres_lpi 0 "Poblacion con ingreso igual o mayor a la Linea de Pobreza por Ingresos" 1 "Poblacion con ingreso menor a la Linea de Pobreza por Ingresos"
label value pobres_lpi pobres_lpi

save lp_ingresos16.dta,replace
*merge
use ic_rezedu16.dta,clear
merge 1:1 idpersona using ic_asalud16.dta
drop _merge
merge 1:1 idpersona using ic_segsoc16.dta
drop _merge
merge m:1 idhogar using ic_cev16.dta
drop _merge
merge m:1 idhogar using ic_sbv16.dta
drop _merge
merge m:1 idhogar using ic_ali16.dta
drop _merge
merge m:1 idhogar using lp_ingresos16.dta
drop _merge

save basefinal16.dta,replace

********
**2018**
********
*Ingreso Monetario*
use trabajos18.dta,clear
keep folioviv foliohog numren id_trabajo pres_2 
destring pres_2 id_trabajo,replace
reshape wide pres_2,i(folioviv foliohog numren) j(id_trabajo)
egen idhogar=concat(folioviv foliohog)
label var idhogar "Identificador del Hogar"
egen idpersona=concat(folioviv foliohog numren)
label var idpersona "Identificador de la persona"
gen trabajo=1
label var trabajo "Poblacion con al menos un empleo"
gen aguinaldo1=0
replace aguinaldo1=1 if pres_21==2
gen aguinaldo2=0
replace aguinaldo2=1 if pres_22==2
label var aguinaldo1 "Aguinaldo por trabajo principal"
label var aguinaldo2 "Aguinaldo por trabajo secundario"
label define aguinaldo 0 "No recibe aguinaldo" 1 "Recibe aguinaldo"
label value aguinaldo1 aguinaldo
label value aguinaldo2 aguinaldo
save aguinaldo18.dta,replace


**base ingresos
use ingresos18.dta,clear
egen idhogar=concat(folioviv foliohog)
label var idhogar "Identificador del Hogar"
egen idpersona=concat(folioviv foliohog numren)
label var idpersona "Identificador de la persona"
*merge con aguinaldos
merge m:1 idpersona using aguinaldo18.dta
drop _merge
*delimitar
drop if clave=="P009" & aguinaldo1!=1
drop if clave=="P016" & aguinaldo2!=1
*deflactar el ingreso de los hogares a precios de julio de 2023. 

gen ingresod1=(ing_1/77.5499092652)*100 if mes_1=="07"
replace ingresod1=(ing_1/78.0023596622)*100 if mes_1=="08"
replace ingresod1=(ing_1/78.3322466468)*100 if mes_1=="09"
replace ingresod1=(ing_1/78.7382016890)*100 if mes_1=="10"

gen ingresod2=(ing_2/77.1364761331)*100 if mes_2=="06"
replace ingresod2=(ing_2/77.5499092652)*100 if mes_2=="07"
replace ingresod2=(ing_2/78.0023596622)*100 if mes_2=="08"
replace ingresod2=(ing_2/78.3322466468)*100 if mes_2=="09"

gen ingresod3=(ing_3/76.8396672978)*100 if mes_3=="05"
replace ingresod3=(ing_3/77.1364761331)*100 if mes_3=="06"
replace ingresod3=(ing_3/77.5499092652)*100 if mes_3=="07"
replace ingresod3=(ing_3/78.0023596622)*100 if mes_3=="08"

gen ingresod4=(ing_4/76.9644552953)*100 if mes_4=="04"
replace ingresod4=(ing_4/76.8396672978)*100 if mes_4=="05"
replace ingresod4=(ing_4/77.1364761331)*100 if mes_4=="06"
replace ingresod4=(ing_4/77.5499092652)*100 if mes_4=="07"

gen ingresod5=(ing_5/77.2262768416)*100 if mes_5=="03"
replace ingresod5=(ing_5/76.9644552953)*100 if mes_5=="04"
replace ingresod5=(ing_5/76.8396672978)*100 if mes_5=="05"
replace ingresod5=(ing_5/77.1364761331)*100 if mes_5=="06"

gen ingresod6=(ing_6/76.9772839680)*100 if mes_6=="02"
replace ingresod6=(ing_6/77.2262768416)*100 if mes_6=="03"
replace ingresod6=(ing_6/76.9644552953)*100 if mes_6=="04"
replace ingresod6=(ing_6/76.8396672978)*100 if mes_6=="05"

*deflactar los repartos de utilidades y los aguinaldos                  
replace ingresod1=((ing_1/76.8396672978)*100)/12 if clave=="P008" |clave=="P015"
replace ingresod1=((ing_1/76.2798706732)*100)/12 if clave=="P009" |clave=="P016"
recode ingresod1 ingresod2 ingresod3 ingresod4 ingresod5 ingresod6 (0=.) 
 
*ingreso promedio mensual
egen ingresopromediodeflac=rowmean(ingresod1 ingresod2 ingresod3 ingresod4 ingresod5 ingresod6)
*ingreso promedio monetario
gen ingresomonetariop=ingresopromediodeflac if clave>="P001" & clave<="P009" |clave>="P011" & clave<="P016" |clave>="P018" & clave<="P048" |clave>="P067" & clave<="P081" |clave>="P101" & clave<="P108"
*gen ingreso laboral promedio*
gen ingresolaboralp=ingresopromediodeflac if clave>="P001" & clave<="P009" |clave>="P011" & clave<="P016" |clave>="P018" & clave<="P022" |clave>="P067" & clave<="P081"
*ingreso por rentas*
gen ingresorentasp=ingresopromediodeflac if clave>="P023" & clave<="P031"
*ingreso por transferencias
gen ingresotransferenciasp=ingresopromediodeflac if clave>="P032" & clave<="P048" |clave>="P101" & clave<="P108"
*collapse
collapse (sum) ingresopromediodeflac ingresomonetariop ingresolaboralp ingresorentasp ingresotransferenciasp,by(idhogar)
label var ingresopromediodeflac "Ingreso corriente total del hogar"
label var ingresomonetariop "Ingreso corriente monetario del hogar"
label var ingresolaboralp "Ingreso corriente monetario laboral"
label var ingresorentasp "Ingreso corriente monetario por rentas"
label var ingresotransferenciasp "ingreso corriente monetario por transferencias"
save ingresodeflactado18.dta,replace

*Ingreso no monetario*

use gastoshogar18.dta,clear
gen base=1
append using gastospersona18.dta
recode base (.=2)
replace frecuencia=frec_rem if base==2
label var base "Origen del monto obtenido"
label define base 1 "Monto del Hogar" 2 "Monto de la persona"
label value base base
egen idhogar=concat(folioviv foliohog)
label var idhogar "Identificador del Hogar"
egen idpersona=concat(folioviv foliohog numren)
label var idpersona "Identificador de la persona"
**decena de levantamiento**
gen d_lev=substr(folioviv,8,1)
label var d_lev "Decena de levantamiento"
destring d_lev,replace
**deflactores**
*alimentos semanal (sin incluir las bebidas alcoholicas)
scalar dalimentosw718=68.138956820663
scalar dalimentosw818=68.784043903838
scalar dalimentosw918=68.930799579528
scalar dalimentosw1018=68.850254604033
scalar dalimentosw1118=69.900752208161
*bebidas alcoholicas y tabaco
scalar dbatw718=73.024524269736
scalar dbatw818=73.542564852028
scalar dbatw918=73.563025210084
scalar dbatw1018=73.871392035075
scalar dbatw1118=73.999269272927
*Ropa,vestido, calzado y accesorios
scalar drvcat518=84.169676292330
scalar drvcat618=84.186848554915
scalar drvcat718=84.377271338813
scalar drvcat818=84.698729506478
*vivienda
scalar dviviendam718=89.94639294
scalar dviviendam818=90.17125168
scalar dviviendam918=90.75708899
scalar dviviendam1018=92.00975496
scalar dviviendam1118=93.39830638
*accesorios y articulos de limpieza para el hogar (mensual)
scalar daalhm718=72.13248976
scalar daalhm818=72.35250876
scalar daalhm918=72.80438762
scalar daalhm1018=73.0566326
scalar daalhm1118=73.45590037
*accesorios y articulos de limpieza para el hogar (trimestral)
scalar daalht518=72.135872700868
scalar daalht618=72.227551466912
scalar daalht718=72.429795379251
scalar daalht818=72.737842993115
*muebles y aparatos domesticos (semestral)
scalar dmads218=91.85514588
scalar dmads318=91.93719134
scalar dmads418=91.87709568
scalar dmads518=91.80493617
*salud (trimestral)
scalar dsaludt518=77.728717202901
scalar dsaludt618=77.988360099238
scalar dsaludt718=78.209531293007
scalar dsaludt818=78.416315488268
*transporte publico urbano (semanal)
scalar dtpurbanow718=84.568884825170
scalar dtpurbanow818=84.811689300029
scalar dtpurbanow918=84.913158918334
scalar dtpurbanow1018=84.990106712215
scalar dtpurbanow1118=85.065363345792
*transporte (mensual)
scalar dtransportem718=80.08409694
scalar dtransportem818=80.83497857
scalar dtransportem918=81.44572318
scalar dtransportem1018=82.13351311
scalar dtransportem1118=82.27235518
*transporte (semestral)
scalar dtransportes218=78.69269693
scalar dtransportes318=79.24025767
scalar dtransportes418=79.7947098
scalar dtransportes518=79.7947098
*educacion y esparcimiento 
scalar deducacionyespm718=84.67842631
scalar deducacionyespm818=84.81424109
scalar deducacionyespm918=85.5007322
scalar deducacionyespm1018=85.7834548
scalar deducacionyespm1118=85.85455869
*accesorios y cuidados del vestido (mensual)
scalar dacvm718=83.38562771
scalar dacvm818=83.64697545
scalar dacvm918=83.95797724
scalar dacvm1018=84.17809647
scalar dacvm1118=84.21061408
*accesorios y cuidados del vestido (trimestral)
scalar dacvt518=83.526303313530
scalar dacvt618=83.573658378882
scalar dacvt718=83.663526796642
scalar dacvt818=83.927683050541
*inpc semestral
scalar dinpcs218=77.11567813
scalar dinpcs318=77.28652408
scalar dinpcs418=77.47085238
scalar dinpcs518=77.76647678
*regalos y especie
gen gastonomonetario=gas_nm_tri/3
label var gastonomonetario "Gasto no monetario mensual"
gen especie=1 if tipo_gasto=="G4"
gen regalos=1 if tipo_gasto=="G5"
replace regalos=1 if tipo_gasto=="G6"
drop if tipo_gasto=="G2" |tipo_gasto=="G3" |tipo_gasto=="G7"
label var especie "Gasto no monetario por remuneraciones en especie"
label var regalos "Gasto no monetario por regalos de otro hogar y transferencias de instituciones"
*control de la frecuencia de los regalos recibidos en el hogar
drop if ((frecuencia>="5" & frecuencia<="6") |frecuencia==" " |frecuencia=="0") & base==1 & tipo_gasto=="G5"
drop if (frecuencia=="9" |frecuencia=="") & base==2 & tipo_gasto=="G5"
**Hora de deflactar
*Gasto no monetario en alimentos deflactado
gen gnm_alimentos=gastonomonetario if (clave>="A001" & clave<="A222") |(clave>="A242" & clave<="A247")
replace gnm_alimentos=(gnm_alimentos/dalimentosw818)*100 if d_lev==1
replace gnm_alimentos=(gnm_alimentos/dalimentosw818)*100 if d_lev==2
replace gnm_alimentos=(gnm_alimentos/dalimentosw818)*100 if d_lev==3
replace gnm_alimentos=(gnm_alimentos/dalimentosw918)*100 if d_lev==4
replace gnm_alimentos=(gnm_alimentos/dalimentosw918)*100 if d_lev==5
replace gnm_alimentos=(gnm_alimentos/dalimentosw918)*100 if d_lev==6
replace gnm_alimentos=(gnm_alimentos/dalimentosw1018)*100 if d_lev==7
replace gnm_alimentos=(gnm_alimentos/dalimentosw1018)*100 if d_lev==8
replace gnm_alimentos=(gnm_alimentos/dalimentosw1018)*100 if d_lev==9
replace gnm_alimentos=(gnm_alimentos/dalimentosw1118)*100 if d_lev==0
label var gnm_alimentos "Gasto no monetario en alimentos deflactado"
*Gasto no monetario en alcohol y tabaco deflactado
gen gnm_alcoholytabaco=gastonomonetario if clave>="A223" & clave<="A241"
replace gnm_alcoholytabaco=(gnm_alcoholytabaco/dbatw818)*100 if d_lev==1
replace gnm_alcoholytabaco=(gnm_alcoholytabaco/dbatw818)*100 if d_lev==2
replace gnm_alcoholytabaco=(gnm_alcoholytabaco/dbatw818)*100 if d_lev==3
replace gnm_alcoholytabaco=(gnm_alcoholytabaco/dbatw918)*100 if d_lev==4
replace gnm_alcoholytabaco=(gnm_alcoholytabaco/dbatw918)*100 if d_lev==5
replace gnm_alcoholytabaco=(gnm_alcoholytabaco/dbatw918)*100 if d_lev==6
replace gnm_alcoholytabaco=(gnm_alcoholytabaco/dbatw1018)*100 if d_lev==7
replace gnm_alcoholytabaco=(gnm_alcoholytabaco/dbatw1018)*100 if d_lev==8
replace gnm_alcoholytabaco=(gnm_alcoholytabaco/dbatw1018)*100 if d_lev==9
replace gnm_alcoholytabaco=(gnm_alcoholytabaco/dbatw1118)*100 if d_lev==0
label var gnm_alcoholytabaco "Gasto no monetario en alcohol y tabaco deflactado"
*Gasto no monetario en vestido y calzado deflactado
gen gnm_vestidoycalzado=gastonomonetario if (clave>="H001" & clave<="H122") |clave=="H136"
replace gnm_vestidoycalzado=(gnm_vestidoycalzado/drvcat518)*100 if d_lev==1
replace gnm_vestidoycalzado=(gnm_vestidoycalzado/drvcat518)*100 if d_lev==2
replace gnm_vestidoycalzado=(gnm_vestidoycalzado/drvcat618)*100 if d_lev==3
replace gnm_vestidoycalzado=(gnm_vestidoycalzado/drvcat618)*100 if d_lev==4
replace gnm_vestidoycalzado=(gnm_vestidoycalzado/drvcat618)*100 if d_lev==5
replace gnm_vestidoycalzado=(gnm_vestidoycalzado/drvcat718)*100 if d_lev==6
replace gnm_vestidoycalzado=(gnm_vestidoycalzado/drvcat718)*100 if d_lev==7
replace gnm_vestidoycalzado=(gnm_vestidoycalzado/drvcat718)*100 if d_lev==8
replace gnm_vestidoycalzado=(gnm_vestidoycalzado/drvcat818)*100 if d_lev==9
replace gnm_vestidoycalzado=(gnm_vestidoycalzado/drvcat818)*100 if d_lev==0
label var gnm_vestidoycalzado "Gasto no monetario en vestido y calzado deflactado"
*Gasto no monetario en viviendas deflactado
gen gnm_viviendas=gastonomonetario if (clave>="G001" & clave<="G016") |(clave>="R001" & clave<="R004") |clave=="R013"
replace gnm_viviendas=(gnm_viviendas/dviviendam718)*100 if d_lev==1
replace gnm_viviendas=(gnm_viviendas/dviviendam718)*100 if d_lev==2
replace gnm_viviendas=(gnm_viviendas/dviviendam818)*100 if d_lev==3
replace gnm_viviendas=(gnm_viviendas/dviviendam818)*100 if d_lev==4
replace gnm_viviendas=(gnm_viviendas/dviviendam818)*100 if d_lev==5
replace gnm_viviendas=(gnm_viviendas/dviviendam918)*100 if d_lev==6
replace gnm_viviendas=(gnm_viviendas/dviviendam918)*100 if d_lev==7
replace gnm_viviendas=(gnm_viviendas/dviviendam918)*100 if d_lev==8
replace gnm_viviendas=(gnm_viviendas/dviviendam1018)*100 if d_lev==9
replace gnm_viviendas=(gnm_viviendas/dviviendam1018)*100 if d_lev==0
label var gnm_viviendas "Gasto no monetario en viviendas deflactado"
*Gasto no monetario en Articulos de limpieza deflactado
gen gnm_alimpieza=gastonomonetario if clave>="C001" & clave<="C024"
replace gnm_alimpieza=(gnm_alimpieza/daalhm718)*100 if d_lev==1
replace gnm_alimpieza=(gnm_alimpieza/daalhm718)*100 if d_lev==2
replace gnm_alimpieza=(gnm_alimpieza/daalhm818)*100 if d_lev==3
replace gnm_alimpieza=(gnm_alimpieza/daalhm818)*100 if d_lev==4
replace gnm_alimpieza=(gnm_alimpieza/daalhm818)*100 if d_lev==5
replace gnm_alimpieza=(gnm_alimpieza/daalhm918)*100 if d_lev==6
replace gnm_alimpieza=(gnm_alimpieza/daalhm918)*100 if d_lev==7
replace gnm_alimpieza=(gnm_alimpieza/daalhm918)*100 if d_lev==8
replace gnm_alimpieza=(gnm_alimpieza/daalhm1018)*100 if d_lev==9
replace gnm_alimpieza=(gnm_alimpieza/daalhm1018)*100 if d_lev==0
label var gnm_alimpieza "Gasto no monetario en accesorios y articulos de limpieza"
*Gasto no monetario en cristaleria y blancos deflactado
gen gnm_cristaleria=gastonomonetario if clave>="I001" & clave<="I026"
replace gnm_cristaleria=(gnm_cristaleria/daalht518)*100 if d_lev==1
replace gnm_cristaleria=(gnm_cristaleria/daalht518)*100 if d_lev==2
replace gnm_cristaleria=(gnm_cristaleria/daalht618)*100 if d_lev==3
replace gnm_cristaleria=(gnm_cristaleria/daalht618)*100 if d_lev==4
replace gnm_cristaleria=(gnm_cristaleria/daalht618)*100 if d_lev==5
replace gnm_cristaleria=(gnm_cristaleria/daalht718)*100 if d_lev==6
replace gnm_cristaleria=(gnm_cristaleria/daalht718)*100 if d_lev==7
replace gnm_cristaleria=(gnm_cristaleria/daalht718)*100 if d_lev==8
replace gnm_cristaleria=(gnm_cristaleria/daalht818)*100 if d_lev==9
replace gnm_cristaleria=(gnm_cristaleria/daalht818)*100 if d_lev==0
label var gnm_cristaleria "Gasto no monetario en cristaleria y blancos deflactado"
*Gasto no monetario en enseres domesticos y muebles deflactado
gen gnm_edomesticos=gastonomonetario if clave>="K001" & clave<="K037"
replace gnm_edomesticos=(gnm_edomesticos/dmads218)*100 if d_lev==1
replace gnm_edomesticos=(gnm_edomesticos/dmads218)*100 if d_lev==2
replace gnm_edomesticos=(gnm_edomesticos/dmads318)*100 if d_lev==3
replace gnm_edomesticos=(gnm_edomesticos/dmads318)*100 if d_lev==4
replace gnm_edomesticos=(gnm_edomesticos/dmads318)*100 if d_lev==5
replace gnm_edomesticos=(gnm_edomesticos/dmads418)*100 if d_lev==6
replace gnm_edomesticos=(gnm_edomesticos/dmads418)*100 if d_lev==7
replace gnm_edomesticos=(gnm_edomesticos/dmads418)*100 if d_lev==8
replace gnm_edomesticos=(gnm_edomesticos/dmads518)*100 if d_lev==9
replace gnm_edomesticos=(gnm_edomesticos/dmads518)*100 if d_lev==0
label var gnm_edomesticos "Gasto no monetario en enseres domesticos y muebles deflactado"
*Gasto no monetario en salud deflactado
gen gnm_salud=gastonomonetario if clave>="J001" & clave<="J072"
replace gnm_salud=(gnm_salud/dsaludt518)*100 if d_lev==1
replace gnm_salud=(gnm_salud/dsaludt518)*100 if d_lev==2
replace gnm_salud=(gnm_salud/dsaludt618)*100 if d_lev==3
replace gnm_salud=(gnm_salud/dsaludt618)*100 if d_lev==4
replace gnm_salud=(gnm_salud/dsaludt618)*100 if d_lev==5
replace gnm_salud=(gnm_salud/dsaludt718)*100 if d_lev==6
replace gnm_salud=(gnm_salud/dsaludt718)*100 if d_lev==7
replace gnm_salud=(gnm_salud/dsaludt718)*100 if d_lev==8
replace gnm_salud=(gnm_salud/dsaludt818)*100 if d_lev==9
replace gnm_salud=(gnm_salud/dsaludt818)*100 if d_lev==0
label var gnm_salud "Gasto no monetario en salud deflactado"
*Gasto no monetario en transporte publico deflactado
gen gnm_transportepublico=gastonomonetario if clave>="B001" & clave<="B007"
replace gnm_transportepublico=(gnm_transportepublico/dtpurbanow818)*100 if d_lev==1
replace gnm_transportepublico=(gnm_transportepublico/dtpurbanow818)*100 if d_lev==2
replace gnm_transportepublico=(gnm_transportepublico/dtpurbanow818)*100 if d_lev==3
replace gnm_transportepublico=(gnm_transportepublico/dtpurbanow918)*100 if d_lev==4
replace gnm_transportepublico=(gnm_transportepublico/dtpurbanow918)*100 if d_lev==5
replace gnm_transportepublico=(gnm_transportepublico/dtpurbanow918)*100 if d_lev==6
replace gnm_transportepublico=(gnm_transportepublico/dtpurbanow1018)*100 if d_lev==7
replace gnm_transportepublico=(gnm_transportepublico/dtpurbanow1018)*100 if d_lev==8
replace gnm_transportepublico=(gnm_transportepublico/dtpurbanow1018)*100 if d_lev==9
replace gnm_transportepublico=(gnm_transportepublico/dtpurbanow1118)*100 if d_lev==0
label var gnm_transportepublico "Gasto no monetario en transporte publico deflactado"
*Gasto no monetario en transporte foraneo deflactado*
gen gnm_transporteforaneo=gastonomonetario if (clave>="M001" & clave<="M018") |(clave>="F007" & clave<="F014")
replace gnm_transporteforaneo=(gnm_transporteforaneo/dtransportes218)*100 if d_lev==1
replace gnm_transporteforaneo=(gnm_transporteforaneo/dtransportes218)*100 if d_lev==2
replace gnm_transporteforaneo=(gnm_transporteforaneo/dtransportes318)*100 if d_lev==3
replace gnm_transporteforaneo=(gnm_transporteforaneo/dtransportes318)*100 if d_lev==4
replace gnm_transporteforaneo=(gnm_transporteforaneo/dtransportes318)*100 if d_lev==5
replace gnm_transporteforaneo=(gnm_transporteforaneo/dtransportes418)*100 if d_lev==6
replace gnm_transporteforaneo=(gnm_transporteforaneo/dtransportes418)*100 if d_lev==7
replace gnm_transporteforaneo=(gnm_transporteforaneo/dtransportes418)*100 if d_lev==8
replace gnm_transporteforaneo=(gnm_transporteforaneo/dtransportes518)*100 if d_lev==9
replace gnm_transporteforaneo=(gnm_transporteforaneo/dtransportes518)*100 if d_lev==0
label var gnm_transporteforaneo "Gasto no monetario en transporte foraneo deflactado"
*Gasto no monetario en comunicaciones deflactado*
gen gnm_comunicaciones=gastonomonetario if (clave>="F001" & clave<="F006") |(clave>="R005" & clave<="R008") |(clave>="R010" & clave<="R011")
replace gnm_comunicaciones=(gnm_comunicaciones/dtransportem718)*100 if d_lev==1
replace gnm_comunicaciones=(gnm_comunicaciones/dtransportem718)*100 if d_lev==2
replace gnm_comunicaciones=(gnm_comunicaciones/dtransportem818)*100 if d_lev==3
replace gnm_comunicaciones=(gnm_comunicaciones/dtransportem818)*100 if d_lev==4
replace gnm_comunicaciones=(gnm_comunicaciones/dtransportem818)*100 if d_lev==5
replace gnm_comunicaciones=(gnm_comunicaciones/dtransportem918)*100 if d_lev==6
replace gnm_comunicaciones=(gnm_comunicaciones/dtransportem918)*100 if d_lev==7
replace gnm_comunicaciones=(gnm_comunicaciones/dtransportem918)*100 if d_lev==8
replace gnm_comunicaciones=(gnm_comunicaciones/dtransportem1018)*100 if d_lev==9
replace gnm_comunicaciones=(gnm_comunicaciones/dtransportem1018)*100 if d_lev==0
label var gnm_comunicaciones "Gasto no monetario en comunicaciones deflactado"
*Gasto no monetario en educacion y recreacion deflectado
gen gnm_educacionyrecreacion=gastonomonetario if (clave>="E001" & clave<="E034") |(clave>="H134" & clave<="H135") |(clave>="L001" & clave<="L029") |(clave>="N003" & clave<="N005") |clave=="R009"
replace gnm_educacionyrecreacion=(gnm_educacionyrecreacion/deducacionyespm718)*100 if d_lev==1
replace gnm_educacionyrecreacion=(gnm_educacionyrecreacion/deducacionyespm718)*100 if d_lev==2
replace gnm_educacionyrecreacion=(gnm_educacionyrecreacion/deducacionyespm818)*100 if d_lev==3
replace gnm_educacionyrecreacion=(gnm_educacionyrecreacion/deducacionyespm818)*100 if d_lev==4
replace gnm_educacionyrecreacion=(gnm_educacionyrecreacion/deducacionyespm818)*100 if d_lev==5
replace gnm_educacionyrecreacion=(gnm_educacionyrecreacion/deducacionyespm918)*100 if d_lev==6
replace gnm_educacionyrecreacion=(gnm_educacionyrecreacion/deducacionyespm918)*100 if d_lev==7
replace gnm_educacionyrecreacion=(gnm_educacionyrecreacion/deducacionyespm918)*100 if d_lev==8
replace gnm_educacionyrecreacion=(gnm_educacionyrecreacion/deducacionyespm1018)*100 if d_lev==9
replace gnm_educacionyrecreacion=(gnm_educacionyrecreacion/deducacionyespm1018)*100 if d_lev==0
label var gnm_educacionyrecreacion "Gasto no monetario en educacion y recreacion deflactado"
*Gasto no monetario en Educacion basica deflactado*
gen gnm_educacionbasica=gastonomonetario if (clave>="E002" & clave<="E003") |(clave>="H134" & clave<="H135")
replace gnm_educacionbasica=(gnm_educacionbasica/deducacionyespm718)*100 if d_lev==1
replace gnm_educacionbasica=(gnm_educacionbasica/deducacionyespm718)*100 if d_lev==2
replace gnm_educacionbasica=(gnm_educacionbasica/deducacionyespm818)*100 if d_lev==3
replace gnm_educacionbasica=(gnm_educacionbasica/deducacionyespm818)*100 if d_lev==4
replace gnm_educacionbasica=(gnm_educacionbasica/deducacionyespm818)*100 if d_lev==5
replace gnm_educacionbasica=(gnm_educacionbasica/deducacionyespm918)*100 if d_lev==6
replace gnm_educacionbasica=(gnm_educacionbasica/deducacionyespm918)*100 if d_lev==7
replace gnm_educacionbasica=(gnm_educacionbasica/deducacionyespm918)*100 if d_lev==8
replace gnm_educacionbasica=(gnm_educacionbasica/deducacionyespm1018)*100 if d_lev==9
replace gnm_educacionbasica=(gnm_educacionbasica/deducacionyespm1018)*100 if d_lev==0
label var gnm_educacionbasica "Gasto no monetario en educacion basica deflactado"
*Gasto no monetario en cuidado personal deflactado*
gen gnm_cuidadopersonal=gastonomonetario if (clave>="D001" & clave<="D026") |clave=="H132"
replace gnm_cuidadopersonal=(gnm_cuidadopersonal/dacvm718)*100 if d_lev==1
replace gnm_cuidadopersonal=(gnm_cuidadopersonal/dacvm718)*100 if d_lev==2
replace gnm_cuidadopersonal=(gnm_cuidadopersonal/dacvm818)*100 if d_lev==3
replace gnm_cuidadopersonal=(gnm_cuidadopersonal/dacvm818)*100 if d_lev==4
replace gnm_cuidadopersonal=(gnm_cuidadopersonal/dacvm818)*100 if d_lev==5
replace gnm_cuidadopersonal=(gnm_cuidadopersonal/dacvm918)*100 if d_lev==6
replace gnm_cuidadopersonal=(gnm_cuidadopersonal/dacvm918)*100 if d_lev==7
replace gnm_cuidadopersonal=(gnm_cuidadopersonal/dacvm918)*100 if d_lev==8
replace gnm_cuidadopersonal=(gnm_cuidadopersonal/dacvm1018)*100 if d_lev==9
replace gnm_cuidadopersonal=(gnm_cuidadopersonal/dacvm1018)*100 if d_lev==0
label var gnm_cuidadopersonal "Gasto no monetario en cuidado personal deflactado"
*Gasto no monetario en accesorios personales deflactado*
gen gnm_accesoriospersonales=gastonomonetario if (clave>="H123" & clave<="H131") |clave=="H133"
replace gnm_accesoriospersonales=(gnm_accesoriospersonales/dacvt518)*100 if d_lev==1
replace gnm_accesoriospersonales=(gnm_accesoriospersonales/dacvt518)*100 if d_lev==2
replace gnm_accesoriospersonales=(gnm_accesoriospersonales/dacvt618)*100 if d_lev==3
replace gnm_accesoriospersonales=(gnm_accesoriospersonales/dacvt618)*100 if d_lev==4
replace gnm_accesoriospersonales=(gnm_accesoriospersonales/dacvt618)*100 if d_lev==5
replace gnm_accesoriospersonales=(gnm_accesoriospersonales/dacvt718)*100 if d_lev==6
replace gnm_accesoriospersonales=(gnm_accesoriospersonales/dacvt718)*100 if d_lev==7
replace gnm_accesoriospersonales=(gnm_accesoriospersonales/dacvt718)*100 if d_lev==8
replace gnm_accesoriospersonales=(gnm_accesoriospersonales/dacvt818)*100 if d_lev==9
replace gnm_accesoriospersonales=(gnm_accesoriospersonales/dacvt818)*100 if d_lev==0
label var gnm_accesoriospersonales "Gasto no monetario en accesorios personales deflactado"
*gasto no monetario en otros gastos y transferencias deflactado*
gen gnm_otrosgastos=gastonomonetario if (clave>="N001" & clave<="N002") |(clave>="N006" & clave<="N016") |(clave>="T901" & clave<="T915") |clave=="R012"
replace gnm_otrosgastos=(gnm_otrosgastos/dinpcs218)*100 if d_lev==1
replace gnm_otrosgastos=(gnm_otrosgastos/dinpcs218)*100 if d_lev==2
replace gnm_otrosgastos=(gnm_otrosgastos/dinpcs318)*100 if d_lev==3
replace gnm_otrosgastos=(gnm_otrosgastos/dinpcs318)*100 if d_lev==4
replace gnm_otrosgastos=(gnm_otrosgastos/dinpcs318)*100 if d_lev==5
replace gnm_otrosgastos=(gnm_otrosgastos/dinpcs418)*100 if d_lev==6
replace gnm_otrosgastos=(gnm_otrosgastos/dinpcs418)*100 if d_lev==7
replace gnm_otrosgastos=(gnm_otrosgastos/dinpcs418)*100 if d_lev==8
replace gnm_otrosgastos=(gnm_otrosgastos/dinpcs518)*100 if d_lev==9
replace gnm_otrosgastos=(gnm_otrosgastos/dinpcs518)*100 if d_lev==0
label var gnm_otrosgastos "Gasto no monetario en otros gastos y transferencias deflactado"
*Gasto no monetario en regalos otoragdos deflactado
gen gnm_regalosotorgados=gastonomonetario if (clave>="T901" & clave<="T915") |clave=="N013"
replace gnm_regalosotorgados=(gnm_regalosotorgados/dinpcs218)*100 if d_lev==1
replace gnm_regalosotorgados=(gnm_regalosotorgados/dinpcs218)*100 if d_lev==2
replace gnm_regalosotorgados=(gnm_regalosotorgados/dinpcs318)*100 if d_lev==3
replace gnm_regalosotorgados=(gnm_regalosotorgados/dinpcs318)*100 if d_lev==4
replace gnm_regalosotorgados=(gnm_regalosotorgados/dinpcs318)*100 if d_lev==5
replace gnm_regalosotorgados=(gnm_regalosotorgados/dinpcs418)*100 if d_lev==6
replace gnm_regalosotorgados=(gnm_regalosotorgados/dinpcs418)*100 if d_lev==7
replace gnm_regalosotorgados=(gnm_regalosotorgados/dinpcs418)*100 if d_lev==8
replace gnm_regalosotorgados=(gnm_regalosotorgados/dinpcs518)*100 if d_lev==9
replace gnm_regalosotorgados=(gnm_regalosotorgados/dinpcs518)*100 if d_lev==0
label var gnm_regalosotorgados "Gasto no monetario en regalos otorgados deflactado"

save ingresonomonetariodeflac18.dta,replace

*En especie
use ingresonomonetariodeflac18.dta,clear
keep if especie==1
collapse (sum) gnm_*,by(idhogar)
rename gnm_alcoholytabaco gnm_alcoholytabacoespecie
rename gnm_alimentos gnm_alimentosespecie
rename gnm_vestidoycalzado gnm_vestidoycalzadoespecie
rename gnm_viviendas gnm_viviendasespecie
rename gnm_alimpieza gnm_alimpiezaespecie
rename gnm_cristaleria gnm_cristaleriaespecie
rename gnm_edomesticos gnm_edomesticosespecie
rename gnm_salud gnm_saludespecie
rename gnm_transportepublico gnm_transportepublicoespecie
rename gnm_transporteforaneo gnm_transporteforaneoespecie
rename gnm_comunicaciones gnm_comunicacionesespecie
rename gnm_educacionyrecreacion gnm_educacionyrecreacionespecie
rename gnm_educacionbasica gnm_educacionbasicaespecie
rename gnm_cuidadopersonal gnm_cuidadopersonalespecie
rename gnm_accesoriospersonales gnm_accesoriospersonalesespecie
rename gnm_otrosgastos gnm_otrosgastosespecie
rename gnm_regalosotorgados gnm_regalosotorgadosespecie
sort idhogar
save especiedeflac18.dta,replace
*En regalos
use ingresonomonetariodeflac18.dta,clear
keep if regalos==1
collapse (sum) gnm_*,by(idhogar)

rename gnm_alcoholytabaco gnm_alcoholytabacoregalos
rename gnm_alimentos gnm_alimentosregalos
rename gnm_vestidoycalzado gnm_vestidoycalzadoregalos
rename gnm_viviendas gnm_viviendasregalos
rename gnm_alimpieza gnm_alimpiezaregalos
rename gnm_cristaleria gnm_cristaleriaregalos
rename gnm_edomesticos gnm_edomesticosregalos
rename gnm_salud gnm_saludregalos
rename gnm_transportepublico gnm_transportepublicoregalos
rename gnm_transporteforaneo gnm_transporteforaneoregalos
rename gnm_comunicaciones gnm_comunicacionesregalos
rename gnm_educacionyrecreacion gnm_educacionyrecreacionregalos
rename gnm_educacionbasica gnm_educacionbasicaregalos
rename gnm_cuidadopersonal gnm_cuidadopersonalregalos
rename gnm_accesoriospersonales gnm_accesoriospersonalesregalos
rename gnm_otrosgastos gnm_otrosgastosregalos
rename gnm_regalosotorgados gnm_regalosotorgadosregalos

save regalosdeflac18.dta,replace

**Ingreso corriente total
*concentrado del hogar
use concentradohogar18.dta,clear
egen idhogar=concat(folioviv foliohog)
label var idhogar "Identificador del Hogar"
keep idhogar tam_loc factor tot_integ est_dis upm ubica_geo
*merge con las otras bases*
merge 1:m idhogar using ingresodeflactado18.dta
drop _merge
merge 1:m idhogar using especiedeflac18.dta
drop _merge
merge 1:m idhogar using regalosdeflac18.dta
drop _merge
*Localidad rural
gen rural=. 
replace rural=0 if tam_loc<="3"
replace rural=1 if tam_loc=="4"
label var rural "Localidad rural"
label define rural 0 "Urbano" 1 "Rural"
label value rural rural
*Suma de los pagos en especie y regalos
egen pago_especie=rsum(gnm_alcoholytabacoespecie gnm_alimentosespecie gnm_vestidoycalzadoespecie gnm_viviendasespecie gnm_alimpiezaespecie gnm_cristaleriaespecie gnm_edomesticosespecie gnm_saludespecie gnm_transportepublicoespecie gnm_transporteforaneoespecie gnm_comunicacionesespecie gnm_educacionyrecreacionespecie gnm_educacionbasicaespecie gnm_cuidadopersonalespecie gnm_accesoriospersonalesespecie gnm_otrosgastosespecie gnm_regalosotorgadosespecie) 
egen pago_regalos=rsum(gnm_alcoholytabacoregalos gnm_alimentosregalos gnm_vestidoycalzadoregalos gnm_viviendasregalos gnm_alimpiezaregalos gnm_cristaleriaregalos gnm_edomesticosregalos gnm_saludregalos gnm_transportepublicoregalos gnm_transporteforaneoregalos gnm_comunicacionesregalos gnm_educacionyrecreacionregalos gnm_educacionbasicaregalos gnm_cuidadopersonalregalos gnm_accesoriospersonalesregalos gnm_otrosgastosregalos gnm_regalosotorgadosregalos)
egen inomonetario=rsum(pago_especie pago_regalos)
egen icorrientetotal=rsum(ingresomonetariop inomonetario)
label var pago_especie "Ingreso corriente no monetario pago especie"
label var pago_regalos "Ingreso corriente no monetario regalos especie"
label var inomonetario "Suma del ingreso corriente no monetario"
label var icorrientetotal "Ingreso corriente total"

save ingresocorrientetotal18.dta,replace

*tamaño del hogar*
use poblacion18.dta,replace
egen idhogar=concat(folioviv foliohog)
label var idhogar "Identificador del Hogar"
drop if parentesco>="400" & parentesco<"500"
drop if parentesco>="700" & parentesco<"800"
*total de los integrantes del hogar*
gen integrante=1
egen tot_integrantes=sum(integrante),by(idhogar)
*collapse
collapse (max) tot_integrantes,by(idhogar)
save tamañohogar18.dta,replace
**Bienestar economico*
use ingresocorrientetotal18.dta,clear
*merge con tamaño de hogar en escala
merge 1:m idhogar using tamañohogar18.dta
drop _merge
*per capita*
gen ictpercapita=icorrientetotal/tot_integrantes
label var ictpercapita "Ingreso corriente total per capita"
*linea de pobreza por ingresos
*julio 2018
scalar lpi_rural18=2288.44
scalar lpi_urbano18=3287.59
gen pobres_lpi=.
replace pobres_lpi=1 if (ictpercapita<lpi_urbano18 & rural==0)
replace pobres_lpi=1 if (ictpercapita<lpi_rural18 & rural==1)
replace pobres_lpi=0 if (ictpercapita>=lpi_urbano18 & rural==0) & ictpercapita!=.
replace pobres_lpi=0 if (ictpercapita>=lpi_rural18 & rural==1) & ictpercapita!=.
label var pobres_lpi "Poblacion con ingreso menor o igual a la linea de pobreza por ingresos"
label define pobres_lpi 0 "Poblacion con ingreso igual o mayor a la Linea de Pobreza por Ingresos" 1 "Poblacion con ingreso menor a la Linea de Pobreza por Ingresos"
label value pobres_lpi pobres_lpi

save lp_ingresos18.dta,replace
*merge
use ic_rezedu18.dta,clear
merge 1:1 idpersona using ic_asalud18.dta
drop _merge
merge 1:1 idpersona using ic_segsoc18.dta
drop _merge
merge m:1 idhogar using ic_cev18.dta
drop _merge
merge m:1 idhogar using ic_sbv18.dta
drop _merge
merge m:1 idhogar using ic_ali18.dta
drop _merge
merge m:1 idhogar using lp_ingresos18.dta
drop _merge

save basefinal18.dta,replace

********
**2020**
********
*Ingreso Monetario*
use trabajos20.dta,clear
keep folioviv foliohog numren id_trabajo pres_2 
destring pres_2 id_trabajo,replace
reshape wide pres_2,i(folioviv foliohog numren) j(id_trabajo)
egen idhogar=concat(folioviv foliohog)
label var idhogar "Identificador del Hogar"
egen idpersona=concat(folioviv foliohog numren)
label var idpersona "Identificador de la persona"
gen trabajo=1
label var trabajo "Poblacion con al menos un empleo"
gen aguinaldo1=0
replace aguinaldo1=1 if pres_21==2
gen aguinaldo2=0
replace aguinaldo2=1 if pres_22==2
label var aguinaldo1 "Aguinaldo por trabajo principal"
label var aguinaldo2 "Aguinaldo por trabajo secundario"
label define aguinaldo 0 "No recibe aguinaldo" 1 "Recibe aguinaldo"
label value aguinaldo1 aguinaldo
label value aguinaldo2 aguinaldo
save aguinaldo20.dta,replace

**base ingresos
use ingresos20.dta,clear
egen idhogar=concat(folioviv foliohog)
label var idhogar "Identificador del Hogar"
egen idpersona=concat(folioviv foliohog numren)
label var idpersona "Identificador de la persona"
*merge con aguinaldos
merge m:1 idpersona using aguinaldo20.dta
drop _merge
*delimitar
drop if clave=="P009" & aguinaldo1!=1
drop if clave=="P016" & aguinaldo2!=1
*La base es el mes de julio de 2023
gen ingresod1=(ing_1/83.3985345256)*100 if mes_1=="07"
replace ingresod1=(ing_1/83.7268691008)*100 if mes_1=="08"
replace ingresod1=(ing_1/83.9185916542)*100 if mes_1=="09"
replace ingresod1=(ing_1/84.4308867362)*100 if mes_1=="10"

gen ingresod2=(ing_2/82.8544150522)*100 if mes_2=="06"
replace ingresod2=(ing_2/83.3985345256)*100 if mes_2=="07"
replace ingresod2=(ing_2/83.7268691008)*100 if mes_2=="08"
replace ingresod2=(ing_2/83.9185916542)*100 if mes_2=="09"

gen ingresod3=(ing_3/82.4034401391)*100 if mes_3=="05"
replace ingresod3=(ing_3/82.8544150522)*100 if mes_3=="06"
replace ingresod3=(ing_3/83.3985345256)*100 if mes_3=="07"
replace ingresod3=(ing_3/83.7268691008)*100 if mes_3=="08"

gen ingresod4=(ing_4/82.0875248385)*100 if mes_4=="04"
replace ingresod4=(ing_4/82.4034401391)*100 if mes_4=="05"
replace ingresod4=(ing_4/82.8544150522)*100 if mes_4=="06"
replace ingresod4=(ing_4/83.3985345256)*100 if mes_4=="07"

gen ingresod5=(ing_5/82.9281544958)*100 if mes_5=="03"
replace ingresod5=(ing_5/82.0875248385)*100 if mes_5=="04"
replace ingresod5=(ing_5/82.4034401391)*100 if mes_5=="05"
replace ingresod5=(ing_5/82.8544150522)*100 if mes_5=="06"

gen ingresod6=(ing_6/82.9677409339)*100 if mes_6=="02"
replace ingresod6=(ing_6/82.9281544958)*100 if mes_6=="03"
replace ingresod6=(ing_6/82.0875248385)*100 if mes_6=="04"
replace ingresod6=(ing_6/82.4034401391)*100 if mes_6=="05"

*deflactar los repartos de utilidades y los aguinaldos                  
replace ingresod1=((ing_1/82.4034401391)*100)/12 if clave=="P008" |clave=="P015"
replace ingresod1=((ing_1/82.2264654744)*100)/12 if clave=="P009" |clave=="P016"
recode ingresod1 ingresod2 ingresod3 ingresod4 ingresod5 ingresod6 (0=.) 
 
*ingreso promedio mensual
egen ingresopromediodeflac=rowmean(ingresod1 ingresod2 ingresod3 ingresod4 ingresod5 ingresod6)
*ingreso promedio monetario
gen ingresomonetariop=ingresopromediodeflac if clave>="P001" & clave<="P009" |clave>="P011" & clave<="P016" |clave>="P018" & clave<="P048" |clave>="P067" & clave<="P081" |clave>="P101" & clave<="P108"
*gen ingreso laboral promedio*
gen ingresolaboralp=ingresopromediodeflac if clave>="P001" & clave<="P009" |clave>="P011" & clave<="P016" |clave>="P018" & clave<="P022" |clave>="P067" & clave<="P081"
*ingreso por rentas*
gen ingresorentasp=ingresopromediodeflac if clave>="P023" & clave<="P031"
*ingreso por transferencias
gen ingresotransferenciasp=ingresopromediodeflac if clave>="P032" & clave<="P048" |clave>="P101" & clave<="P108"
*collapse
collapse (sum) ingresopromediodeflac ingresomonetariop ingresolaboralp ingresorentasp ingresotransferenciasp,by(idhogar)
label var ingresopromediodeflac "Ingreso corriente total del hogar"
label var ingresomonetariop "Ingreso corriente monetario del hogar"
label var ingresolaboralp "Ingreso corriente monetario laboral"
label var ingresorentasp "Ingreso corriente monetario por rentas"
label var ingresotransferenciasp "ingreso corriente monetario por transferencias"
save ingresodeflactado20.dta,replace

*Ingreso no monetario*
*Ingreso no monetario*
use gastoshogar20.dta,clear
gen base=1
append using gastospersona20.dta
recode base (.=2)
replace frecuencia=frec_rem if base==2
label var base "Origen del monto obtenido"
label define base 1 "Monto del Hogar" 2 "Monto de la persona"
label value base base
egen idhogar=concat(folioviv foliohog)
label var idhogar "Identificador del Hogar"
egen idpersona=concat(folioviv foliohog numren)
label var idpersona "Identificador de la persona"
**decena de levantamiento**
gen d_lev=substr(folioviv,8,1)
label var d_lev "Decena de levantamiento"
destring d_lev,replace
**deflactores**
*alimentos semanal (sin incluir las bebidas alcoholicas)
scalar dalimentosw720=76.134796794583
scalar dalimentosw820=76.596223942335
scalar dalimentosw920=76.813285825450
scalar dalimentosw1020=77.351162441468
scalar dalimentosw1120=77.317033214564
*bebidas alcoholicas y tabaco
scalar dbatw720=85.028132992327
scalar dbatw820=84.648154914140
scalar dbatw920=84.481549141396
scalar dbatw1020=84.700767263427
scalar dbatw1120=84.553891121666
*Ropa,vestido, calzado y accesorios
scalar drvcat520=85.499042587390
scalar drvcat620=86.181111716774
scalar drvcat720=86.877437209107
scalar drvcat820=87.385634617804
*vivienda
scalar dviviendam720=93.83295987
scalar dviviendam820=94.17762299
scalar dviviendam920=94.25501471
scalar dviviendam1020=95.7794516
scalar dviviendam1120=97.70434564
*accesorios y articulos de limpieza para el hogar (mensual)
scalar daalhm720=78.95484094
scalar daalhm820=79.20852732
scalar daalhm920=79.40455771
scalar daalhm1020=79.41969241
scalar daalhm1120=79.27411102
*accesorios y articulos de limpieza para el hogar (trimestral)
scalar daalht520=78.318943357789
scalar daalht620=78.875563947706
scalar daalht720=79.189308656567
scalar daalht820=79.344259144481
*muebles y aparatos domesticos (semestral)
scalar dmads220=90.4159927
scalar dmads320=90.99560694
scalar dmads420=91.65502537
scalar dmads520=92.38046222
*salud (trimestral)
scalar dsaludt520=84.855886790490
scalar dsaludt620=85.065813433224
scalar dsaludt720=85.372249102544
scalar dsaludt820=85.570469798658
*transporte publico urbano (semanal)
scalar dtpurbanow720=91.286296528048
scalar dtpurbanow820=91.451184657794
scalar dtpurbanow920=91.726843787523
scalar dtpurbanow1020=91.741218650116
scalar dtpurbanow1120=91.771659535607
*transporte (mensual)
scalar dtransportem720=83.59897915
scalar dtransportem820=83.70491645
scalar dtransportem920=83.78517199
scalar dtransportem1020=83.73782122
scalar dtransportem1120=82.30927272
*transporte (semestral)
scalar dtransportes220=81.23117339
scalar dtransportes320=81.07226743
scalar dtransportes420=81.34674136
scalar dtransportes520=82.56729426
*educacion y esparcimiento 
scalar deducacionyespm720=89.2912466
scalar deducacionyespm820=89.43430085
scalar deducacionyespm920=89.98874188
scalar deducacionyespm1020=90.11402016
scalar deducacionyespm1120=89.99720663
*accesorios y cuidados del vestido (mensual)
scalar dacvm720=87.76337183
scalar dacvm820=88.2528036
scalar dacvm920=88.75140701
scalar dacvm1020=88.68387043
scalar dacvm1120=87.98765998
*accesorios y cuidados del vestido (trimestral)
scalar dacvt520=86.543266491572
scalar dacvt620=87.333416711830
scalar dacvt720=88.255860813496
scalar dacvt820=88.562693681300
*inpc semestral
scalar dinpcs220=82.77330166
scalar dinpcs320=82.89982303
scalar dinpcs420=83.06489589
scalar dinpcs520=83.4554562
*regalos y especie
gen gastonomonetario=gas_nm_tri/3
label var gastonomonetario "Gasto no monetario mensual"
gen especie=1 if tipo_gasto=="G4"
gen regalos=1 if tipo_gasto=="G5"
replace regalos=1 if tipo_gasto=="G6"
drop if tipo_gasto=="G2" |tipo_gasto=="G3" |tipo_gasto=="G7"
label var especie "Gasto no monetario por remuneraciones en especie"
label var regalos "Gasto no monetario por regalos de otro hogar y transferencias de instituciones"
*control de la frecuencia de los regalos recibidos en el hogar
drop if ((frecuencia>="5" & frecuencia<="6") |frecuencia==" " |frecuencia=="0") & base==1 & tipo_gasto=="G5"
drop if (frecuencia=="9" |frecuencia=="") & base==2 & tipo_gasto=="G5"
**Hora de deflactar
*Gasto no monetario en alimentos deflactado
gen gnm_alimentos=gastonomonetario if (clave>="A001" & clave<="A222") |(clave>="A242" & clave<="A247")
replace gnm_alimentos=(gnm_alimentos/dalimentosw820)*100 if d_lev==1
replace gnm_alimentos=(gnm_alimentos/dalimentosw820)*100 if d_lev==2
replace gnm_alimentos=(gnm_alimentos/dalimentosw820)*100 if d_lev==3
replace gnm_alimentos=(gnm_alimentos/dalimentosw920)*100 if d_lev==4
replace gnm_alimentos=(gnm_alimentos/dalimentosw920)*100 if d_lev==5
replace gnm_alimentos=(gnm_alimentos/dalimentosw920)*100 if d_lev==6
replace gnm_alimentos=(gnm_alimentos/dalimentosw1020)*100 if d_lev==7
replace gnm_alimentos=(gnm_alimentos/dalimentosw1020)*100 if d_lev==8
replace gnm_alimentos=(gnm_alimentos/dalimentosw1020)*100 if d_lev==9
replace gnm_alimentos=(gnm_alimentos/dalimentosw1120)*100 if d_lev==0
label var gnm_alimentos "Gasto no monetario en alimentos deflactado"
*Gasto no monetario en alcohol y tabaco deflactado
gen gnm_alcoholytabaco=gastonomonetario if clave>="A223" & clave<="A241"
replace gnm_alcoholytabaco=(gnm_alcoholytabaco/dbatw820)*100 if d_lev==1
replace gnm_alcoholytabaco=(gnm_alcoholytabaco/dbatw820)*100 if d_lev==2
replace gnm_alcoholytabaco=(gnm_alcoholytabaco/dbatw820)*100 if d_lev==3
replace gnm_alcoholytabaco=(gnm_alcoholytabaco/dbatw920)*100 if d_lev==4
replace gnm_alcoholytabaco=(gnm_alcoholytabaco/dbatw920)*100 if d_lev==5
replace gnm_alcoholytabaco=(gnm_alcoholytabaco/dbatw920)*100 if d_lev==6
replace gnm_alcoholytabaco=(gnm_alcoholytabaco/dbatw1020)*100 if d_lev==7
replace gnm_alcoholytabaco=(gnm_alcoholytabaco/dbatw1020)*100 if d_lev==8
replace gnm_alcoholytabaco=(gnm_alcoholytabaco/dbatw1020)*100 if d_lev==9
replace gnm_alcoholytabaco=(gnm_alcoholytabaco/dbatw1120)*100 if d_lev==0
label var gnm_alcoholytabaco "Gasto no monetario en alcohol y tabaco deflactado"
*Gasto no monetario en vestido y calzado deflactado
gen gnm_vestidoycalzado=gastonomonetario if (clave>="H001" & clave<="H122") |clave=="H136"
replace gnm_vestidoycalzado=(gnm_vestidoycalzado/drvcat520)*100 if d_lev==1
replace gnm_vestidoycalzado=(gnm_vestidoycalzado/drvcat520)*100 if d_lev==2
replace gnm_vestidoycalzado=(gnm_vestidoycalzado/drvcat620)*100 if d_lev==3
replace gnm_vestidoycalzado=(gnm_vestidoycalzado/drvcat620)*100 if d_lev==4
replace gnm_vestidoycalzado=(gnm_vestidoycalzado/drvcat620)*100 if d_lev==5
replace gnm_vestidoycalzado=(gnm_vestidoycalzado/drvcat720)*100 if d_lev==6
replace gnm_vestidoycalzado=(gnm_vestidoycalzado/drvcat720)*100 if d_lev==7
replace gnm_vestidoycalzado=(gnm_vestidoycalzado/drvcat720)*100 if d_lev==8
replace gnm_vestidoycalzado=(gnm_vestidoycalzado/drvcat820)*100 if d_lev==9
replace gnm_vestidoycalzado=(gnm_vestidoycalzado/drvcat820)*100 if d_lev==0
label var gnm_vestidoycalzado "Gasto no monetario en vestido y calzado deflactado"
*Gasto no monetario en viviendas deflactado
gen gnm_viviendas=gastonomonetario if (clave>="G001" & clave<="G016") |(clave>="R001" & clave<="R004") |clave=="R013"
replace gnm_viviendas=(gnm_viviendas/dviviendam720)*100 if d_lev==1
replace gnm_viviendas=(gnm_viviendas/dviviendam720)*100 if d_lev==2
replace gnm_viviendas=(gnm_viviendas/dviviendam820)*100 if d_lev==3
replace gnm_viviendas=(gnm_viviendas/dviviendam820)*100 if d_lev==4
replace gnm_viviendas=(gnm_viviendas/dviviendam820)*100 if d_lev==5
replace gnm_viviendas=(gnm_viviendas/dviviendam920)*100 if d_lev==6
replace gnm_viviendas=(gnm_viviendas/dviviendam920)*100 if d_lev==7
replace gnm_viviendas=(gnm_viviendas/dviviendam920)*100 if d_lev==8
replace gnm_viviendas=(gnm_viviendas/dviviendam1020)*100 if d_lev==9
replace gnm_viviendas=(gnm_viviendas/dviviendam1020)*100 if d_lev==0
label var gnm_viviendas "Gasto no monetario en viviendas deflactado"
*Gasto no monetario en Articulos de limpieza deflactado
gen gnm_alimpieza=gastonomonetario if clave>="C001" & clave<="C024"
replace gnm_alimpieza=(gnm_alimpieza/daalhm720)*100 if d_lev==1
replace gnm_alimpieza=(gnm_alimpieza/daalhm720)*100 if d_lev==2
replace gnm_alimpieza=(gnm_alimpieza/daalhm820)*100 if d_lev==3
replace gnm_alimpieza=(gnm_alimpieza/daalhm820)*100 if d_lev==4
replace gnm_alimpieza=(gnm_alimpieza/daalhm820)*100 if d_lev==5
replace gnm_alimpieza=(gnm_alimpieza/daalhm920)*100 if d_lev==6
replace gnm_alimpieza=(gnm_alimpieza/daalhm920)*100 if d_lev==7
replace gnm_alimpieza=(gnm_alimpieza/daalhm920)*100 if d_lev==8
replace gnm_alimpieza=(gnm_alimpieza/daalhm1020)*100 if d_lev==9
replace gnm_alimpieza=(gnm_alimpieza/daalhm1020)*100 if d_lev==0
label var gnm_alimpieza "Gasto no monetario en accesorios y articulos de limpieza"
*Gasto no monetario en cristaleria y blancos deflactado
gen gnm_cristaleria=gastonomonetario if clave>="I001" & clave<="I026"
replace gnm_cristaleria=(gnm_cristaleria/daalht520)*100 if d_lev==1
replace gnm_cristaleria=(gnm_cristaleria/daalht520)*100 if d_lev==2
replace gnm_cristaleria=(gnm_cristaleria/daalht620)*100 if d_lev==3
replace gnm_cristaleria=(gnm_cristaleria/daalht620)*100 if d_lev==4
replace gnm_cristaleria=(gnm_cristaleria/daalht620)*100 if d_lev==5
replace gnm_cristaleria=(gnm_cristaleria/daalht720)*100 if d_lev==6
replace gnm_cristaleria=(gnm_cristaleria/daalht720)*100 if d_lev==7
replace gnm_cristaleria=(gnm_cristaleria/daalht720)*100 if d_lev==8
replace gnm_cristaleria=(gnm_cristaleria/daalht820)*100 if d_lev==9
replace gnm_cristaleria=(gnm_cristaleria/daalht820)*100 if d_lev==0
label var gnm_cristaleria "Gasto no monetario en cristaleria y blancos deflactado"
*Gasto no monetario en enseres domesticos y muebles deflactado
gen gnm_edomesticos=gastonomonetario if clave>="K001" & clave<="K037"
replace gnm_edomesticos=(gnm_edomesticos/dmads220)*100 if d_lev==1
replace gnm_edomesticos=(gnm_edomesticos/dmads220)*100 if d_lev==2
replace gnm_edomesticos=(gnm_edomesticos/dmads320)*100 if d_lev==3
replace gnm_edomesticos=(gnm_edomesticos/dmads320)*100 if d_lev==4
replace gnm_edomesticos=(gnm_edomesticos/dmads320)*100 if d_lev==5
replace gnm_edomesticos=(gnm_edomesticos/dmads420)*100 if d_lev==6
replace gnm_edomesticos=(gnm_edomesticos/dmads420)*100 if d_lev==7
replace gnm_edomesticos=(gnm_edomesticos/dmads420)*100 if d_lev==8
replace gnm_edomesticos=(gnm_edomesticos/dmads520)*100 if d_lev==9
replace gnm_edomesticos=(gnm_edomesticos/dmads520)*100 if d_lev==0
label var gnm_edomesticos "Gasto no monetario en enseres domesticos y muebles deflactado"
*Gasto no monetario en salud deflactado
gen gnm_salud=gastonomonetario if clave>="J001" & clave<="J072"
replace gnm_salud=(gnm_salud/dsaludt520)*100 if d_lev==1
replace gnm_salud=(gnm_salud/dsaludt520)*100 if d_lev==2
replace gnm_salud=(gnm_salud/dsaludt620)*100 if d_lev==3
replace gnm_salud=(gnm_salud/dsaludt620)*100 if d_lev==4
replace gnm_salud=(gnm_salud/dsaludt620)*100 if d_lev==5
replace gnm_salud=(gnm_salud/dsaludt720)*100 if d_lev==6
replace gnm_salud=(gnm_salud/dsaludt720)*100 if d_lev==7
replace gnm_salud=(gnm_salud/dsaludt720)*100 if d_lev==8
replace gnm_salud=(gnm_salud/dsaludt820)*100 if d_lev==9
replace gnm_salud=(gnm_salud/dsaludt820)*100 if d_lev==0
label var gnm_salud "Gasto no monetario en salud deflactado"
*Gasto no monetario en transporte publico deflactado
gen gnm_transportepublico=gastonomonetario if clave>="B001" & clave<="B007"
replace gnm_transportepublico=(gnm_transportepublico/dtpurbanow820)*100 if d_lev==1
replace gnm_transportepublico=(gnm_transportepublico/dtpurbanow820)*100 if d_lev==2
replace gnm_transportepublico=(gnm_transportepublico/dtpurbanow820)*100 if d_lev==3
replace gnm_transportepublico=(gnm_transportepublico/dtpurbanow920)*100 if d_lev==4
replace gnm_transportepublico=(gnm_transportepublico/dtpurbanow920)*100 if d_lev==5
replace gnm_transportepublico=(gnm_transportepublico/dtpurbanow920)*100 if d_lev==6
replace gnm_transportepublico=(gnm_transportepublico/dtpurbanow1020)*100 if d_lev==7
replace gnm_transportepublico=(gnm_transportepublico/dtpurbanow1020)*100 if d_lev==8
replace gnm_transportepublico=(gnm_transportepublico/dtpurbanow1020)*100 if d_lev==9
replace gnm_transportepublico=(gnm_transportepublico/dtpurbanow1120)*100 if d_lev==0
label var gnm_transportepublico "Gasto no monetario en transporte publico deflactado"
*Gasto no monetario en transporte foraneo deflactado*
gen gnm_transporteforaneo=gastonomonetario if (clave>="M001" & clave<="M018") |(clave>="F007" & clave<="F014")
replace gnm_transporteforaneo=(gnm_transporteforaneo/dtransportes220)*100 if d_lev==1
replace gnm_transporteforaneo=(gnm_transporteforaneo/dtransportes220)*100 if d_lev==2
replace gnm_transporteforaneo=(gnm_transporteforaneo/dtransportes320)*100 if d_lev==3
replace gnm_transporteforaneo=(gnm_transporteforaneo/dtransportes320)*100 if d_lev==4
replace gnm_transporteforaneo=(gnm_transporteforaneo/dtransportes320)*100 if d_lev==5
replace gnm_transporteforaneo=(gnm_transporteforaneo/dtransportes420)*100 if d_lev==6
replace gnm_transporteforaneo=(gnm_transporteforaneo/dtransportes420)*100 if d_lev==7
replace gnm_transporteforaneo=(gnm_transporteforaneo/dtransportes420)*100 if d_lev==8
replace gnm_transporteforaneo=(gnm_transporteforaneo/dtransportes520)*100 if d_lev==9
replace gnm_transporteforaneo=(gnm_transporteforaneo/dtransportes520)*100 if d_lev==0
label var gnm_transporteforaneo "Gasto no monetario en transporte foraneo deflactado"
*Gasto no monetario en comunicaciones deflactado*
gen gnm_comunicaciones=gastonomonetario if (clave>="F001" & clave<="F006") |(clave>="R005" & clave<="R008") |(clave>="R010" & clave<="R011")
replace gnm_comunicaciones=(gnm_comunicaciones/dtransportem720)*100 if d_lev==1
replace gnm_comunicaciones=(gnm_comunicaciones/dtransportem720)*100 if d_lev==2
replace gnm_comunicaciones=(gnm_comunicaciones/dtransportem820)*100 if d_lev==3
replace gnm_comunicaciones=(gnm_comunicaciones/dtransportem820)*100 if d_lev==4
replace gnm_comunicaciones=(gnm_comunicaciones/dtransportem820)*100 if d_lev==5
replace gnm_comunicaciones=(gnm_comunicaciones/dtransportem920)*100 if d_lev==6
replace gnm_comunicaciones=(gnm_comunicaciones/dtransportem920)*100 if d_lev==7
replace gnm_comunicaciones=(gnm_comunicaciones/dtransportem920)*100 if d_lev==8
replace gnm_comunicaciones=(gnm_comunicaciones/dtransportem1020)*100 if d_lev==9
replace gnm_comunicaciones=(gnm_comunicaciones/dtransportem1020)*100 if d_lev==0
label var gnm_comunicaciones "Gasto no monetario en comunicaciones deflactado"
*Gasto no monetario en educacion y recreacion deflectado
gen gnm_educacionyrecreacion=gastonomonetario if (clave>="E001" & clave<="E034") |(clave>="H134" & clave<="H135") |(clave>="L001" & clave<="L029") |(clave>="N003" & clave<="N005") |clave=="R009"
replace gnm_educacionyrecreacion=(gnm_educacionyrecreacion/deducacionyespm720)*100 if d_lev==1
replace gnm_educacionyrecreacion=(gnm_educacionyrecreacion/deducacionyespm720)*100 if d_lev==2
replace gnm_educacionyrecreacion=(gnm_educacionyrecreacion/deducacionyespm820)*100 if d_lev==3
replace gnm_educacionyrecreacion=(gnm_educacionyrecreacion/deducacionyespm820)*100 if d_lev==4
replace gnm_educacionyrecreacion=(gnm_educacionyrecreacion/deducacionyespm820)*100 if d_lev==5
replace gnm_educacionyrecreacion=(gnm_educacionyrecreacion/deducacionyespm920)*100 if d_lev==6
replace gnm_educacionyrecreacion=(gnm_educacionyrecreacion/deducacionyespm920)*100 if d_lev==7
replace gnm_educacionyrecreacion=(gnm_educacionyrecreacion/deducacionyespm920)*100 if d_lev==8
replace gnm_educacionyrecreacion=(gnm_educacionyrecreacion/deducacionyespm1020)*100 if d_lev==9
replace gnm_educacionyrecreacion=(gnm_educacionyrecreacion/deducacionyespm1020)*100 if d_lev==0
label var gnm_educacionyrecreacion "Gasto no monetario en educacion y recreacion deflactado"
*Gasto no monetario en Educacion basica deflactado*
gen gnm_educacionbasica=gastonomonetario if (clave>="E002" & clave<="E003") |(clave>="H134" & clave<="H135")
replace gnm_educacionbasica=(gnm_educacionbasica/deducacionyespm720)*100 if d_lev==1
replace gnm_educacionbasica=(gnm_educacionbasica/deducacionyespm720)*100 if d_lev==2
replace gnm_educacionbasica=(gnm_educacionbasica/deducacionyespm820)*100 if d_lev==3
replace gnm_educacionbasica=(gnm_educacionbasica/deducacionyespm820)*100 if d_lev==4
replace gnm_educacionbasica=(gnm_educacionbasica/deducacionyespm820)*100 if d_lev==5
replace gnm_educacionbasica=(gnm_educacionbasica/deducacionyespm920)*100 if d_lev==6
replace gnm_educacionbasica=(gnm_educacionbasica/deducacionyespm920)*100 if d_lev==7
replace gnm_educacionbasica=(gnm_educacionbasica/deducacionyespm920)*100 if d_lev==8
replace gnm_educacionbasica=(gnm_educacionbasica/deducacionyespm1020)*100 if d_lev==9
replace gnm_educacionbasica=(gnm_educacionbasica/deducacionyespm1020)*100 if d_lev==0
label var gnm_educacionbasica "Gasto no monetario en educacion basica deflactado"
*Gasto no monetario en cuidado personal deflactado*
gen gnm_cuidadopersonal=gastonomonetario if (clave>="D001" & clave<="D026") |clave=="H132"
replace gnm_cuidadopersonal=(gnm_cuidadopersonal/dacvm720)*100 if d_lev==1
replace gnm_cuidadopersonal=(gnm_cuidadopersonal/dacvm720)*100 if d_lev==2
replace gnm_cuidadopersonal=(gnm_cuidadopersonal/dacvm820)*100 if d_lev==3
replace gnm_cuidadopersonal=(gnm_cuidadopersonal/dacvm820)*100 if d_lev==4
replace gnm_cuidadopersonal=(gnm_cuidadopersonal/dacvm820)*100 if d_lev==5
replace gnm_cuidadopersonal=(gnm_cuidadopersonal/dacvm920)*100 if d_lev==6
replace gnm_cuidadopersonal=(gnm_cuidadopersonal/dacvm920)*100 if d_lev==7
replace gnm_cuidadopersonal=(gnm_cuidadopersonal/dacvm920)*100 if d_lev==8
replace gnm_cuidadopersonal=(gnm_cuidadopersonal/dacvm1020)*100 if d_lev==9
replace gnm_cuidadopersonal=(gnm_cuidadopersonal/dacvm1020)*100 if d_lev==0
label var gnm_cuidadopersonal "Gasto no monetario en cuidado personal deflactado"
*Gasto no monetario en accesorios personales deflactado*
gen gnm_accesoriospersonales=gastonomonetario if (clave>="H123" & clave<="H131") |clave=="H133"
replace gnm_accesoriospersonales=(gnm_accesoriospersonales/dacvt520)*100 if d_lev==1
replace gnm_accesoriospersonales=(gnm_accesoriospersonales/dacvt520)*100 if d_lev==2
replace gnm_accesoriospersonales=(gnm_accesoriospersonales/dacvt620)*100 if d_lev==3
replace gnm_accesoriospersonales=(gnm_accesoriospersonales/dacvt620)*100 if d_lev==4
replace gnm_accesoriospersonales=(gnm_accesoriospersonales/dacvt620)*100 if d_lev==5
replace gnm_accesoriospersonales=(gnm_accesoriospersonales/dacvt720)*100 if d_lev==6
replace gnm_accesoriospersonales=(gnm_accesoriospersonales/dacvt720)*100 if d_lev==7
replace gnm_accesoriospersonales=(gnm_accesoriospersonales/dacvt720)*100 if d_lev==8
replace gnm_accesoriospersonales=(gnm_accesoriospersonales/dacvt820)*100 if d_lev==9
replace gnm_accesoriospersonales=(gnm_accesoriospersonales/dacvt820)*100 if d_lev==0
label var gnm_accesoriospersonales "Gasto no monetario en accesorios personales deflactado"
*gasto no monetario en otros gastos y transferencias deflactado*
gen gnm_otrosgastos=gastonomonetario if (clave>="N001" & clave<="N002") |(clave>="N006" & clave<="N016") |(clave>="T901" & clave<="T915") |clave=="R012"
replace gnm_otrosgastos=(gnm_otrosgastos/dinpcs220)*100 if d_lev==1
replace gnm_otrosgastos=(gnm_otrosgastos/dinpcs220)*100 if d_lev==2
replace gnm_otrosgastos=(gnm_otrosgastos/dinpcs320)*100 if d_lev==3
replace gnm_otrosgastos=(gnm_otrosgastos/dinpcs320)*100 if d_lev==4
replace gnm_otrosgastos=(gnm_otrosgastos/dinpcs320)*100 if d_lev==5
replace gnm_otrosgastos=(gnm_otrosgastos/dinpcs420)*100 if d_lev==6
replace gnm_otrosgastos=(gnm_otrosgastos/dinpcs420)*100 if d_lev==7
replace gnm_otrosgastos=(gnm_otrosgastos/dinpcs420)*100 if d_lev==8
replace gnm_otrosgastos=(gnm_otrosgastos/dinpcs520)*100 if d_lev==9
replace gnm_otrosgastos=(gnm_otrosgastos/dinpcs520)*100 if d_lev==0
label var gnm_otrosgastos "Gasto no monetario en otros gastos y transferencias deflactado"
*Gasto no monetario en regalos otoragdos deflactado
gen gnm_regalosotorgados=gastonomonetario if (clave>="T901" & clave<="T915") |clave=="N013"
replace gnm_regalosotorgados=(gnm_regalosotorgados/dinpcs220)*100 if d_lev==1
replace gnm_regalosotorgados=(gnm_regalosotorgados/dinpcs220)*100 if d_lev==2
replace gnm_regalosotorgados=(gnm_regalosotorgados/dinpcs320)*100 if d_lev==3
replace gnm_regalosotorgados=(gnm_regalosotorgados/dinpcs320)*100 if d_lev==4
replace gnm_regalosotorgados=(gnm_regalosotorgados/dinpcs320)*100 if d_lev==5
replace gnm_regalosotorgados=(gnm_regalosotorgados/dinpcs420)*100 if d_lev==6
replace gnm_regalosotorgados=(gnm_regalosotorgados/dinpcs420)*100 if d_lev==7
replace gnm_regalosotorgados=(gnm_regalosotorgados/dinpcs420)*100 if d_lev==8
replace gnm_regalosotorgados=(gnm_regalosotorgados/dinpcs520)*100 if d_lev==9
replace gnm_regalosotorgados=(gnm_regalosotorgados/dinpcs520)*100 if d_lev==0
label var gnm_regalosotorgados "Gasto no monetario en regalos otorgados deflactado"

save ingresonomonetariodeflac20.dta,replace

*En especie
use ingresonomonetariodeflac20.dta,clear
keep if especie==1
collapse (sum) gnm_*,by(idhogar)
rename gnm_alcoholytabaco gnm_alcoholytabacoespecie
rename gnm_alimentos gnm_alimentosespecie
rename gnm_vestidoycalzado gnm_vestidoycalzadoespecie
rename gnm_viviendas gnm_viviendasespecie
rename gnm_alimpieza gnm_alimpiezaespecie
rename gnm_cristaleria gnm_cristaleriaespecie
rename gnm_edomesticos gnm_edomesticosespecie
rename gnm_salud gnm_saludespecie
rename gnm_transportepublico gnm_transportepublicoespecie
rename gnm_transporteforaneo gnm_transporteforaneoespecie
rename gnm_comunicaciones gnm_comunicacionesespecie
rename gnm_educacionyrecreacion gnm_educacionyrecreacionespecie
rename gnm_educacionbasica gnm_educacionbasicaespecie
rename gnm_cuidadopersonal gnm_cuidadopersonalespecie
rename gnm_accesoriospersonales gnm_accesoriospersonalesespecie
rename gnm_otrosgastos gnm_otrosgastosespecie
rename gnm_regalosotorgados gnm_regalosotorgadosespecie
sort idhogar
save especiedeflac20.dta,replace
*En regalos
use ingresonomonetariodeflac20.dta,clear
keep if regalos==1
collapse (sum) gnm_*,by(idhogar)

rename gnm_alcoholytabaco gnm_alcoholytabacoregalos
rename gnm_alimentos gnm_alimentosregalos
rename gnm_vestidoycalzado gnm_vestidoycalzadoregalos
rename gnm_viviendas gnm_viviendasregalos
rename gnm_alimpieza gnm_alimpiezaregalos
rename gnm_cristaleria gnm_cristaleriaregalos
rename gnm_edomesticos gnm_edomesticosregalos
rename gnm_salud gnm_saludregalos
rename gnm_transportepublico gnm_transportepublicoregalos
rename gnm_transporteforaneo gnm_transporteforaneoregalos
rename gnm_comunicaciones gnm_comunicacionesregalos
rename gnm_educacionyrecreacion gnm_educacionyrecreacionregalos
rename gnm_educacionbasica gnm_educacionbasicaregalos
rename gnm_cuidadopersonal gnm_cuidadopersonalregalos
rename gnm_accesoriospersonales gnm_accesoriospersonalesregalos
rename gnm_otrosgastos gnm_otrosgastosregalos
rename gnm_regalosotorgados gnm_regalosotorgadosregalos

save regalosdeflac20.dta,replace

**Ingreso corriente total
*concentrado del hogar
use concentradohogar20.dta,clear
egen idhogar=concat(folioviv foliohog)
label var idhogar "Identificador del Hogar"
keep idhogar tam_loc factor tot_integ est_dis upm ubica_geo
*merge con las otras bases*
merge 1:m idhogar using ingresodeflactado20.dta
drop _merge
merge 1:m idhogar using especiedeflac20.dta
drop _merge
merge 1:m idhogar using regalosdeflac20.dta
drop _merge
*Localidad rural
gen rural=. 
replace rural=0 if tam_loc<="3"
replace rural=1 if tam_loc=="4"
label var rural "Localidad rural"
label define rural 0 "Urbano" 1 "Rural"
label value rural rural
*Suma de los pagos en especie y regalos
egen pago_especie=rsum(gnm_alcoholytabacoespecie gnm_alimentosespecie gnm_vestidoycalzadoespecie gnm_viviendasespecie gnm_alimpiezaespecie gnm_cristaleriaespecie gnm_edomesticosespecie gnm_saludespecie gnm_transportepublicoespecie gnm_transporteforaneoespecie gnm_comunicacionesespecie gnm_educacionyrecreacionespecie gnm_educacionbasicaespecie gnm_cuidadopersonalespecie gnm_accesoriospersonalesespecie gnm_otrosgastosespecie gnm_regalosotorgadosespecie) 
egen pago_regalos=rsum(gnm_alcoholytabacoregalos gnm_alimentosregalos gnm_vestidoycalzadoregalos gnm_viviendasregalos gnm_alimpiezaregalos gnm_cristaleriaregalos gnm_edomesticosregalos gnm_saludregalos gnm_transportepublicoregalos gnm_transporteforaneoregalos gnm_comunicacionesregalos gnm_educacionyrecreacionregalos gnm_educacionbasicaregalos gnm_cuidadopersonalregalos gnm_accesoriospersonalesregalos gnm_otrosgastosregalos gnm_regalosotorgadosregalos)
egen inomonetario=rsum(pago_especie pago_regalos)
egen icorrientetotal=rsum(ingresomonetariop inomonetario)
label var pago_especie "Ingreso corriente no monetario pago especie"
label var pago_regalos "Ingreso corriente no monetario regalos especie"
label var inomonetario "Suma del ingreso corriente no monetario"
label var icorrientetotal "Ingreso corriente total"

save ingresocorrientetotal20.dta,replace

*tamaño del hogar*
use poblacion20.dta,replace
egen idhogar=concat(folioviv foliohog)
label var idhogar "Identificador del Hogar"
drop if parentesco>="400" & parentesco<"500"
drop if parentesco>="700" & parentesco<"800"
*total de los integrantes del hogar*
gen integrante=1
egen tot_integrantes=sum(integrante),by(idhogar)
*collapse
collapse (max) tot_integrantes,by(idhogar)
save tamañohogar20.dta,replace
**Bienestar economico*
use ingresocorrientetotal20.dta,clear
*merge con tamaño de hogar en escala
merge 1:m idhogar using tamañohogar20.dta
drop _merge
*per capita*
gen ictpercapita=icorrientetotal/tot_integrantes
label var ictpercapita "Ingreso corriente total per capita"
*linea de pobreza por ingresos
*julio 2020
scalar lpi_rural20=2503.19
scalar lpi_urbano20=3536.85
gen pobres_lpi=.
replace pobres_lpi=1 if (ictpercapita<lpi_urbano20 & rural==0)
replace pobres_lpi=1 if (ictpercapita<lpi_rural20 & rural==1)
replace pobres_lpi=0 if (ictpercapita>=lpi_urbano20 & rural==0) & ictpercapita!=.
replace pobres_lpi=0 if (ictpercapita>=lpi_rural20 & rural==1) & ictpercapita!=.
label var pobres_lpi "Poblacion con ingreso menor o igual a la linea de pobreza por ingresos"
label define pobres_lpi 0 "Poblacion con ingreso igual o mayor a la Linea de Pobreza por Ingresos" 1 "Poblacion con ingreso menor a la Linea de Pobreza por Ingresos"
label value pobres_lpi pobres_lpi

save lp_ingresos20.dta,replace

*merge
use ic_rezedu20.dta,clear
merge 1:1 idpersona using ic_asalud20.dta
drop _merge
merge 1:1 idpersona using ic_segsoc20.dta
drop _merge
merge m:1 idhogar using ic_cev20.dta
drop _merge
merge m:1 idhogar using ic_sbv20.dta
drop _merge
merge m:1 idhogar using ic_ali20.dta
drop _merge
merge m:1 idhogar using lp_ingresos20.dta
drop _merge

save basefinal20.dta,replace

********
**2022**
********
*Ingreso Monetario*
use trabajos22.dta,clear
keep folioviv foliohog numren id_trabajo pres_2 
destring pres_2 id_trabajo,replace
reshape wide pres_2,i(folioviv foliohog numren) j(id_trabajo)
egen idhogar=concat(folioviv foliohog)
label var idhogar "Identificador del Hogar"
egen idpersona=concat(folioviv foliohog numren)
label var idpersona "Identificador de la persona"
gen trabajo=1
label var trabajo "Poblacion con al menos un empleo"
gen aguinaldo1=0
replace aguinaldo1=1 if pres_21==2
gen aguinaldo2=0
replace aguinaldo2=1 if pres_22==2
label var aguinaldo1 "Aguinaldo por trabajo principal"
label var aguinaldo2 "Aguinaldo por trabajo secundario"
label define aguinaldo 0 "No recibe aguinaldo" 1 "Recibe aguinaldo"
label value aguinaldo1 aguinaldo
label value aguinaldo2 aguinaldo
save aguinaldo22.dta,replace
 
**base ingresos
use ingresos22.dta,clear
egen idhogar=concat(folioviv foliohog)
label var idhogar "Identificador del Hogar"
egen idpersona=concat(folioviv foliohog numren)
label var idpersona "Identificador de la persona"
*merge con aguinaldos
merge m:1 idpersona using aguinaldo22.dta
drop _merge
*delimitar
drop if clave=="P009" & aguinaldo1!=1
drop if clave=="P016" & aguinaldo2!=1
*La base es el mes de julio de 2023
gen ingresod1=(ing_1/95.4328117238)*100 if mes_1=="07"
replace ingresod1=(ing_1/96.0964667163)*100 if mes_1=="08"
replace ingresod1=(ing_1/96.6925919026)*100 if mes_1=="09"
replace ingresod1=(ing_1/97.2398161947)*100 if mes_1=="10"

gen ingresod2=(ing_2/94.7311227024)*100 if mes_2=="06"
replace ingresod2=(ing_2/95.4328117238)*100 if mes_2=="07"
replace ingresod2=(ing_2/96.0964667163)*100 if mes_2=="08"
replace ingresod2=(ing_2/96.6925919026)*100 if mes_2=="09"

gen ingresod3=(ing_3/93.9378415301)*100 if mes_3=="05"
replace ingresod3=(ing_3/94.7311227024)*100 if mes_3=="06"
replace ingresod3=(ing_3/95.4328117238)*100 if mes_3=="07"
replace ingresod3=(ing_3/96.0964667163)*100 if mes_3=="08"

gen ingresod4=(ing_4/93.7725099354)*100 if mes_4=="04"
replace ingresod4=(ing_4/93.9378415301)*100 if mes_4=="05"
replace ingresod4=(ing_4/94.7311227024)*100 if mes_4=="06"
replace ingresod4=(ing_4/95.4328117238)*100 if mes_4=="07"

gen ingresod5=(ing_5/93.2679769001)*100 if mes_5=="03"
replace ingresod5=(ing_5/93.7725099354)*100 if mes_5=="04"
replace ingresod5=(ing_5/93.9378415301)*100 if mes_5=="05"
replace ingresod5=(ing_5/94.7311227024)*100 if mes_5=="06"

gen ingresod6=(ing_6/92.3536077993)*100 if mes_6=="02"
replace ingresod6=(ing_6/93.2679769001)*100 if mes_6=="03"
replace ingresod6=(ing_6/93.7725099354)*100 if mes_6=="04"
replace ingresod6=(ing_6/93.9378415301)*100 if mes_6=="05"

*deflactar los repartos de utilidades y los aguinaldos                  
replace ingresod1=((ing_1/93.9378415301)*100)/12 if clave=="P008" |clave=="P015"
replace ingresod1=((ing_1/91.0550173870)*100)/12 if clave=="P009" |clave=="P016"
recode ingresod1 ingresod2 ingresod3 ingresod4 ingresod5 ingresod6 (0=.) 
 
*ingreso promedio mensual
egen ingresopromediodeflac=rowmean(ingresod1 ingresod2 ingresod3 ingresod4 ingresod5 ingresod6)
*ingreso promedio monetario
gen ingresomonetariop=ingresopromediodeflac if clave>="P001" & clave<="P009" |clave>="P011" & clave<="P016" |clave>="P018" & clave<="P048" |clave>="P067" & clave<="P081" |clave>="P101" & clave<="P108"
*gen ingreso laboral promedio*
gen ingresolaboralp=ingresopromediodeflac if clave>="P001" & clave<="P009" |clave>="P011" & clave<="P016" |clave>="P018" & clave<="P022" |clave>="P067" & clave<="P081"
*ingreso por rentas*
gen ingresorentasp=ingresopromediodeflac if clave>="P023" & clave<="P031"
*ingreso por transferencias
gen ingresotransferenciasp=ingresopromediodeflac if clave>="P032" & clave<="P048" |clave>="P101" & clave<="P108"
*collapse
collapse (sum) ingresopromediodeflac ingresomonetariop ingresolaboralp ingresorentasp ingresotransferenciasp,by(idhogar)
label var ingresopromediodeflac "Ingreso corriente total del hogar"
label var ingresomonetariop "Ingreso corriente monetario del hogar"
label var ingresolaboralp "Ingreso corriente monetario laboral"
label var ingresorentasp "Ingreso corriente monetario por rentas"
label var ingresotransferenciasp "ingreso corriente monetario por transferencias"
save ingresodeflactado22.dta,replace

*Ingreso no monetario*
*Ingreso no monetario*
use gastoshogar22.dta,clear
gen base=1
append using gastospersona22.dta
recode base (.=2)
replace frecuencia=frec_rem if base==2
label var base "Origen del monto obtenido"
label define base 1 "Monto del Hogar" 2 "Monto de la persona"
label value base base
egen idhogar=concat(folioviv foliohog)
label var idhogar "Identificador del Hogar"
egen idpersona=concat(folioviv foliohog numren)
label var idpersona "Identificador de la persona"
**decena de levantamiento**
gen d_lev=substr(folioviv,8,1)
label var d_lev "Decena de levantamiento"
destring d_lev,replace
**deflactores**
*alimentos semanal (sin incluir las bebidas alcoholicas)
scalar dalimentosw722=93.000778146374
scalar dalimentosw822=94.227382561330
scalar dalimentosw922=95.459447652592
scalar dalimentosw1022=95.907223109582
scalar dalimentosw1122=96.182304678434
*bebidas alcoholicas y tabaco
scalar dbatw722=93.454877603215
scalar dbatw822=94.176835951772
scalar dbatw922=94.507124588966
scalar dbatw1022=95.229813664596
scalar dbatw1122=95.419802703690
*Ropa,vestido, calzado y accesorios
scalar drvcat522=94.980083022349
scalar drvcat622=95.382615623297
scalar drvcat722=95.924357415405
scalar drvcat822=96.748431101234
*vivienda
scalar dviviendam722=101.7188161
scalar dviviendam822=101.7377141
scalar dviviendam922=101.5208373
scalar dviviendam1022=102.0589797
scalar dviviendam1122=103.7426995
*accesorios y articulos de limpieza para el hogar (mensual)
scalar daalhm722=92.97605835
scalar daalhm822=93.96485867
scalar daalhm922=94.77564611
scalar daalhm1022=95.67580034
scalar daalhm1122=96.1975871
*accesorios y articulos de limpieza para el hogar (trimestral)
scalar daalht522=91.972363559489
scalar daalht622=92.998880512754
scalar daalht722=93.905521042036
scalar daalht822=94.805435038509
*muebles y aparatos domesticos (semestral)
scalar dmads222=101.3389028
scalar dmads322=101.2456192
scalar dmads422=101.2280041
scalar dmads522=101.2211112
*salud (trimestral)
scalar dsaludt522=93.434264606420
scalar dsaludt622=93.857759741949
scalar dsaludt722=94.232350033817
scalar dsaludt822=94.571042089381
*transporte publico urbano (semanal)
scalar dtpurbanow722=96.170367489134
scalar dtpurbanow822=96.525511153202
scalar dtpurbanow922=96.862052053914
scalar dtpurbanow1022=97.031168084423
scalar dtpurbanow1122=97.152086046236
*transporte (mensual)
scalar dtransportem722=96.28898413
scalar dtransportem822=96.4061572
scalar dtransportem922=96.42220831
scalar dtransportem1022=96.95831528
scalar dtransportem1122=97.14691578
*transporte (semestral)
scalar dtransportes222=94.56121625
scalar dtransportes322=95.24833737
scalar dtransportes422=95.70606143
scalar dtransportes522=96.1109506
*educacion y esparcimiento 
scalar deducacionyespm722=95.71260486
scalar deducacionyespm822=96.08336084
scalar deducacionyespm922=96.99840016
scalar deducacionyespm1022=97.46819371
scalar deducacionyespm1122=97.76022753
*accesorios y cuidados del vestido (mensual)
scalar dacvm722=96.49393421
scalar dacvm822=96.9550173
scalar dacvm922=97.74878059
scalar dacvm1022=98.00391879
scalar dacvm1122=97.65623046
*accesorios y cuidados del vestido (trimestral)
scalar dacvt522=96.130403968817
scalar dacvt622=96.532566251164
scalar dacvt722=97.065910701630
scalar dacvt822=97.569238893289
*inpc semestral
scalar dinpcs222=93.91597843
scalar dinpcs322=94.53978825
scalar dinpcs422=95.11055742
scalar dinpcs522=95.68844179
*regalos y especie
gen gastonomonetario=gas_nm_tri/3
label var gastonomonetario "Gasto no monetario mensual"
gen especie=1 if tipo_gasto=="G4"
gen regalos=1 if tipo_gasto=="G5"
replace regalos=1 if tipo_gasto=="G6"
drop if tipo_gasto=="G2" |tipo_gasto=="G3" |tipo_gasto=="G7"
label var especie "Gasto no monetario por remuneraciones en especie"
label var regalos "Gasto no monetario por regalos de otro hogar y transferencias de instituciones"
*control de la frecuencia de los regalos recibidos en el hogar
drop if ((frecuencia>="5" & frecuencia<="6") |frecuencia==" " |frecuencia=="0") & base==1 & tipo_gasto=="G5"
drop if (frecuencia=="9" |frecuencia=="") & base==2 & tipo_gasto=="G5"
**Hora de deflactar
*Gasto no monetario en alimentos deflactado
gen gnm_alimentos=gastonomonetario if (clave>="A001" & clave<="A222") |(clave>="A242" & clave<="A247")
replace gnm_alimentos=(gnm_alimentos/dalimentosw822)*100 if d_lev==1
replace gnm_alimentos=(gnm_alimentos/dalimentosw822)*100 if d_lev==2
replace gnm_alimentos=(gnm_alimentos/dalimentosw822)*100 if d_lev==3
replace gnm_alimentos=(gnm_alimentos/dalimentosw922)*100 if d_lev==4
replace gnm_alimentos=(gnm_alimentos/dalimentosw922)*100 if d_lev==5
replace gnm_alimentos=(gnm_alimentos/dalimentosw922)*100 if d_lev==6
replace gnm_alimentos=(gnm_alimentos/dalimentosw1022)*100 if d_lev==7
replace gnm_alimentos=(gnm_alimentos/dalimentosw1022)*100 if d_lev==8
replace gnm_alimentos=(gnm_alimentos/dalimentosw1022)*100 if d_lev==9
replace gnm_alimentos=(gnm_alimentos/dalimentosw1122)*100 if d_lev==0
label var gnm_alimentos "Gasto no monetario en alimentos deflactado"
*Gasto no monetario en alcohol y tabaco deflactado
gen gnm_alcoholytabaco=gastonomonetario if clave>="A223" & clave<="A241"
replace gnm_alcoholytabaco=(gnm_alcoholytabaco/dbatw822)*100 if d_lev==1
replace gnm_alcoholytabaco=(gnm_alcoholytabaco/dbatw822)*100 if d_lev==2
replace gnm_alcoholytabaco=(gnm_alcoholytabaco/dbatw822)*100 if d_lev==3
replace gnm_alcoholytabaco=(gnm_alcoholytabaco/dbatw922)*100 if d_lev==4
replace gnm_alcoholytabaco=(gnm_alcoholytabaco/dbatw922)*100 if d_lev==5
replace gnm_alcoholytabaco=(gnm_alcoholytabaco/dbatw922)*100 if d_lev==6
replace gnm_alcoholytabaco=(gnm_alcoholytabaco/dbatw1022)*100 if d_lev==7
replace gnm_alcoholytabaco=(gnm_alcoholytabaco/dbatw1022)*100 if d_lev==8
replace gnm_alcoholytabaco=(gnm_alcoholytabaco/dbatw1022)*100 if d_lev==9
replace gnm_alcoholytabaco=(gnm_alcoholytabaco/dbatw1122)*100 if d_lev==0
label var gnm_alcoholytabaco "Gasto no monetario en alcohol y tabaco deflactado"
*Gasto no monetario en vestido y calzado deflactado
gen gnm_vestidoycalzado=gastonomonetario if (clave>="H001" & clave<="H122") |clave=="H136"
replace gnm_vestidoycalzado=(gnm_vestidoycalzado/drvcat522)*100 if d_lev==1
replace gnm_vestidoycalzado=(gnm_vestidoycalzado/drvcat522)*100 if d_lev==2
replace gnm_vestidoycalzado=(gnm_vestidoycalzado/drvcat622)*100 if d_lev==3
replace gnm_vestidoycalzado=(gnm_vestidoycalzado/drvcat622)*100 if d_lev==4
replace gnm_vestidoycalzado=(gnm_vestidoycalzado/drvcat622)*100 if d_lev==5
replace gnm_vestidoycalzado=(gnm_vestidoycalzado/drvcat722)*100 if d_lev==6
replace gnm_vestidoycalzado=(gnm_vestidoycalzado/drvcat722)*100 if d_lev==7
replace gnm_vestidoycalzado=(gnm_vestidoycalzado/drvcat722)*100 if d_lev==8
replace gnm_vestidoycalzado=(gnm_vestidoycalzado/drvcat822)*100 if d_lev==9
replace gnm_vestidoycalzado=(gnm_vestidoycalzado/drvcat822)*100 if d_lev==0
label var gnm_vestidoycalzado "Gasto no monetario en vestido y calzado deflactado"
*Gasto no monetario en viviendas deflactado
gen gnm_viviendas=gastonomonetario if (clave>="G001" & clave<="G016") |(clave>="R001" & clave<="R004") |clave=="R013"
replace gnm_viviendas=(gnm_viviendas/dviviendam722)*100 if d_lev==1
replace gnm_viviendas=(gnm_viviendas/dviviendam722)*100 if d_lev==2
replace gnm_viviendas=(gnm_viviendas/dviviendam822)*100 if d_lev==3
replace gnm_viviendas=(gnm_viviendas/dviviendam822)*100 if d_lev==4
replace gnm_viviendas=(gnm_viviendas/dviviendam822)*100 if d_lev==5
replace gnm_viviendas=(gnm_viviendas/dviviendam922)*100 if d_lev==6
replace gnm_viviendas=(gnm_viviendas/dviviendam922)*100 if d_lev==7
replace gnm_viviendas=(gnm_viviendas/dviviendam922)*100 if d_lev==8
replace gnm_viviendas=(gnm_viviendas/dviviendam1022)*100 if d_lev==9
replace gnm_viviendas=(gnm_viviendas/dviviendam1022)*100 if d_lev==0
label var gnm_viviendas "Gasto no monetario en viviendas deflactado"
*Gasto no monetario en Articulos de limpieza deflactado
gen gnm_alimpieza=gastonomonetario if clave>="C001" & clave<="C024"
replace gnm_alimpieza=(gnm_alimpieza/daalhm722)*100 if d_lev==1
replace gnm_alimpieza=(gnm_alimpieza/daalhm722)*100 if d_lev==2
replace gnm_alimpieza=(gnm_alimpieza/daalhm822)*100 if d_lev==3
replace gnm_alimpieza=(gnm_alimpieza/daalhm822)*100 if d_lev==4
replace gnm_alimpieza=(gnm_alimpieza/daalhm822)*100 if d_lev==5
replace gnm_alimpieza=(gnm_alimpieza/daalhm922)*100 if d_lev==6
replace gnm_alimpieza=(gnm_alimpieza/daalhm922)*100 if d_lev==7
replace gnm_alimpieza=(gnm_alimpieza/daalhm922)*100 if d_lev==8
replace gnm_alimpieza=(gnm_alimpieza/daalhm1022)*100 if d_lev==9
replace gnm_alimpieza=(gnm_alimpieza/daalhm1022)*100 if d_lev==0
label var gnm_alimpieza "Gasto no monetario en accesorios y articulos de limpieza"
*Gasto no monetario en cristaleria y blancos deflactado
gen gnm_cristaleria=gastonomonetario if clave>="I001" & clave<="I026"
replace gnm_cristaleria=(gnm_cristaleria/daalht522)*100 if d_lev==1
replace gnm_cristaleria=(gnm_cristaleria/daalht522)*100 if d_lev==2
replace gnm_cristaleria=(gnm_cristaleria/daalht622)*100 if d_lev==3
replace gnm_cristaleria=(gnm_cristaleria/daalht622)*100 if d_lev==4
replace gnm_cristaleria=(gnm_cristaleria/daalht622)*100 if d_lev==5
replace gnm_cristaleria=(gnm_cristaleria/daalht722)*100 if d_lev==6
replace gnm_cristaleria=(gnm_cristaleria/daalht722)*100 if d_lev==7
replace gnm_cristaleria=(gnm_cristaleria/daalht722)*100 if d_lev==8
replace gnm_cristaleria=(gnm_cristaleria/daalht822)*100 if d_lev==9
replace gnm_cristaleria=(gnm_cristaleria/daalht822)*100 if d_lev==0
label var gnm_cristaleria "Gasto no monetario en cristaleria y blancos deflactado"
*Gasto no monetario en enseres domesticos y muebles deflactado
gen gnm_edomesticos=gastonomonetario if clave>="K001" & clave<="K037"
replace gnm_edomesticos=(gnm_edomesticos/dmads222)*100 if d_lev==1
replace gnm_edomesticos=(gnm_edomesticos/dmads222)*100 if d_lev==2
replace gnm_edomesticos=(gnm_edomesticos/dmads322)*100 if d_lev==3
replace gnm_edomesticos=(gnm_edomesticos/dmads322)*100 if d_lev==4
replace gnm_edomesticos=(gnm_edomesticos/dmads322)*100 if d_lev==5
replace gnm_edomesticos=(gnm_edomesticos/dmads422)*100 if d_lev==6
replace gnm_edomesticos=(gnm_edomesticos/dmads422)*100 if d_lev==7
replace gnm_edomesticos=(gnm_edomesticos/dmads422)*100 if d_lev==8
replace gnm_edomesticos=(gnm_edomesticos/dmads522)*100 if d_lev==9
replace gnm_edomesticos=(gnm_edomesticos/dmads522)*100 if d_lev==0
label var gnm_edomesticos "Gasto no monetario en enseres domesticos y muebles deflactado"
*Gasto no monetario en salud deflactado
gen gnm_salud=gastonomonetario if clave>="J001" & clave<="J072"
replace gnm_salud=(gnm_salud/dsaludt522)*100 if d_lev==1
replace gnm_salud=(gnm_salud/dsaludt522)*100 if d_lev==2
replace gnm_salud=(gnm_salud/dsaludt622)*100 if d_lev==3
replace gnm_salud=(gnm_salud/dsaludt622)*100 if d_lev==4
replace gnm_salud=(gnm_salud/dsaludt622)*100 if d_lev==5
replace gnm_salud=(gnm_salud/dsaludt722)*100 if d_lev==6
replace gnm_salud=(gnm_salud/dsaludt722)*100 if d_lev==7
replace gnm_salud=(gnm_salud/dsaludt722)*100 if d_lev==8
replace gnm_salud=(gnm_salud/dsaludt822)*100 if d_lev==9
replace gnm_salud=(gnm_salud/dsaludt822)*100 if d_lev==0
label var gnm_salud "Gasto no monetario en salud deflactado"
*Gasto no monetario en transporte publico deflactado
gen gnm_transportepublico=gastonomonetario if clave>="B001" & clave<="B007"
replace gnm_transportepublico=(gnm_transportepublico/dtpurbanow822)*100 if d_lev==1
replace gnm_transportepublico=(gnm_transportepublico/dtpurbanow822)*100 if d_lev==2
replace gnm_transportepublico=(gnm_transportepublico/dtpurbanow822)*100 if d_lev==3
replace gnm_transportepublico=(gnm_transportepublico/dtpurbanow922)*100 if d_lev==4
replace gnm_transportepublico=(gnm_transportepublico/dtpurbanow922)*100 if d_lev==5
replace gnm_transportepublico=(gnm_transportepublico/dtpurbanow922)*100 if d_lev==6
replace gnm_transportepublico=(gnm_transportepublico/dtpurbanow1022)*100 if d_lev==7
replace gnm_transportepublico=(gnm_transportepublico/dtpurbanow1022)*100 if d_lev==8
replace gnm_transportepublico=(gnm_transportepublico/dtpurbanow1022)*100 if d_lev==9
replace gnm_transportepublico=(gnm_transportepublico/dtpurbanow1122)*100 if d_lev==0
label var gnm_transportepublico "Gasto no monetario en transporte publico deflactado"
*Gasto no monetario en transporte foraneo deflactado*
gen gnm_transporteforaneo=gastonomonetario if (clave>="M001" & clave<="M018") |(clave>="F007" & clave<="F014")
replace gnm_transporteforaneo=(gnm_transporteforaneo/dtransportes222)*100 if d_lev==1
replace gnm_transporteforaneo=(gnm_transporteforaneo/dtransportes222)*100 if d_lev==2
replace gnm_transporteforaneo=(gnm_transporteforaneo/dtransportes322)*100 if d_lev==3
replace gnm_transporteforaneo=(gnm_transporteforaneo/dtransportes322)*100 if d_lev==4
replace gnm_transporteforaneo=(gnm_transporteforaneo/dtransportes322)*100 if d_lev==5
replace gnm_transporteforaneo=(gnm_transporteforaneo/dtransportes422)*100 if d_lev==6
replace gnm_transporteforaneo=(gnm_transporteforaneo/dtransportes422)*100 if d_lev==7
replace gnm_transporteforaneo=(gnm_transporteforaneo/dtransportes422)*100 if d_lev==8
replace gnm_transporteforaneo=(gnm_transporteforaneo/dtransportes522)*100 if d_lev==9
replace gnm_transporteforaneo=(gnm_transporteforaneo/dtransportes522)*100 if d_lev==0
label var gnm_transporteforaneo "Gasto no monetario en transporte foraneo deflactado"
*Gasto no monetario en comunicaciones deflactado*
gen gnm_comunicaciones=gastonomonetario if (clave>="F001" & clave<="F006") |(clave>="R005" & clave<="R008") |(clave>="R010" & clave<="R011")
replace gnm_comunicaciones=(gnm_comunicaciones/dtransportem722)*100 if d_lev==1
replace gnm_comunicaciones=(gnm_comunicaciones/dtransportem722)*100 if d_lev==2
replace gnm_comunicaciones=(gnm_comunicaciones/dtransportem822)*100 if d_lev==3
replace gnm_comunicaciones=(gnm_comunicaciones/dtransportem822)*100 if d_lev==4
replace gnm_comunicaciones=(gnm_comunicaciones/dtransportem822)*100 if d_lev==5
replace gnm_comunicaciones=(gnm_comunicaciones/dtransportem922)*100 if d_lev==6
replace gnm_comunicaciones=(gnm_comunicaciones/dtransportem922)*100 if d_lev==7
replace gnm_comunicaciones=(gnm_comunicaciones/dtransportem922)*100 if d_lev==8
replace gnm_comunicaciones=(gnm_comunicaciones/dtransportem1022)*100 if d_lev==9
replace gnm_comunicaciones=(gnm_comunicaciones/dtransportem1022)*100 if d_lev==0
label var gnm_comunicaciones "Gasto no monetario en comunicaciones deflactado"
*Gasto no monetario en educacion y recreacion deflectado
gen gnm_educacionyrecreacion=gastonomonetario if (clave>="E001" & clave<="E034") |(clave>="H134" & clave<="H135") |(clave>="L001" & clave<="L029") |(clave>="N003" & clave<="N005") |clave=="R009"
replace gnm_educacionyrecreacion=(gnm_educacionyrecreacion/deducacionyespm722)*100 if d_lev==1
replace gnm_educacionyrecreacion=(gnm_educacionyrecreacion/deducacionyespm722)*100 if d_lev==2
replace gnm_educacionyrecreacion=(gnm_educacionyrecreacion/deducacionyespm822)*100 if d_lev==3
replace gnm_educacionyrecreacion=(gnm_educacionyrecreacion/deducacionyespm822)*100 if d_lev==4
replace gnm_educacionyrecreacion=(gnm_educacionyrecreacion/deducacionyespm822)*100 if d_lev==5
replace gnm_educacionyrecreacion=(gnm_educacionyrecreacion/deducacionyespm922)*100 if d_lev==6
replace gnm_educacionyrecreacion=(gnm_educacionyrecreacion/deducacionyespm922)*100 if d_lev==7
replace gnm_educacionyrecreacion=(gnm_educacionyrecreacion/deducacionyespm922)*100 if d_lev==8
replace gnm_educacionyrecreacion=(gnm_educacionyrecreacion/deducacionyespm1022)*100 if d_lev==9
replace gnm_educacionyrecreacion=(gnm_educacionyrecreacion/deducacionyespm1022)*100 if d_lev==0
label var gnm_educacionyrecreacion "Gasto no monetario en educacion y recreacion deflactado"
*Gasto no monetario en Educacion basica deflactado*
gen gnm_educacionbasica=gastonomonetario if (clave>="E002" & clave<="E003") |(clave>="H134" & clave<="H135")
replace gnm_educacionbasica=(gnm_educacionbasica/deducacionyespm722)*100 if d_lev==1
replace gnm_educacionbasica=(gnm_educacionbasica/deducacionyespm722)*100 if d_lev==2
replace gnm_educacionbasica=(gnm_educacionbasica/deducacionyespm822)*100 if d_lev==3
replace gnm_educacionbasica=(gnm_educacionbasica/deducacionyespm822)*100 if d_lev==4
replace gnm_educacionbasica=(gnm_educacionbasica/deducacionyespm822)*100 if d_lev==5
replace gnm_educacionbasica=(gnm_educacionbasica/deducacionyespm922)*100 if d_lev==6
replace gnm_educacionbasica=(gnm_educacionbasica/deducacionyespm922)*100 if d_lev==7
replace gnm_educacionbasica=(gnm_educacionbasica/deducacionyespm922)*100 if d_lev==8
replace gnm_educacionbasica=(gnm_educacionbasica/deducacionyespm1022)*100 if d_lev==9
replace gnm_educacionbasica=(gnm_educacionbasica/deducacionyespm1022)*100 if d_lev==0
label var gnm_educacionbasica "Gasto no monetario en educacion basica deflactado"
*Gasto no monetario en cuidado personal deflactado*
gen gnm_cuidadopersonal=gastonomonetario if (clave>="D001" & clave<="D026") |clave=="H132"
replace gnm_cuidadopersonal=(gnm_cuidadopersonal/dacvm722)*100 if d_lev==1
replace gnm_cuidadopersonal=(gnm_cuidadopersonal/dacvm722)*100 if d_lev==2
replace gnm_cuidadopersonal=(gnm_cuidadopersonal/dacvm822)*100 if d_lev==3
replace gnm_cuidadopersonal=(gnm_cuidadopersonal/dacvm822)*100 if d_lev==4
replace gnm_cuidadopersonal=(gnm_cuidadopersonal/dacvm822)*100 if d_lev==5
replace gnm_cuidadopersonal=(gnm_cuidadopersonal/dacvm922)*100 if d_lev==6
replace gnm_cuidadopersonal=(gnm_cuidadopersonal/dacvm922)*100 if d_lev==7
replace gnm_cuidadopersonal=(gnm_cuidadopersonal/dacvm922)*100 if d_lev==8
replace gnm_cuidadopersonal=(gnm_cuidadopersonal/dacvm1022)*100 if d_lev==9
replace gnm_cuidadopersonal=(gnm_cuidadopersonal/dacvm1022)*100 if d_lev==0
label var gnm_cuidadopersonal "Gasto no monetario en cuidado personal deflactado"
*Gasto no monetario en accesorios personales deflactado*
gen gnm_accesoriospersonales=gastonomonetario if (clave>="H123" & clave<="H131") |clave=="H133"
replace gnm_accesoriospersonales=(gnm_accesoriospersonales/dacvt522)*100 if d_lev==1
replace gnm_accesoriospersonales=(gnm_accesoriospersonales/dacvt522)*100 if d_lev==2
replace gnm_accesoriospersonales=(gnm_accesoriospersonales/dacvt622)*100 if d_lev==3
replace gnm_accesoriospersonales=(gnm_accesoriospersonales/dacvt622)*100 if d_lev==4
replace gnm_accesoriospersonales=(gnm_accesoriospersonales/dacvt622)*100 if d_lev==5
replace gnm_accesoriospersonales=(gnm_accesoriospersonales/dacvt722)*100 if d_lev==6
replace gnm_accesoriospersonales=(gnm_accesoriospersonales/dacvt722)*100 if d_lev==7
replace gnm_accesoriospersonales=(gnm_accesoriospersonales/dacvt722)*100 if d_lev==8
replace gnm_accesoriospersonales=(gnm_accesoriospersonales/dacvt822)*100 if d_lev==9
replace gnm_accesoriospersonales=(gnm_accesoriospersonales/dacvt822)*100 if d_lev==0
label var gnm_accesoriospersonales "Gasto no monetario en accesorios personales deflactado"
*gasto no monetario en otros gastos y transferencias deflactado*
gen gnm_otrosgastos=gastonomonetario if (clave>="N001" & clave<="N002") |(clave>="N006" & clave<="N016") |(clave>="T901" & clave<="T915") |clave=="R012"
replace gnm_otrosgastos=(gnm_otrosgastos/dinpcs222)*100 if d_lev==1
replace gnm_otrosgastos=(gnm_otrosgastos/dinpcs222)*100 if d_lev==2
replace gnm_otrosgastos=(gnm_otrosgastos/dinpcs322)*100 if d_lev==3
replace gnm_otrosgastos=(gnm_otrosgastos/dinpcs322)*100 if d_lev==4
replace gnm_otrosgastos=(gnm_otrosgastos/dinpcs322)*100 if d_lev==5
replace gnm_otrosgastos=(gnm_otrosgastos/dinpcs422)*100 if d_lev==6
replace gnm_otrosgastos=(gnm_otrosgastos/dinpcs422)*100 if d_lev==7
replace gnm_otrosgastos=(gnm_otrosgastos/dinpcs422)*100 if d_lev==8
replace gnm_otrosgastos=(gnm_otrosgastos/dinpcs522)*100 if d_lev==9
replace gnm_otrosgastos=(gnm_otrosgastos/dinpcs522)*100 if d_lev==0
label var gnm_otrosgastos "Gasto no monetario en otros gastos y transferencias deflactado"
*Gasto no monetario en regalos otoragdos deflactado
gen gnm_regalosotorgados=gastonomonetario if (clave>="T901" & clave<="T915") |clave=="N013"
replace gnm_regalosotorgados=(gnm_regalosotorgados/dinpcs222)*100 if d_lev==1
replace gnm_regalosotorgados=(gnm_regalosotorgados/dinpcs222)*100 if d_lev==2
replace gnm_regalosotorgados=(gnm_regalosotorgados/dinpcs322)*100 if d_lev==3
replace gnm_regalosotorgados=(gnm_regalosotorgados/dinpcs322)*100 if d_lev==4
replace gnm_regalosotorgados=(gnm_regalosotorgados/dinpcs322)*100 if d_lev==5
replace gnm_regalosotorgados=(gnm_regalosotorgados/dinpcs422)*100 if d_lev==6
replace gnm_regalosotorgados=(gnm_regalosotorgados/dinpcs422)*100 if d_lev==7
replace gnm_regalosotorgados=(gnm_regalosotorgados/dinpcs422)*100 if d_lev==8
replace gnm_regalosotorgados=(gnm_regalosotorgados/dinpcs522)*100 if d_lev==9
replace gnm_regalosotorgados=(gnm_regalosotorgados/dinpcs522)*100 if d_lev==0
label var gnm_regalosotorgados "Gasto no monetario en regalos otorgados deflactado"

save ingresonomonetariodeflac22.dta,replace

*En especie
use ingresonomonetariodeflac22.dta,clear
keep if especie==1
collapse (sum) gnm_*,by(idhogar)
rename gnm_alcoholytabaco gnm_alcoholytabacoespecie
rename gnm_alimentos gnm_alimentosespecie
rename gnm_vestidoycalzado gnm_vestidoycalzadoespecie
rename gnm_viviendas gnm_viviendasespecie
rename gnm_alimpieza gnm_alimpiezaespecie
rename gnm_cristaleria gnm_cristaleriaespecie
rename gnm_edomesticos gnm_edomesticosespecie
rename gnm_salud gnm_saludespecie
rename gnm_transportepublico gnm_transportepublicoespecie
rename gnm_transporteforaneo gnm_transporteforaneoespecie
rename gnm_comunicaciones gnm_comunicacionesespecie
rename gnm_educacionyrecreacion gnm_educacionyrecreacionespecie
rename gnm_educacionbasica gnm_educacionbasicaespecie
rename gnm_cuidadopersonal gnm_cuidadopersonalespecie
rename gnm_accesoriospersonales gnm_accesoriospersonalesespecie
rename gnm_otrosgastos gnm_otrosgastosespecie
rename gnm_regalosotorgados gnm_regalosotorgadosespecie
sort idhogar
save especiedeflac22.dta,replace
*En regalos
use ingresonomonetariodeflac22.dta,clear
keep if regalos==1
collapse (sum) gnm_*,by(idhogar)

rename gnm_alcoholytabaco gnm_alcoholytabacoregalos
rename gnm_alimentos gnm_alimentosregalos
rename gnm_vestidoycalzado gnm_vestidoycalzadoregalos
rename gnm_viviendas gnm_viviendasregalos
rename gnm_alimpieza gnm_alimpiezaregalos
rename gnm_cristaleria gnm_cristaleriaregalos
rename gnm_edomesticos gnm_edomesticosregalos
rename gnm_salud gnm_saludregalos
rename gnm_transportepublico gnm_transportepublicoregalos
rename gnm_transporteforaneo gnm_transporteforaneoregalos
rename gnm_comunicaciones gnm_comunicacionesregalos
rename gnm_educacionyrecreacion gnm_educacionyrecreacionregalos
rename gnm_educacionbasica gnm_educacionbasicaregalos
rename gnm_cuidadopersonal gnm_cuidadopersonalregalos
rename gnm_accesoriospersonales gnm_accesoriospersonalesregalos
rename gnm_otrosgastos gnm_otrosgastosregalos
rename gnm_regalosotorgados gnm_regalosotorgadosregalos

save regalosdeflac22.dta,replace

**Ingreso corriente total
*concentrado del hogar
use concentradohogar22.dta,clear
egen idhogar=concat(folioviv foliohog)
label var idhogar "Identificador del Hogar"
keep idhogar tam_loc factor tot_integ est_dis upm ubica_geo
*merge con las otras bases*
merge 1:m idhogar using ingresodeflactado22.dta
drop _merge
merge 1:m idhogar using especiedeflac22.dta
drop _merge
merge 1:m idhogar using regalosdeflac22.dta
drop _merge
*Localidad rural
gen rural=. 
replace rural=0 if tam_loc<="3"
replace rural=1 if tam_loc=="4"
label var rural "Localidad rural"
label define rural 0 "Urbano" 1 "Rural"
label value rural rural
*Suma de los pagos en especie y regalos
egen pago_especie=rsum(gnm_alcoholytabacoespecie gnm_alimentosespecie gnm_vestidoycalzadoespecie gnm_viviendasespecie gnm_alimpiezaespecie gnm_cristaleriaespecie gnm_edomesticosespecie gnm_saludespecie gnm_transportepublicoespecie gnm_transporteforaneoespecie gnm_comunicacionesespecie gnm_educacionyrecreacionespecie gnm_educacionbasicaespecie gnm_cuidadopersonalespecie gnm_accesoriospersonalesespecie gnm_otrosgastosespecie gnm_regalosotorgadosespecie) 
egen pago_regalos=rsum(gnm_alcoholytabacoregalos gnm_alimentosregalos gnm_vestidoycalzadoregalos gnm_viviendasregalos gnm_alimpiezaregalos gnm_cristaleriaregalos gnm_edomesticosregalos gnm_saludregalos gnm_transportepublicoregalos gnm_transporteforaneoregalos gnm_comunicacionesregalos gnm_educacionyrecreacionregalos gnm_educacionbasicaregalos gnm_cuidadopersonalregalos gnm_accesoriospersonalesregalos gnm_otrosgastosregalos gnm_regalosotorgadosregalos)
egen inomonetario=rsum(pago_especie pago_regalos)
egen icorrientetotal=rsum(ingresomonetariop inomonetario)
label var pago_especie "Ingreso corriente no monetario pago especie"
label var pago_regalos "Ingreso corriente no monetario regalos especie"
label var inomonetario "Suma del ingreso corriente no monetario"
label var icorrientetotal "Ingreso corriente total"

save ingresocorrientetotal22.dta,replace

*tamaño del hogar*
use poblacion22.dta,replace
egen idhogar=concat(folioviv foliohog)
label var idhogar "Identificador del Hogar"
drop if parentesco>="400" & parentesco<"500"
drop if parentesco>="700" & parentesco<"800"
*total de los integrantes del hogar*
gen integrante=1
egen tot_integrantes=sum(integrante),by(idhogar)
*collapse
collapse (max) tot_integrantes,by(idhogar)
save tamañohogar22.dta,replace
**Bienestar economico*
use ingresocorrientetotal22.dta,clear
*merge con tamaño de hogar en escala
merge 1:m idhogar using tamañohogar22.dta
drop _merge
*per capita*
gen ictpercapita=icorrientetotal/tot_integrantes
label var ictpercapita "Ingreso corriente total per capita"
*linea de pobreza por ingresos
*julio 2022
scalar lpi_rural22=2928.47
scalar lpi_urbano22=4105.11
gen pobres_lpi=.
replace pobres_lpi=1 if (ictpercapita<lpi_urbano22 & rural==0)
replace pobres_lpi=1 if (ictpercapita<lpi_rural22 & rural==1)
replace pobres_lpi=0 if (ictpercapita>=lpi_urbano22 & rural==0) & ictpercapita!=.
replace pobres_lpi=0 if (ictpercapita>=lpi_rural22 & rural==1) & ictpercapita!=.
label var pobres_lpi "Poblacion con ingreso menor o igual a la linea de pobreza por ingresos"
label define pobres_lpi 0 "Poblacion con ingreso igual o mayor a la Linea de Pobreza por Ingresos" 1 "Poblacion con ingreso menor a la Linea de Pobreza por Ingresos"
label value pobres_lpi pobres_lpi

save lp_ingresos22.dta,replace

*merge*
use ic_rezedu22.dta,clear
merge 1:1 idpersona using ic_asalud22.dta
drop _merge
merge 1:1 idpersona using ic_segsoc22.dta
drop _merge
merge m:1 idhogar using ic_cev22.dta
drop _merge
merge m:1 idhogar using ic_sbv22.dta
drop _merge
merge m:1 idhogar using ic_ali22.dta
drop _merge
merge m:1 idhogar using lp_ingresos22.dta
drop _merge

save basefinal22.dta,replace

*Estados de México
set more off
local flist basefinal16.dta basefinal18.dta basefinal20.dta basefinal22.dta

foreach file in `flist' {
	use `file'
rename *, lower
gen entidadfederativa=substr(folioviv,1,2)
destring entidadfederativa,replace
label var entidadfederativa "Entidad Federativa"
label define entidadfederativa 1 "Aguascalientes" 2 "Baja California" 3 "Baja California Sur" 4 "Campeche" 5 "Coahuila" 6 "Colima" 7 "Chiapas" 8 "Chihuahua" 9 "Ciudad de México" 10 "Durango" 11 "Guanajuato" 12 "Guerrero" 13 "Hidalgo" 14 "Jalisco" 15 "Estado de México" 16 "Michoacán" 17 "Morelos" 18 "Nayarit" 19 "Nuevo León" 20 "Oaxaca" 21 "Puebla" 22 "Queretaro" 23 "Quintana Roo" 24 "San Luis Potosí" 25 "Sinaloa" 26 "Sonora" 27 "Tabasco" 28 "Tamaulipas" 29 "Tlaxcala" 30 "Veracrúz" 31 "Yucatán" 32 "Zacatecas"
label value entidadfederativa entidadfederativa
save,replace
clear
}




*para cada año
*scalar wingreso=(1/7)
*scalar weducacion=(1/7)
*scalar wsalud=(1/7)
*scalar wespaciosv=(1/7)
*scalar wsbv=(1/7)
*scalar walimentos=(1/7)
*scalar wss=(1/7)
*gen ck=(pobres_lpi*wingreso)+(rezagoeducativo*weducacion)+(ic_asalud*wsalud)+(iccv*wespaciosv)+(ic_sbv*wsbv)+(ic_ali_nc*walimentos)+(ic_seguridadsocial*wss)
*Para considerar si una persona es pobre por este método, ck tiene que ser mayor o igual a .4285714
*tab ck if ck>=.4285714 [fw=fac]
*tab ck if ck<.4285714 [fw=fac]
*ck censurada
*gen ckcensurada=ck
*replace ckcensurada=0 if ck<.4285714
**Por Rural y urbano
*mean(ckcensurada) if rural==1 [fw=factor]
*mean(ckcensurada) if rural==0 [fw=factor]
**Por estados**
*ckcensurada
*by entidadfederativa:egen ckcensuradapromedioestatal=mean(ckcensurada)
*by entidadfederativa:tab ckcensuradapromedioestatal
*menor a mayor*
*by ckcensuradapromedioestatal:tab entidad
*by ckpromedioestatal:tab entidad

*La Mo,H y A se calcularon por separado.Vease en el excel Tablas.





# Diccionarios




Lista_Deflactores_Transporte_Publico = {
 
}


Lista_Deflactores_Transporte_Foraneo = {

}


Lista_Deflactores_Comunicaciones = {

}

Lista_Deflactores_Educacion = {

}

Lista_Deflactores_Educacion_Basica = {

}

Lista_Deflactores_Cuidado_Personal = {

}

Lista_Deflactores_Accesorios_Personales = {

}

Lista_Deflactores_Otros_Gastos = {

}

Lista_Deflactores_Regalos_otorgados = {

}