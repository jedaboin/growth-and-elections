// Loading the electoral file


// Generate Political Connections Dummy
// 2007 elections
//Fixing some mistakes in the database
replace votos = 11829 in 355 
replace votos = 3190 in 981
replace votos = 2467 in 2668
replace votos = 6546 in 2968
replace votos = 2410 in 3033
replace votos = 8287 in 3240
replace votos = 2270 in 3281
replace votos = 3200 in 3348
replace votos = 1470 in 4127
replace votos = 1610 in 5682
replace votos = 12451 in 5723
replace votos = 2443 in 7596

// Coalition Cesar Gaviria (President in 1992)
gen politc=1 if codigo_partido==1 //Partido Liberal
replace politc=0 if politc==. //Everything else
gen party=1 if codigo_partido==1 // Only Ruling Party, excluding coalition
replace party=0 if party==.
rename ano year

// Coalition Ernesto Samper (Winner in 1994, same party as Gaviria)
gen politc_flip=1 if codigo_partido==1 //Partido Liberal
replace politc_flip=0 if politc_flip==. //Everything else
gen party=1 if codigo_partido==1 // Only Ruling Party, excluding coalition
replace party=0 if party==.

// Coalition Andres Pastrana (Winner in 1998)
gen politc=1 if codigo_partido==2 // Partido Conservador
replace politc=1 if codigo_partido==6 // Movimiento de Nacional Progresista **
replace politc=1 if codigo_partido==10 // Movimiento de Salvación Nacional
replace politc=1 if codigo_partido==11 // Movimiento Fuerza Progresista
replace politc=1 if codigo_partido==12 // Movimiento Nueva Colombia **
replace politc=1 if codigo_partido==14 // Movimiento Conservatismo Independiente
replace politc=1 if codigo_partido==28 // Movimiento Nueva Fuerza Democratica
replace politc=1 if codigo_partido==31 // Movimiento de Participación Popular
replace politc=1 if codigo_partido==33 // Movimiento Seriedad Colombia **
replace politc=1 if codigo_partido==39 // Movimiento Fuerza Colombia **
replace politc=1 if codigo_partido==41 // Movimiento Cambio Radical **
replace politc=1 if codigo_partido==44 // Movimiento de Colombia Mi País **
replace politc=1 if codigo_partido==49 // Movimiento Unionista **
replace politc=1 if codigo_partido==55 // Movimiento Renovador de Acción **
replace politc=1 if codigo_partido==58 // MOVIMIENTO DEJEN JUGAR AL MORENO **
replace politc=1 if codigo_partido==61 // Movimiento Ciudadanos por Boyaca **
replace politc=1 if codigo_partido==103 // Movimiento Nacional Conservador
replace politc=0 if politc==. //Everything else
gen party=1 if codigo_partido==2 // Only Ruling Party, excluding coalition
replace party=0 if party==.

// Coalition Alvaro Uribe (Winner in 2002)
gen politc=1 if codigo_partido==2 // Partido Conservador
replace politc=1 if codigo_partido==10 // Movimiento de Salvacion Nacional
replace politc=1 if codigo_partido==28 // Movimiento Nueva Fuerza Democratica
replace politc=1 if codigo_partido==14 // Movimiento Conservatismo Independiente
replace politc=1 if codigo_partido==11 // Movimiento Fuerza Progresista
replace politc=1 if codigo_partido==103 // Movimiento Nacional Conservador
replace politc=1 if codigo_partido==51 // Movimiento Progresismo Democratico **
replace politc=1 if codigo_partido==49 // Movimiento Unionista **
replace politc=1 if codigo_partido==41 // Movimiento Cambio Radical **
replace politc=1 if codigo_partido==165 // Partido Cambio Radical **
replace politc=1 if codigo_partido==163 // Partido Colombia Democratica **
replace politc=1 if codigo_partido==195 // Partido Alas Equipo Colombia **
replace politc=1 if codigo_partido==155 // Partido Movimiento Colombia Viva **
replace politc=0 if politc==. //Everything else
gen party=1 if codigo_partido==2 // Only Ruling Party, excluding coalition
replace party=0 if party==.

// Coalicion de Alvaro Uribe (Winner in 2006)

gen politc=1 if codigo_partido==198 // Partido de la U
replace politc=1 if codigo_partido==2 // Partido Conservador
replace politc=1 if codigo_partido==165 // Partido Cambio Radical
replace politc=1 if codigo_partido==163 // Partido Colombia Democratica
replace politc=1 if codigo_partido==195 // Partido Alas Equipo Colombia
replace politc=1 if codigo_partido==155 // Partido Movimiento Colombia Viva
replace politc=0 if politc==. //Everything else
gen party=1 if codigo_partido==198 // Only Ruling Party, excluding coalition
replace party=0 if party==.

// Coalition Juan Manuel Santos (Winner in 2010)
gen politc=1 if codigo_partido1==2 // Partido Conservador
replace politc=1 if codigo_partido1==198 // Partido de la U
replace politc=1 if codigo_partido1==165 // Partido Cambio Radical
replace politc=1 if codigo_partido1==35 // Partido Convergencia Ciudadana
replace politc=1 if codigo_partido1==163 // Partido Colombia Democratica
replace politc=1 if codigo_partido1==195 // Partido Alas Equipo Colombia
replace politc=1 if codigo_partido1==155 // Partido Movimiento Colombia Viva


// Computing Voting Shares and Margins

egen totalvotes=total(votos), by(codmpio) //Total votes
gen sharevotes=votos/totalvotes // Share of votes
gsort codmpio -sharevotes // Descending order of votes
bysort codmpio: gen nid=_n // Generating ids per order of candidates

egen maxshare=max(sharevotes), by(codmpio) // Maximum share on each municipality
gen margin=maxshare-sharevotes // Margin
replace margin=. if margin==0 // Dropping same values
egen margin12=min(margin), by(codmpio) // Getting the margin between two front-runners
gen coalition=1 if nid==2 & politc==1
replace coalition=0 if coalition==. 
egen coal_runner=max(coalition), by(codmpio) // If runner-up is from the coalition
keep if curules==1 //Keeping only the winner

//Generating the margin for the RD
gen marginRD=margin12 if politc==1
replace marginRD=margin12*(-1) if politc==0
keep year coddpto codmpio municipio codigo_partido nombre primer_apellido votos curules politc party totalvotes sharevotes maxshare margin margin12 coalition coal_runner marginRD

// Keeping only races that can be considered as random
gen rdm=1 if politc!=coal_runner
replace rdm=0 if rdm==.
keep if rdm==1

//Cleaning Governor elections
keep if curules==1
keep codmpio politc_flip
keep coddpto codigo_partido
rename codigo_partido partido_dep

//Merging Governors Results
merge m:1 coddpto using "C:\Users\JESUS\Documents\ITFD\Master Project\Night Lights, Electoral Cycles and Growth\Electoral Data\gobernador2007_merge", nogen
replace partido_dep=codigo_partido if partido_dep==.
drop if codmpio==.

//Flipped Control
keep if curules==1
gen politc_flip=1 if codigo_partido==2
replace politc_flip=1 if codigo_partido==14 // Movimiento Conservatismo Independiente
replace politc_flip=1 if codigo_partido==28 // Movimiento Nueva Fuerza Democratica
replace politc_flip=1 if codigo_partido==31 // Movimiento de Participación Popular
replace politc_flip=1 if codigo_partido==103 // Movimiento Nacional Conservador
replace politc_flip=0 if politc_flip==.
keep codmpio politc_flip


merge 1:1 codmpio using "C:\Users\JESUS\Documents\ITFD\Master Project\Night Lights, Electoral Cycles and Growth\Electoral Data\flip2003"
gen flip=1 if politc==politc_flip
replace flip=0 if flip==.
drop if _merge==2
drop _merge

// 1997 elections
gen politc_dep=1 if partido_dep==codigo_partido & politc==1
gen dep_gob=1 if partido_dep==2 
replace dep_gob=1 if partido_dep==6 
replace dep_gob=1 if partido_dep==7 
replace dep_gob=1 if partido_dep==10  
replace dep_gob=1 if partido_dep==11
replace dep_gob=1 if partido_dep==12 
replace dep_gob=1 if partido_dep==14 
replace dep_gob=1 if partido_dep==24 
replace dep_gob=1 if partido_dep==28 
replace dep_gob=1 if partido_dep==31
replace dep_gob=1 if partido_dep==33 
replace dep_gob=1 if partido_dep==39 
replace dep_gob=1 if partido_dep==41 
replace dep_gob=1 if partido_dep==44 
replace dep_gob=1 if partido_dep==49 
replace dep_gob=1 if partido_dep==55 
replace dep_gob=1 if partido_dep==58
replace dep_gob=1 if partido_dep==98
replace dep_gob=1 if partido_dep==198
replace politc_dep=1 if politc==1 & dep_gob==1
replace politc_dep=0 if politc_dep==.
gen margindRD_dep=margin12 if politc_dep==1
replace margindRD_dep=margin12*-1 if politc_dep==0
