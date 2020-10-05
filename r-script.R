# installeer en activeer de nodige libraries
# install.packages("dplyr")
library("dplyr")
# install.packages("xlsx")
library("xlsx")

# stel de working directory in 
setwd("~/Documents/middelbareschoolkeuzebijlage")

# file1: leerlingen per onderwijstype, file5: zittenblijvers en opstromers, file7: eindexamendata
file1 <- read.csv2(file = '01.csv', as.is = T, fileEncoding = "Latin1")
file2 <- read.csv2(file = '02.csv', as.is = T, fileEncoding = "Latin1")
file1oud <- read.csv2(file = '2017-01.csv', as.is = T, fileEncoding = "Latin1")
file5 <- read.csv2(file = '05.csv', as.is = T, fileEncoding = "Latin1")
file7 <- read.csv2(file = '07.csv', as.is = T, fileEncoding = "Latin1")

gemeentes <- read.csv2(file = 'gemeentes-per-titel.csv', as.is = T, fileEncoding = "Latin1")
colnames(gemeentes) <- c('TITEL', 'GEMEENTE')
scholen <- read.csv2(file = 'scholen2017.csv', as.is = T, fileEncoding = "Latin1")

# herstructureer file5 zodat het meer lijkt op de andere bestanden
file5 <- file5[c(1,3,2,4:11)]

# voeg het vestigingsnummer en brinnummer samen in file1 en file5
volledigvestigingsnummer <- function(brin,vestiging) {
  volledigevestiging <- paste(brin, vestiging, sep = '')
  if (nchar(vestiging) == 1){
    volledigevestiging <- paste(brin, '0',vestiging, sep = '')
  }
  return(volledigevestiging)
}
file1$VESTIGINGSNUMMER <- mapply(volledigvestigingsnummer, file1$BRIN.NUMMER, file1$VESTIGINGSNUMMER)
file5$VESTIGINGSNUMMER <- mapply(volledigvestigingsnummer, file5$BRINNUMMER, file5$VESTIGINGSNUMMER)

# check welke schoolnummers uit 7 niet voorkomen in 1 
onbekendeScholen7 <- data.frame(unique(file7[!(file7$BRIN.NUMMER %in% file1$BRIN.NUMMER),1:5]))
onbekendeVestigingen7 <- data.frame(unique(file7[!(file7$VESTIGINGSNUMMER %in% file1$VESTIGINGSNUMMER),1:5]))

# check welke schoolnummers uit 7 niet voorkomen in 5 
onbekendeScholen5 <- data.frame(unique(file7[!(file7$BRIN.NUMMER %in% file5$BRINNUMMER),1:5]))
onbekendeVestigingen5 <- data.frame(unique(file7[!(file7$VESTIGINGSNUMMER %in% file5$VESTIGINGSNUMMER),1:5]))

# check welke schoolnummers uit 7 niet voorkomen in 2 
onbekendeScholen2 <- data.frame(unique(file7[!(file7$BRIN.NUMMER %in% file2$BRIN.NUMMER),1:5]))
onbekendeVestigingen2 <- data.frame(unique(file7[!(file7$VESTIGINGSNUMMER %in% file2$VESTIGINGSNUMMER),1:5]))

# check welke schoolnummers uit 7 niet voorkomen in de scholen tabel
onbekendeVestigingen <- data.frame(unique(file7[!(file7$VESTIGINGSNUMMER %in% scholen$VESTIGINGSNUMMER),1:5]))
onbekendeVestigingen <- onbekendeVestigingen[onbekendeVestigingen$GEMEENTENAAM.VESTIGING %in% gemeentes$GEMEENTE,]

# Dockinga NOARDEAST-FRYSLAN bestaat niet meer, kan verwijderd
# WellantCollege mist de data over leerlingen -> samenvoegen met andere wellantcollege in dordrecht
# DaCapo rijksweg zuid en born zijn samengevoegd, gewogen gemiddelde nemen
# Beukenrode
# Gomarus Zuidhorn is opgeheven, kan verwijderd
# PC en RK SGM Ubbo Emmius is samengevoegd met Winkler Prins Veendam, kan verwijderd
# Baudartius is 20DH10 in file1 en file2
# Mariendael? Er is er eentje met een ander nummer, 00TQ00 in file1 en file5
# Bisschop Coll Broekhin (14PS02) is verhuisd naar Reuver (14PS03)
# CSG - locatie Rehoboth, opgegaan in Aletta Jacobs, kan verwijderd
# De Nieuwste School ander nummer in file1 31LW00   
# RK SGM METAMEER 16SW05 samenvoegen met 16SW00
# 17KF02	Van Maerlant kan verwijderd
# OSG Schoonoord 17BI05 is 17BI06 in file1
# 17WQ05 Lentiz is 31MD02	Lentiz Maassluis in file1
# 17WQ06 Lentiz MAASSLUIS is 31MD00	Lentiz Maassluis in file1
# 17WQ15 Lentiz MAASSLUIS is 31MD03	Lentiz Maassluis in file1
# Het Hooghuis loc. Den Bongerd lijkt niet meer te bestaan, opgeheven, kan verwijderd
# Saenredam College heeft nummer 20CN05 in file1
# Bonaventuracollege is opgeheven, kan verwijderd

# verwijder niet meer bestaande scholen
verwijderen = c("00ZX04", "02RM00", "02UV02", "02VJ02", "14RP05", "14PG00", "17KF02", "19XH07", "21GW01")
file7 <- file7[!(file7$VESTIGINGSNUMMER %in% verwijderen),]

# hernummer scholen die een nieuw nummer hebben gekregen in file7 en file5 en voeg scholen samen
vervangen <- data.frame(c("02VS00","14PS02","16OX01","17BI05","17WQ05","17WQ06","17WQ15","20FC00","01OE11","02IB08","18DE00","16SW05"), 
                        c("20DH10","14PS03","31LW00","17BI06","31MD02","31MD00","31MD03","20CN05","01OE12","02IB00","02IB00","16SW00"),
                        stringsAsFactors = FALSE
                        )
colnames(vervangen) <- c("OUD", "NIEUUW")
vervangFunctie <- function(x) {
  if (x %in% vervangen$OUD) {
    nieuw <- vervangen[vervangen$OUD == x, c(2)]
    return(nieuw)
  }
  else {
    return(x)
  }
}
file7$VESTIGINGSNUMMER <- lapply(file7$VESTIGINGSNUMMER, vervangFunctie)
file5$VESTIGINGSNUMMER <- lapply(file5$VESTIGINGSNUMMER, vervangFunctie)
scholen$VESTIGINGSNUMMER <- lapply(scholen$VESTIGINGSNUMMER, vervangFunctie)


# pak alle vestigingen vanuit file7 en pak vanuit file2 de bijbehorende informatie
data <- unique(file7[c(2)])
data <- merge(unique(file7[c(2)]), file2[c(3:5,11:12)], by=c("VESTIGINGSNUMMER"), all.x = TRUE, all.y = FALSE, sort=FALSE)

# pak vanuit file1 de informatie over de aantallen leerlingen
# bereken percentages van verschillende niveau's
file1['Totaal.Rij'] <- apply(file1[,c(11:22)], 1, sum)
opleidingsnamen <- unique(file1$OPLEIDINGSNAAM)
opleidingsnamen.brugklassen <- grep('Ljvb|Lj', opleidingsnamen, value = T, ignore.case = T)
#opleidingsnamen.lwoo <- grep('LWOO', grep('Ljvb|Lj', opleidingsnamen, value = T, invert = T,ignore.case = T), value = T, ignore.case = T)
opleidingsnamen.vmbo <- grep('VMBO|MAVO|LWOO|Praktijk', grep('Ljvb|Lj', opleidingsnamen, value = T, invert = T, ignore.case = T), value = T, ignore.case = T)
opleidingsnamen.havo <- grep('HAVO', grep('Ljvb|Lj', opleidingsnamen, value = T, invert = T, ignore.case = T), value = T, ignore.case = T)
opleidingsnamen.vwo <- grep('VWO', grep('Ljvb|Lj', opleidingsnamen, value = T, invert = T, ignore.case = T), value = T, ignore.case = T)

data$totaal.leerlingen <- 0
data$geslaagd.vmbo <- '-'
data$cijfer.vmbo <- '-'
data$geslaagd.havo <- '-'
data$cijfer.havo <- '-'
data$geslaagd.vwo <- '-'
data$cijfer.vwo <- '-'
data$percentage.zittenblijvers <- '-'
data$percentage.opstromers <- '-'

for (vestiging in data[['VESTIGINGSNUMMER']]) {
  if (vestiging %in% scholen$VESTIGINGSNUMMER) {
    data$VESTIGINGSNAAM[data$VESTIGINGSNUMMER == vestiging] <- scholen$INSTELLINGSNAAM.VESTIGING[scholen$VESTIGINGSNUMMER == vestiging]
  }
  # percentages/verhoudingen van leerlingen toevoegen
  perc <- file1[file1$VESTIGINGSNUMMER == vestiging,]
  leerlingen.aantal <- sum(file1$Totaal.Rij[file1$VESTIGINGSNUMMER == vestiging])
  
  data$totaal.leerlingen[data$VESTIGINGSNUMMER == vestiging] <- leerlingen.aantal
  data$totaal.leerlingen[data$VESTIGINGSNUMMER == vestiging] <- sum(file1$Totaal.Rij[file1$VESTIGINGSNUMMER == vestiging])

  slagperc <- file7[file7$VESTIGINGSNUMMER == vestiging,]
  
  vmbo.slagperc <- sum(slagperc$GESLAAGDEN[slagperc$ONDERWIJSTYPE.VO == 'VMBO'])/sum(slagperc$EXAMENKANDIDATEN[slagperc$ONDERWIJSTYPE.VO == 'VMBO']) * 100
  vmbo.gemiddelde <- with(slagperc[slagperc$ONDERWIJSTYPE.VO == 'VMBO',], sum(EXAMENKANDIDATEN * GEMIDDELD.CIJFER.CIJFERLIJST)/sum(EXAMENKANDIDATEN))
  if (!is.nan(vmbo.slagperc)) {
    data$geslaagd.vmbo[data$VESTIGINGSNUMMER == vestiging] <- vmbo.slagperc 
    data$cijfer.vmbo[data$VESTIGINGSNUMMER == vestiging] <- vmbo.gemiddelde
  }
  else{data$geslaagd.vmbo[data$VESTIGINGSNUMMER == vestiging] <- '-'}
  
  havo.slagperc <- sum(slagperc$GESLAAGDEN[slagperc$ONDERWIJSTYPE.VO == 'HAVO'])/sum(slagperc$EXAMENKANDIDATEN[slagperc$ONDERWIJSTYPE.VO == 'HAVO']) * 100
  havo.gemiddelde <- with(slagperc[slagperc$ONDERWIJSTYPE.VO == 'HAVO',], sum(EXAMENKANDIDATEN * GEMIDDELD.CIJFER.CIJFERLIJST)/sum(EXAMENKANDIDATEN))
  if (!is.nan(havo.slagperc)) {
    data$geslaagd.havo[data$VESTIGINGSNUMMER == vestiging] <- havo.slagperc
    data$cijfer.havo[data$VESTIGINGSNUMMER == vestiging] <- havo.gemiddelde
  }
  else{data$geslaagd.havo[data$VESTIGINGSNUMMER == vestiging] <- '-'}
  
  vwo.slagperc <- sum(slagperc$GESLAAGDEN[slagperc$ONDERWIJSTYPE.VO == 'VWO'])/sum(slagperc$EXAMENKANDIDATEN[slagperc$ONDERWIJSTYPE.VO == 'VWO']) * 100
  vwo.gemiddelde <- with(slagperc[slagperc$ONDERWIJSTYPE.VO == 'VWO',], sum(EXAMENKANDIDATEN * GEMIDDELD.CIJFER.CIJFERLIJST)/sum(EXAMENKANDIDATEN))
  if (!is.nan(vwo.slagperc)) {
    data$geslaagd.vwo[data$VESTIGINGSNUMMER == vestiging] <- vwo.slagperc
    data$cijfer.vwo[data$VESTIGINGSNUMMER == vestiging] <- vwo.gemiddelde
  }
  else{data$geslaagd.vwo[data$VESTIGINGSNUMMER == vestiging] <- '-'}
  
  # voeg percentage zittenblijvers en opstromers toe
  #cat(sprintf("Brin:%s vestiging:%s\n", brin, vestiging))
  d <- file5[file5$VESTIGINGSNUMMER == vestiging,]
  if (nrow(d) == 0) { next }
  data$percentage.zittenblijvers[data$VESTIGINGSNUMMER == vestiging] <- d$AANTAL_ZITTENBLIJVERS / d$AANTAL_LEERLINGEN * 100
  data$percentage.opstromers[data$VESTIGINGSNUMMER == vestiging] <- d$AANTAL_OPSTROMERS / d$AANTAL_LEERLINGEN * 100

}

wb <- createWorkbook()

for (Titel in unique(gemeentes[['TITEL']])){
  df <- data[data$GEMEENTENAAM %in% gemeentes$GEMEENTE[gemeentes$TITEL == Titel],]
  if (exists("df.onbekend")){
    df.onbekend <- rbind(df.onbekend, df[df$schoolnaam == 'onbekend',])
  }
  else {
    df.onbekend <- df[df$schoolnaam == 'onbekend',]
  }
  df <- df[order(df$GEMEENTENAAM, df$VESTIGINGSNAAM), c(3:ncol(df))]
  
  print(paste(Titel, ': ', nrow(df[df$schoolnaam == 'onbekend',])/nrow(df),
              ' (', nrow(df[df$schoolnaam == 'onbekend',]), ' van ', nrow(df),
              ')', sep = ""))
  write.csv2(df, file = paste(Titel, '.csv', sep = ""), row.names = F)
  sheet <- createSheet(wb, sheetName = Titel)
  addDataFrame(df, sheet, row.names = F)
}

data2 <- data[order(data$GEMEENTENAAM, data$VESTIGINGSNAAM), c(3:ncol(data))]
sheet <- createSheet(wb, sheetName = "Alle scholen")
addDataFrame(data2,sheet,row.names = F)

saveWorkbook(wb, file = 'alle_scholen_2.xlsx')

