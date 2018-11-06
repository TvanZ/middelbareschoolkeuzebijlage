# stel de working directory in 
setwd("C:/Users/Thomas/Documents/Freelance werk/AD/Middelbareschoolkeuze 2017-2018")


# file1: leerlingen per onderwijstype, file5: zittenblijvers en opstromers, file7: eindexamendata
file1 <- read.csv2(file = '01.csv', as.is = T)
file5 <- read.csv2(file = '05.csv', as.is = T)
file7 <- read.csv2(file = '07.csv', as.is = T)

# set-up export bestand
data <- file5[c(1:4,6)]
data$VOLLEDIG.VESTIGINGSNUMMER <- '000000'
data$schoolnaam <- 'onbekend'
data$denominatie <- 'onbekend'
data$totaal.leerlingen <- 0
data$percentage.vmbo <- 0
data$percentage.havo <- 0
data$percentage.vwo <- 0
data$geslaagd.vmbo <- 0
data$cijfer.vmbo <- 0
data$geslaagd.havo <- 0
data$cijfer.havo <- 0
data$geslaagd.vwo <- 0
data$cijfer.vwo <- 0
data$percentage.zittenblijvers <- 0
data$percentage.opstromers <- 0

# bereken percentages van verschillende niveau's
file1['Totaal.Rij'] <- apply(file1[,c(12:23)], 1, sum)
opleidingsnamen <- unique(file1$OPLEIDINGSNAAM)
opleidingsnamen.brugklassen <- grep('Ljvb|Lj', opleidingsnamen, value = T, ignore.case = T)
#opleidingsnamen.lwoo <- grep('LWOO', grep('Ljvb|Lj', opleidingsnamen, value = T, invert = T,ignore.case = T), value = T, ignore.case = T)
opleidingsnamen.vmbo <- grep('VMBO|MAVO|LWOO|Praktijk', grep('Ljvb|Lj', opleidingsnamen, value = T, invert = T, ignore.case = T), value = T, ignore.case = T)
opleidingsnamen.havo <- grep('HAVO', grep('Ljvb|Lj', opleidingsnamen, value = T, invert = T, ignore.case = T), value = T, ignore.case = T)
opleidingsnamen.vwo <- grep('VWO', grep('Ljvb|Lj', opleidingsnamen, value = T, invert = T, ignore.case = T), value = T, ignore.case = T)

# loop over unieke combi brin en vestiging
for (brin in unique(data[['BRINNUMMER']])){
  d <- file5[file5$BRINNUMMER == brin,]
  for (vestiging in d[['VESTIGINGSNUMMER']]) {
    
    # percentages/verhoudingen van leerlingen toevoegen
    perc <- file1[file1$BRIN.NUMMER == brin &
                    file1$VESTIGINGSNUMMER == vestiging,]
    leerlingen.aantal <- sum(perc$Totaal.Rij)
    vmbo.totaal <- sum(perc$Totaal.Rij[perc$OPLEIDINGSNAAM %in% opleidingsnamen.vmbo])
    havo.totaal <- sum(perc$Totaal.Rij[perc$OPLEIDINGSNAAM %in% opleidingsnamen.havo])
    vwo.totaal <- sum(perc$Totaal.Rij[perc$OPLEIDINGSNAAM %in% opleidingsnamen.vwo])
    totaal.leerlingen <- sum(vmbo.totaal, havo.totaal, vwo.totaal)
    vmbo.percentage <- vmbo.totaal / totaal.leerlingen * 100
    havo.percentage <- havo.totaal / totaal.leerlingen * 100
    vwo.percentage <- vwo.totaal / totaal.leerlingen * 100
    
    data$totaal.leerlingen [data$BRINNUMMER == brin &
                              data$VESTIGINGSNUMMER == vestiging] <- d$TOTAAL.LEERLINGEN[d$VESTIGINGSNUMMER == vestiging]
    data$percentage.vmbo[data$BRINNUMMER == brin &
                           data$VESTIGINGSNUMMER == vestiging] <- if(is.nan(vmbo.percentage)) 0 else vmbo.percentage
    data$percentage.havo[data$BRINNUMMER == brin &
                           data$VESTIGINGSNUMMER == vestiging] <- if(is.nan(havo.percentage)) 0 else havo.percentage
    data$percentage.vwo[data$BRINNUMMER == brin &
                           data$VESTIGINGSNUMMER == vestiging] <- if(is.nan(vwo.percentage)) 0 else vwo.percentage
    
    # voeg percentage zittenblijvers en opstromers toe
    #cat(sprintf("Brin:%s vestiging:%s\n", brin, vestiging))
    data$percentage.zittenblijvers[data$BRINNUMMER == brin 
                                   & data$VESTIGINGSNUMMER == vestiging] <- d$PERCENTAGE.ZITTENBLIJVERS[d$VESTIGINGSNUMMER == vestiging]
    data$percentage.opstromers[data$BRINNUMMER == brin 
                               & data$VESTIGINGSNUMMER == vestiging] <- d$PERCENTAGE.OPSTROMERS[d$VESTIGINGSNUMMER == vestiging]
    
    # volledige vestigingnummer toevoegen
    volledigevestiging <- paste(brin, vestiging, sep = '')
    if (nchar(vestiging) == 1){
      volledigevestiging <- paste(brin, '0',vestiging, sep = '')
    }
    
    data$VOLLEDIG.VESTIGINGSNUMMER[data$BRINNUMMER == brin & 
                                     data$VESTIGINGSNUMMER == vestiging] <- volledigevestiging
    
    
    # slagingspercentage en gemiddelde cijfer toevoegen voor scholen waarvoor dit bekend is
    slagperc <- file7[file7$BRIN.NUMMER == brin &
                        (file7$VESTIGINGSNUMMER == paste(brin, vestiging, sep="") | 
                        file7$VESTIGINGSNUMMER == paste(brin, vestiging, sep = "0")),]
    
    if (nrow(slagperc) == 0) { next }
    
    vmbo.slagperc <- sum(slagperc$GESLAAGDEN[slagperc$ONDERWIJSTYPE.VO == 'VMBO'])/sum(slagperc$EXAMENKANDIDATEN[slagperc$ONDERWIJSTYPE.VO == 'VMBO']) * 100
    vmbo.gemiddelde <- with(slagperc[slagperc$ONDERWIJSTYPE.VO == 'VMBO',], sum(EXAMENKANDIDATEN * GEMIDDELD.CIJFER.CIJFERLIJST)/sum(EXAMENKANDIDATEN))
    if (!is.nan(vmbo.slagperc)) {
      data$geslaagd.vmbo[data$BRINNUMMER == brin & data$VESTIGINGSNUMMER == vestiging] <- 
        vmbo.slagperc 
      data$cijfer.vmbo[data$BRINNUMMER == brin & data$VESTIGINGSNUMMER == vestiging] <- 
        vmbo.gemiddelde
      }
    else{data$geslaagd.vmbo[data$BRINNUMMER == brin 
                            & data$VESTIGINGSNUMMER == vestiging] <- 0}
    
    havo.slagperc <- sum(slagperc$GESLAAGDEN[slagperc$ONDERWIJSTYPE.VO == 'HAVO'])/sum(slagperc$EXAMENKANDIDATEN[slagperc$ONDERWIJSTYPE.VO == 'HAVO']) * 100
    havo.gemiddelde <- with(slagperc[slagperc$ONDERWIJSTYPE.VO == 'HAVO',], sum(EXAMENKANDIDATEN * GEMIDDELD.CIJFER.CIJFERLIJST)/sum(EXAMENKANDIDATEN))
    if (!is.nan(havo.slagperc)) {
      data$geslaagd.havo[data$BRINNUMMER == brin & data$VESTIGINGSNUMMER == vestiging] <- 
        havo.slagperc
      data$cijfer.havo[data$BRINNUMMER == brin & data$VESTIGINGSNUMMER == vestiging] <- 
        havo.gemiddelde
      }
    else{data$geslaagd.havo[data$BRINNUMMER == brin 
                            & data$VESTIGINGSNUMMER == vestiging] <- 0}
    
    vwo.slagperc <- sum(slagperc$GESLAAGDEN[slagperc$ONDERWIJSTYPE.VO == 'VWO'])/sum(slagperc$EXAMENKANDIDATEN[slagperc$ONDERWIJSTYPE.VO == 'VWO']) * 100
    vwo.gemiddelde <- with(slagperc[slagperc$ONDERWIJSTYPE.VO == 'VWO',], sum(EXAMENKANDIDATEN * GEMIDDELD.CIJFER.CIJFERLIJST)/sum(EXAMENKANDIDATEN))
    if (!is.nan(vwo.slagperc)) {
      data$geslaagd.vwo[data$BRINNUMMER == brin & data$VESTIGINGSNUMMER == vestiging] <- 
        vwo.slagperc
      data$cijfer.vwo[data$BRINNUMMER == brin & data$VESTIGINGSNUMMER == vestiging] <- 
        vwo.gemiddelde
      }
    else{data$geslaagd.vwo[data$BRINNUMMER == brin 
                            & data$VESTIGINGSNUMMER == vestiging] <- 0}
    
    #break
  }
  #break
}

gemeentes <- read.csv2(file = 'gemeentes-per-titel.csv', as.is = T)
colnames(gemeentes) <- c('TITEL', 'GEMEENTE')
scholen <- read.csv2(file = 'scholen2017.csv', as.is = T)

for (vestigingsnummer in scholen$VESTIGINGSNUMMER){
  data$schoolnaam[data$VOLLEDIG.VESTIGINGSNUMMER == vestigingsnummer] <- 
    scholen$INSTELLINGSNAAM.VESTIGING[scholen$VESTIGINGSNUMMER == vestigingsnummer]
  data$denominatie[data$VOLLEDIG.VESTIGINGSNUMMER == vestigingsnummer] <- 
    scholen$DENOMINATIE[scholen$VESTIGINGSNUMMER == vestigingsnummer]
}

rm(df.onbekend)
library(xlsx)
wb <- createWorkbook()

for (Titel in unique(gemeentes[['TITEL']])){
  df <- data[data$GEMEENTENAAM.VESTIGING %in% gemeentes$GEMEENTE[gemeentes$TITEL == Titel]|data$PLAATSNAAM.VESTIGING %in% gemeentes$GEMEENTE[gemeentes$TITEL == Titel],]
  if (exists("df.onbekend")){
    df.onbekend <- rbind(df.onbekend, df[df$schoolnaam == 'onbekend',])
  }
  else {
    df.onbekend <- df[df$schoolnaam == 'onbekend',]
  }
  df <- df[c(1:3,6,7,4,5,8:9, 13:ncol(df))]
  df <- df[df$geslaagd.vmbo != 0 |
             df$geslaagd.havo != 0 |
             df$geslaagd.vwo != 0,]
  df <- df[order(df$GEMEENTENAAM.VESTIGING, df$INSTELLINGSNAAM.VESTIGING), c(7,5,9:15,8,16:17)]
  
  print(paste(Titel, ': ', nrow(df[df$schoolnaam == 'onbekend',])/nrow(df),
              ' (', nrow(df[df$schoolnaam == 'onbekend',]), ' van ', nrow(df),
              ')', sep = ""))
  write.csv2(df, file = paste(Titel, '.csv', sep = ""), row.names = F)
  sheet <- createSheet(wb, sheetName = Titel)
  addDataFrame(df, sheet, row.names = F)
}

saveWorkbook(wb, file = 'r-output.xlsx')

