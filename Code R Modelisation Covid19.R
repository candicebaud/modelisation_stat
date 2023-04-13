save(data, file = "Données hospitalisation.RData")
load(file = "Données hospitalisation.RData")
load(file = "Données hospitalisation concaténées")
require(zoo)

#setwd("C:/Users/baptg/OneDrive/Bureau/Projet Modelisation Stat")
setwd("C:/Users/candi/Desktop/ETUDES/ENSAE2A/semestre 2/options/projet stat/modelisation_stat")
data <- read.csv('covid-hospit-incid-2023-03-31-18h01 (1).csv', sep=";")

require(dplyr)

plot_data <- function(Column_name){
  indices <- seq(from = 1, to = 1108)
  otherindices <- seq(from = 1, to = 95)
  new_values = as.data.frame(Column_name)[c(1:106368),]
  
  resultats <- new_values[c(1:1108)]
  for (j in otherindices) {
    for (i in indices){
      somme <- resultats[[i]] + new_values[[i+j*1108]]
      resultats[[i]] <- somme
    }
    
  }

  return(resultats)
}

Date = as.Date(as.data.frame(data$jour)[c(1:1108),])

Hospitalisation <- plot_data(data$incid_hosp)
Reanimation <- plot_data(data$incid_rea)
Deces <- plot_data(data$incid_dc)
Retour_domicile <- plot_data(data$incid_rad)

df_donnees_covid19 <- data.frame(Date,Hospitalisation,Reanimation,Deces,Retour_domicile)
save(df_donnees_covid19, file = "Données hospitalisation concaténées")




plot(Date,log(Hospitalisation),type = "l",xaxt = "n",xlab = "Date")
axis(1, x, format(x, "%d-%m-%Y"))

#Par semaine


new_df <- slice(df_donnees_covid19, -c(1, 2, 3, 4,1104,1105,1106,1107,1108))
new_df$Date <- NULL

#Moyenne mobile

x <- c(7:600)
df_donnees_week_averagemob <- rollmean(new_df, k = 7, align = "right", fill = NA)
plot(x,df_donnees_week_averagemob[7:600,1],type = "l",xaxt = "n",xlab = "Date")

#Moyenne sur 7 jours

df_donnees_week <- data.frame()

for (k in c(0:156)){
  ROW <- colSums(new_df[c((1+7*k):(7+7*k)), ])
  df_donnees_week <- rbind(df_donnees_week, ROW)
}


colnames(df_donnees_week) <- c("Hospitalisation", "Reanimation", "Deces", "Retour_domicile")


Hospitalisation <- df_donnees_week$Hospitalisation
Reanimation <- df_donnees_week$Reanimation
Deces <- df_donnees_week$Deces
Retour_domicile <- df_donnees_week$Retour_domicile

x<-c(1:90)
plot(x,Retour_domicile[1:90],type = "l",xaxt = "n",xlab = "Date")
axis(1, x, format(x, "%d-%m-%Y"))


