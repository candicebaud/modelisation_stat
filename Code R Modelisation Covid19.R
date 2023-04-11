setwd("C:/Users/baptg/OneDrive/Bureau/Projet Modelisation Stat")
data <- read.csv('covid-hospit-incid-2023-03-31-18h01 (1).csv', sep=";")

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


y <- plot_data(data$incid_rad)



x = as.Date(as.data.frame(data$jour)[c(1:1108),])

plot(x,log(y),type = "l",xaxt = "n",xlab = "Date")
axis(1, x, format(x, "%d-%m-%Y"))



log 

variation









#Broullion

new_values = as.data.frame(data$incid_hosp)[c(1:106368),]


x = as.Date(as.data.frame(data$jour)[c(1:1108),])
#y = as.data.frame(data$incid_hosp)[c(1:1108),]
#plot(x,y,type = "l", xaxt = "n")
#axis(1, x, format(x, "%d-%m-%Y"))



indices <- seq(from = 1, to = 1108)
otherindices <- seq(from = 1, to = 95)


resultats <- new_values[c(1:1108)]
for (j in otherindices) {
  for (i in indices){
    somme <- resultats[[i]] + new_values[[i+j*1108]]
    resultats[[i]] <- somme
  }
  
}



plot(x,resultats,type = "l",xaxt = "n")
axis(1, x, format(x, "%d-%m-%Y"))
