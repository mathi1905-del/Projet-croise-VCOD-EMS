#install.packages("seqinr")

library(seqinr)
library(dbplyr)

# Fonction qui permet de lire les fichiers et de séparer chaque élément des séquences
lecture_split_seq <- function(nom_fichier){
  adn <- read.alignment(file = nom_fichier, format = "fasta")
  
  seq_split <- list()
  seq_split$nam <- adn$nam
  for (i in 1:adn$nb){
    adn_seq <- strsplit(x = adn$seq[[i]], split = "")
    seq_split$seq[i] <- adn_seq
  }
  return (seq_split)
}

adn_fr_split <- lecture_split_seq("hiv_db_FR_0.fasta")
adn_be_split <- lecture_split_seq("hiv_db_BE_0.fasta")
adn_gb_split <- lecture_split_seq("hiv_db_GB_0.fasta")
adn_rw_split <- lecture_split_seq("hiv_db_RW_0.fasta")
adn_ug_split <- lecture_split_seq("hiv_db_UG_0.fasta")
adn_us_split <- lecture_split_seq("hiv_db_US_0.fasta")


########### Fonction qui calcul le nombre de nucléotides, de tirets ########### 
compatage_nucleotide <- function(adn){
  nb_nucleotides <- matrix(NA, nrow = length(adn$seq), ncol = 4)
  colnames(nb_nucleotides) <- c("ID", "nb_nucleotides", "nb_tirets", "Total")
  for (i in 1:length(adn$seq)){
    sequence <- adn$seq[[i]]
    nb_nucleotides[i, 1] <- adn$nam[i]
    nb_tiret <- sum(sequence=='-')
    nb_nucleotides[i, 3] <- nb_tiret
    nb_nucleotides[i, 2] <- length(adn$seq[[i]]) - nb_tiret
    nb_nucleotides[i, 4] <- length(adn$seq[[i]])
  }
  return(as.data.frame(nb_nucleotides))
}

# Evolution des nucléotides en fonction du temps sequences serveur
adn_all_split <- lecture_split_seq("ALL_sequences.fasta")

# Tableau avec le nombre de nucléotide
comptage <- compatage_nucleotide(adn_all_split)

# Ecriture du résultat dans un fichier csv
write.table(x = comptage, file = "nb_nucléotide.csv", sep =";", col.names = NA)

#################### Evolution taille des séquences en fonction des année moyenne ###########################
recuperer_info<- function(dataframe){
  tableau <- matrix(NA, ncol = 4, nrow = nrow(dataframe))
  for (i in 1:nrow(dataframe)){
    decomp_id <- strsplit(dataframe[i, 1], split = "[.]")
    tableau[i, 1] <- dataframe[i, 1]
    tableau[i, 2] <- decomp_id[[1]][5]
    if (tableau[i, 2] == "NL"){
      tableau[i, 3] <- decomp_id[[1]][3]
      mois <- decomp_id[[1]][2]
      trimestre <- c("1", "2", "3", "4")
      if(mois == "1" || mois == "2" || mois == "3"){
        tableau[i, 4] <- trimestre[1]
      } else if(mois == "4" || mois == "5" || mois == "6") {
        tableau[i, 4] <- trimestre[2]
      } else if(mois == "7" || mois == "8" || mois == "9") {
        tableau[i, 4] <- trimestre[3]
      } else {
        tableau[i, 4] <- trimestre[4]
      }
    }
  else {
    tableau[i, 3] <- decomp_id[[1]][4]
    mois <- decomp_id[[1]][3]
    trimestre <- c("1", "2", "3", "4")
    if(mois == "1" || mois == "2" || mois == "3"){
      tableau[i, 4] <- trimestre[1]
    } else if(mois == "4" || mois == "5" || mois == "6") {
      tableau[i, 4] <- trimestre[2]
    } else if(mois == "7" || mois == "8" || mois == "9") {
      tableau[i, 4] <- trimestre[3]
    } else {
      tableau[i, 4] <- trimestre[4]
    }
  }
  }
  tableau <- as.data.frame(tableau)
  colnames(tableau) <- c("ID", "Pays", "Année", "Trimestre")
  return(tableau)
}
info_comptage <- recuperer_info(comptage)

# Ecriture du résultat dans un fichier csv
write.table(x = info_comptage, file = "info_comptage.csv", sep =";", col.names = NA)

bd_taille_sequence <- cbind (info_comptage, comptage[,-1])
bd_taille_sequence$nb_nucleotides <- as.numeric(bd_taille_sequence$nb_nucleotides)
# Ecriture du résultat dans un fichier csv
write.table(x = bd_taille_sequence, file = "bd_taille_sequence.csv", sep =";", col.names = NA)

colnames(bd_taille_sequence)

########## Sequence moodle ###########
# lecture et fusioon de tous les fichiers moodle 
lecture_fichiers_adn <- function(vecteur_noms_fichiers){
  adn_split <- list()
  valeur_i <- 0
  n <- 1
  for (a in 1:length(vecteur_noms_fichiers)){
    adn <- read.alignment(file = vecteur_noms_fichiers[a], format = "fasta")
    adn_split$nam <- c(adn_split$nam, adn$nam)
    for (i in valeur_i+1:adn$nb){
      adn_split$seq[i] <- strsplit(adn$seq[[n]], split = "")
      n <- n + 1
    }
    valeur_i <- i
    n <- 1
  }
  return(adn_split)
}

noms_fichiers <- c("hiv_db_BE_0.fasta", "hiv_db_FR_0.fasta", "hiv_db_GB_0.fasta", "hiv_db_RW_0.fasta", "hiv_db_UG_0.fasta", "hiv_db_US_0.fasta")
adn_final <- lecture_fichiers_adn(noms_fichiers)
nb_nucleotides_final <- compatage_nucleotide(adn_final)
info_moodle <- recuperer_info (nb_nucleotides_final)
bd_taille_sequence_moodle <- cbind (info_moodle, nb_nucleotides_final[,-1])
bd_taille_sequence_moodle$nb_nucleotides <- as.numeric(bd_taille_sequence_moodle$nb_nucleotides)

bd_final <- rbind (bd_taille_sequence, bd_taille_sequence_moodle)
################ Calcul des moyennes par année ################ 
install.packages("sqldf")
library(sqldf)

moyenne_taille_annee <- sqldf("select Année, avg(nb_nucleotides) as moyenne from bd_final group by Année")

install.packages("ggplot2")
library(ggplot2)
plot( moyenne_taille_annee$Année, moyenne_taille_annee$moyenne,
main= " Moyenne des tailles des séquences en fonction des années" ,
ylab="Moyenne des tailles",
xlab="Année")
