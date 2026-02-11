setwd("/Users/TOURE/OneDrive - Université Clermont Auvergne/sae_croise/all/")
getwd()

install.packages("seqinr")
install.packages("dplyr")
install.packages("stringr")
install.packages("readr")
install.packages("ggplot2")

library(seqinr)
library(dplyr)
library(stringr)
library(readr)
library(ggplot2)

bio_medi <- read.csv("ALL_biomedical.csv")
cancer   <- read.csv("ALL_cancer.csv")
social   <- read.csv("ALL_social.csv")

sequences <- read.alignment(
  file = "ALL_sequences.fasta",
  format = "fasta"
)

head(sequences)

detection_seq <- function(adn, sequence_init, tolerance = 2){
  
  adn <- tolower(as.character(adn))
  sequence_init <- tolower(sequence_init)
  
  n <- nchar(sequence_init)
  len_adn <- nchar(adn)
  
  if(len_adn < n){
    return(0)
  }
  
  compteur <- 0
  
  for(i in 1:(len_adn - n + 1)){
    
    fragment <- substr(adn, i, i + n - 1)
    
    diff <- sum(strsplit(fragment, "")[[1]] != 
                  strsplit(sequence_init, "")[[1]])
    
    if(diff <= tolerance){
      compteur <- compteur + 1
    }
  }
  
  return(compteur)
}

longueurs <- nchar(sequences$seq)

nb_motif <- sapply(sequences$seq, detection_seq,
                   sequence_init = "gggccc",
                   tolerance = 2)

rapport_sequences <- data.frame(
  nom_sequence = sequences$nam,
  longueur_sequence = longueurs,
  nb_occurrences_gggccc = nb_motif
)

rapport_sequences
