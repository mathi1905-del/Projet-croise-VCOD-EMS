#install.packages("seqinr")

library(seqinr)

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

adn_fr_split$nam <- lecture_split_seq("hiv_db_FR_0.fasta")
adn_be_split <- lecture_split_seq("hiv_db_BE_0.fasta")
adn_gb_split <- lecture_split_seq("hiv_db_GB_0.fasta")
adn_rw_split <- lecture_split_seq("hiv_db_RW_0.fasta")
adn_ug_split <- lecture_split_seq("hiv_db_UG_0.fasta")
adn_us_split <- lecture_split_seq("hiv_db_US_0.fasta")


# Fonction qui calcul le nombre de nucléotides, de tirets
nb_elm_seq <- function (adn_split){
  
  nb_elm_seq <- matrix (data = "NA", nrow = length(adn_split$seq), ncol = 4)
  colnames (nb_elm_seq) <- c("ID", "Nucléotide", "Tiret", "Total")
  
  for (i in 1:(length(adn_split$seq))){
    nb_nucleo = 0
    nb_tiret = 0
    
    for (j in 1:length(adn_split$seq[[i]])){
      if (adn_split$seq[[i]][j] != "-"){
        nb_nucleo = nb_nucleo + 1
      }
      else {
        nb_tiret <- nb_tiret + 1
      }
      nb_elm_seq[i,1] <- adn_split$nam[i]
      nb_elm_seq[i,2] <- nb_nucleo
      nb_elm_seq[i,3] <- nb_tiret
      nb_elm_seq[i,4] <- nb_nucleo + nb_tiret
    }
  }
  return (nb_elm_seq)
}

elm <- nb_elm_seq(adn_fr_split)


# Fonction pour détecter des séquences
detection_seq <- function(adn, sequence_init){
  adn <- strsplit(x = adn, split = "")
  
  sequence <- strsplit(x = sequence_init, split = "")
  
  sequence_res = ""

  indice_premier_elm <- 1
  cpt <- 0
  elm = adn[[1]][1]
  elm_seq <- sequence[[1]][1]
    for (i in 1:length(adn)){
      print (sequence_res)
      print(sequence)
      if (elm == elm_seq){
        elm = adn[[1]][i+1]
        print(elm)
        elm_seq = sequence[[1]][i+1]
        print(elm_seq)
        sequence_res = paste (sequence_res, elm, sep="")
        print(sequence_res)
        cpt = cpt + 1
        print(cpt)
      }
      else {
        elm = adn[[1]][i+1]
        print(elm)
        sequence_res = ""
        indice_premier_elm <- i + 1
        cpt = cpt + 1
      }
    }

  return (list(sequence_res, indice_premier_elm))
}
 
detection_seq("agctctgattcgagggcccatgcgatc", "gggccc")









