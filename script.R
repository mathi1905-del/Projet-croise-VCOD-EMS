#install.packages("seqinr")

library(seqinr)

########### Fonction qui permet de lire les fichiers et de séparer chaque élément des séquences ###########
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


########### Fonction qui calcul le nombre de nucléotides, de tirets ###########
compatage_nucleotide <- function(adn){
  nb_nucleotides <- matrix(NA, nrow = length(adn$seq), ncol = 4)
  colnames(nb_nucleotides) <- c("nom_sequence", "nb_nucleotides", "nb_tirets", "Total")
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

# Evolution des nucléotides en fonction du temps
adn_all_split <- lecture_split_seq("ALL_sequences.fasta")

# Tableau avec le nombre de nucléotide
comptage <- compatage_nucleotide(adn_all_split)

# Ecriture du résultat dans un fichier csv
write.table(x = comptage, file = "nb_nucléotide.csv", sep =";", col.names = NA)


########### Fonction pour détecter des séquences ###########
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









