#######################################################################
###############       MATRIX CREATION      ############################
#######################################################################

source("../Code/ReadMorphNexus.txt")

install.packages("devtools")
library(devtools)

install_github("graemetlloyd/Claddis")
library(Claddis)

remove.packages("Claddis")

install_github("TGuillerme/Claddis")
library(Claddis)

install.packages("gdata")
library(gdata)

## Read in the data ##

Beck_Nexus<- ReadMorphNexus("../In/2014-Beck-ProcB-matrix-morpho.nex", gap.as.missing=FALSE)
Halliday_Nexus<- ReadMorphNexus("../In/2015-Halliday-LinSoc-matrix-morpho.nex", gap.as.missing=FALSE)

## Extract the matrices ##

Beck_matrix<- Beck_Nexus$matrix
Halliday_matrix<- Halliday_Nexus$matrix

## Extract the row names ##

Beck_species<-row.names(Beck_matrix)
Halliday_species<-row.names(Halliday_matrix)

## Find the matching and unique species ##

Match_Beck<-match(Beck_species, Halliday_species)

Unique_Beck_species<-Beck_species[is.na(Match_Beck)]
Common_Beck_species<-Beck_species[!is.na(Match_Beck)]

Match_Halliday<-match(Halliday_species, Beck_species)

Unique_Halliday_species<-Halliday_species[is.na(Match_Halliday)]
Common_Halliday_species<-Halliday_species[!is.na(Match_Halliday)]

## Becuase it alphebetized them so use function sort ##

sort(Common_Halliday_species)==sort(Common_Beck_species)

## Upload the combination list ##

Combo_Excel<-read.csv("../In/Combining Sheet.csv")

## Create a matrix of becks unique species and Halidays characters ## 

Draft_Beck_matrix<-matrix(NA,nrow=length(Unique_Beck_species),ncol = (ncol(Halliday_matrix)))

## Name the new matrix rows ##

row.names(Draft_Beck_matrix)<-Unique_Beck_species

## Create a loop to place all of the correct characters in the correct columns ##

for(character in 1:nrow(Combo_Excel)){
  cat(paste("replace character" ,Combo_Excel[character, 3], "into", Combo_Excel[character, 2], "\n"))
  Draft_Beck_matrix[,Combo_Excel[character,3]]<-Beck_matrix[match(Unique_Beck_species,row.names(Beck_matrix)),Combo_Excel[character,2]]
}

## Combine the matrix of unique Beck taxa with all of Hallidays taxa and characters ##

Beck_Halliday_matrix_draft_first<-rbind(Halliday_matrix,Draft_Beck_matrix)

## *Avoid magic numbers* Create a matrix of Becks unique characters ##

New_matrix_beck_unique_characters<-matrix(NA,ncol = 13,nrow= nrow(Halliday_matrix))

## Name the new matrix with Hallidays species ##

row.names(New_matrix_beck_unique_characters)<-row.names(Halliday_matrix)

## Create a Matrix of Becks unique characters with Becks taxa ##

Unique_Beck_mini_matrix<-Beck_matrix[match(Unique_Beck_species,row.names(Beck_matrix)),409:421]

## Combine the two new smaller matrices ##

Beck_characters_matrix<-rbind(New_matrix_beck_unique_characters,Unique_Beck_mini_matrix)

## Combine all into the BIG matrix ##

Big_matrix<-cbind(Beck_Halliday_matrix_draft_first,Beck_characters_matrix)


############################################################################
###############      LOOPING SECTION      ##################################
############################################################################

source("flip.characters.R")

## Read in Flip excel ##

Flip_Loop<-read.csv("../In/FlippingLOOPdaLOOP.csv")

## Extract which species need flipping (all Beck species) ##

Species_list<- which(rownames(Big_matrix) %in% Beck_species)
rownames(Big_matrix)
which(rownames(Big_matrix) %in% Beck_species)
rownames(Big_matrix)[Species_list]

## Create a new Big matrix to compare and protect ##

Big_matrix_flipped<-Big_matrix

## Where are character numbers = Every row, colum one ##

Character_numbers<-unique(Flip_Loop[,1])

## Loop for each character (unique) read left to right flip characters using function ##
for(one_character_number in 1:length(Character_numbers)){ ## For as long as Flip sheet (ie. character numbers) ##
  Character_number_one<-Character_numbers[one_character_number] ## Indiviudual (unique) character ##
  Character <- Big_matrix[,Character_number_one]  ## Extract from Big Matrix ##
  Character_rows<-which(Flip_Loop[,1] %in% Character_number_one) ## Which characters to flip ##
  tmp_list <- t(Flip_Loop[Character_rows,2:3]) ## Flip states in row 2 to be states in row 3 ##
  conv_list <- list() ## Create an empty conversion list ##
  for(tmpcolumn in 1:ncol(tmp_list)){ ## Sub-loop using all columns to fill conv_list and print as character ##
    conv_list[[tmpcolumn]]<-as.character(tmp_list[,tmpcolumn])
  }
  Big_matrix_flipped[,Character_number_one]<-flip.characters(Character, Species_list,conv_list) ## Insert the flipped characters into a New flipped Matrix ##
}

###################################################################################
##########################       WRITING TO NEXUS       ###########################
###################################################################################

## Modified write.nexus.data function to cope with standard data
write.nexus.std <- ape::write.nexus.data
body(write.nexus.std)[[2]] <- substitute(format <- match.arg(toupper(format), c("DNA", "PROTEIN", "STANDARD")))
body(write.nexus.std)[[26]][[3]][[4]] <- substitute(fcat(indent, "FORMAT", " ", DATATYPE, " ", MISSING, " ", GAP, 
                                                         " ", INTERLEAVE, " symbols=\"0123456789\";\n"))

####################################################################################
##########################       ATTEMPTS TO FIX/DEBUG       #######################
####################################################################################


write.nexus.std(Big_matrix_TWO, file = "Big_matrix_TWO.nex")

Big_matrix_TWO <- gsub(NA, "?",Big_matrix_flipped)

class(Big_matrix_TWO) 
length(Big_matrix_TWO) 
dim(Big_matrix_TWO) 
str(Big_matrix_TWO)

nexus_matrix <- MakeMorphMatrix(Big_matrix_flipped)
WriteMorphNexus(nexus_matrix, file = "Big_matrix_flipped_again.nex")

setdiff(unique(unlist(strsplit(as.character(unique(as.vector(Big_matrix_flipped))),  "&"))), c(as.character(0:31), NA, "-"))
