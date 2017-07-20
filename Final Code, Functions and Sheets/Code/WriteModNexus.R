## Modified write.nexus.data function to cope with standard data
write.nexus.std <- ape::write.nexus.data
body(write.nexus.std)[[2]] <- substitute(format <- match.arg(toupper(format), c("DNA", "PROTEIN", "STANDARD")))
body(write.nexus.std)[[26]][[3]][[4]] <- substitute(fcat(indent, "FORMAT", " ", DATATYPE, " ", MISSING, " ", GAP, 
                                                         " ", INTERLEAVE, " symbols=\"0123456789\";\n"))

write.nexus.std(Big_matrix_TWO, file = "Big_matrix_TWO.nex")

Big_matrix_TWO <- gsub(NA, "?",Big_matrix_flipped)

class(Big_matrix_TWO) 
length(Big_matrix_TWO) 
dim(Big_matrix_flipped) 
str(Big_matrix_TWO)

nexus_matrix <- MakeMorphMatrix(Big_matrix_flipped)
WriteMorphNexus(nexus_matrix, file = "Big_matrix_flipped_again.nex")

setdiff(unique(unlist(strsplit(as.character(unique(as.vector(Big_matrix_flipped))),  "&"))), c(as.character(0:31), NA, "-"))

source("MakeMorphMatrix.R")
