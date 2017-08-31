library(ape)
library(phylotate)


## Functions for grabbing node values
get.node.values <- function(node) {
    return(list("hpd" = get.node.hpd(node), "median" = get.node.median(node), "mean" = get.node.mean(node)))
}

get.node.hpd <- function(node) {
    return(as.numeric(unlist(strsplit(strsplit(strsplit(node, "age_95%HPD=\\{")[[1]][2], "\\}")[[1]][1], ","))))
}

get.node.median <- function(node) {
    return(as.numeric(strsplit(strsplit(node, "age_median=")[[1]][2], ",")[[1]][1]))
}

get.node.mean <- function(node) {
    return(as.numeric(strsplit(strsplit(node, "age_mean=")[[1]][2], ",")[[1]][1]))
}

##################
## Beck_Tree_98 ##
##################

## Reading the tree (make sure which one you want to read)
tree <- read_annotated("Topologies/original_tree_files/Beck_tree_98constrain.tre")

## Extracting clades (most recent common ancestor)
mrca_table <- mrca(tree) #This gives a table of nodes of mrca numbered from tips through nodes.
mrca_table

## Example we want the primates MRCA, (shortest tree including Notharctus and Purgatorius) - check the tree for the furthest related in a clade, use these.
Eutharia_mrca <- mrca_table[which(rownames(mrca_table) == "Juramaia"), which(colnames(mrca_table) == "Simpsonotus")]
Placentalia_mrca <- mrca_table[which(rownames(mrca_table) == "Dasypodidae"), which(colnames(mrca_table) == "Simpsonotus")]
Metratheria_mrca <- mrca_table[which(rownames(mrca_table) == "Deltatheridium"), which(colnames(mrca_table) == "Pucadelphys")]
Artiodactyla_mrca <- mrca_table[which(rownames(mrca_table) == "Gujaratia"), which(colnames(mrca_table) == "Diacodexis")]
Primates_mrca <- mrca_table[which(rownames(mrca_table) == "Purgatorius"), which(colnames(mrca_table) == "Adapis")]
Carnivora_mrca <- mrca_table[which(rownames(mrca_table) == "Protictis"), which(colnames(mrca_table) == "Miacis")]
Sirenia_mrca <- mrca_table[which(rownames(mrca_table) == "Trichechus"), which(colnames(mrca_table) == "Pezosiren")]
Glires_mrca <- mrca_table[which(rownames(mrca_table) == "Tribosphenomys"), which(colnames(mrca_table) == "Mimotona")]
Xenarthra_mrca <- mrca_table[which(rownames(mrca_table) == "Dasypodidae"), which(colnames(mrca_table) == "Myrmecophagidae")]
Paenungulata_mrca <- mrca_table[which(rownames(mrca_table) == "Procavia"), which(colnames(mrca_table) == "Trichechus")]
Notoungulata_mrca <- mrca_table[which(rownames(mrca_table) == "Henricosbornia"), which(colnames(mrca_table) == "Simpsonotus")]
Euarchonta_mrca <- mrca_table[which(rownames(mrca_table) == "Cynocephalus"), which(colnames(mrca_table) == "Adapis")]
Ferae_mrca <- mrca_table[which(rownames(mrca_table) == "Patriomanis"), which(colnames(mrca_table) == "Miacis")]

## This also include the tips numbers, in this case, primates_mrca = 175 but it is node number 73 (175 - Ntip(tree))

## Selecting the node (WARNING maybe this is not the actual node position in the comments, make sure it works on couple of nodes/trees)
node_Eutharia <- Eutharia_mrca-Ntip(tree)
node_Placentalia <- Placentalia_mrca-Ntip(tree)
node_Metratheria <- Metratheria_mrca-Ntip(tree)
node_Artiodactyla <- Artiodactyla_mrca-Ntip(tree)
node_Primates <- Primates_mrca-Ntip(tree)
node_Carnivora <- Carnivora_mrca-Ntip(tree)
node_Sirenia <- Sirenia_mrca-Ntip(tree)
node_Glires <- Glires_mrca-Ntip(tree)
node_Xenarthra <- Xenarthra_mrca-Ntip(tree)
node_Paenungulata <- Paenungulata_mrca-Ntip(tree)
node_Notoungulata <- Notoungulata_mrca-Ntip(tree)
node_Euarchonta <- Euarchonta_mrca-Ntip(tree)
node_Ferae <- Ferae_mrca-Ntip(tree)


## Create a list of nodes (each one will be your node of interest)
node_list <- c(node_Eutharia, node_Placentalia, node_Metratheria, node_Artiodactyla, node_Primates, node_Carnivora, node_Sirenia,
                node_Glires, node_Xenarthra, node_Paenungulata, node_Notoungulata, node_Euarchonta, node_Ferae)

## Loop to get the values
node_values <- list()
for(one_node in 1:length(node_list)) {
    node_values[[one_node]] <- get.node.values(tree$node.comment[[ node_list[one_node] ]])
}
## Naming the elements of the list
names(node_values) <- node_list

## Clever option
names(node_values) <- c("Eutharia", "Placentalia", "Metatheria", "Artiodactyla", "Primates",
                         "Carnivora", "Sirenia", "Glires", "Xenarthra", "Paenungulata", 
                         "Notoungulata", "Euarchonta", "Ferae")


node_values_Beck_98 <- node_values

##################
## Beck_Tree_66 ##
##################

tree <- read_annotated("Topologies/original_tree_files/Beck_tree_66constrain.tre")

## Extracting clades (most recent common ancestor)
mrca_table <- mrca(tree) #This gives a table of nodes of mrca numbered from tips through nodes.

## Example we want the primates MRCA, (shortest tree including Notharctus and Purgatorius) - check the tree for the furthest related in a clade, use these.
Eutharia_mrca <- mrca_table[which(rownames(mrca_table) == "Juramaia"), which(colnames(mrca_table) == "Pyrotherium")]
Placentalia_mrca <- mrca_table[which(rownames(mrca_table) == "Dasypodidae"), which(colnames(mrca_table) == "Pyrotherium")]
Metratheria_mrca <- mrca_table[which(rownames(mrca_table) == "Deltatheridium"), which(colnames(mrca_table) == "Pucadelphys")]
Artiodactyla_mrca <- mrca_table[which(rownames(mrca_table) == "Gujaratia"), which(colnames(mrca_table) == "Diacodexis")]
Primates_mrca <- mrca_table[which(rownames(mrca_table) == "Purgatorius"), which(colnames(mrca_table) == "Adapis")]
Carnivora_mrca <- mrca_table[which(rownames(mrca_table) == "Protictis"), which(colnames(mrca_table) == "Miacis")]
Sirenia_mrca <- mrca_table[which(rownames(mrca_table) == "Trichechus"), which(colnames(mrca_table) == "Pezosiren")]
Glires_mrca <- mrca_table[which(rownames(mrca_table) == "Tribosphenomys"), which(colnames(mrca_table) == "Gomphos")]
Xenarthra_mrca <- mrca_table[which(rownames(mrca_table) == "Dasypodidae"), which(colnames(mrca_table) == "Myrmecophagidae")]
Paenungulata_mrca <- mrca_table[which(rownames(mrca_table) == "Procavia"), which(colnames(mrca_table) == "Trichechus")]
Notoungulata_mrca <- mrca_table[which(rownames(mrca_table) == "Simpsonotus"), which(colnames(mrca_table) == "Pyrotherium")]
Euarchonta_mrca <- mrca_table[which(rownames(mrca_table) == "Ptilocercus"), which(colnames(mrca_table) == "Adapis")]
Ferae_mrca <- mrca_table[which(rownames(mrca_table) == "Patriomanis"), which(colnames(mrca_table) == "Miacis")]

## This also include the tips numbers, in this case, primates_mrca = 175 but it is node number 73 (175 - Ntip(tree))

## Selecting the node (WARNING maybe this is not the actual node position in the comments, make sure it works on couple of nodes/trees)
node_Eutharia <- Eutharia_mrca - Ntip(tree)
node_Placentalia <- Placentalia_mrca-Ntip(tree)
node_Metratheria <- Metratheria_mrca-Ntip(tree)
node_Artiodactyla <- Artiodactyla_mrca-Ntip(tree)
node_Primates <- Primates_mrca-Ntip(tree)
node_Carnivora <- Carnivora_mrca-Ntip(tree)
node_Sirenia <- Sirenia_mrca-Ntip(tree)
node_Glires <- Glires_mrca-Ntip(tree)
node_Xenarthra <- Xenarthra_mrca-Ntip(tree)
node_Paenungulata <- Paenungulata_mrca-Ntip(tree)
node_Notoungulata <- Notoungulata_mrca-Ntip(tree)
node_Euarchonta <- Euarchonta_mrca-Ntip(tree)
node_Ferae <- Ferae_mrca-Ntip(tree)


## Create a list of nodes (each one will be your node of interest)
node_list <- c(node_Eutharia, node_Placentalia, node_Metratheria, node_Artiodactyla, node_Primates, node_Carnivora, node_Sirenia,
                node_Glires, node_Xenarthra, node_Paenungulata, node_Notoungulata, node_Euarchonta, node_Ferae)

## Loop to get the values
node_values <- list()
for(one_node in 1:length(node_list)) {
    node_values[[one_node]] <- get.node.values(tree$node.comment[[ node_list[one_node] ]])
}
## Naming the elements of the list
names(node_values) <- node_list

## Clever option
names(node_values) <- c("Eutharia", "Placentalia", "Metatheria", "Artiodactyla", "Primates", "Carnivora", "Sirenia", "Glires", "Xenarthra", "Paenungulata", "Notoungulata", "Euarchonta", "Ferae")


node_values_Beck_66 <- node_values 
# ... etc

list_node_values <- list(node_values_Beck_98, node_values_Beck_66)

##############################
## My_Beck_Tree_Constrained ##
##############################

tree <- read_annotated("Topologies/original_tree_files/Beck_Constrained.con.tre")

## Extracting clades (most recent common ancestor)
mrca_table <- mrca(tree) #This gives a table of nodes of mrca numbered from tips through nodes.

## Example we want the primates MRCA, (shortest tree including Notharctus and Purgatorius) - check the tree for the furthest related in a clade, use these.
Eutharia_mrca <- mrca_table[which(rownames(mrca_table) == "Acristatherium"), which(colnames(mrca_table) == "Henricosbornia")]
Placentalia_mrca <- mrca_table[which(rownames(mrca_table) == "Chaetophractus"), which(colnames(mrca_table) == "Henricosbornia")]
Metratheria_mrca <- mrca_table[which(rownames(mrca_table) == "Deltatheridium"), which(colnames(mrca_table) == "Mayulestes")]
Artiodactyla_mrca <- mrca_table[which(rownames(mrca_table) == "Gujaratia"), which(colnames(mrca_table) == "Diacodexis")]
Primates_mrca <- mrca_table[which(rownames(mrca_table) == "Purgatorius"), which(colnames(mrca_table) == "Notharctus")]
Carnivora_mrca <- mrca_table[which(rownames(mrca_table) == "Protictis"), which(colnames(mrca_table) == "Vulpavus")]
Sirenia_mrca <- mrca_table[which(rownames(mrca_table) == "Trichechus"), which(colnames(mrca_table) == "Pezosiren")]
Glires_mrca <- mrca_table[which(rownames(mrca_table) == "Paramys"), which(colnames(mrca_table) == "Gomphos")]
Xenarthra_mrca <- mrca_table[which(rownames(mrca_table) == "Chaetophractus"), which(colnames(mrca_table) == "Bradypus")]
Paenungulata_mrca <- mrca_table[which(rownames(mrca_table) == "Moeritherium"), which(colnames(mrca_table) == "Pezosiren")]
Notoungulata_mrca <- mrca_table[which(rownames(mrca_table) == "Simpsonotus"), which(colnames(mrca_table) == "Henricosbornia")]
Euarchonta_mrca <- mrca_table[which(rownames(mrca_table) == "Cynocephalus"), which(colnames(mrca_table) == "Notharctus")]
Ferae_mrca <- mrca_table[which(rownames(mrca_table) == "Patriomanis"), which(colnames(mrca_table) == "Vulpavus")]

## This also include the tips numbers, in this case, primates_mrca = 175 but it is node number 73 (175 - Ntip(tree))

## Selecting the node (WARNING maybe this is not the actual node position in the comments, make sure it works on couple of nodes/trees)
node_Eutharia <- Eutharia_mrca - Ntip(tree)
node_Placentalia <- Placentalia_mrca-Ntip(tree)
node_Metratheria <- Metratheria_mrca-Ntip(tree)
node_Artiodactyla <- Artiodactyla_mrca-Ntip(tree)
node_Primates <- Primates_mrca-Ntip(tree)
node_Carnivora <- Carnivora_mrca-Ntip(tree)
node_Sirenia <- Sirenia_mrca-Ntip(tree)
node_Glires <- Glires_mrca-Ntip(tree)
node_Xenarthra <- Xenarthra_mrca-Ntip(tree)
node_Paenungulata <- Paenungulata_mrca-Ntip(tree)
node_Notoungulata <- Notoungulata_mrca-Ntip(tree)
node_Euarchonta <- Euarchonta_mrca-Ntip(tree)
node_Ferae <- Ferae_mrca-Ntip(tree)


## Create a list of nodes (each one will be your node of interest)
node_list <- c(node_Eutharia, node_Placentalia, node_Metratheria, node_Artiodactyla, node_Primates, node_Carnivora, node_Sirenia,
               node_Glires, node_Xenarthra, node_Paenungulata, node_Notoungulata, node_Euarchonta, node_Ferae)

## Loop to get the values
node_values <- list()
for(one_node in 1:length(node_list)) {
  node_values[[one_node]] <- get.node.values(tree$node.comment[[ node_list[one_node] ]])
}
## Naming the elements of the list
names(node_values) <- node_list

## Clever option
names(node_values) <- c("Eutharia", "Placentalia", "Metatheria", "Artiodactyla", "Primates", "Carnivora", "Sirenia", "Glires", "Xenarthra", "Paenungulata", "Notoungulata", "Euarchonta", "Ferae")


node_values_Beck_con <- node_values 
# ... etc

list_node_values <- list(node_values_Beck_98, node_values_Beck_66, node_values_Beck_con)

## Plotting the things in the list


## First lets get the plot window variables
range_nodes <- c(1,length(node_list)+1) # nodes of interest (+ 1 for space on the plot)
range_ages <- c(min(unlist(list_node_values)), max(unlist(list_node_values))) # age ranges
color_vector <- c("red", "blue", "green") # make sure you edit that in length for the number of trees

plot(1,1, col = "white", ylim = range_nodes, xlim = range_ages, xlab = "ages (Mya)", ylab = "nodes", main="Node Ages")


## Adding the lines
for(one_node in 1:length(node_list)) {
    offset <- 0
    ## Loop through the trees
    for(one_tree in 1:length(list_node_values)) {
        ## Plots one line
        lines(c(list_node_values[[one_tree]][[one_node]]$hpd), c(one_node+offset, one_node+offset), col = color_vector[[one_tree]])
        ## Plot one point
        points(list_node_values[[one_tree]][[one_node]]$median, one_node+offset, col = color_vector[[one_tree]], pch = 19,)
        ## Update the offset
        offset <- offset + 0.1
        
    }
}

