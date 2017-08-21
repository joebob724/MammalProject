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

## Reading the tree (make sure which one you want to read)
tree <- read_annotated("Topologies/original_tree_files/2014-Beck-ProcB-TEM.tre")

## Extracting clades
mrca_table <- mrca(tree) #This gives a table of nodes mrca

## Example we want the primates MRCA, (shortest tree including Notharctus and Purgatorius)
primates_mrca <- mrca_table[which(rownames(mrca_table) == "Notharctus"), which(colnames(mrca_table) == "Purgatorius")]

## This also include the tips numbers, in this case, primates_mrca = 175 but it is node number 73 (175 - Ntip(tree))

## Selecting the node (WARNING maybe this is not the actual node position in the comments, make sure it works on couple of nodes/trees)
my_node <- primates_mrca-Ntip(tree)


## Create a list of nodes (each one will be your node of interest)
my_node_list <- c(my_node, 1, 89)


## Loop to get the values
my_values <- list()
for(one_node in 1:length(my_node_list)) {
    my_values[[one_node]] <- get.node.values(tree$node.comment[[ my_node_list[one_node] ]])
}
## Naming the elements of the list
names(my_values) <- my_node_list

## Clever option
names(my_values) <- c("primates", "rodents", "carnivores")


my_values_beck <- my_values
my_values_hallidays <- my_values
# ... etc

list_of_values <- list(my_values_beck, my_values_hallidays)

## Plotting the things in the list


## First lets get the plot window variables
range_nodes <- c(1,length(my_node_list)+1) # nodes of interest (+ 1 if for space)
range_ages <- c(min(unlist(list_of_values)), max(unlist(list_of_values))) # age ranges
color_vector <- c("red", "blue") # make sure you edit that in length for the number of trees

plot(1,1, col = "white", ylim = range_nodes, xlim = range_ages, xlab = "ages (Mya)", ylab = "nodes")

## Adding the lines
for(one_node in 1:length(my_node_list)) {
    offset <- 0
    ## Loop through the trees
    for(one_tree in 1:length(list_of_values)) {
        ## Plots one line
        lines(c(list_of_values[[one_tree]][[one_node]]$hpd), c(one_node+offset, one_node+offset), col = color_vector[[one_tree]])
        ## Plot one point
        points(list_of_values[[one_tree]][[one_node]]$median, one_node+offset, col = color_vector[[one_tree]], pch = 19)
        ## Update the offset
        offset <- offset + 0.1
    }
}
