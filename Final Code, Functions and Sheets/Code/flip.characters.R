#' @title Flip characters
#'
#' @description Changes a set of character tokens
#'
#' @param character The character to modify
#' @param subset An optional subset of taxa to be affected by the change
#' @param changes A list of changes (from one state to the other)
#' 
#' @examples
#'
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export

flip.characters <- function(character, subset, changes) {

    ## Selecting the character
    char_orig <- character

    ## Selecting the subset to modify
    if(!missing(subset)) {
        char_modif <- char_orig[subset]
    } else {
        char_modif <- char_orig
    }

    ## Check the length of the changes
    if(!all(unlist(lapply(changes, length) == 2))) {
        stop("Changes must be a list of pairs of changes (e.g. c(\"0\", \"1\") )")
    }

    ## Securing change list
    save.change <- function(change) {
        ## Adding a tmp suffix
        change[2] <- paste(change[2], "tmp", sep = "")
        return(change)
    }
    changes <- lapply(changes, save.change)


    ## Changing the characters
    change.char <- function(change, char_modif) {
        char_out <- char_modif
        char_out[char_modif %in% change[1]] <- change[2]
        return(char_out)
    }

    for(one_change in 1:length(changes)) {
       char_modif <- change.char(changes[[one_change]], char_modif) 
    }
    ## Cleaning the tmp
    char_modif <- gsub("tmp", "", char_modif)

    ## Reinjecting the modifications
    if(!missing(subset)) {
        char_orig[subset] <- char_modif
    } else {
        char_orig <- char_modif
    }

    return(char_orig)
}