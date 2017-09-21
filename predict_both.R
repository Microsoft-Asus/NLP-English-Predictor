# Contains functions used by the shinyServer to predict the next word.
# It also loads all required data. It is customized for the English Word 
# Predictor.
# 
# Created on 2017 by Reynaldo Vazquez for the purpose of developing an English 
# language word predictor.
# 
# Permission is granted to copy, use, or modify all or parts of this program and  
# accompanying documents for purposes of research or education, provided this 
# notice and attribution are retained, and changes made are noted.
# 
# This program and accompanying documents are distributed without any warranty,
# expressed or implied. They have not been tested to the degree that would be 
# advisable in important tasks. Any use of this program is entirely at the 
# user's own risk.
# 
################################################################################
################################################################################
# load required data
wcode   <- read.csv("dictionary.csv", stringsAsFactors = FALSE)
pred1   <- read.csv("pred1m.csv")
pred2   <- read.csv("pred2.csv")
pred3   <- read.csv("pred3.csv")
pred4   <- read.csv("pred4.csv")
predC   <- read.csv("pred1c.csv")
chCode  <- read.csv("letter_codes.csv", stringsAsFactors = FALSE)
means   <- read.csv("means.csv", stringsAsFactors = FALSE)
predTables <- c("pred1", "pred2", "pred3", "pred4") # "predC" will be special
################################################################################
################################################################################
p  <- 10  ## is the number of predictions
back_off <- c(4, 9, 10, 44, 56, 2) #(handpicked) use when no predictns are found
# initialize global variables
w_index  <- NULL
xn_glob  <- NULL
cw_index <- NULL
cw_preds <- NULL

preprocess.input <- function(x){
    # Preprocesses text. Removes not recognized objects, excessive space and 
    # converts to low caps.
    x <- tolower(x)
    x <- gsub("[^[:alnum:][:space:]']"," ", x, perl=T, useBytes=T)
    x <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", x, perl=TRUE, useBytes = T)
}
update.index <- function(w_index, temp_idx){
    # Updates index of word predicions, checks for uniqueness and validity
    #
    # Args. w_index: is the "old" existing index
    #       temp_index: is the index just found to be merged at the end
    new_index  <- unique(c(w_index, temp_idx))
    new_index  <- new_index[which(!is.na(new_index))]
}
char.code <- function(letter) {
    # Returns numeric code for character
    #
    # Arg. letter: ONE alnum character. 
    index      <- which(letter == chCode[,2])
    char_code  <- chCode[index, 1]}
word.code <- function(word) {
    # Returns the unique code for a word
    #
    # Arg. word: A string containing English lower-case letters and 0-9
    word       <- unlist(strsplit(word, "")) 
    word_code  <- as.numeric(unlist(sapply(word, char.code)))
    wgt        <- 1:length(word_code) ## gives a letter a different weight given 
    word_code  <- sum(sqrt(word_code/wgt))}                      ## its position
ph.num.code <- function(coded_words_vec, n){
    # Returns the unique numeric value for a phrase
    #
    # Args. phrase: A vector of coded words returned by word.codes.vector()
    #      n: the number of words in the phrase, used for the weigths
    # Only required for phrases with more than 1 word
    wgths   <- c(1:n) + 0.01 ## gives a word different weight given its position
    phNumCode <- coded_words_vec * wgths
    phNumCode <- sum(phNumCode)
}
integer.code <- function(numCode, den = 6, xp = 10, mn) {
    # Converts the numeric value of a phrase into a valid and unique integer. 
    # Done b/c integers require less resources. Phrases are stored as
    # integers in the prediction tables. 
    #
    # Args. numCode: the numeric code calculated by ph.num.code()
    #       den, xp: are hand picked parameters, they differ only for the compl-
    #                ementary predictions. 
    #       mn: is the index of the the corresponding mean
    mean_n   <- means[mn,1]
    procCode <- (numCode-mean_n)/mean_n
    procCode <- procCode*(10^(xp))/den
    procCode <- round(procCode)
    intCode  <- suppressWarnings(as.integer(procCode))
}
pred.index   <- function(xn, predn, n, den = 6, xp = 10, lc = 6, mn) {
    # Finds the word indexes for the predictions
    if (n > 1){
        phNumCode <- ph.num.code(xn, n)
    } else {phNumCode <- xn}
    intCode <- integer.code(phNumCode, den = den, xp = xp, mn = mn)
    index   <- which(predn[,1] == intCode)
    w_index <- unlist(predn[index, 2:lc])
}
scan.predictions <- function(xn, n, length_preds, w_index){
    # Scans through prediciton tables finding the most possible predictions up 
    # to "p", given the parameters.
    while (length_preds < p & n > 0) {
        predn    <- get(predTables[n])
        temp_idx <- pred.index(xn = xn, predn = predn, n = n, mn = n)
        w_index  <- update.index(w_index, temp_idx)
        length_preds <- length(w_index)
        if (n == 1 & length_preds < p){
            temp_idx <- pred.index(xn = xn, predn = predC, n = n, 
                                   xp = 9, den = 2, mn = 5, lc = 3)
            w_index  <- update.index(w_index, temp_idx)
        }
        n  <- n -1
        xn <- xn[-1]
    }
    return(w_index)
}
predict.both <- function(x){
    ## Provides: Suggestions to complete, or correct, the word being typed and
    ## Predicts next word for x. Formerly two separate fucntion, thus the name. 
    ## 
    ## Arg. x: A phrase of one or more words
    ## 
    ## To predict current word, it separates the last set of chars. 
    ## If there are more than 1 set of chars. It uses all the previous chars to 
    ## the last set to predict what are the words most likely to follow. Then it 
    ## looks for matches among those predicted words, then in a prob ranked
    ## dictionary.
    phrase      <- preprocess.input(x) ## removes xs space and symbols excpt '
    words_orig  <- unlist(strsplit(phrase, " "))
    wco         <- length(words_orig)
    words  <- gsub("'", "", words_orig, perl=T, useBytes = T)
    if (wco > 3) {words <- words[(wco-3):wco]}
    xs  <- sapply(words, word.code)
    xn  <- xs
    wc  <- length(xs)
    n   <- wc
    length_preds <- length(w_index)
    w_index      <- scan.predictions(xn, n, length_preds, w_index)
    length_preds <- length(w_index)
    ## Next: retrive matches for the current word in the dictionary
    grepEn  <- words_orig[wco]
    grepEn  <- paste0("^", grepEn)
    current <- grep(grepEn, wcode$word[1:12000], perl = TRUE, 
                    ignore.case = TRUE, value = TRUE)
    ## The following should be done, regardless of length_preds, so it can be 
    ## used by the current predictor. It is also used as a back-off method for 
    ## the next word
    if (wco > 1 & identical(xn_glob, xs[-wc]) == FALSE){
        xn_glob  <<- xs[-wc]  ## ignore last word. Make a global var so that it 
                              ## won't need to be recalculated over and over
        cw_index <<- scan.predictions(xn = xn_glob, n = (wc-1), 
                                      length_preds = 0, w_index = NULL) 
        ## pretend there's no predictions so that it makes a full search.
        ## later on, can adjust p as a parameter so that it does more searches
        cw_preds <<- wcode[cw_index,]
    }
    if (wco > 1){
        w_index      <- update.index(w_index, cw_index)
        length_preds <- length(w_index)
        ## next two lines are for the current word preds
        toAttach <- grep(grepEn, cw_preds, perl=TRUE, ignore.case = TRUE, 
                          value = TRUE)
        current <- update.index(toAttach, current)    ## not an idx but fn works
    }
    if (length(current) < 4) {
        current <- regex.searches(grepEn, current)
    }
    current <- current[!is.na(current)]
    dup <- which(duplicated(tolower(current)))
    if (length(dup) > 0){current <- current[-dup]} ## this ends search for currt
    if (length_preds < p){ ## rm "s" (plurals) at end of 4 letter or longer wrds
        srem <- gsub("(?<=[[:alnum:]]{4})s", "", words, perl=TRUE,useBytes = T)
        if (identical(srem, words) == FALSE){
            xn <- sapply(srem, word.code)
            n  <- wc
            w_index <- scan.predictions(xn, n, length_preds, w_index)
            length_preds <- length(w_index)
        }
        if (length_preds < p){
            w_index  <- update.index(w_index, back_off)
            length_preds <- length(w_index)
        }
    }
    rand_index  <- c(sample(1:2, prob = c(.6, .4), replace = FALSE), 
                     c(3:length_preds))
    w_index <- w_index[rand_index]
    predictions <- wcode[w_index,]
    dup <- which(duplicated(c(tolower(predictions), words[wco]), fromLast=TRUE))
    if (length(dup) > 0){predictions <- predictions[-dup]}
    ## this ends the search for next-word predictions
    outputLists <- list("predictions" = predictions, "current" = current)
    return(outputLists)
    }
regex.searches <- function(grepEn, current){
    # Makes regex transformations and searches for new matches in the prediction
    # dictionary.
    # 
    # Args: grepEn: an "unfinised" word, current: the vector of matches so far
    grepEn <- gsub("h", "h*", grepEn, perl=TRUE)   ## mark "h" as error suspect
    grepEn <- gsub("'", "'*", grepEn, perl=TRUE)   ## mark ' as error suspect
    grepEn <- gsub("(?=[^h*^])", "h*", grepEn, perl =TRUE ) ## add an h* before
                                                   ## any char except h and *
    grepEn <- gsub("$", "h*", grepEn, perl =TRUE ) ## add an h* at the end
    grepEn <- gsub("[ck](?=[aou])", "[ck]", grepEn, perl =TRUE ) ## consider c 
                                ## and k for any c or k followed by a, o, or u
    grepEn <- gsub("[csz](?=[eih])", "[csz]", grepEn, perl =TRUE ) ## consider
                           ## c, s and z for any c, s, or z followed by e, i, h
    grepEn <- gsub("[sz](?=[aou])", "[sz]", grepEn, perl =TRUE ) ## consider
                           ## c, s and z for any c, s, or z followed by e or i
    current <- unique(c(current, grep(grepEn, wcode$word, perl = TRUE, 
                                      ignore.case = TRUE, value = TRUE)))
    if (length(current) < 4) {
        grepEn <- gsub("(?=[^'*^\\]cksz])", "'*", grepEn, perl =TRUE )
                    ## add a ' before any char except ', *, and other exceptions
        grepEn <- gsub("(?=[sk][^\\]z])", "'*", grepEn, perl=TRUE)#completes the
                                                            ## above for k and s
        grepEn <- gsub("[bv]", "[bv]", grepEn, perl = TRUE )## consider v<->b
        grepEn <- gsub("[iy]", "[iy]", grepEn, perl = TRUE )## consider i<->y
        current <- unique(c(current, grep(grepEn, wcode$word, perl = TRUE, 
                                          ignore.case = TRUE, value = TRUE)))
    }
    return(current)
}
################################################################################
replace.last <- function(phrase, lastword){
    # It's called when a user selects a current word option. It completes or
    # substitutes the word being typed. It requires the entire phrase and the 
    # word which has been selected.
    # 
    words  <- unlist(strsplit(phrase, " "))
    toRemove <- length(words)
    if (toRemove > 1 & lastword != ""){
        words <- words[-toRemove]
        newPhrase <- paste(paste(words, collapse = " "), lastword)
    } else if (toRemove > 0 & lastword != "") {
        newPhrase <- lastword
    } else {newPhrase <- phrase}
    return(newPhrase)
}