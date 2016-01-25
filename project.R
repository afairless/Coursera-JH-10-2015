# October - November 2015; updated January 2016
# This script was written for Johns Hopkins University Coursera "Capstone" course 

#--------------------------
library(jsonlite)

# download data from:  http://www.yelp.com/dataset_challenge
# data assumed to be in directory 'yelp_dataset_challenge_academic_dataset',
# which is inside the current working directory

dirname <- "yelp_dataset_challenge_academic_dataset"
filestem <- "yelp_academic_dataset_"
filenamepart <- c("business", "checkin", "review", "tip", "user")
fileext <- ".json"
alldata <- list()

for (iter in 1:length(filenamepart)) {
     filename = paste(dirname, "/", filestem, filenamepart[iter], fileext, sep = "")
     dataframe = filenamepart[iter]
     alldata[[iter]] <- fromJSON(sprintf("[%s]", paste(readLines(filename), collapse = ",")))
     print(filename)
     print(dataframe)
}
     
names(alldata) <- filenamepart


#--------------------------
# identify rows of entries pertaining to doctors or hospitals, i.e., "mainstream" medicine
# including "Health" here does not change number of physicians or hospitals included; it
# does increase numbers of "Counseling & Mental Health", "Diagnostic Imaging", "Diagnostic
# Services", and "Optometrists".  It's debatable whether they should be included.
medrows = unique(c(grep("Doctor", alldata$business$categories), 
                   grep("Hospital", alldata$business$categories),
                   grep("Allergist", alldata$business$categories),
                   grep("Anesthesiologist", alldata$business$categories),
                   grep("Cardiologist", alldata$business$categories),
                   grep("Surgeon", alldata$business$categories),
                   grep("Dentist", alldata$business$categories),
                   grep("Drugstore", alldata$business$categories),
                   grep("Ear Nose & Throat", alldata$business$categories),
                   grep("Endodontist", alldata$business$categories),
                   grep("Internal Medicine", alldata$business$categories),
                   grep("Laser Eye Surgery/Lasik", alldata$business$categories),
                   grep("Obstetrician", alldata$business$categories),
                   grep("Gastroenterologist", alldata$business$categories),
                   grep("Gynecologist", alldata$business$categories),
                   grep("Ophthalmologist", alldata$business$categories),
                   grep("Oncologist", alldata$business$categories),
                   grep("Orthodontist", alldata$business$categories),
                   grep("Orthopedist", alldata$business$categories),
                   grep("Orthotic", alldata$business$categories),
                   grep("Pediatric", alldata$business$categories),
                   grep("Periodontist", alldata$business$categories),
                   grep("Pharmacy", alldata$business$categories),
                   grep("Podiatrist", alldata$business$categories),
                   grep("Psychiatrist", alldata$business$categories),
                   grep("Pulmonologist", alldata$business$categories),
                   grep("Radiologist", alldata$business$categories),
                   grep("Rheumatologist", alldata$business$categories),
                   grep("Urologist", alldata$business$categories),
                   grep("Medical Center", alldata$business$categories)))
# length(medrows)
# [1] 2632

# remove rows of "alternative" medicine
rmaltmedrows = unique(c(grep("Acupuncture", alldata$business$categories[medrows]),
                        grep("Massage", alldata$business$categories[medrows]),
                        grep("Naturopath", alldata$business$categories[medrows]),
                        grep("Psychic", alldata$business$categories[medrows]),
                        grep("Yoga", alldata$business$categories[medrows]),
                        grep("Spas", alldata$business$categories[medrows]),
                        grep("Food", alldata$business$categories[medrows]),
                        grep("Fitness", alldata$business$categories[medrows]),
                        grep("Osteopath", alldata$business$categories[medrows]),
                        grep("Chinese Medicine", alldata$business$categories[medrows]),
                        grep("Shopping", alldata$business$categories[medrows]),
                        grep("Chiropractor", alldata$business$categories[medrows]),
                        grep("Cannabis", alldata$business$categories[medrows]),
                        grep("Reflexology", alldata$business$categories[medrows]),
                        grep("Rolfing", alldata$business$categories[medrows]),
                        grep("Coach", alldata$business$categories[medrows]),
                        grep("Reiki", alldata$business$categories[medrows])))
medrows = medrows[-rmaltmedrows]
# length(rmaltmedrows)
# [1] 753
# length(medrows)
# [1] 1879

#--------------------------
# identify rows of entries clearly pertaining to "alternative" medicine
# includes only words that are clearly alternative medicine and excludes words that
# might be compatible with both mainstream and alternative medicine (e.g., "yoga" 
# could be considered exercise and thus compatible with mainstream medicine).
altmedrows = unique(c(grep("Acupuncture", alldata$business$categories), 
                      grep("Chiropractor", alldata$business$categories),
                      grep("Chinese Medicine", alldata$business$categories),
                      grep("Reflexology", alldata$business$categories),
                      grep("Reiki", alldata$business$categories),
                      grep("Osteopath", alldata$business$categories),
                      grep("Rolfing", alldata$business$categories),
                      grep("Naturopathic", alldata$business$categories)))
# length(altmedrows)
# [1] 378


# remove rows of "mainstream" medicine
rmmedrows = unique(c(grep("Dermatologists", alldata$business$categories[altmedrows]),
                     grep("Neurologist", alldata$business$categories[altmedrows]),
                     grep("Obstetrician", alldata$business$categories[altmedrows]),
                     grep("Gynecologist", alldata$business$categories[altmedrows]),
                     grep("Orthopedist", alldata$business$categories[altmedrows]),
                     grep("Allergist", alldata$business$categories[altmedrows]),
                     grep("Internal Medicine", alldata$business$categories[altmedrows])))
altmedrows = altmedrows[-rmmedrows]
# length(rmmedrows)
# [1] 8
# length(altmedrows)
# [1] 370



#--------------------------
# this section assesses what business categories are included in each group

# number of clearly mainstream medicine businesses (after eliminating businesses associated with alternative medicine words)
length(medrows)
# [1] 1879
# number of reviews of clearly mainstream medicine businesses
sum(alldata$business$review_count[medrows])
# [1] 14250
# business categories associated with clearly mainstream medicine businesses
table(unlist(alldata$business$categories[medrows]))
# mednames = as.data.frame(table(unlist(alldata$business$categories[medrows])))[ , 1]

# number of clearly alternative medicine businesses (after eliminating businesses associated with mainstream medicine words)
length(altmedrows)
# [1] 370
# number of reviews of clearly alternative medicine businesses
sum(alldata$business$review_count[altmedrows])
# [1] 3136
# business categories associated with clearly alternative medicine businesses
table(unlist(alldata$business$categories[altmedrows]))


#--------------------------
# get row indices for reviews of mainstream medicine businesses
medrevrows = NA
for (iter in 1:length(medrows)) {
     medrevrows = c(medrevrows, which(alldata$review$business_id == alldata$business$business_id[medrows[iter]]))
}
medrevrows = medrevrows[-1]

# get row indices for reviews of alternative medicine businesses
altmedrevrows = NA
for (iter in 1:length(altmedrows)) {
     altmedrevrows = c(altmedrevrows, which(alldata$review$business_id == alldata$business$business_id[altmedrows[iter]]))
}
altmedrevrows = altmedrevrows[-1]

length(medrevrows)
# [1] 12957
length(altmedrevrows)
# [1] 2883
# matching business IDs to get reviews (here) and summing business review counts (above)
# don't give same number of reviews:  mainstream medicine - 14250 above vs. 12957 here;
# alternative medicine - 3136 above vs. 2883 here

# create data frame of only mainstream and alternative medicine reviews
medrevs = alldata$review[medrevrows, c(1:4, 6, 8)]
altmedrevs = alldata$review[altmedrevrows, c(1:4, 6, 8)]

medrevs[ , "medicine"] = "conventional"
altmedrevs[ , "medicine"] = "alternative"

revs = rbind(medrevs, altmedrevs)
saveRDS(revs, file = "revs.rds")

#--------------------------
# now working in different R global environment (Linux instead of Windows)
# save revs to RDS then read:
revs <- readRDS("revs.rds")
# re-arrange data frame of reviews
revs <- cbind(revs$business_id, revs[ , 2:3], revs$medicine, revs$stars, revs$votes, revs$text, stringsAsFactors = F)
colnames(revs) <- gsub("revs\\$", "", colnames(revs))
revs$medicine <- as.factor(revs$medicine)
revs$business_id <- as.factor(revs$business_id)
revs <- revs[order(revs$review_id), ]


#--------------------------
# remove problematic reviews from data frame
library(qdap)

# according to 'Encoding' and 'check_text' functions, 76 reviews are in UTF-8,
# which can't be read by 'word_stats' function; 'iconv' couldn't convert UTF-8
# to ASCII, so the 76 UTF-8 rows are being deleted
utfrows <- which(Encoding(revs$text[1:15840]) == "UTF-8")
revs <- revs[-utfrows, ]

# remove texts not written in English
# create data frame with misspelling rate for each review
misspellnumbs <- table(check_spelling(revs$text, assume.first.correct = FALSE)$row)
misspells <- cbind(as.integer(rownames(misspellnumbs)), as.integer(misspellnumbs))
colnames(misspells) <- c("textrownumber", "misspellingsnumber")
# 'missingrows': not returned by check_spelling function; indicates no misspellings
missingrows <- setdiff(1:dim(revs)[1], misspells[ , 1])
nomisspells <- cbind(missingrows, rep(0, length(missingrows)))   # reviews with no misspellings
misspells <- as.data.frame(rbind(misspells, nomisspells))        # all reviews combined  
misspells <- misspells[order(misspells[ , 1]), ]  
rownames(misspells) <- 1:dim(misspells)[1]
misspells$totalwords <- word_count(revs$text)
misspells$errorrate <- misspells[ , 2] / misspells [ , 3]        # misspellings per word
# plot(misspells[ , 4])    # shows several rows with high misspelling rate
# identify reviews with misspelling rates > 50%
hierrorrownumbers <- misspells[misspells[ , 4] > 0.5, 1]
hierrorrownumbers <- hierrorrownumbers[!is.na(hierrorrownumbers)]
# revs$text[hierrorrownumbers]
foreigntexts <- c(1, 3)  # visual inspection shows that these reviews are not in English
# revs$text[hierrorrownumbers[foreigntexts]]
# revs[hierrorrownumbers[foreigntexts], ]
# these are the row indices of the foreign-language reviews to delete from 'revs'
nonenglish <- hierrorrownumbers[foreigntexts]
revs <- revs[-nonenglish, ]

# row 4364:  error in 'formality' function; it says it needs a 2-D array; haven't solved
problemrows <- c(4364)
revs <- revs[-problemrows, ]


#--------------------------
# prepare text of reviews for analysis

revs$text = add_incomplete(revs$text)
revs$text = incomplete_replace(revs$text)


saveRDS(revs, file = "revsculledprepped.rds")

#--------------------------
# add analytical metrics/scores of reviews to reviews data frame

revs <- readRDS("revsculledprepped.rds")

# This function detects whether a sequence of a vector's elements repeats.  
# If they do, it eliminates the duplicates/repetitions so that only one 
# sequence remains.

# The sequence can extend over any number of vector elements.  There can be
# any number of duplicates in the vector; all the duplicates will be eliminated.

# The limitation of this function is that it checks for only 1 duplication.
# If it finds 1 duplicate and the overall length of the vector is a multiple
# of the length of the sequence, then it assumes any additional elements 
# following the first duplicate are also duplicated sequences.

removerepeats <- function(vectora) {
     # vectors must have at least 2 elements to continue through the function
     if (length(vectora) < 2) {return(vectora)}
     
     # checks whether the vector's 1st element is duplicated
     firstrepeat <- match(vectora[1], vectora[-1], nomatch = 1)
     # if not, function exits
     if (firstrepeat == 1) {
          return(vectora)
          # if so, 'firstrepeat' is the vector index of that duplicate
          # thus, 'firstrepeat - 1' is the length of the possible duplication sequence
     } else if (firstrepeat > 1) {
          firstrepeat <- firstrepeat + 1
     }
     
     # vector length must be a multiple of the sequence length to continue
     # through function
     lengthremainder <- length(vectora) %% (firstrepeat - 1)
     if (lengthremainder != 0) {
          return(vectora)
     } 
     
     # to be a duplicate, each element of the 2nd sequence must match the 
     # corresponding element of the 1st sequence
     repeattest = rep(FALSE, (firstrepeat - 1))
     for (iter in 1:(firstrepeat - 1)) {
          repeattest[iter] <- (vectora[iter] == vectora[firstrepeat + iter - 1])
     }
     
     # if any element between the 2 sequences mismatched, exit function
     if (FALSE %in% repeattest) {
          return(vectora)
          # otherwise, the 2nd sequence is a duplicate, and the vector is modified
          # so that only the 1st sequence is retained
     } else {
          vectora <- vectora[1:(firstrepeat - 1)]
          return(vectora)
     }
     
}

# 'n.imper' and 'n.incom' might be in wrong order in 'wordstatscolnames' below
# 'p.imper' and 'p.incom' might be in wrong order in 'wordstatscolnames' below
wordstatscolnames = c(# from word_stats function
     "all", "n.sent", "n.words", "n.char", "n.syl", "n.poly", 
     "wps", "cps", "sps", "psps", "cpw", "spw", "pspw", 
     "n.state", "n.quest", "n.exclm", "n.imper", "n.incom", 
     "p.state", "p.quest", "p.exclm", "p.imper", "p.incom", 
     "n.hapax", "n.dis", "grow.rate", "prop.dis",
     # from 'pronoun_type' function
     "pronoun.word.count", "I", "we", "you", "he", "she", 
     "they", "it", "me", "us", "him", "her", "them", "their",
     # from 'polarity' function
     "total.sentences", "total.words", "ave.polarity",
     "sd.polarity", "stan.mean.polarity",
     # from 'automated_readability_index' function
     "read.word.count", "sentence.count", "character.count",
     "Automated_Readability_Index",
     # from 'diversity' function
     "wc", "simpson", "shannon", "collision", 
     "berger_parker", "brillouin",
     # from 'lexical_classification' function
     "lexical.word.count", "ave.content.rate", "SE", "n.content",
     "n.functional", # "content", "functional",
     # from 'formality' function
     "formality.word.count", "formality") 
# number of columns following 'prop.dis' (last element of 'word_stats' function)
wstatsendcol = which(wordstatscolnames == "prop.dis")
afterwstatscolnum = length(wordstatscolnames) - wstatsendcol

temparray = rep(NA, length(wordstatscolnames))
temparray = as.data.frame(t(temparray))
colnames(temparray) = wordstatscolnames

# starttime = proc.time()
for (iter in 1:dim(revs)[1]) {
     #      print(paste(iter, "flag1"))
     otherstats = rep(NA, afterwstatscolnum)         # initialize 'otherstats', which contains results from functions other than 'wordstats'
     otherstats = as.data.frame(t(otherstats))
     if (grepl("[a-zA-Z]", revs$text[iter]) == FALSE) {          # if there is no text (i.e., no alphabetic characters) in the review
          wordstats = rep(NA, length(wordstatscolnames))         # create 'wordstats' with 'NA's for all values
          wordstats = as.data.frame(t(wordstats))
          colnames(wordstats) = wordstatscolnames
     } else {                                                    # else create 'wordstats' with 'word_stats' function calculations
          tempsplit = sentSplit(revs[iter, ], "text")
          adjrownum = length(removerepeats(tempsplit$text))
          tempsplit = tempsplit[1:adjrownum, ]
          otherstats[1:14] = pronoun_type(tempsplit$text)$prop[ , -1]
          otherstats[15:19] = polarity(tempsplit$text)$group[ , -1]
          otherstats[20:23] = automated_readability_index(tempsplit$text)$Readability[ , -1]
          otherstats[24:29] = diversity(tempsplit$text)[ , -1]
          # 'lexical_classification' and 'formality' produce errors if there's
          # only one word of text in the review, so they are skipped in this case
          if (otherstats[1] > 1) {
               otherstats[30:34] = lexical_classification(tempsplit$text)$lexical_classification[ , 2:6]
               dummy <- capture.output(otherstats[35:36] <- formality(tempsplit$text)$formality[ , -1])   # hacky way of preventing 'formality' from printing output
          } else {
               otherstats[30:36] = NA
          }
          wordstats = word_stats(tempsplit$text)$gts
          
          if (colnames(wordstats)[dim(wordstats)[2]] != wordstatscolnames[wstatsendcol]) {  
               wordstats[dim(wordstats)[2] + 1] = NA
               colnames(wordstats)[dim(wordstats)[2]] = wordstatscolnames[wstatsendcol]
          }
          # if the last column name of 'wordstats' is not "prop.dis" (i.e., the last element in 'wordstatscolnames'),
          # the 'if' condition in the 'for' loop below will produce an error; the 'if' statement above avoids this
          
          # if a column in 'wordstats' is not present, insert column with 'NA' as value
          for (iter2 in 1:wstatsendcol) {
               if (colnames(wordstats)[iter2] != wordstatscolnames[iter2]) {
                    wordstats = data.frame(wordstats[1:(iter2 - 1)], NA, wordstats[iter2:dim(wordstats)[2]])
               }
          }
     }
     wordstats[28:63] = otherstats
     colnames(wordstats) = wordstatscolnames
     wordstats$all = as.character(wordstats$all)  # convert 'factor' to 'character' so 'review_id' can be added (below)
     wordstats$all = revs$review_id[iter]
     temparray = rbind.data.frame(temparray, wordstats)
     if (iter %% 100 == 0) {print(iter)}
}
# proc.time() - starttime
temparray = temparray[-1, ]
rownames(temparray) = seq(from = 1, to = dim(temparray)[1])
# table(revs$review_id == temparray$all)    # checks that rows match between 'revs' and 'temparrayall'
revsall = cbind(revs, temparray)
saveRDS(revsall, file = "revsall.rds")


#--------------------------
# add word frequencies to reviews data frame

library(tm)

revsall <- readRDS("revsall.rds")

# takes a vector of type 'character' (i.e., strings) and returns a vector of 
# numbers of the words/tokens contained within original vector
corpfreqprep <- function(vectoroftexts, removepunct = TRUE) {
     corpus <- Corpus(VectorSource(vectoroftexts))
     corpus <- tm_map(corpus, content_transformer(removeNumbers))
     corpus <- tm_map(corpus, content_transformer(stripWhitespace))
     if (removepunct) corpus <- tm_map(corpus, content_transformer(removePunctuation))
     corpfreq <- colSums(as.matrix(DocumentTermMatrix(corpus)))
     corpfreq <- sort(corpfreq, decreasing = TRUE)
     return(corpfreq)
}

# frequencies of all words used across all reviews (not adjusted for length of reviews)
alltextsfreq <- corpfreqprep(revsall$text, removepunct = TRUE)

# remove words (mostly proper nouns) that might not generalize well to other data sets
# 331 vegas, 393 yelp, 543 phoenix, 561 las, 660 scottsdale, 753 groupon, 1047 arizona
# 1051 mayo, 1209 chandler, 1217 summerlin, 1331 john, 1333 nevada, 1385 miller
# 1426 gilbert, 1317 north, 1407 west
wordstoremove <- c(331, 393, 543, 561, 660, 753, 1047, 1051, 1209, 1217, 1317, 1331, 
                 1333, 1385, 1407, 1426)
alltextsfreq <- alltextsfreq[-wordstoremove]

# selects words that appear at least 100 times across all reviews
# using 100 as cut-off is somewhat arbitrary
commonwordsnum <- match(99, alltextsfreq) - 1
commonwords <- names(alltextsfreq)[1:commonwordsnum]

# add columns to data frame for each common word's frequency in each review
revsallcolnum <- dim(revsall)[2]
revsall[(revsallcolnum + 1):(revsallcolnum + commonwordsnum)] <- NA
colnames(revsall)[(revsallcolnum + 1):(revsallcolnum + commonwordsnum)] <- commonwords

# for each row in 'revsall', i.e., for each review
for (iter in 1:dim(revsall)[1]) {
     if (iter %% 100 == 0) {print(iter)}
     tempfreq <- corpfreqprep(revsall$text[iter], removepunct = TRUE)
     # if 'corpfreqprep' returns 'logical(0)' to 'tempfreq', assigning "" to 'tempfreq'
     # avoid subsequent error
     # this error occurs 8 times in the data set at these row indices in 'revsall':
     # 1711, 1954, 6760, 8069, 8417, 8486, 9783, 10902
     # the review texts at these row indices are:  "Ok|", "|", ".", "|", "c|", "A|", "NT|", ":)|"
     # these errors do not affect the totals for the most common words, so the
     # correction below is adequate
     if (length(tempfreq) == 0) {
          tempfreq <- 0
          names(tempfreq) <- ""
     }
     # for each word counted in a review
     for (iter2 in 1:length(tempfreq)) {
          # if the word counted in the review is in the common words
          if (names(tempfreq)[iter2] %in% commonwords) {
               wordcoltemp <- which(colnames(revsall)[(revsallcolnum + 1):dim(revsall)[2]] == names(tempfreq)[iter2])
               wordcol <- wordcoltemp + revsallcolnum
               # save the frequency of the word as a proportion of the number of words in the review
               revsall[iter, wordcol] <- tempfreq[iter2] / revsall$n.words[iter]
          }
     }
}

saveRDS(revsall, file = "revsallwordfreqs.rds")

#--------------------------
# modify variables and remove unneeded variables

expdata <- readRDS("revsallwordfreqs.rds")

# checks word frequencies in 'expdata' against word frequencies in 'alltextsfreq'
# to make sure data were added to 'revsall'/'expdata' correctly
# startcol <- 73
# stopcol <- dim(expdata)[2]
# coltotals <- rep(NA, length(startcol:stopcol))
# nwordscolnum <- 12
# for (colnum in startcol:stopcol){
#      rowwordtotal <- 0
#      for (iter in 1:dim(expdata)[1]) {
#           if (!is.na(expdata[iter, colnum])) {
#                rowwordnum <- expdata[iter, colnum] * expdata[iter, nwordscolnum]
#                rowwordtotal <- rowwordtotal + rowwordnum
#           }
#      }
#      coltotals[colnum - (startcol - 1)] <- rowwordtotal
# }
# table(coltotals == alltextsfreq[1:commonwordsnum])     # all 1462 should be TRUE; should be no FALSEs

# 'ave.polarity' is the total polarity divided by the number of sentences, but
# the sentence number counts are unreliable.  Therefore, 'ave.polarity' will be
# transformed to be polarity per word (instead of "polarity per sentence").
# plot(abs(revsall$n.words - revsall$total.words)/mean(c(revsall$n.words, revsall$total.words), na.rm=T))
expdata$ave.polarity = (expdata$ave.polarity * expdata$total.sentences) / expdata$total.words

# replace 'NA's with zeroes
expdata[is.na(expdata)] <- 0

# word counts for different functions don't completely agree
# columns of word counts:  12, 37, 52, 56, 60, 66
# cor(revsall[,c(12, 37, 52, 56, 60, 66)], use = "pairwise.complete.obs")
# correlation matrix and graphs show the agreement is very strong -- strong enough to delete extra columns
# table(revsall$n.words == revsall$total.words)

# delete: 37, 52, 56, 60, 66: columns of word counts
# delete 9, review text
# delete column 10, duplicate of 'review_id'
# delete 11 'n.sent':  number of sentences is often incorrect
# delete 16:19:  all 'per sentence' stats, which are often incorrect
# delete 28:32:  proportions of all sentences, which are often incorrect
# delete 33:36:  unsure how unique words would be useful; deleting
# delete 54:55:  polarity standard deviation is by sentence count, which is often incorrect
# delete 56:59:  readability relies on sentence counts, which are often incorrect; can I calculate something useful from this?
# delete 63, 65:  shannon, collision, brillouin correlate very highly; keep shannon
# delete 66, 68:70:  'ave.content.rate' (col 67) is good (expressed as %); other cols unnecessary
# delete 71:  'formality' (col 72) is good (expressed as %); 71 is unnecessary
cullcols = c(9:11, 16:19, 28:32, 33:37, 51:52, 54:60, 63, 65:66, 68:71)
expdata = expdata[ , -cullcols]


#--------------------------
# which word frequencies show largest difference between 'conventional' and 'alternative' medicine?
# commonly used words with large differences might be particularly good at
# discriminating between conventional and alternative medicine reviews

options(scipen = 999)    # avoids scientific notation when displaying results

startcol <- 40
relativediffs <- rep(NA, length(startcol:dim(expdata)[2]))
relativediffs <- cbind.data.frame(relativediffs, relativediffs)

# calculate word usage rate for reviews of conventional and alternative medicine
# rate includes all reviews for either kind of medicine, not only reviews where the word appears
for (varcol in startcol:dim(expdata)[2]) {
     reldiffsrow <- varcol - (startcol - 1)
     conmedcol <- expdata[expdata$medicine == "conventional", varcol]
     altmedcol <- expdata[expdata$medicine == "alternative", varcol]
     relativediffs[reldiffsrow, 1] <- mean(conmedcol)
     relativediffs[reldiffsrow, 2] <- mean(altmedcol)
}

nwordscol <- which(colnames(expdata) == "n.words")
medicinecol <- which(colnames(expdata) == "medicine")
# word frequencies for each word
wordfreqs <- expdata[ , 40:dim(expdata)[2]] * expdata[ , nwordscol]             
# word frequencies for each word for reviews of conventional medicine
conmedwordfreqs <- wordfreqs[expdata[ , medicinecol] == "conventional", ]       
# word frequencies for each word for reviews of alternative medicine
altmedwordfreqs <- wordfreqs[expdata[ , medicinecol] == "alternative", ]        
# ratio of number of conventional medicine to alternative medicine reviews
contoaltratio <- table(expdata$medicine)[[2]] / table(expdata$medicine)[[1]]    

# difference in word usage rate as proportion of conventional medicine rate
relativediffs[ , 3] <- (abs(relativediffs[ , 1] - relativediffs[ , 2]) / relativediffs[ , 1])  
relativediffs[ , 4] <- colSums(wordfreqs)
relativediffs[ , 5] <- colSums(conmedwordfreqs)
relativediffs[ , 6] <- colSums(altmedwordfreqs)
relativediffs[ , 7] <- relativediffs[ , 5] + (relativediffs[ , 6] * contoaltratio)
# index of how well word's usage rate may discriminate between conventional and
# alternative medicine reviews; accounts for difference in word's usage rates
# between conventional and alternative medicine and how frequently word is used
relativediffs[ , 8] <- relativediffs[ , 3] * relativediffs[ , 4]
# same index as above, but word frequencies for alternative medicine reviews
# have been overweighted to equal frequencies for conventional medicine review
# this should optimize word selection for training data, which will likewise
# overweight alternative medicine reviews
relativediffs[ , 9] <- relativediffs[ , 3] * relativediffs[ , 8]
relativediffs[ , 10] <- startcol:dim(expdata)[2]

colnames(relativediffs) <- c("conmedmean", "altmedmean", "reldiff", "totalfreq", "confreq", 
                             "altfreq", "totalwfreq", "diffxfreq", "diffxwfreq", "colindex")
rownames(relativediffs) <- colnames(expdata)[startcol:dim(expdata)[2]]
rownames(relativediffs) <- gsub(".1", "", rownames(relativediffs))


# plot relative differences to determine which words might be best discriminators
sortreldiffs <- relativediffs[order(relativediffs[ , 3], decreasing = TRUE), ]
sortdifffreq <- relativediffs[order(relativediffs[ , 8], decreasing = TRUE), ]
sortdiffwfreq <- relativediffs[order(relativediffs[ , 9], decreasing = TRUE), ]
# plot(sort(log(relativediffs[ , 3]), decreasing = TRUE))
# abline(v = 70)
# abline(v = 160)
# plot(sort(log(relativediffs[ , 8]), decreasing = TRUE))
# abline(v = 60)
# plot(sort(log(relativediffs[ , 9]), decreasing = TRUE))
# abline(v = 40)
# abline(v = 80)


#--------------------------
# splitting data into training (60% of data) and testing (40%) sets, stratified
# by 'medicine' ('conventional' or 'alternative') and star ratings (1 - 5)

var1 = levels(expdata$medicine)
var2 = sort(unique(expdata$stars))
testrows = NA
set.seed(43317)

for (iter1 in 1:length(var1)) {
     for (iter2 in 1:length(var2)) {
          set = which(expdata$medicine == var1[iter1] & expdata$stars == var2[iter2])
          sampledrows = sample(set, size = round(0.4 * length(set)), replace = FALSE)
          testrows = append(testrows, sampledrows)          
     }
}
testrows = testrows[-1]

traindata = expdata[-testrows, ]
testdata = expdata[testrows, ]


#--------------------------
# oversample 'alternative' medicine stratified by star ratings to match 
# 'conventional' medicine sample size

starsbymed = cbind(as.data.frame(table(traindata[traindata$medicine == "conventional", ]$stars)),
                   as.data.frame(table(traindata[traindata$medicine == "alternative", ]$stars)))
starsbymed = starsbymed[ , -c(1, 3)]
colnames(starsbymed) = c("conventional", "alternative")


var = sort(unique(expdata$stars))
overrows = NA
set.seed(17830)

for (iter in 1:length(var)) {
     set = which(traindata$medicine == "alternative" & traindata$stars == iter)
     sampledrows = sample(set, size = starsbymed$conventional[iter], replace = TRUE)
     overrows = append(overrows, sampledrows)          
}
overrows = overrows[-1]

overtraindata = rbind(traindata[traindata$medicine == "conventional", ], 
                      traindata[overrows, ])


#--------------------------
# tree model on oversampled training set

library(caret)

wordfreqnums <- 80       # number of word frequency variables for inclusion
# column indices for data to be included in analysis
# columns 4 - 39 are Yelp data and the standard analyses from the package 'qdap'
# the remaining columns are word usage rates
# the number of words are selected inclusion are specified in 'wordfreqnums'
# words are included based on the difference in usage rates between reviews
# for conventional or alternative medicine multipled by word frequency that 
# has been weighted so that reviews for alternative medicine count equally as 
# those for conventional medicine
# indices skip '1' and run from 2 to 'wordfreqnums + 1' to avoid 'reflexology',
# which did not appear in conventional medicine reviews; since the difference
# was divided by the word usage rate in conventional medicine reviews, the
# difference for 'reflexology' is listed as 'infinity', which is not useful
diffwfreqcols <- c(4:39, sortdiffwfreq$colindex[2:wordfreqnums + 1])

rpartmodelover = train(medicine ~ ., method = "rpart", data = overtraindata[ , diffwfreqcols])
predrpartover = predict(rpartmodelover, testdata[ , diffwfreqcols])

sink(file = "treeoutput.txt")
rpartmodelover$finalModel
confusionMatrix(predrpartover, testdata$medicine)
sink(file = NULL)

# plot of tree for report
library(rpart.plot)
par(mfrow = c(1, 1))
prp(rpartmodelover$finalModel)
