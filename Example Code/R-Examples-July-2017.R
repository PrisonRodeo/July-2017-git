##############################################
# This is some code from AccessLex
# replicating the example(s) in the
# slides.
#
# File created July 11, 2017
#
# File last updated June 11, 2017
###############################################
# Preliminaries:
#
# Set working directory:

setwd("~/Dropbox (Personal)/LawyerMetrics/AG Things/Training")

# Options:

options(scipen = 6) # bias against scientific notation
options(digits = 3) # show fewer decimal places

###################################################
# Get "country-level" data:

url <- getURL("https://raw.githubusercontent.com/PrisonRodeo/July-2017-git/master/Data/CountryData2000.csv")
Data <- read.csv(text = url) # read the "Country" data
rm(url)  # remove unnecessary objects

# Summary statistics

# install.packages("psych") <- Install psych package, if necessary
library(psych)

with(Data, describe(infantmortalityperK)) # Infant mortality per thousand
with(Data, describe(DPTpct)) # DPT percentage

# Linear regression:

IMDPT<-lm(infantmortalityperK~DPTpct,data=Data,na.action=na.exclude)
summary.lm(IMDPT)

# ANOVA:

anova(IMDPT)

# Draw a scatterplot:

pdf("IM-DPT.pdf",7,6) # send this to a 7"x6" PDF file
par(mar=c(4,4,2,2)) # Set margins around the plot (optional)
with(Data,
     plot(DPTpct,infantmortalityperK,pch=19,
          xlab="DPT Percent",ylab="Infant Mortality Per 1K"))
dev.off() # turns off output to the PDF

######################################################
# Data cleaning / management...
#
# Read the toy "Simpsons" data:

url <- getURL("https://raw.githubusercontent.com/PrisonRodeo/July-2017-git/master/Data/Simpsons.csv") # the last bit is important
Simp <- read.csv(text = url) # read the "Country" data
rm(url)  # remove unnecessary objects

Simp

# Challenge is to separate the bits in the text field. 
# What kind of variable is "text"?

str(Simp$text)

# It's a "factor" variable; we need to convert it to
# a character / "string" variable:

Simp$text <- as.character(Simp$text)
str(Simp$text)

# Now it is a character / string variable. 
#
# Next, break it down into substrings, separated by
# commas:

strsplit(Simp$text,",")

# That did what we want to do, now we need to reassemble
# those elements back into the data. 
#
# First, assign that list to an object:

foo <- strsplit(Simp$text,",")
foo

# Now turn that into data:

n.words <- sapply(foo, length) # how many words in each?
seq.max <- seq_len(max(n.words)) # max. number of words
df <- data.frame(t(sapply(foo, "[", i = seq.max))) # break apart the list
df

# Now we can rename the columns:

colnames(df) <- c("text1","text2","text3","text4")

# ... and merge those rows back together with the "simpsons"
# data we had before:

Simp2 <- cbind(Simp,df) # we could also have used --merge--
View(Simp2)

# Done!
