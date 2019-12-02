#see https://www.economist.com/news/science-and-technology/21732082-there-easy-fix-women-ask-fewer-questions-men-seminars

#read in data
library(tidyverse)
library(ggplot2)
library(readxl)    
read_excel_allsheets <- function(filename) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  names(x) <- sheets
  x
}

sheets <- read_excel_allsheets("../data/questions_2017-2018.xlsx")

dat <- sheets[[4]][c(1:15), c(1, 2, 3, 4, 6, 7)]

#turn all to numeric
dat$Wpresent <- as.numeric(dat$`W present`)
dat$Mpresent <- as.numeric(dat$`M present`)
dat$Wquestions <- as.numeric(dat$`W w Qs`)
dat$Mquestions <- as.numeric(dat$`M w Qs`)
dat$Date <- as.factor(dat$Date)
#create ID for seminar that isn't a date
dat$SeminarID <- as.numeric(as.factor(dat$Date))

#create indicator for male gender: 1 = male
dat$speaker <- ifelse(grepl("fe", dat$`Speaker gender`), "female", "male")
       
#calculate prop(women) in seminar over total # in seminar, add to data sheet
dat$total = dat$Wpresent + dat$Mpresent
dat$pwomen = dat$Wpresent/dat$total

#calc p(women ask >=1 question)
dat$Qtotal <- dat$Wquestions + dat$Mquestions
dat$pQuestionsFromWomen <- dat$Wquestions/dat$Qtotal
 
#ratio of questions to #present in audience
#number = 1 means that an equal proportion of women would ask questions
#numer < 1 means that fewer women asked questions
#> 1 means that a greater proportion of women asked questions
dat$WoverMratio <- (dat$Wquestions/dat$Wpresent)/(dat$Mquestions/dat$Mpresent)

#imbalance in ratio 
#<0 means > men
dat$imbalanceInRatio <- (dat$Wquestions/dat$Wpresent) - (dat$Mquestions/dat$Mpresent)
#greater proportion of men indicator 1 means more male
dat$greaterMaleRatio <- ifelse(dat$imbalanceInRatio < 0, 1, 0)

dat$PercentQuestionsFromWomen <- as.numeric(dat$pQuestionsFromWomen*100)
dat$PercentWomenPresent <- as.numeric(dat$pwomen*100)

#BinDat for binomial data analysis
#want a row for each participant in each seminar (each date)
nrow = sum(dat$total)
personSeminarID <- seq(1:nrow)
BinDat <- data.frame(personSeminarID = personSeminarID)
SeminarID = c()
gender = c()
question = c()
speakergender = c()
for (d in unique(dat$SeminarID)){ #these vectors should all be 452 long
  print(length(dat[dat$SeminarID == d,]$SeminarID)*dat[dat$SeminarID == d,]$total)
  SeminarID <- c(SeminarID, rep(d, times = length(dat[dat$SeminarID == d,]$SeminarID)*dat[dat$SeminarID == d,]$total))
  print(dat[dat$SeminarID == d,]$`Speaker gender`[1])
  speakergender <- c(speakergender, rep(dat[dat$SeminarID == d,]$`Speaker gender`[1], times = length(dat[dat$SeminarID == d,]$SeminarID)*dat[dat$SeminarID == d,]$total))
  
  gender <- c(gender, rep("male", times = length(dat[dat$SeminarID == d,]$SeminarID)*dat[dat$SeminarID == d,]$Mpresent))
  gender <- c(gender, rep("female", times = length(dat[dat$SeminarID == d,]$SeminarID)*dat[dat$SeminarID == d,]$Wpresent))
  
  question <- c(question, rep(1, times = length(dat[dat$SeminarID == d,]$SeminarID)*dat[dat$SeminarID == d,]$Mquestions))
  question <- c(question, rep(0, times = length(dat[dat$SeminarID == d,]$SeminarID)*(dat[dat$SeminarID == d,]$Mpresent-dat[dat$SeminarID == d,]$Mquestions)))
  question <- c(question, rep(1, times = length(dat[dat$SeminarID == d,]$SeminarID)*dat[dat$SeminarID == d,]$Wquestions))
  question <- c(question, rep(0, times = length(dat[dat$SeminarID == d,]$SeminarID)*(dat[dat$SeminarID == d,]$Wpresent-dat[dat$SeminarID == d,]$Wquestions)))
}

length(speakergender)
BinDat$speakergender <- speakergender
BinDat$speakerGender <- ifelse(grepl("fe", BinDat$speakergender), "female", "male")

length(question)
BinDat$SeminarID <- SeminarID
length(gender)
BinDat$gender <- gender
BinDat$question <- question

#Check this data cleaning using summarize and group_by
sum <- BinDat %>%
  group_by(SeminarID, gender) %>%
  summarise(sum = sum(question))

#compare sum and dat: they match even if they dont have same order for some reason
sum[sum$gender == "male",c("sum", "SeminarID")]
dat[,c("Mquestions", "SeminarID")]

