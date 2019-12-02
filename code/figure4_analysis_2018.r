#use data from seminar_data_cleaning.r
sum <- BinDat %>%
  group_by(SeminarID, gender) %>%
  summarise(sum = sum(question))

#seminar summary
tab_Seminar <- dat %>%
  group_by(speaker) %>%
  summarise()

##summary table N:avg # people attending, avg # w, m attending, avg # questions, avg # w, m who ask 1 or more questions
tab <- dat %>%
  summarise(A = mean(total), sdA = sd(total), WA = mean(Wpresent), sdWA = sd(Wpresent), MA = mean(Mpresent), 
            sdMA = sd(Mpresent), Q = mean(Wquestions + Mquestions), sdQ = sd(Wquestions + Mquestions),
            WQ = mean(Wquestions), sdWQ = sd(Wquestions), MQ = mean(Mquestions), sdMQ = sd(Mquestions))

#sumary table by speaker gender: avg # people attending, avg # w, m attending, avg # questions, avg # w, m who ask 1 or more questions
tab_speakerGender <- dat %>%
  group_by(speaker) %>%
  summarise(A = mean(total), sdA = sd(total), Q = mean(Wquestions + Mquestions), 
            sdQ = sd(Wquestions + Mquestions))

#summary table p
c("Mean N women attending", "Mean % women who asked >=1 question", "Mean N men attending", "% men who asked >=1 question")
summaryP <- dat %>%
  summarise(meanWomenAttending = mean(Wpresent), meanMenAttending = mean(Mpresent), 
            meanPercentWomenAttendingWhoAskedAtLeastOneQ = 100*mean(Wquestions/Wpresent),
            meanPercentMenAttendingWhoAskedAtLeastOneQ = 100*mean(Mquestions/Mpresent),
            overallPAskedQ = 110*mean(Qtotal/total))


summaryPM <- dat[dat$`Speaker gender` == "male", ] %>%
  summarise(meanWomenAttending = mean(Wpresent), meanMenAttending = mean(Mpresent), 
            meanPercentWomenAttendingWhoAskedAtLeastOneQ = 100*mean(Wquestions/Wpresent), 
            sdPercentWomenAttendingWhoAskedAtLeastOneQ = 100*sd(Wquestions/Wpresent),
            meanPercentMenAttendingWhoAskedAtLeastOneQ = 100*mean(Mquestions/Mpresent),
            sdPercentMenAttendingWhoAskedAtLeastOneQ = 100*sd(Mquestions/Mpresent),
            overallPAskedQ = 110*mean(Qtotal/total))

#TODO
dat$`Speaker gender` <- ifelse(dat$`Speaker gender` != "male", "female", "male")

summaryPW <- dat[dat$`Speaker gender` == "female", ] %>%
  summarise(meanWomenAttending = mean(Wpresent), meanMenAttending = mean(Mpresent), 
            meanPercentWomenAttendingWhoAskedAtLeastOneQ = 100*mean(Wquestions/Wpresent), 
            sdPercentWomenAttendingWhoAskedAtLeastOneQ = 100*sd(Wquestions/Wpresent),
            meanPercentMenAttendingWhoAskedAtLeastOneQ = 100*mean(Mquestions/Mpresent),
            sdPercentMenAttendingWhoAskedAtLeastOneQ = 100*sd(Mquestions/Mpresent),
            overallPAskedQ = 110*mean(Qtotal/total))

#Two Sample Proportion Test
summarySum <- dat %>%
  summarise(sumWomenAttending = sum(Wpresent), sumMenAttending = sum(Mpresent), 
            sumWomenAskedAtLeastOneQ = sum(Wquestions), sumMenAskedAtLeastOneQ = sum(Mquestions))

prop.test(x = c(summarySum$sumWomenAskedAtLeastOneQ, summarySum$sumMenAskedAtLeastOneQ), 
                 n = c(summarySum$sumWomenAttending, summarySum$sumMenAttending))
require(Barnard)
barnard.test(summarySum$sumWomenAskedAtLeastOneQ, summarySum$sumMenAskedAtLeastOneQ, summarySum$sumWomenAttending - summarySum$sumWomenAskedAtLeastOneQ, 
             summarySum$sumMenAttending - summarySum$sumMenAskedAtLeastOneQ, 
              dp = 0.001, pooled = TRUE) #p = 0.0009
#more here: https://www.r-statistics.com/2010/02/barnards-exact-test-a-powerful-alternative-for-fishers-exact-test-implemented-in-r/

require(lme4)
#Question:
#1. Does gender have an effect on p(asking question)?
#2. Does speaker gender seem to matter?

#for binomial, need to set up data differently BinDat
#binary question 0 or 1 var, for each question say if said by woman for each date
fit.b = glmer(question ~ as.factor(gender) + as.factor(speakerGender) + (1 | SeminarID), 
              data=BinDat,
              family=binomial)
summary(fit.b)
plot(fit.b)
pchisq(deviance(fit.b),
       df.residual(fit.b),lower=FALSE)
anova(fit.b, test="Chisq")

library(MASS)
library(arm)
invlogit(confint(fit.b))
invlogit(fixef(fit.b))

#P(asking question if woman and female speaker) = 0.228
invlogit(-1.2193)
#P(asking question if woman and female speaker)
invlogit(-1.2193 + -0.7287) #0.1247 ??
#P(asking question if male and female speaker) 
invlogit(-1.2193 + 0.7794) #0.39
#P(asking question if male and male speaker) 
invlogit(-1.2193 + 0.7794 + -0.7287) #0.237

#TODO create graph with CI for these predictions, but might be too much + unnecessary
#observed

summarySpeakerP <- dat %>%
  group_by(speaker) %>%
  summarise(meanWomenAttending = mean(Wpresent), meanMenAttending = mean(Mpresent), 
            meanPercentWomenAttendingWhoAskedAtLeastOneQ = 100*mean(Wquestions/Wpresent), 
            meanPercentMenAttendingWhoAskedAtLeastOneQ = 100*mean(Mquestions/Mpresent))


#Interaction Model
fit.b.i = glmer(question ~ as.factor(gender) * as.factor(speakerGender) + (1 | SeminarID), 
              data=BinDat,
              family=binomial)
summary(fit.b.i)

anova(fit.b, fit.b.i) #interaction doesn't add much

#P(asking question if woman questioner and female speaker)
invlogit(-1.1774) #.2355
#P(asking question if woman questioner and male speaker)
invlogit(-1.1774 + -0.8371) #0.1177
#P(asking question if male questioner and female speaker) 
invlogit(-1.1774 + 0.7178) #0.387
#P(asking question if male and male speaker) 
invlogit(-1.1774 + 0.7178 + -0.8371 + 0.1558) #0.242

#difference in slopes by speaker
#P(asking question if woman questioner and female speaker) - #P(asking question if male questioner and female speaker) 
invlogit(-1.1774 + 0.7178) - invlogit(-1.1774) #0.1516
#P(asking question if woman questioner and male speaker), #P(asking question if male and male speaker) 
invlogit(-1.1774 + 0.7178 + -0.8371 + 0.1558) - invlogit(-1.1774 + -0.8371) #0.12446


#Poisson: for other question (# questions, not # question-askers)
#count of questions would be poisson with offset for numWomenAudience
fit.p <- glmer(Wquestions ~ log(Wpresent) + speaker + (1|Date), family = poisson, data = dat)
summary(fit.p)
plot(x = log(dat$Wquestions), y=log(dat$Wpresent))

#see if status matter (exclude all deans, dir, profs) see if effects change 
#(see students only analysis)

