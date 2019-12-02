#use dat from seminar_data_cleaning.r
require(ggplot2)
require(ggthemes)

#for shading
df_poly <- data.frame(
  PercentWomenPresent=c(-Inf, Inf, Inf),
  PercentWomenQuestions=c(-Inf, Inf, -Inf),
  speaker = c("male", "male", "male")
)

f1 <- ggplot(dat, aes(x = PercentWomenPresent, y = PercentQuestionsFromWomen, colour = as.factor(speaker), shape = as.factor(speaker))) + 
  geom_point(size = 3) + geom_abline(slope=1, intercept=0, size = 1.6) + 
  xlim(c(0, 50)) + ylim(c(0, 50)) +
  #ggtitle("Gender Imbalance in Question Asking") + xlab("% Audience Female") + 
  ylab("% Question-Askers Female") + labs(color = "Speaker Gender") + 
  scale_color_manual(name = "Speaker Gender", labels=c("female","male"), values = c("grey", "black")) + 
  scale_shape_manual(name = "Speaker Gender", labels=c("female","male"), values = c(19, 15)) + 
  theme_bw() +
  theme(title = element_text(size = 14), legend.title = element_text(size = 14), legend.position = c(0.2, 0.8), legend.background = 
        element_rect(color = "black", fill = "white", size = 1, linetype = "solid"),
        panel.background = element_rect(fill = 'white', colour = 'white'),
        legend.text=element_text(size=12)); f1 
# f2 <- f1 + geom_polygon(data=df_poly, aes(PercentWomenPresent, PercentWomenQuestions), fill="royalblue2", colour = "black", alpha=0.2) +
#   geom_text(x=22, y=8, label="higher proportion of\nmen ask questions", size = 7, color = "black"); f2
#without text
f3 <- f1 + geom_polygon(data=df_poly, aes(PercentWomenPresent, PercentWomenQuestions), fill="royalblue2", colour = "black", alpha=0.2); f3

##Version for Men
f1m <- ggplot(dat, aes(x = (1-PercentWomenPresent), y = (1-PercentQuestionsFromWomen), colour = as.factor(speaker))) + 
  geom_point(size = 3) + geom_abline(slope=1, intercept=0, size = 1.6) + 
  xlim(c(0, 50)) + ylim(c(0, 50)) +
  ggtitle("Gender Imbalance in Question Asking") + xlab("% Audience Male") + 
  ylab("% Question-Askers Male") + labs(color = "Speaker Gender") + 
  scale_color_manual(labels=c("female","male"), values = c("violetred2", "royalblue2")) + #TODO check labels
  theme_bw() +
  theme(title = element_text(size = 14), legend.title = element_text(size = 14), legend.position = c(0.2, 0.8), legend.background = 
          element_rect(color = "black", fill = "white", size = 1, linetype = "solid"),
        panel.background = element_rect(fill = 'white', colour = 'white'),
        legend.text=element_text(size=12)); f1m
f2m <- f1m + geom_polygon(data=df_poly, aes(PercentWomenPresent, PercentWomenQuestions), fill="royalblue2", colour = "black", alpha=0.2) +
  geom_text(x=22, y=8, label="higher proportion of\nmen ask questions", size = 7, color = "black"); f2m
#without text
f3m <- f1m + geom_polygon(data=df_poly, aes(PercentWomenPresent, PercentWomenQuestions), fill="royalblue2", colour = "black", alpha=0.2); f3m

#Ratio Plot
#"size of imbalance at each seminar"
f3 <- ggplot(data = dat, aes(dat$imbalanceInRatio, fill = as.factor(dat$greaterMaleRatio))) + 
  geom_histogram(bins = 10) + geom_vline(xintercept = 0, size = 1.6) +
  ggtitle("Gender Imbalance in Ratio of Questions to People Present") + xlab("P(Women in Audience)") + 
  ylab("P(Women Who Asked Questions)") + labs(fill = "Proportion Greater for:") + 
  scale_fill_manual(labels=c("female","male"), values = c("violetred2", "royalblue2")); f3

#Ratios
#Interaction between Attendee Gender and Speaker Gender 
IP<-data.frame(attendee = factor(c("male", "male", "female", "female")))
IP$speaker <- c("female", "male", "female", "male")
IP$P1orMoreQ <- c(39.9, 25.1, 24.1, 12.5)

pI <- ggplot(data = IP, aes(x = attendee, y = P1orMoreQ, group = speaker, color = speaker, shape = speaker)) + 
  geom_point(size = 3) + geom_line(size = 1) + 
  ggtitle("Probability of Asking At Least One Question:\nAttendee by Speaker Gender") +
  xlab("Attendee Gender") + ylab("Probability of Asking At Least One Question") +
  scale_color_manual(name = "Speaker Gender", labels=c("female","male"), values = c("violetred2", "royalblue2")) +
  scale_shape_manual(name = "Speaker Gender", labels=c("female","male"), values = c(19, 15)) + 
  theme_bw() + labs(color = "Speaker Gender") + 
  theme(title = element_text(size = 14), legend.title = element_text(size = 14), legend.position = c(0.2, 0.86), 
        legend.background = element_rect(color = "black", fill = "white", size = 1, linetype = "solid"),
        panel.background = element_rect(fill = 'white', colour = 'white'),
        legend.text=element_text(size=12), axis.text=element_text(size=14,face="bold")) + 
  geom_text(x=1.25, y=32, label="diff = 16.4%", size = 5, color = "violetred2") + 
  geom_text(x=1.45, y=21.5, label="diff = 11.2%", size = 5, color = "royalblue2"); pI

pI_notext <- ggplot(data = IP, aes(x = attendee, y = P1orMoreQ, group = speaker, color = speaker, shape = speaker)) + 
  geom_point(size = 3) + geom_line(size = 1) + 
  ggtitle("Probability of Asking At Least One Question:\nAttendee by Speaker Gender") +
  xlab("Attendee Gender") + ylab("Probability of Asking At Least One Question") +
  scale_color_manual(labels=c("female","male"), values = c("violetred2", "royalblue2")) +
  scale_shape_manual(name = "Speaker Gender", labels=c("female","male"), values = c(19, 15)) + 
  theme_bw() + labs(color = "Speaker Gender") + 
  theme(title = element_text(size = 14), legend.title = element_text(size = 14), legend.position = c(0.2, 0.86), 
        legend.background = element_rect(color = "black", fill = "white", size = 1, linetype = "solid"),
        panel.background = element_rect(fill = 'white', colour = 'white'),
        legend.text=element_text(size=12), axis.text=element_text(size=14,face="bold")); pI_notext

#Add rectangles
# d=data.frame(x1=c(0, 1), x2=c(.5, 1.5), y1=c(0, 0), y2=c(60, 60), t=c('a','b'))
# pI_attendeegender <- pI + geom_rect(data=d, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill=t))
# 
# pI_attendeegender
# ggsave(filename = "/Users/cora/Dropbox/UW-Madison/CurrentResearch/SeminarQuestions/Interaction_colorsAdded.png", width = 7, height = 7, plot = f2)             


##GRAYSCALE FOR PUBLICATION
#Helvetica font, size 11
#"female/women" to a grey/circle (in Prism the grey I selected is called "steel"), "male/men" is black/square
#figure 3A: women ask fewer questions than men
pI_notext <- ggplot(data = IP, aes(x = attendee, y = P1orMoreQ, group = speaker, color = speaker, shape = speaker)) + 
  geom_point(size = 3) + geom_line(size = 1) + 
  ggtitle("Probability of Asking At Least One Question:\nAttendee by Speaker Gender") +
  xlab("Student attendee gender") + ylab("Probability of Asking At Least One Question") +
  scale_color_manual(labels=c("female","male"), values = c("grey", "black")) +
  scale_shape_manual(name = "Speaker Gender", labels=c("female","male"), values = c(19, 15)) + 
  theme_bw() + labs(color = "Speaker Gender") + 
  theme(title = element_text(size = 14), legend.title = element_text(size = 14), legend.position = c(0.2, 0.86), 
        legend.background = element_rect(color = "black", fill = "white", size = 1, linetype = "solid"),
        panel.background = element_rect(fill = 'white', colour = 'white'),
        legend.text=element_text(size=12), axis.text=element_text(size=14,face="bold")); pI_notext

## 4B ##
pI_notext <- ggplot(data = IP, aes(x = attendee, y = P1orMoreQ, group = speaker, color = speaker, shape = speaker), mgpg, aes(cty,hwy)) + 
  geom_point(size = 3) + geom_line(size = 1) + 
  ggtitle("") +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
  xlab("\nStudent attendee gender") + ylab("Probability of asking at least one question\n") +
  scale_color_manual(labels=c("female","male"), values = c("darkgrey", "black")) +
  scale_shape_manual(name = "Speaker Gender", labels=c("female","male"), values = c(19, 15)) + 
  theme_bw() + labs(color = "Speaker Gender") + theme(text=element_text(family="Helvetica", size=10)) + 
  theme(title = element_text(size = 16), legend.title = element_text(size = 12), legend.position = c(0.15, 0.875), 
        #legend.background = element_rect(color = "black", fill = "white", size = 1),
        panel.background = element_rect(fill = 'white', colour = 'white'),
        legend.text=element_text(size=12), axis.text=element_text(size=12)); pI_notext
ggsave(filename = "../figures/figure4B_speaker_question-asker_interaction.png", width = 7, height = 7, plot = pI_notext)             

## 4A ##
f1 <- ggplot(dat, aes(x = PercentWomenPresent, y = PercentQuestionsFromWomen, colour = as.factor(speaker), shape = as.factor(speaker))) + 
  geom_point(size = 3) + geom_abline(slope=1, intercept=0, size = 1.6) + 
  xlim(c(0, 50)) + ylim(c(0, 50)) +
  ggtitle("") + xlab("\nPercent audience female") + 
  ylab("Percent question-askers female\n") + labs(color = "Speaker Gender") + 
  scale_color_manual(name = "Speaker Gender", labels=c("female","male"), values = c("darkgrey", "black")) + 
  scale_shape_manual(name = "Speaker Gender", labels=c("female","male"), values = c(19, 15)) +
  theme_bw() + theme(text=element_text(family="Helvetica", size=12)) + 
  theme(title = element_text(size = 16), legend.title = element_text(size = 12), legend.position = c(0.15, 0.875),
        panel.background = element_rect(fill = 'white', colour = 'white'),
        legend.text=element_text(size=12)); f1 
#without text
f3 <- f1 + geom_polygon(data=df_poly, aes(PercentWomenPresent, PercentWomenQuestions), fill="grey", colour = "black", alpha=0.2); f3
ggsave(filename = "../figures/figure4A_speakers_gender.png", width = 7, height = 7, plot = f3)             

