
# ICML 2020
# Measuring Non-Expert Comprehension of Machine Learning Fairness Metrics (#1357)
# Supplementary Materials - Analysis code

# DEBJANI SAHA, Candice Schumann, Duncan C. McElFresh, John P. Dickerson, Michelle L. Mazurek, Michael C. Tschantz
# Manuscript: https://arxiv.org/abs/2001.00089
# Code: https://github.com/saharaja/ICML2020-fairness
# Conference: https://icml.cc/Conferences/2020



# NOTE:  #****** = significant test



# ================================== #
#                SETUP               #
# ================================== #

library(scales)
library(ggplot2)
library(reshape2)
library(grid)
library(gridExtra)
library(psych)


#######################################
#######################################
#                                     #
#           Study-1 (Pilot)           #
#   Abstract published at AIES 2020   #
#                                     #
#######################################
#######################################



# ====== SETUP ====== #

# Read in reformatted data
setwd("C:/Users/Debjani Saha/Documents/University of Maryland/Projects/Alg Fairness/20191024 AIES submission (11-04)")
all.scenarios <- read.table("../Study 0/20190813_FINAL_PARTICIPANT_LIST.txt",header=T,sep="\t",quote="",row.names=1)

# Reformat T/F questions
all.scenarios$Q5 <- factor(all.scenarios$Q5,levels=c("TRUE","FALSE"))
all.scenarios$Q6 <- factor(all.scenarios$Q6,levels=c("TRUE","FALSE"))
all.scenarios$Q7 <- factor(all.scenarios$Q7,levels=c("TRUE","FALSE"))
all.scenarios$Q8 <- factor(all.scenarios$Q8,levels=c("TRUE","FALSE"))

# Reformat Y/N questions
all.scenarios$Q9 <- factor(all.scenarios$Q9,levels=c("Yes","No"))
all.scenarios$Q10 <- factor(all.scenarios$Q10,levels=c("Yes","No"))
all.scenarios$Q11 <- factor(all.scenarios$Q11,levels=c("Yes","No"))

# Reformat likert questions
all.scenarios$Q1 <- as.factor(all.scenarios$Q1)
all.scenarios$Q13 <- as.factor(all.scenarios$Q13)

# Reformat multiple choice question
all.scenarios$Q3 <- as.factor(all.scenarios$Q3)	# Suppose a different teacher/manager/hiring manager is considering...
all.scenarios$Q14 <- factor(all.scenarios$Q14)  # Please select the choice that best describes your experience

# Reformat demographic questions
all.scenarios$edu <- as.factor(gsub("High school","HS",as.character(all.scenarios$edu)))
all.scenarios$edu <- factor(all.scenarios$edu,levels=c("Some high school credit, no diploma or equivalent",
                                                       "HS graduate, diploma or the equivalent (for example: GED)",
                                                       "Some college credit, no degree",
                                                       "Trade/technical/vocational training",
                                                       "Associate's degree",
                                                       "Bachelor's degree",
                                                       "Master's degree",
                                                       "Professional or doctoral degree (JD, MD, PhD)"))
all.scenarios$exp.hr <- as.factor(all.scenarios$exp.hr)
all.scenarios$exp.mng <- as.factor(all.scenarios$exp.mng)
all.scenarios$exp.edu <- as.factor(all.scenarios$exp.edu)
all.scenarios$exp.it <- as.factor(all.scenarios$exp.it)
all.scenarios$exp.cs <- as.factor(all.scenarios$exp.cs)
all.scenarios$exp.ml <- as.factor(all.scenarios$exp.ml)

# Remove participant who took particularly long (over 2hrs)
remove <- grep("2159e15c-5da7-0ae3-a3eb-d772e28fdb81",rownames(all.scenarios))
remove <- which(all.scenarios$Duration > 10000)
all.scenarios <- droplevels(all.scenarios[-remove,])
n.respondents <- dim(all.scenarios)[1]
#correct.answers <- correct.answers[-remove,]
#score.sheet <- score.sheet[-remove,]


# ====== DEMOGRAPHICS ====== #

# Age Distribution
hist((2019-all.scenarios$YOB),xlim=c(10,80),xlab="Age",main="")

# Education Level
x <- data.frame(table(all.scenarios$edu)/n.respondents)
colnames(x) <- c("Level","Percent")
x$Percent <- percent(x$Percent)
x
# Requested breaks (from 2017 census)
# Less than HS graduate: 12.1%
# HS graduate: 27.7%
# Some college or associate's degree: 30.8%
# Bachelor's degree or above: 29.4%

# Gender
x <- data.frame(table(all.scenarios$gender)/n.respondents)
colnames(x) <- c("Gender","Percent")
x$Percent <- percent(x$Percent)
x

# Ethnicity
x <- data.frame(table(all.scenarios$eth)/n.respondents)
colnames(x) <- c("Ethnicity","Percent")
x$Percent <- percent(x$Percent)
x
rm(x)
# Requested breaks (from 2017 census):
# American Indian or Alaska Native: 0.7%
# Asian: 5.5%
# Native Hawaiian and Other Pacific Islander: 0.2%
# Black or African American: 12.3%
# Hispanic or Latino: 18.1%
# Other: 0.3%
# Mixed: 2.3%
# White: 60.6%



# ====== COMPREHENSION SCORE ====== #

# Descriptive Statistics
correct.answers <- cbind(rep("20",n.respondents),
                         rep("Correct",n.respondents),
                         rep("TRUE",n.respondents),
                         rep("FALSE",n.respondents),
                         rep("TRUE",n.respondents),
                         rep("FALSE",n.respondents),
                         rep("Yes",n.respondents),
                         rep("No",n.respondents),
                         rep("No",n.respondents))
colnames(correct.answers) <- c("Q3","Q4","Q5","Q6","Q7","Q8","Q9","Q10","Q11")

all.scenarios$score <- rowSums(all.scenarios[,colnames(correct.answers)]==correct.answers)
score.sheet <- data.frame(all.scenarios[,colnames(correct.answers)]==correct.answers)

summary(all.scenarios$score)
#boxplot(all.scenarios$score)
sd(all.scenarios$score)

# Proportion of respondents (out of 147) answering each question correctly:
x <- data.frame(Percent=colSums(score.sheet)/n.respondents)
x$Percent <- percent(x$Percent)
colnames(x) <- c("Percent Correct")
x
rm(x)

# Respondents answering each question correctly, split by scenario
# (each column comprises all 147 respondents)
score.sheet$CintID <- all.scenarios$CintID
score.sheet$scenario <- all.scenarios$scenario
score.sheet.melt <- melt(score.sheet,id.vars=c("CintID","scenario"))
colnames(score.sheet.melt) <- c("CintID","scenario","question","value")
score.sheet.melt$value <- gsub("FALSE","Incorrect",gsub("TRUE","Correct",score.sheet.melt$value))
ggplot(score.sheet.melt,aes(x=value)) + geom_bar(stat="count",color="black",aes(fill=value)) + 
  scale_fill_manual(values=c("white","grey"),name="") + scale_x_discrete("") + scale_y_continuous("# of Participants") + 
  facet_grid(scenario ~ question) + theme_bw() + theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())


# Internal Validity
# Assessed using two measures: 
# Cronbach's alpha
# Item-total correlation
responses <- score.sheet[,colnames(correct.answers)]
responses[responses=="TRUE"] <- 1
psych::alpha(responses)

### Resources for internal validity
### https://mattchoward.com/introduction-to-cronbachs-alpha/
### https://mattchoward.com/calculating-cronbachs-alpha-in-r/
### https://rpubs.com/hauselin/reliabilityanalysis
### https://www.researchgate.net/post/Is_it_correct_to_use_Cronbach_s_Alpha_to_assess_the_internal_consistency_of_a_questionnaire_with_binary_data_yes_no
### https://en.wikipedia.org/wiki/Item_response_theory



# ====== PRELIMINARY ANALYSIS AND RESULTS ====== #

## Test 1: comprehension score vs. [scenario,education,age]
test.score <- data.frame(score=all.scenarios$score,
                         scenario=all.scenarios$scenario,
                         edu=all.scenarios$edu,
                         age=(2019-all.scenarios$YOB))
rownames(test.score) <- rownames(all.scenarios)

# Comprehension score by scenario
ggplot(test.score,aes(x=score)) + geom_histogram(binwidth=1,color="black",fill="white") + 
  facet_grid(. ~ scenario) + scale_x_continuous("Comprehension Score",breaks=c(0:9)) + 
  scale_y_continuous("# of Participants") + theme_bw()

# Comprehension score by education level
ggplot(test.score,aes(x=score)) + geom_histogram(binwidth=1,color="black",fill="white") + 
  facet_wrap(~edu) + scale_x_continuous("Comprehension Score",breaks=c(0:9)) + theme_bw() +
  scale_y_continuous("# of Participants",breaks=c(0:11),labels=c("0","","2","","4","","6","","8","","10",""))

# Comprehension score by age
ggplot(test.score,aes(x=age,y=score)) + geom_point() + 
  scale_y_continuous("Comprehension Score",breaks=c(0:9)) + 
  scale_x_continuous("Age") + theme_bw()

# Test using GLM with poisson distribution, treating education as categorical:
fit.score <- glm(score ~ scenario + edu + age, test.score, family="poisson")
summary(fit.score)

# Test using GLM with poisson distribution, treating education as ordinal:
fit.score <- glm(score ~ scenario + as.numeric(edu) + age, test.score, family="poisson")
summary(fit.score)


## Test 2: perceived realism (Q1) vs. [scenario,gender,ethnicity]
# Realism of the presented scenario was estimated using the following question (Q1):
# To what extent do you agree with the following statement: a scenario similar to the one described above might occur in real life.
# (1)	Strongly agree
# (2)	Agree
# (3)	Neither agree nor disagree
# (4)	Disagree
# (5)	Strongly disagree

test.realism <- data.frame(realism=as.numeric(all.scenarios$Q1),
                           scenario=all.scenarios$scenario,
                           gender=all.scenarios$gender,
                           eth=all.scenarios$eth)
rownames(test.realism) <- rownames(all.scenarios)

ggplot(test.realism,aes(x=realism)) + geom_histogram(binwidth=1,color="black",fill="white") + 
  facet_grid(. ~ scenario) + scale_x_continuous("Likert Rating",breaks=c(1:5)) + 
  scale_y_continuous("# of Participants") + theme_bw()

# Test using GLM with poisson distribution:
fit.realism <- glm(realism ~ scenario + gender + eth, test.realism, family="poisson")
summary(fit.realism)

# Redo the above test, but make ethnicity a binary category (white vs. other), since the survey population was predominantly white (73.3%):
test.realism$eth2 <- factor(gsub("^[A-N].*","Other",test.realism$eth))
fit.realism <- glm(realism ~ scenario + gender + eth2, test.realism, family="poisson")
summary(fit.realism)


## Test 3: hours of effort (Q2) vs. scenario
# Participants were asked how many hours should be dedicated to ensure that the decision described in the survey was fair (Q2).
test.effort <- data.frame(effort=all.scenarios$Q2,scenario=all.scenarios$scenario)
rownames(test.effort) <- rownames(all.scenarios)

ggplot(test.effort,aes(x=effort)) + geom_histogram(binwidth=1,color="black",fill="white") + 
  facet_grid(. ~ scenario) + scale_x_continuous("Hours of Effort") + 
  scale_y_continuous("# of Participants") + theme_bw()

# Test using Kruskal-Wallis:
kruskal.test(effort ~ scenario, test.effort)

# Pairwaise post-hoc tests using Mann-Whitney U:
# Employee awards vs. Hiring
wilcox.test(effort ~ scenario,droplevels(test.effort[test.effort$scenario!="AP",]))
# Art project vs. Hiring
wilcox.test(effort ~ scenario,droplevels(test.effort[test.effort$scenario!="EA",]))
# Art project vs. Employee awards
wilcox.test(effort ~ scenario,droplevels(test.effort[test.effort$scenario!="HR",]))


## Test 4: self-report of rule usage (Q14) vs. [scenario,age,gender,education, comprehension score]
# Participants were asked to self-resport their application of the rule in answering the questions (Q14):
# rule only (1)
# their own personal notions of fairness (3)
# some combination thereof (2).

test.selfreport <- data.frame(self=as.numeric(all.scenarios$Q14),
                              scenario=all.scenarios$scenario,
                              age=(2019-all.scenarios$YOB),
                              gender=all.scenarios$gender,
                              edu=all.scenarios$edu,
                              score=all.scenarios$score)
rownames(test.selfreport) <- rownames(all.scenarios)

# Self-report by scenario
ggplot(test.selfreport,aes(x=self)) + geom_histogram(binwidth=1,color="black",fill="white") + 
  facet_grid(. ~ scenario) + scale_x_continuous("Self-report of rule usage (Q14)") + 
  scale_y_continuous("# of Participants") + theme_bw()

# Self-report by gender
ggplot(test.selfreport,aes(x=self)) + geom_histogram(binwidth=1,color="black",fill="white") + 
  facet_grid(. ~ gender) + scale_x_continuous("Self-report of rule usage (Q14)") + 
  scale_y_continuous("# of Participants") + theme_bw()

# Self-report by comprehension score
x <- test.selfreport[!is.na(test.selfreport$self),]
ggplot(x,aes(x=as.factor(self),y=score)) + geom_boxplot() + geom_jitter(aes(color=as.factor(self)),height=0.2) +
  scale_y_continuous("Comprehension Score",breaks=c(0:9)) + scale_x_discrete("Self-report of rule usage (Q14)") + 
  theme_bw() + theme(legend.position="none")
rm(x)

# Test using GLM with poisson distribution:
fit.selfreport <- glm(self ~ scenario + age + gender + edu + score,
                      test.selfreport, family="poisson")
#chisq.test(all.scenarios$gender, all.scenarios$Q14)
summary(fit.selfreport)

# We follow up with a post-hoc test (below) using Spearman correlation:
cor.test(test.selfreport$self,test.selfreport$score,method="spearman")


## Test 5: comprehension score vs. free response explanation (Q12)
# In free response Q12 we asked participants to explain the rule in their own words
# These responses were assigned one of 5 codes:
# correct: describes rule correctly
# partially correct: description has some errors or is somewhat vague
# neither: vague description of purpose of the rule rather than how it works, or pure opinion
# incorrect: incorrect/irrelevant
# none: no answer, or expresses confusion

# Get codes
q12.codes <- read.table("../Study 0/20190911_coding_q12.txt",header=T,sep="\t",quote="",row.names=1)
colnames(q12.codes) <- c("CintID","scenario","Q12","code")

# Remove secondary codes
q12.codes$code <- factor(gsub(";.*","",q12.codes$code),levels=c("correct","partially correct","neither","incorrect","none"))

# Make relevant data frame
test.q12 <- data.frame(score=all.scenarios$score,
                       code=q12.codes[rownames(all.scenarios),"code"],
                       scenario=all.scenarios$scenario)
rownames(test.q12) <- rownames(all.scenarios)
table(test.q12$code)

library(ggplot2)
ggplot(test.q12,aes(x=code,y=score)) + geom_boxplot() + geom_jitter(aes(color=code),height=0.2) +
  scale_y_continuous("Comprehension Score",breaks=c(0:9)) + scale_x_discrete("Assigned Code") + theme_bw() +
  theme(axis.text.x=element_text(angle=45,vjust=0.95),legend.position="none")

# Test using Kruskal-Wallis:
kruskal.test(score ~ code, test.q12)

# We follow up with 4 post-hoc tests (below) using Mann-Whitney U:
correct <- droplevels(test.q12[test.q12$code=="correct",])
partial <- droplevels(test.q12[test.q12$code=="partially correct",])
neither <- droplevels(test.q12[test.q12$code=="neither",])
incorrect <- droplevels(test.q12[test.q12$code=="incorrect",])
none <- droplevels(test.q12[test.q12$code=="none",])

# Correct vs. partially correct
wilcox.test(score ~ code, as.data.frame(rbind(correct,partial)))

# Correct vs. neither
wilcox.test(score ~ code, as.data.frame(rbind(correct,neither)))

# Partially correct vs. neither
wilcox.test(score ~ code, as.data.frame(rbind(partial,neither)))

# Neither vs. incorrect
wilcox.test(score ~ code, as.data.frame(rbind(neither,incorrect)))


## Test 6: comprehension score vs. self-report of rule understanding (Q13)
# Self-reported participant understanding of the rule was estimated by asking the following question (Q13):
# To what extent do you agree with the following statement: I am confident I know how to apply the award rule described above?
# (1)	Strongly agree
# (2)	Agree
# (3)	Neither agree nor disagree
# (4)	Disagree
# (5)	Strongly disagree

test.q13 <- data.frame(score=all.scenarios$score,
                       understanding=all.scenarios$Q13)
rownames(test.q13) <- rownames(all.scenarios)

table(test.q13$understanding)

ggplot(test.q13,aes(x=understanding,y=score)) + geom_boxplot() + geom_jitter(aes(color=understanding),height=0.2) +
  scale_y_continuous("Comprehension Score",breaks=c(0:9)) + scale_x_discrete("Self-report of rule understanding (Q13)") + 
  theme_bw() + theme(legend.position="none")

# Test using Kruskal-Wallis:
kruskal.test(score ~ understanding,test.q13)

# We follow up with six post-hoc tests (below) using Mann-Whitney U:
sa <- droplevels(test.q13[test.q13$understanding==1,])
a <- droplevels(test.q13[test.q13$understanding==2,])
n <- droplevels(test.q13[test.q13$understanding==3,])
d <- droplevels(test.q13[test.q13$understanding==4,])
sd <- droplevels(test.q13[test.q13$understanding==5,])

# Strongly agree (1) vs. agree (2)
wilcox.test(score ~ understanding, as.data.frame(rbind(sa,a)))

# Agree (2) vs. neither agree nor disagree (3)
wilcox.test(score ~ understanding, as.data.frame(rbind(a,n)))

# Strongly agree (1) vs. disagree (4)
wilcox.test(score ~ understanding, as.data.frame(rbind(sa,d)))

# Strongly agree (1) vs. strongly disagree (5)
wilcox.test(score ~ understanding, as.data.frame(rbind(sa,sd)))

# Neither agree nor disagree (3) vs. disagree (4)
wilcox.test(score ~ understanding, as.data.frame(rbind(n,d)))

# Disagree (4) vs. strongly disagree (5)
wilcox.test(score ~ understanding, as.data.frame(rbind(d,sd)))


## Test 7: comprehension score vs. free response opinion (Q15)
# Participants were asked for their opinion on the presented rule (Q15).
# These responses were then coded to assign a single primary code and zero or more secondary codes:

# Primary codes
# agree: expresses generally positive sentiment towards rule
# depends: describes both pros and cons of the given rule
# disagree: expresses generally negative sentiment towards rule
# none: no answer, or lacks opinion on fairness specifically
# not understood: expresses confusion about rule

# Secondary codes
# like: agrees with or likes rule
# dislike: disagrees with or dislikes rule
# fair: thinks the rule is fair
# unfair: thinks the rule is not fair
# alternative: proposes alternative method to make decision
# merit: mentions importance of qualifications/merit
# regurgitation: restates provided examples or rule itself

# Get codes
q15.codes <- read.table("../Study 0/20190911_coding_q15.txt",header=T,sep="\t",quote="",row.names=1)
colnames(q15.codes) <- c("CintID","scenario","Q15","code")

# Remove secondary codes
q15.codes$p.code <- factor(gsub(";.*","",q15.codes$code),levels=c("agree","depends","disagree","not understood","none"))
#q15.codes$s.codes <- gsub("^; ","",gsub("([a-z, ]*)(;*.*)","\\2",q15.codes$code))

# Make relevant data frame
test.q15 <- data.frame(score=all.scenarios$score,
                       code=q15.codes[rownames(all.scenarios),"p.code"],
                       scenario=all.scenarios$scenario)
rownames(test.q15) <- rownames(all.scenarios)
table(test.q15$code)

#x <- test.q15[!is.na(test.q15$code),]
#library(ggplot2)
ggplot(test.q15,aes(x=code,y=score)) + geom_boxplot() + geom_jitter(aes(color=code),height=0.2) +
  scale_y_continuous("Comprehension Score",breaks=c(0:9)) + scale_x_discrete("Primary Code") + theme_bw() +
  theme(axis.text.x=element_text(angle=45,vjust=0.95),legend.position="none")

# Test using Kruskal-Wallis:
kruskal.test(score ~ code, test.q15)

# We follow up with 6 post-hoc tests (below) using Mann-Whitney U:
agree <- droplevels(test.q15[test.q15$code=="agree",])
depends <- droplevels(test.q15[test.q15$code=="depends",])
disagree <- droplevels(test.q15[test.q15$code=="disagree",])
not <- droplevels(test.q15[test.q15$code=="not understood",])
none <- droplevels(test.q15[test.q15$code=="none",])

# Agree vs. depends
wilcox.test(score ~ code, as.data.frame(rbind(agree,depends)))

# Agree vs. disagree
wilcox.test(score ~ code, as.data.frame(rbind(agree,disagree)))

# Agree vs. not understood
wilcox.test(score ~ code, as.data.frame(rbind(agree,not)))

# Agree vs. none
wilcox.test(score ~ code, as.data.frame(rbind(agree,none)))

# Depends vs. disagree
wilcox.test(score ~ code, as.data.frame(rbind(depends,disagree)))

# Depends vs. none
wilcox.test(score ~ code, as.data.frame(rbind(depends,none)))

# Disagree vs. none
wilcox.test(score ~ code, as.data.frame(rbind(disagree,none)))

# Not understood vs. none
wilcox.test(score ~ code, as.data.frame(rbind(not,none)))


## Further exploration of free response opinion (Q15)
# Here we look further into participants' opinion of the rule, specifically with regards to the assigned secondary codes.
# Recall that responses may have been assigned multiple secondary codes, but that depending on content, not all responses were assigned one. 
# First we look at comprehension score in the context of whether or not participants (1) liked the rule, and (2) thought the rule was fair. 

l <- grep("; like",q15.codes[rownames(all.scenarios),]$code)
dl <- grep("; dislike",q15.codes[rownames(all.scenarios),]$code)
f <- grep("; fair",q15.codes[rownames(all.scenarios),]$code)
uf <- grep("; unfair",q15.codes[rownames(all.scenarios),]$code)
m <- grep("; merit",q15.codes[rownames(all.scenarios),]$code)
alt <- grep("; alternative",q15.codes[rownames(all.scenarios),]$code)
reg <- grep("; regurgitation",q15.codes[rownames(all.scenarios),]$code)

l.dl <- rbind(data.frame(droplevels(test.q15[l,]),s.code=rep("like",length(l))),
              data.frame(droplevels(test.q15[dl,]),s.code=rep("dislike",length(dl))))
f.uf <- rbind(data.frame(droplevels(test.q15[f,]),s.code=rep("fair",length(f))),
              data.frame(droplevels(test.q15[uf,]),s.code=rep("unfair",length(uf))))
table(rbind(l.dl,f.uf)[,"s.code"])

p.l.dl <- ggplot(l.dl,aes(x=s.code,y=score)) + geom_boxplot() + geom_jitter(aes(color=s.code),height=0.2) + theme_bw() + 
  scale_y_continuous("Comprehension Score",breaks=c(0:9)) + scale_x_discrete("Secondary Code") + theme(legend.position="none") + 
  labs(title="Like vs. Dislike")
p.f.uf <- ggplot(f.uf,aes(x=s.code,y=score)) + geom_boxplot() + geom_jitter(aes(color=s.code),height=0.2) + theme_bw() + 
  scale_y_continuous(" ",breaks=c(0:9)) + scale_x_discrete("Secondary Code") + theme(legend.position="none") + 
  labs(title="Fair vs. Unfair")

grid.arrange(p.l.dl,p.f.uf,nrow=1)

# We run Mann-Whitney U tests on the above:
# Like vs. dislike
wilcox.test(score ~ s.code,l.dl)

# Fair vs. unfair
wilcox.test(score ~ s.code,f.uf)

# Visualizing comprehension scores in the context of the remaining three secondary codes (and lack thereof):
m.nm <- rbind(data.frame(droplevels(test.q15[m,]),s.code=rep("merit",length(m))),
              data.frame(droplevels(test.q15[-m,]),s.code=rep("(no merit)",n.respondents-length(m))))
alt.nalt <- rbind(data.frame(droplevels(test.q15[alt,]),s.code=rep("alternative",length(alt))),
                  data.frame(droplevels(test.q15[-alt,]),s.code=rep("(no alternative)",n.respondents-length(alt))))
reg.nreg <- rbind(data.frame(droplevels(test.q15[reg,]),s.code=rep("regurgitation",length(reg))),
                  data.frame(droplevels(test.q15[-reg,]),s.code=rep("(no regurgitation)",n.respondents-length(reg))))
table(m.nm$s.code)
table(alt.nalt$s.code)
table(reg.nreg$s.code)

p.m.nm <- ggplot(m.nm,aes(x=s.code,y=score)) + geom_boxplot() + geom_jitter(aes(color=s.code),height=0.2) + theme_bw() + 
  scale_y_continuous("Comprehension Score",breaks=c(0:9)) + scale_x_discrete(" ") + theme(legend.position="none") + labs(title="Merit")
p.alt.nalt <- ggplot(alt.nalt,aes(x=s.code,y=score)) + geom_boxplot() + geom_jitter(aes(color=s.code),height=0.2) + theme_bw() + 
  scale_y_continuous(" ",breaks=c(0:9)) + scale_x_discrete("Secondary Code") + theme(legend.position="none") + labs(title="Alternative")
p.reg.nreg <- ggplot(reg.nreg,aes(x=s.code,y=score)) + geom_boxplot() + geom_jitter(aes(color=s.code),height=0.2) + theme_bw() + 
  scale_y_continuous("",breaks=c(0:9)) + scale_x_discrete(" ") + theme(legend.position="none") + labs(title="Regurgitation")

grid.arrange(p.m.nm,p.alt.nalt,p.reg.nreg,nrow=1)

# We run Mann-Whitney U tests on the above except "regurgitation," due to small sample size (n=7):
# Merit vs. (no merit)
wilcox.test(score ~ s.code,m.nm)

# Alternative vs. (no alternative)
wilcox.test(score ~ s.code,alt.nalt)

# Regurgitation vs. (no regurgitation)
#wilcox.test(score ~ s.code,reg.nreg)



# ====== EFFECTS OF SCENARIO ====== #

## Comprehension score vs. scenario
kruskal.test(score ~ scenario, all.scenarios) 

## Perceived realism (Q1, scale 1-5) vs. scenario
chisq.test(all.scenarios$Q1,all.scenarios$scenario)
table(all.scenarios$Q1,all.scenarios$scenario)

## Coded free response rule explanation (Q12) vs. scenario
chisq.test(test.q12$code,test.q12$scenario)
table(test.q12$code,test.q12$scenario)

## Self report of rule understanding (Q13, scale of 1-5) vs. scenario
chisq.test(all.scenarios$Q13,all.scenarios$scenario)
table(all.scenarios$Q13,all.scenarios$scenario)

## Self report of rule usage (Q14, scale 1-3) vs. scenario
chisq.test(all.scenarios$Q14,all.scenarios$scenario)
table(all.scenarios$Q14,all.scenarios$scenario)

## Coded free response opinion on rule (Q15) vs. scenario
chisq.test(test.q15$code,test.q15$scenario)
table(test.q15$code,test.q15$scenario)

# The only measure that may vary with scenario is perceived realism (note that *p* > 0.05, 
# and in the GLM in section 4.2 there is no such association), 
# in addition to hours of effort spent on the decision (see secion 3.4). 
# This suggests that while there may be some differences in the way distinct scenarios are perceived, 
# these differences do not appear to have any bearing on comprehension and application of the rule.



# ====== NON-COMPLIANT PARTICIPANTS ====== #

# The question still remains as to why "non-compliant participants," 
# i.e. those who in Q14 admitted to NOT using only the rule to answer the relevant questions, 
# behave the way they do. In appears that these participants are less likely to self-report 
# high understanding of the rule as determined by Q13 (see below).
# Moreover, better self-reported understanding of the rule (Q13) appears to correlate
# with correctly explaining the rule (Q12, see below). 
# Finally, negative participant sentiment towards the rule (Q15) appears to be associated 
# with rule use (Q14, see below) and rule understanding (Q13, see below).

## Test 1: self-report of rule usage (Q14) vs. self-report of rule understanding (Q13)
ggplot(all.scenarios,aes(x=Q14,y=Q13)) + geom_jitter(aes(color=Q14)) + theme_bw() + 
  scale_x_discrete("Self-report of rule use (Q14)",labels=c("Rule only (1)","Combo (2)","Personal\nnotions (3)")) + 
  scale_y_discrete("Self-report of rule understanding (Q13)") + theme(legend.position="none")

# Using the Kruskal-Wallis test:
#chisq.test(all.scenarios$Q14, all.scenarios$Q13)
kruskal.test(as.numeric(Q13) ~ Q14, all.scenarios)


## Test 2: self-report of rule understanding (Q13) vs. free response explanation (Q12)
test.q12.q13 <- data.frame(Q12=q12.codes[rownames(all.scenarios),]$code,
                           Q13=all.scenarios$Q13)
rownames(test.q12.q13) <- rownames(all.scenarios)
ggplot(test.q12.q13,aes(x=Q13)) + geom_histogram(stat="count",color="black",fill="white") + 
  facet_grid(. ~ Q12) + scale_x_discrete("Self-report of rule understanding (Q13)") + 
  scale_y_continuous("# of Participants") + theme_bw()

# We assess the relationship using a the Kruskal-Wallis test:
kruskal.test(as.numeric(Q13) ~ Q12,test.q12.q13)

# We follow up with six post-hoc tests using Mann-Whitney U:
correct <- droplevels(test.q12.q13[test.q12.q13$Q12=="correct",])
partial <- droplevels(test.q12.q13[test.q12.q13$Q12=="partially correct",])
neither <- droplevels(test.q12.q13[test.q12.q13$Q12=="neither",])
incorrect <- droplevels(test.q12.q13[test.q12.q13$Q12=="incorrect",])
none <- droplevels(test.q12.q13[test.q12.q13$Q12=="none",])

# Correct vs. partially correct
wilcox.test(as.numeric(Q13) ~ Q12, as.data.frame(rbind(correct,partial)))

# Correct vs. neither
wilcox.test(as.numeric(Q13) ~ Q12, as.data.frame(rbind(correct,neither)))

# Correct vs. incorrect
wilcox.test(as.numeric(Q13) ~ Q12, as.data.frame(rbind(correct,incorrect)))

# Partially correct vs. neither
wilcox.test(as.numeric(Q13) ~ Q12, as.data.frame(rbind(partial,neither)))

# Partially correct vs. incorrect
wilcox.test(as.numeric(Q13) ~ Q12, as.data.frame(rbind(partial,incorrect)))

# Neither vs. incorrect
wilcox.test(as.numeric(Q13) ~ Q12, as.data.frame(rbind(neither,incorrect)))


## Test 3: self-report of rule usage (Q14) vs. free response opinion (Q15)
q14.q15 <- data.frame(test.q15[rownames(all.scenarios),],Q14=all.scenarios$Q14)
table(q14.q15[,c("Q14","code")])

ggplot(q14.q15,aes(x=Q14)) + geom_histogram(stat="count",color="black",fill="white") + 
  theme_bw() + facet_grid(. ~ code) + scale_y_continuous("# of Participants") + 
  scale_x_discrete("Self-report of rule use (Q14)") #,labels=c("Rule only (1)","Combo (2)","Personal\nnotions (3)"))

ggplot(q14.q15,aes(x=Q14,y=score)) + geom_boxplot() + geom_jitter(aes(color=Q14),height=0.2) + theme_bw() + 
  facet_grid(. ~ code) + scale_x_discrete("Self-report of rule use (Q14)") + #,labels=c("Rule only (1)","Combo (2)","Personal\nnotions (3)")) + 
  scale_y_continuous("Comprehension Score",breaks=c(0:9)) + theme(legend.position="none")

# Using Kruskal-Wallis test:
#chisq.test(q14.q15$Q14,q14.q15$code)
kruskal.test(Q14 ~ code,q14.q15)


## Test 4: self-report of rule understanding (Q13) vs. free response opinion (Q15)
q13.q15 <- data.frame(test.q15[rownames(all.scenarios),],Q13=all.scenarios$Q13)
table(q13.q15[,c("Q13","code")])

ggplot(q13.q15,aes(x=Q13)) + geom_histogram(stat="count",color="black",fill="white") + 
  theme_bw() + facet_grid(. ~ code) + scale_y_continuous("# of Participants") + 
  scale_x_discrete("Self-report of rule understanding (Q13)") #,labels=c("Rule only (1)","Combo (2)","Personal\nnotions (3)"))

# Using Kruskal-Wallis test:
#chisq.test(q13.q15$Q13,q13.q15$code)
kruskal.test(Q13 ~ code,q13.q15)



# ====== MULTI VISUALIZATION ====== #

quad <- data.frame(score=all.scenarios$score,
                   Q12=q12.codes[rownames(all.scenarios),]$code,
                   Q13=all.scenarios$Q13,
                   Q14=all.scenarios$Q14,
                   Q15=q15.codes[rownames(all.scenarios),]$p.code)
#quad$Q13.condensed <- factor(gsub("[12]","Understood rule",gsub("[345]","Did not understand rule",quad$Q13)),
#                             levels=c("Understood rule","Did not understand rule"))
quad$Q13.condensed <- factor(gsub("[12]","Understood rule",gsub("[45]","Did not understand rule",gsub("3","Neither",quad$Q13))),
                             levels=c("Understood rule","Neither","Did not understand rule"))
quad$Q14.condensed <- factor(gsub("1","Used rule only",gsub("[23]","Used personal notions of fairness",quad$Q14)),
                             levels=c("Used rule only","Used personal notions of fairness"))
quad <- quad[-which(is.na(quad$Q14)),]

table(quad$Q13.condensed,quad$Q14.condensed)
ggplot(quad,aes(x=Q15,y=score)) + geom_boxplot() + geom_jitter(aes(color=Q15),height=0.2) + 
  scale_x_discrete("Q15 (opinion of rule) Primary Code",labels=gsub(" ","\n",levels(quad$Q15))) +
  scale_y_continuous("Comprehension Score",breaks=c(0:9)) + theme_bw() + 
  facet_grid(Q13.condensed ~ Q14.condensed) + theme(legend.position="none")












#######################################
#######################################
#                                     #
#            Study-2 (Full)           #
#        Published at ICML 2020       #
#                                     #
#######################################
#######################################



# ================================== #
#           INITAL DATA GRAB         #
# ================================== #

# ====== GET DATA ====== #

setwd("<path to data>")
all.defs <- read.table("<data file.txt>",header=TRUE,quote="",sep="\t")
rownames(all.defs) <- all.defs$CintID


# ====== REFORMATTING ====== #

# Likert questions (5pt scale)
all.defs$Q1 <- factor(all.defs$Q1)
all.defs$Q13 <- factor(all.defs$Q13)
all.defs$Q15 <- factor(all.defs$Q15)
all.defs$Q16 <- factor(all.defs$Q16)

# Self-report of usage (3 categories)
all.defs$Q14 <- factor(all.defs$Q14)

# Demographics
all.defs$gender <- factor(all.defs$gender)
all.defs$eth <- factor(all.defs$eth)
all.defs$edu <- factor(all.defs$edu)

# Domain experience
all.defs$exp.hr <- factor(all.defs$exp.hr)
all.defs$exp.mng <- factor(all.defs$exp.mng)
all.defs$exp.edu <- factor(all.defs$exp.edu)
all.defs$exp.it <- factor(all.defs$exp.it)
all.defs$exp.cs <- factor(all.defs$exp.cs)
all.defs$exp.ml <- factor(all.defs$exp.ml)

# Fairness definitions
all.defs$def <- factor(all.defs$def,levels=c("DP","FNR","FPR","EO"))
table(all.defs$def)

#  DP FNR FPR  EO 
#  95  85  88  81


# ====== SCORING ====== #

dp.answers <- c(2,2,1,2,1,2,1,2,2)
dp.answers <- matrix(dp.answers,nrow=table(all.defs$def)["DP"],ncol=length(dp.answers),byrow=TRUE)
fnr.answers <- c(2,1,1,2,2,1,1,2,2)
fnr.answers <- matrix(fnr.answers,nrow=table(all.defs$def)["FNR"],ncol=length(fnr.answers),byrow=TRUE)
fpr.answers <- c(2,1,1,1,2,1,1,2,2)
fpr.answers <- matrix(fpr.answers,nrow=table(all.defs$def)["FPR"],ncol=length(fpr.answers),byrow=TRUE)
eo.answers <- c(2,1,1,1,2,1,1,2,2)
eo.answers <- matrix(eo.answers,nrow=table(all.defs$def)["EO"],ncol=length(eo.answers),byrow=TRUE)

answer.key <- rbind(dp.answers,fnr.answers,fpr.answers,eo.answers)
colnames(answer.key) <- c("Q3","Q4","Q5","Q6","Q7","Q8","Q9","Q10","Q11")
rownames(answer.key) <- rownames(all.defs)

# Comprehension score (CS)
all.defs$score <- rowSums(all.defs[,c(8:16)]==answer.key,na.rm=TRUE)
score.sheet <- data.frame(all.defs[,c(8:16)]==answer.key)
rm(list=ls()[grep(".answers",ls())])



# ================================== #
#         INTERNAL VALIDITY          #
# ================================== #

### Resources for internal validity
### https://mattchoward.com/introduction-to-cronbachs-alpha/
### https://mattchoward.com/calculating-cronbachs-alpha-in-r/
### https://rpubs.com/hauselin/reliabilityanalysis
### https://www.researchgate.net/post/Is_it_correct_to_use_Cronbach_s_Alpha_to_assess_the_internal_consistency_of_a_questionnaire_with_binary_data_yes_no
### https://en.wikipedia.org/wiki/Item_response_theory

# Assessed using two measures: 
# Cronbach's alpha ("raw_alpha")
# Item-total correlation ("r.cor")
responses <- score.sheet[,colnames(answer.key)]
responses[responses=="TRUE"] <- 1
responses$def <- all.defs$def

psych::alpha(droplevels(responses[,c(1:9)]),check.keys=TRUE)  # all fairness defintions

psych::alpha(droplevels(responses[all.defs$def=="DP",c(1:9)]),check.keys=TRUE)  # DP
psych::alpha(droplevels(responses[all.defs$def=="FNR",c(1:9)]),check.keys=TRUE)  # FNR
psych::alpha(droplevels(responses[all.defs$def=="FPR",c(1:9)]),check.keys=TRUE)  # FPR
psych::alpha(droplevels(responses[all.defs$def=="EO",c(1:9)]),check.keys=TRUE)  # EO

# Poor item-total correlations
# DP: Q5, Q6
# FNR: Q6, Q7, Q8, Q9, Q10, Q11
# FPR: Q7, Q9
# EO: Q4, Q6, Q9

# Use ITERATIVE approach to drop questions
# Drop question with lowest item-total correlation, reassess, and continue till no items have item-total cor < 0.3
# This approach yields the following questions to be DROPPED per defintion:
# DP: Q5, Q6
# FNR: Q6, Q8, Q9, Q10, Q11
# FPR: Q7, Q9
# EO: Q4, Q6

# Reassess validity after dropping the above
psych::alpha(droplevels(responses[all.defs$def=="DP",c(1:3,6:9)]),check.keys=TRUE)  # DP
psych::alpha(droplevels(responses[all.defs$def=="FNR",c(1:3,5)]),check.keys=TRUE)  # FNR
psych::alpha(droplevels(responses[all.defs$def=="FPR",c(1:4,6,8,9)]),check.keys=TRUE)  # FPR
psych::alpha(droplevels(responses[all.defs$def=="EO",c(1,3,5:9)]),check.keys=TRUE)  # EO

# Update scores
dp.ids <- rownames(all.defs)[all.defs$def=="DP"]
fnr.ids <- rownames(all.defs)[all.defs$def=="FNR"]
fpr.ids <- rownames(all.defs)[all.defs$def=="FPR"]
eo.ids <- rownames(all.defs)[all.defs$def=="EO"]

dp.adj.score <- all.defs[dp.ids,c("Q3","Q4","Q7","Q8","Q9","Q10","Q11")] == answer.key[dp.ids,c("Q3","Q4","Q7","Q8","Q9","Q10","Q11")]
fnr.adj.score <- all.defs[fnr.ids,c("Q3","Q4","Q5","Q7")] == answer.key[fnr.ids,c("Q3","Q4","Q5","Q7")]
fpr.adj.score <- all.defs[fpr.ids,c("Q3","Q4","Q5","Q6","Q8","Q10","Q11")] == answer.key[fpr.ids,c("Q3","Q4","Q5","Q6","Q8","Q10","Q11")]
eo.adj.score <- all.defs[eo.ids,c("Q3","Q5","Q7","Q8","Q9","Q10","Q11")] == answer.key[eo.ids,c("Q3","Q5","Q7","Q8","Q9","Q10","Q11")]

dp.adj.score <- rowSums(dp.adj.score,na.rm=TRUE)/7
fnr.adj.score <- rowSums(fnr.adj.score,na.rm=TRUE)/4
fpr.adj.score <- rowSums(fpr.adj.score,na.rm=TRUE)/7
eo.adj.score <- rowSums(eo.adj.score,na.rm=TRUE)/7
all.defs$original.score <- all.defs$score
all.defs$score <- c(dp.adj.score,fnr.adj.score,fpr.adj.score,eo.adj.score)



# ================================== #
#               ANALYSES             #
# ================================== #

# ====== COMPREHENSION SCORE ====== #

# Descriptive Statistics
summary(all.defs$score)
sd(all.defs$score)
n.respondents <- nrow(all.defs)
ggplot(all.defs,aes(x=def,y=score)) + geom_boxplot() + theme_bw() + #geom_jitter() + 
  theme(legend.position="none",axis.title.x=element_text(size=10),axis.title.y=element_text(size=10)) + 
  scale_x_discrete("Fairness Defintiion",labels=paste(gsub(" and","\nand",gsub(", ", ",\n", levels(all.defs$def))),"\n(",table(all.defs$def),")",sep="")) + 
  scale_y_continuous("Comprehension Score",breaks=c(0,0.25,0.50,0.75,1))
ggsave("scores.png",height=2.2,width=3.65)


# Proportion of respondents (out of 349) answering each question correctly:
x <- data.frame(Percent=colSums(score.sheet,na.rm=TRUE)/n.respondents)
x$Percent <- percent(x$Percent)
colnames(x) <- c("Percent Correct")
x
#rm(x)

# Respondents answering each question correctly, split by definition
# (each column comprises all 349 respondents)
score.sheet$CintID <- all.defs$CintID
score.sheet$def <- all.defs$def
score.sheet.melt <- melt(score.sheet,id.vars=c("CintID","def"))
colnames(score.sheet.melt) <- c("CintID","def","question","value")
score.sheet.melt$value <- gsub("FALSE","Incorrect",gsub("TRUE","Correct",score.sheet.melt$value))
ggplot(score.sheet.melt,aes(x=value)) + geom_bar(stat="count",color="black",aes(fill=value)) + 
  scale_fill_manual(values=c("white","grey"),name="") + scale_x_discrete("") + scale_y_continuous("# of Participants") + 
  facet_grid(def ~ question) + theme_bw() + theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())


# ====== REALISM & IMPORTANCE ====== #

# Realism (Q1)
ggplot(all.defs,aes(x=Q1)) + geom_bar() + facet_wrap(def ~ .) + theme_bw()
kruskal.test(Q1 ~ def,all.defs)

# Importance (Q2 hours of effort) - maybe there's a difference?
ggplot(all.defs,aes(x=Q2)) + geom_histogram() + facet_wrap(def ~ .) + theme_bw() # outlier at 8000
ggplot(all.defs,aes(x=Q2)) + geom_histogram() + facet_wrap(def ~ .) + scale_x_continuous(limits=c(0,1000)) + theme_bw() # still outliers...
ggplot(all.defs,aes(x=Q2)) + geom_histogram() + facet_wrap(def ~ .) + scale_x_continuous(limits=c(0,500)) + theme_bw() # still outliers...
ggplot(all.defs,aes(x=Q2)) + geom_histogram() + facet_wrap(def ~ .) + scale_x_continuous(limits=c(0,300)) + theme_bw()
summary(all.defs$Q2)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#    0.0     5.0    12.0    59.4    40.0  8000.0       8    
kruskal.test(all.defs$Q2[all.defs$Q2 < 300] ~ all.defs$def[all.defs$Q2 < 300],all.defs) #******
kruskal.test(all.defs$Q2[all.defs$Q2 < 100] ~ all.defs$def[all.defs$Q2 < 100],all.defs)


# ====== DEMOGRAPHIC ANALYSIS ====== #

# CS x gender
table(all.defs$gender)
kruskal.test(score ~ gender,all.defs)

# CS x ethnicity
table(all.defs$eth)
kruskal.test(score ~ eth,all.defs)

# CS x domain experience
cor.test(all.defs$score,as.numeric(all.defs$exp.hr),method="spearman")
cor.test(all.defs$score,as.numeric(all.defs$exp.mng),method="spearman")
cor.test(all.defs$score,as.numeric(all.defs$exp.edu),method="spearman")
cor.test(all.defs$score,as.numeric(all.defs$exp.it),method="spearman")
cor.test(all.defs$score,as.numeric(all.defs$exp.cs),method="spearman")
cor.test(all.defs$score,as.numeric(all.defs$exp.ml),method="spearman")

# GLMs - education
test.demo <-  data.frame(score=all.defs$score,
                         gender=all.defs$gender,
                         age=(2020-all.defs$YOB),
                         eth=all.defs$eth,
                         edu=all.defs$edu,
                         def=all.defs$def)
rownames(test.demo) <- rownames(all.defs)
test.demo$edu <- as.character(as.numeric(test.demo$edu))

test.demo$edu <- gsub("1","Less than HS",test.demo$edu)
test.demo$edu <- gsub("2","HS",test.demo$edu)
test.demo$edu <- gsub("[345]","Post-secondary, no Bachelor's",test.demo$edu)
test.demo$edu <- gsub("[678]","Bachelor's and above",test.demo$edu)
test.demo$edu <- factor(test.demo$edu,levels=c("Less than HS","HS","Post-secondary, no Bachelor's","Bachelor's and above"))
test.demo <- droplevels(test.demo[-c(which(is.na(all.defs$edu))),]) # 2 participants did not report education level

ggplot(test.demo,aes(x=edu,y=score)) + geom_boxplot() + theme_bw() +
  theme(legend.position="none",axis.title.x=element_text(size=10),axis.title.y=element_text(size=10)) + 
  scale_x_discrete("Education level",labels=paste(gsub(" and","\nand",gsub(", ", ",\n", levels(test.demo$edu))),
                                                  "\n(",table(test.demo$edu),")",sep="")) + 
  scale_y_continuous("Comprehension Score",breaks=c(0,0.25,0.5,0.75,1))
ggsave("edu.png",height=2.2,width=3.65)

library(broom)
library(bbmle)
library(lm.beta)
library(rsq)

m1 <- glm(score ~ gender + age + eth + edu + def, test.demo, family="gaussian")
m2 <- glm(score ~ gender + age + eth + edu, test.demo, family="gaussian")
m3 <- glm(score ~ gender + age + eth, test.demo, family="gaussian")
m4 <- glm(score ~ edu + def, test.demo, family="gaussian")   #****** 
m5 <- glm(score ~ edu, test.demo, family="gaussian")
m6 <- glm(score ~ gender + age + eth + def, test.demo, family="gaussian")
m7 <- glm(score ~ def, test.demo, family="gaussian")
m8 <- glm(score ~ gender + age + edu, test.demo, family="gaussian")
m9 <- glm(score ~ gender + age + def, test.demo, family="gaussian")
m10 <- glm(score ~ gender + edu, test.demo, family="gaussian")
m11 <- glm(score ~ age + edu, test.demo, family="gaussian")
model.names <- c("gender + age + eth + edu + def",
                 "gender + age + eth + edu",
                 "gender + age + eth",
                 "edu + def",
                 "edu",
                 "gender + age + eth + def",
                 "def",
                 "gender + age + edu",
                 "gender + age + def",
                 "gender + edu",
                 "age + edu")

bbmle::AICtab(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,base=TRUE,weights=TRUE,sort=FALSE,mnames=model.names)
summary(m4) # since this was fit using a gaussian distribution, estimates are effect size

# Formatted regression table
cbind(summary(m4)$coefficients[,c(1,4)],confint(m4))[,c(1,3,4,2)]


# ====== RULE EXPLANATION [Free response] (Q12) ====== #

# Get qualitative codes
q12.codes <- read.table("<q12 coding data file.txt>",quote="",header=TRUE,sep="\t",row.names=1)

# Remove secondary codes
q12.codes$pcode <- factor(gsub(";.*","",q12.codes$codes),levels=c("correct","partially correct","neither","incorrect","none"))

# Make relevant data frame
test.q12 <- data.frame(score=all.defs$score,
                       code=q12.codes[rownames(all.defs),"pcode"],
                       def=all.defs$def)
rownames(test.q12) <- rownames(all.defs)
table(test.q12$code)

ggplot(test.q12,aes(x=code,y=score)) + geom_boxplot() + scale_y_continuous("Comprehension Score",c(0,0.25,0.5,0.75,1)) + theme_bw() +
  scale_x_discrete("Assigned Code (Q12)",labels=paste(gsub(" ", "\n", levels(test.q12$code)),"\n(",table(test.q12$code),")",sep="")) + 
  theme(legend.position="none",axis.title.x=element_text(size=8),axis.title.y=element_text(size=8))
ggsave("q12.png",height=1.9,width=3)

# Kruskal-Wallis
kruskal.test(score ~ code, test.q12) #******

# Post-hoc Mann-Whitney U
correct <- droplevels(test.q12[test.q12$code=="correct",])
partial <- droplevels(test.q12[test.q12$code=="partially correct",])
neither <- droplevels(test.q12[test.q12$code=="neither",])
incorrect <- droplevels(test.q12[test.q12$code=="incorrect",])
none <- droplevels(test.q12[test.q12$code=="none",])
wilcox.test(score ~ code, as.data.frame(rbind(correct,partial))) #******
wilcox.test(score ~ code, as.data.frame(rbind(correct,neither))) #******
wilcox.test(score ~ code, as.data.frame(rbind(correct,incorrect))) #******
wilcox.test(score ~ code, as.data.frame(rbind(correct,none))) #******

wilcox.test(score ~ code, as.data.frame(rbind(partial,incorrect)))
wilcox.test(score ~ code, as.data.frame(rbind(partial,neither))) #******
wilcox.test(score ~ code, as.data.frame(rbind(partial,none))) #******

wilcox.test(score ~ code, as.data.frame(rbind(neither,incorrect)))
wilcox.test(score ~ code, as.data.frame(rbind(neither,none)))

wilcox.test(score ~ code, as.data.frame(rbind(incorrect,none)))


# ====== RULE UNDERSTANDING (Q13) ====== #

ggplot(all.defs,aes(x=Q13,y=score)) + geom_boxplot() +  theme_bw() +
  scale_x_discrete("Self-report of understanding (Q13)",limits=rev(levels(all.defs$Q13)),
                   labels=rev(paste(c("strongly\nagree","agree","neither\nagree\nnor disagree","disagree","strongly\ndisagree"),
                                    "\n(",table(all.defs$Q13),")",sep=""))) + 
  scale_y_continuous("Comprehension Score",breaks=c(0,0.25,0.5,0.75,1)) + 
  theme(legend.position="none",axis.title.x=element_text(size=8),axis.title.y=element_text(size=8))
ggsave("q13.png",height=1.9,width=3)

# Spearman correlation - multiply by -1 to reverse the order of Q13 responses
# this way if those who report higher understanding (1 on Likert scale) also have higher score, there will be a positive correlation
cor.test(as.numeric(all.defs$Q13)*-1,all.defs$score,method="spearman")


# ====== RULE APPLICATION (Q14) ====== #

test.q14 <- droplevels(all.defs[!(is.na(all.defs$Q14)),])
ggplot(test.q14,aes(x=as.factor(Q14),y=score)) + geom_boxplot() + theme_bw() + scale_y_continuous("Comprehension Score",breaks=c(0,0.25,0.5,0.75,1)) + 
  scale_x_discrete("Self-report of usage (Q14)",labels=paste(c("rule\nonly","combination","personal\nnotions"),"\n(",table(test.q14$Q14),")",sep="")) + 
  theme(legend.position="none",axis.title.x=element_text(size=8),axis.title.y=element_text(size=8))
ggsave("q14.png",height=1.9,width=3)

# Kruskal-Wallis
kruskal.test(score ~ Q14,all.defs) #******

# Post-hoc Mann-Whitney U
rule <-  droplevels(all.defs[all.defs$Q14==1,])
combo <- droplevels(all.defs[all.defs$Q14==2,])
personal <- droplevels(all.defs[all.defs$Q14==3,])
wilcox.test(score ~ Q14, as.data.frame(rbind(rule,combo))) #******
wilcox.test(score ~ Q14, as.data.frame(rbind(rule,personal))) #******
wilcox.test(score ~ Q14, as.data.frame(rbind(personal,combo)))


# ====== RULE LIKING (Q15) ====== #

ggplot(all.defs,aes(x=Q15,y=score)) + geom_boxplot() +  theme_bw() +
  scale_x_discrete("Self-report of liking (Q15)",limits=rev(levels(all.defs$Q15)),
                   labels=rev(paste(c("strongly\nagree","agree","neither\nagree\nnor disagree","disagree","strongly\ndisagree"),
                                    "\n(",table(all.defs$Q15),")",sep=""))) + 
  scale_y_continuous("Comprehension Score",breaks=c(0,0.25,0.5,0.75,1)) + 
  theme(legend.position="none",axis.title.x=element_text(size=8),axis.title.y=element_text(size=8))
ggsave("q15.png",height=1.9,width=3)

# Spearman correlation - multiply by -1 to reverse the order of Q15 responses
# this way if those who report higher liking (1 on Likert scale) also have higher score, there will be a positive correlation
cor.test(as.numeric(all.defs$Q15)*-1,all.defs$score,method="spearman") #******


# ====== RULE AGREEMENT (Q16) ====== #

ggplot(all.defs,aes(x=Q16,y=score)) + geom_boxplot() +  theme_bw() +
  scale_x_discrete("Self-report of agreement (Q16)",limits=rev(levels(all.defs$Q16)),
                   labels=rev(paste(c("strongly\nagree","agree","neither\nagree\nnor disagree","disagree","strongly\ndisagree"),
                                    "\n(",table(all.defs$Q16),")",sep=""))) + 
  scale_y_continuous("Comprehension Score",c(0,0.25,0.5,0.75,1)) + 
  theme(legend.position="none",axis.title.x=element_text(size=8),axis.title.y=element_text(size=8))
ggsave("q16.png",height=1.9,width=3)

# Spearman correlation - multiply by -1 to reverse the order of Q16 responses
# this way if those who report higher agreement with rule (1 on Likert scale) also have higher score, there will be a positive correlation
cor.test(as.numeric(all.defs$Q16)*-1,all.defs$score,method="spearman") #******


# ====== NON-COMPLIANCE ====== #
# geom_bar position = stack vs. fill

# Two categories: compliant (n=174), non-compliant (n=174)
non.comp <- data.frame(Q12=test.q12$code,
                       Q13=all.defs$Q13,
                       Q14=factor(gsub("3","2",all.defs$Q14)),
                       Q15=all.defs$Q15,
                       Q16=all.defs$Q16)
rownames(non.comp) <- rownames(all.defs)
non.comp <- non.comp[!is.na(non.comp$Q14),]

q13.colors <- c("darkgreen","seagreen2","grey90","tomato","firebrick3")
q13.names <- c("SA","A","N","D","SD")
q12.colors <- c("darkgreen","seagreen2","grey90","firebrick3","grey50")
q12.names <- c("C","PC","N","I","NA")
q15.colors <- c("darkgreen","seagreen2","grey90","tomato","firebrick3")
q15.names <- c("SA","A","N","D","SD")
q16.colors <- c("darkgreen","seagreen2","grey90","tomato","firebrick3")
q16.names <- c("SA","A","N","D","SD")

# Q14 x Q12
ggplot(non.comp,aes(x=Q14)) + geom_bar(stat="count",position="fill",width=0.5,aes(fill=Q12)) + theme_bw() + 
  scale_fill_manual("Q12 code",values=q12.colors,labels=q12.names) + scale_y_continuous("Proportion of participants") + 
  scale_x_discrete(labels=c("compliant","non-\ncompliant")) + 
  theme(legend.position="bottom",legend.box.spacing=unit(-0,"cm"),legend.key.size=unit(0.35,"cm"),
        legend.title=element_text(size=8),legend.text=element_text(size=8),legend.justification="left",
        axis.title.y=element_blank(),axis.title.x=element_text(size=8)) + coord_flip() + guides(fill=guide_legend(reverse=TRUE))
ggsave("nc_q12q14.png",height=1.25,width=3)

# Q14 x Q13
ggplot(non.comp,aes(x=Q14)) + geom_bar(stat="count",position="fill",width=0.5,aes(fill=Q13)) + theme_bw() + 
  scale_fill_manual("Q13 rating",values=q13.colors,labels=q13.names) + scale_y_continuous("Proportion of participants") + 
  scale_x_discrete(labels=c("compliant","non-\ncompliant")) + 
  theme(legend.position="bottom",legend.box.spacing=unit(-0,"cm"),legend.key.size=unit(0.35,"cm"),
        legend.title=element_text(size=8),legend.text=element_text(size=8),legend.justification="left",
        axis.title.y=element_blank(),axis.title.x=element_text(size=8)) + coord_flip() + guides(fill=guide_legend(reverse=TRUE))
ggsave("nc_q13q14.png",height=1.25,width=3)

# Q14 x Q15
ggplot(non.comp,aes(x=Q14)) + geom_bar(stat="count",position="fill",width=0.5,aes(fill=Q15)) + theme_bw() + 
  scale_fill_manual("Q15 rating",values=q15.colors,labels=q15.names) + scale_y_continuous("Proportion of participants") + 
  scale_x_discrete(labels=c("compliant","non-\ncompliant")) + 
  theme(legend.position="bottom",legend.box.spacing=unit(-0,"cm"),legend.key.size=unit(0.35,"cm"),
        legend.title=element_text(size=8),legend.text=element_text(size=8),legend.justification="left",
        axis.title.y=element_blank(),axis.title.x=element_text(size=8)) + coord_flip() + guides(fill=guide_legend(reverse=TRUE))
ggsave("nc_q15q14.png",height=1.25,width=3)

# Q14 x Q16
ggplot(non.comp,aes(x=Q14)) + geom_bar(stat="count",position="fill",width=0.5,aes(fill=Q16)) + theme_bw() + 
  scale_fill_manual("Q16 rating",values=q16.colors,labels=q16.names) + scale_y_continuous("Proportion of participants") + 
  scale_x_discrete(labels=c("compliant","non-\ncompliant")) + 
  theme(legend.position="bottom",legend.box.spacing=unit(-0,"cm"),legend.key.size=unit(0.35,"cm"),
        legend.title=element_text(size=8),legend.text=element_text(size=8),legend.justification="left",
        axis.title.y=element_blank(),axis.title.x=element_text(size=8)) + coord_flip() + guides(fill=guide_legend(reverse=TRUE))
ggsave("nc_q16q14.png",height=1.25,width=3)

chisq.test(non.comp$Q12,non.comp$Q14) #******
kruskal.test(non.comp$Q13,non.comp$Q14) #******
kruskal.test(non.comp$Q15,non.comp$Q14) #******
kruskal.test(non.comp$Q16,non.comp$Q14)


