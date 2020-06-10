
# ICML 2020
# Measuring Non-Expert Comprehension of Machine Learning Fairness Metrics (#1357)
# Supplementary Materials - Analysis code

# DEBJANI SAHA, Candice Schumann, Duncan C. McElFresh, John P. Dickerson, Michelle L. Mazurek, Michael C. Tschantz
# Manuscript: https://arxiv.org/abs/2001.00089
# Code: https://github.com/saharaja/ICML2020-fairness
# Conference: https://icml.cc/Conferences/2020


library(scales)
library(ggplot2)
library(reshape2)
library(grid)
library(gridExtra)
library(psych)


# NOTE:  #****** = significant test





##############################################################################
##############################################################################
#                                                                            #
#                              Study-1 (Pilot)                               #
#                    Abstract published at AIES 2020                         #
#                                                                            #
##############################################################################
##############################################################################




# =================================== #
#            DATA/FORMATTING          #
# =================================== #

# ====== GET DATA ====== #

setwd("C:/Users/Debjani Saha/Documents/University of Maryland/Projects/Alg Fairness/20200127 ICML submission (02-06)")
all.scenarios <- read.table("data/study-1_pilot.txt",header=T,sep="\t",quote="",row.names=1)


# ====== REFORMATTING ====== #

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
levels(all.scenarios$edu)
# [1] "Associate's degree"                                        "Bachelor's degree"                                        
# [3] "HS graduate, diploma or the equivalent (for example: GED)" "Master's degree"                                          
# [5] "Professional or doctoral degree (JD, MD, PhD)"             "Some college credit, no degree"                           
# [7] "Some high school credit, no diploma or equivalent"         "Trade/technical/vocational training"
all.scenarios$edu <- factor(all.scenarios$edu,levels=levels(all.scenarios$edu)[c(7,3,6,8,1,2,4,5)])

# Domain experience
all.scenarios$exp.hr <- as.factor(all.scenarios$exp.hr)
all.scenarios$exp.mng <- as.factor(all.scenarios$exp.mng)
all.scenarios$exp.edu <- as.factor(all.scenarios$exp.edu)
all.scenarios$exp.it <- as.factor(all.scenarios$exp.it)
all.scenarios$exp.cs <- as.factor(all.scenarios$exp.cs)
all.scenarios$exp.ml <- as.factor(all.scenarios$exp.ml)

# Remove participants who took particularly long (over 2hrs)
remove <- which(all.scenarios$Duration > 10000)
all.scenarios <- droplevels(all.scenarios[-remove,])

n.respondents <- dim(all.scenarios)[1]  # n = 147
table(all.scenarios$scenario)
# AP EA HR 
# 41 49 57


# ====== SCORING ====== #

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



# ================================== #
#         INTERNAL VALIDITY          #
# ================================== #

# Assessed using two measures: 
# Cronbach's alpha ("raw_alpha")
# Item-total correlation ("r.cor")
responses <- score.sheet[,colnames(correct.answers)]
responses[responses=="TRUE"] <- 1
psych::alpha(responses)

### Resources for internal validity
### https://mattchoward.com/introduction-to-cronbachs-alpha/
### https://mattchoward.com/calculating-cronbachs-alpha-in-r/
### https://rpubs.com/hauselin/reliabilityanalysis
### https://www.researchgate.net/post/Is_it_correct_to_use_Cronbach_s_Alpha_to_assess_the_internal_consistency_of_a_questionnaire_with_binary_data_yes_no
### https://en.wikipedia.org/wiki/Item_response_theory



# ================================== #
#               ANALYSES             #
# ================================== #

# ====== COMPREHENSION SCORE ====== #

# Descriptive Statistics
summary(all.scenarios$score)
sd(all.scenarios$score)

# Proportion of respondents (out of 147) answering each question correctly:
pct.correct <- data.frame(Percent=colSums(score.sheet)/n.respondents)
pct.correct$Percent <- percent(pct.correct$Percent)
colnames(pct.correct) <- c("Percent Correct")
pct.correct
#     Percent Correct
# Q3            70.1%
# Q4            53.7%
# Q5            62.6%
# Q6            66.0%
# Q7            77.6%
# Q8            64.6%
# Q9            74.1%
# Q10           68.0%
# Q11           82.3%

# Respondents answering each question correctly, split by scenario (each column comprises all 147 respondents)
score.sheet$CintID <- all.scenarios$CintID
score.sheet$scenario <- all.scenarios$scenario
score.sheet.melt <- melt(score.sheet,id.vars=c("CintID","scenario"))
colnames(score.sheet.melt) <- c("CintID","scenario","question","value")
score.sheet.melt$value <- gsub("FALSE","Incorrect",gsub("TRUE","Correct",score.sheet.melt$value))
ggplot(score.sheet.melt,aes(x=value)) + geom_bar(stat="count",color="black",aes(fill=value),width=0.75) + 
  scale_fill_manual(values=c("white","grey"),name="") + scale_x_discrete("") + scale_y_continuous("# of Participants") + 
  facet_wrap(score.sheet.melt$question,nrow=3,ncol=3) + theme_bw() + 
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),legend.position="bottom",
        legend.box.spacing=unit(-0.45,units="cm"),legend.key.size=unit(0.35,"cm"),axis.title.x=element_text(size=8),
        axis.title.y=element_text(size=8),strip.text.x=element_text(margin = margin(0.05,0,0.05,0,"cm")))
ggsave("data/figs-study-1/question_breakdown.png",height=2.75,width=3)


# ====== DEMOGRAPHIC ANALYSIS ====== #

# CS x gender
table(all.scenarios$gender)
kruskal.test(score ~ gender,all.scenarios)

# CS x ethnicity
table(all.scenarios$eth)
kruskal.test(score ~ eth,all.scenarios)

# CS x experience
cor.test(all.scenarios$score,as.numeric(all.scenarios$exp.hr),method="spearman")
cor.test(all.scenarios$score,as.numeric(all.scenarios$exp.mng),method="spearman")
cor.test(all.scenarios$score,as.numeric(all.scenarios$exp.edu),method="spearman")
cor.test(all.scenarios$score,as.numeric(all.scenarios$exp.it),method="spearman")
cor.test(all.scenarios$score,as.numeric(all.scenarios$exp.cs),method="spearman")
cor.test(all.scenarios$score,as.numeric(all.scenarios$exp.ml),method="spearman")

# GLMs - education
test.demo <-  data.frame(score=all.scenarios$score,
                         gender=all.scenarios$gender,
                         age=(2019-all.scenarios$YOB),
                         eth=all.scenarios$eth,
                         edu=all.scenarios$edu)
rownames(test.demo) <- rownames(all.scenarios)
test.demo$edu <- as.character(as.numeric(test.demo$edu))

test.demo$edu <- gsub("1","Less than HS",test.demo$edu)
test.demo$edu <- gsub("2","HS",test.demo$edu)
test.demo$edu <- gsub("[345]","Post-secondary, no Bachelor's",test.demo$edu)
test.demo$edu <- gsub("[678]","Bachelor's and above",test.demo$edu)
test.demo$edu <- factor(test.demo$edu,levels=c("Less than HS","HS","Post-secondary, no Bachelor's","Bachelor's and above"))

ggplot(test.demo,aes(x=edu,y=score)) + geom_boxplot() + scale_y_continuous("Comprehension Score",breaks=c(0:9)) + theme_bw() +
  scale_x_discrete("Education level",labels=paste(gsub(" and","\nand",gsub(", ", ",\n", levels(test.demo$edu))),"\n(",table(test.demo$edu),")",sep="")) + 
  theme(legend.position="none",axis.title.x=element_text(size=10),axis.title.y=element_text(size=10))
ggsave("data/figs-study-1/edu.png",height=2.2,width=3.65)

library(broom)
library(bbmle)
library(lm.beta)
library(rsq)

m1 <- glm(score ~ gender + age + eth + edu, test.demo, family="poisson")
m2 <- glm(score ~ gender + age + eth, test.demo, family="poisson")
m3 <- glm(score ~ edu, test.demo, family="poisson")
model.names <- c("gender + age + eth + edu","gender + age + eth","edu")

summary(m1)
summary(m2)
summary(m3)
bbmle::AICtab(m1,m2,m3,base=TRUE,weights=TRUE,sort=TRUE,mnames=model.names)

# Effect size - GLM uses poisson distribution, so ES = exp(model estimate)
exp(m1$coefficients[11])
# eduBachelor's and above 
#                1.400451


# ====== RULE EXPLANATION [Free response] (Q12) ====== #

# Get codes
q12.codes <- read.table("data/study-1_pilot_q12coding.txt",header=T,sep="\t",quote="",row.names=1)
colnames(q12.codes) <- c("CintID","scenario","Q12","code")

# Remove secondary codes
q12.codes$code <- factor(gsub(";.*","",q12.codes$code),levels=c("correct","partially correct","neither","incorrect","none"))

# Make relevant data frame
test.q12 <- data.frame(score=all.scenarios$score,
                       code=q12.codes[rownames(all.scenarios),"code"],
                       scenario=all.scenarios$scenario)
rownames(test.q12) <- rownames(all.scenarios)
table(test.q12$code)

ggplot(test.q12,aes(x=code,y=score)) + geom_boxplot() + scale_y_continuous("Comprehension Score",breaks=c(0:9)) + theme_bw() +
  scale_x_discrete("Assigned Code (Q12)",labels=paste(gsub(" ", "\n", levels(test.q12$code)),"\n(",table(test.q12$code),")",sep="")) + 
  theme(legend.position="none",axis.title.x=element_text(size=8),axis.title.y=element_text(size=8))
ggsave("data/figs-study-1/q12.png",height=1.9,width=3)

# Kruskal-Wallis
kruskal.test(score ~ code, test.q12) #******

# Post-hoc Mann-Whitney U
correct <- droplevels(test.q12[test.q12$code=="correct",])
partial <- droplevels(test.q12[test.q12$code=="partially correct",])
neither <- droplevels(test.q12[test.q12$code=="neither",])
incorrect <- droplevels(test.q12[test.q12$code=="incorrect",])
none <- droplevels(test.q12[test.q12$code=="none",])
wilcox.test(score ~ code, as.data.frame(rbind(correct,neither))) #******
wilcox.test(score ~ code, as.data.frame(rbind(partial,neither))) #******
wilcox.test(score ~ code, as.data.frame(rbind(correct,incorrect))) #******
wilcox.test(score ~ code, as.data.frame(rbind(partial,incorrect))) #******
wilcox.test(score ~ code, as.data.frame(rbind(correct,partial)))
wilcox.test(score ~ code, as.data.frame(rbind(neither,incorrect)))
wilcox.test(score ~ code, as.data.frame(rbind(correct,none)))
wilcox.test(score ~ code, as.data.frame(rbind(partial,none)))
wilcox.test(score ~ code, as.data.frame(rbind(neither,none)))
wilcox.test(score ~ code, as.data.frame(rbind(incorrect,none)))


# ====== RULE UNDERSTANDING (Q13) ====== #

ggplot(all.scenarios,aes(x=Q13,y=score)) + geom_boxplot() +  theme_bw() + #geom_jitter(aes(color=Q13),height=0.2) +
  scale_x_discrete("Self-report of Q13 (Q13)",limits=rev(levels(all.scenarios$Q13)),
                   labels=rev(paste(c("strongly\nagree","agree","neither\nagree\nnor disagree","disagree","strongly\ndisagree"),
                                    "\n(",table(all.scenarios$Q13),")",sep=""))) + 
  scale_y_continuous("Comprehension Score",breaks=c(0:9)) + theme(legend.position="none",axis.title.x=element_text(size=8),axis.title.y=element_text(size=8))
ggsave("data/figs-study-1/q13.png",height=1.9,width=3)

# Spearman correlation - multiply by -1 to reverse the order of Q13 responses
cor.test(as.numeric(all.scenarios$Q13)*-1,all.scenarios$score,method="spearman") #******


# ====== RULE APPLICATION (Q14) ====== #

test.q14 <- droplevels(all.scenarios[!(is.na(all.scenarios$Q14)),])

ggplot(test.q14,aes(x=as.factor(Q14),y=score)) + geom_boxplot() + theme_bw() + scale_y_continuous("Comprehension Score",breaks=c(0:9)) + 
  scale_x_discrete("Self-report of usage (Q14)",labels=paste(c("rule\nonly","combination","personal\nnotions"),"\n(",table(test.q14$Q14),")",sep="")) + 
  theme(legend.position="none",axis.title.x=element_text(size=8),axis.title.y=element_text(size=8))
ggsave("data/figs-study-1/q14.png",height=1.9,width=3)

# Kruskal-Wallis
kruskal.test(score ~ Q14, test.q14) #******

# Post-hoc Mann-Whitney U
rule <-  droplevels(test.q14[test.q14$Q14==1,])
combo <- droplevels(test.q14[test.q14$Q14==2,])
personal <- droplevels(test.q14[test.q14$Q14==3,])
wilcox.test(score ~ Q14, as.data.frame(rbind(rule,combo))) #******
wilcox.test(score ~ Q14, as.data.frame(rbind(rule,personal))) #******
wilcox.test(score ~ Q14, as.data.frame(rbind(personal,combo)))


# ====== RULE SENTIMENT/OPINION (Q15) ====== #

q15.codes <- read.table("data/study-1_pilot_q15coding.txt",header=T,sep="\t",quote="",row.names=1)
colnames(q15.codes) <- c("CintID","scenario","Q15","code")

# Remove secondary codes
q15.codes$p.code <- factor(gsub(";.*","",q15.codes$code),levels=c("agree","depends","disagree","not understood","none"))

# Make relevant data frame
test.q15 <- data.frame(score=all.scenarios$score,
                       code=q15.codes[rownames(all.scenarios),"p.code"],
                       scenario=all.scenarios$scenario)
rownames(test.q15) <- rownames(all.scenarios)
table(test.q15$code)
ggplot(test.q15,aes(x=code,y=score)) + geom_boxplot() + scale_y_continuous("Comprehension Score",breaks=c(0:9)) + theme_bw() +
  scale_x_discrete("Assigned Code (Q15)",labels=paste(gsub(" ", "\n", levels(test.q15$code)),"\n(",table(test.q15$code),")",sep="")) + 
  theme(legend.position="none",axis.title.x=element_text(size=8),axis.title.y=element_text(size=8))
ggsave("data/figs-study-1/q15.png",height=1.9,width=3)

# Kruskal-Wallis:
kruskal.test(score ~ code, test.q15) #******

# Post-hoc Mann-Whitney U:
agree <- droplevels(test.q15[test.q15$code=="agree",])
depends <- droplevels(test.q15[test.q15$code=="depends",])
disagree <- droplevels(test.q15[test.q15$code=="disagree",])
not <- droplevels(test.q15[test.q15$code=="not understood",])
none <- droplevels(test.q15[test.q15$code=="none",])
wilcox.test(score ~ code, as.data.frame(rbind(agree,depends)))
wilcox.test(score ~ code, as.data.frame(rbind(agree,disagree))) #******
wilcox.test(score ~ code, as.data.frame(rbind(agree,not)))
wilcox.test(score ~ code, as.data.frame(rbind(agree,none)))
wilcox.test(score ~ code, as.data.frame(rbind(depends,disagree)))
wilcox.test(score ~ code, as.data.frame(rbind(depends,not)))
wilcox.test(score ~ code, as.data.frame(rbind(depends,none)))
wilcox.test(score ~ code, as.data.frame(rbind(disagree,not))) #****** 
wilcox.test(score ~ code, as.data.frame(rbind(disagree,none))) #******
wilcox.test(score ~ code, as.data.frame(rbind(not,none)))


# ====== NON-COMPLIANCE ====== #
# geom_bar position = stack vs. fill

## 2 categories (compliant,non-compliant)
non.comp <- data.frame(Q12=test.q12$code,
                       Q13=all.scenarios$Q13,
                       Q14=factor(gsub("3","2",all.scenarios$Q14)),
                       Q15=test.q15$code)
non.comp <- non.comp[!is.na(non.comp$Q14),]

q13.colors <- c("darkgreen","seagreen2","grey90","tomato","firebrick3")
q13.names <- c("SA","A","N","D","SD")
q12.colors <- c("darkgreen","seagreen2","grey90","firebrick3","grey50")
q12.names <- c("C","PC","N","I","NA")
q15.colors <- c("darkgreen","chocolate1","firebrick3","grey90","grey50")
q15.names <- c("A","De","D","NU","NA")

# Q14 x Q13
ggplot(non.comp,aes(x=Q14)) + geom_bar(stat="count",position="fill",width=0.5,aes(fill=Q13)) + theme_bw() + 
  scale_fill_manual("Q13 rating",values=q13.colors,labels=q13.names) + scale_y_continuous("Proportion of participants") + 
  scale_x_discrete(labels=c("compliant","non-\ncompliant")) + 
  theme(legend.position="bottom",legend.box.spacing=unit(-0,"cm"),legend.key.size=unit(0.35,"cm"),
        legend.title=element_text(size=8),legend.text=element_text(size=8),legend.justification="left",
        axis.title.y=element_blank(),axis.title.x=element_text(size=8)) + coord_flip() + guides(fill=guide_legend(reverse=TRUE))
ggsave("data/figs-study-1/nc_q13q14.png",height=1.25,width=3)

# Q14 x Q12
ggplot(non.comp,aes(x=Q14)) + geom_bar(stat="count",position="fill",width=0.5,aes(fill=Q12)) + theme_bw() + 
  scale_fill_manual("Q12 code",values=q12.colors,labels=q12.names) + scale_y_continuous("Proportion of participants") + 
  scale_x_discrete(labels=c("compliant","non-\ncompliant")) + 
  theme(legend.position="bottom",legend.box.spacing=unit(-0,"cm"),legend.key.size=unit(0.35,"cm"),
        legend.title=element_text(size=8),legend.text=element_text(size=8),legend.justification="left",
        axis.title.y=element_blank(),axis.title.x=element_text(size=8)) + coord_flip() + guides(fill=guide_legend(reverse=TRUE))
ggsave("data/figs-study-1/nc_q12q14.png",height=1.25,width=3)

# Q14 x Q15
ggplot(non.comp,aes(x=Q14)) + geom_bar(stat="count",position="fill",width=0.5,aes(fill=Q15)) + theme_bw() + 
  scale_fill_manual("Q15 code",values=q15.colors,labels=q15.names) + scale_y_continuous("Proportion of participants") + 
  scale_x_discrete(labels=c("compliant","non-\ncompliant")) + 
  theme(legend.position="bottom",legend.box.spacing=unit(-0,"cm"),legend.key.size=unit(0.35,"cm"),
        legend.title=element_text(size=8),legend.text=element_text(size=8),legend.justification="left",
        axis.title.y=element_blank(),axis.title.x=element_text(size=8)) + coord_flip() + guides(fill=guide_legend(reverse=TRUE))
ggsave("data/figs-study-1/nc_q15q14.png",height=1.25,width=3)

chisq.test(non.comp$Q14,non.comp$Q13) #******
chisq.test(non.comp$Q14,non.comp$Q12) #******
chisq.test(non.comp$Q14,non.comp$Q15) #******


# ====== EFFECTS OF SCENARIO ====== #

# scenario x comprehension score
kruskal.test(score ~ scenario,all.scenarios)

# Scenario x Q1 (realism)
kruskal.test(Q1 ~ scenario,all.scenarios)

# scenario x Q2 (hours of effort)
ggplot(all.scenarios,aes(x=Q2)) + geom_histogram(binwidth=1,color="black",fill="white") + 
  facet_grid(. ~ scenario) + scale_x_continuous("Hours of Effort") + 
  scale_y_continuous("# of Participants") + theme_bw() +
  theme(legend.position="bottom",axis.title.x=element_text(size=8),axis.title.y=element_text(size=8),
        strip.text.x=element_text(margin = margin(0.05,0,0.05,0,"cm")))
ggsave("data/figs-study-1/q2.png",height=1.25,width=3)
kruskal.test(Q2 ~ scenario,all.scenarios) #******
wilcox.test(Q2 ~ scenario,droplevels(all.scenarios[all.scenarios$scenario!="AP",]))
wilcox.test(Q2 ~ scenario,droplevels(all.scenarios[all.scenarios$scenario!="EA",])) #******
wilcox.test(Q2 ~ scenario,droplevels(all.scenarios[all.scenarios$scenario!="HR",])) #******


rm(list=ls())




##############################################################################
##############################################################################
#                                                                            #
#                                Study-2 (Full)                              #
#                            Published at ICML 2020                          #
#                                                                            #
##############################################################################
##############################################################################



# =================================== #
#            DATA/FORMATTING          #
# =================================== #

# ====== GET DATA ====== #

setwd("C:/Users/Debjani Saha/Documents/University of Maryland/Projects/Alg Fairness/20200127 ICML submission (02-06)")
all.defs <- read.table("data/study-2_full.txt",header=TRUE,quote="",sep="\t")
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

# Assessed using two measures: 
# Cronbach's alpha ("raw_alpha")
# Item-total correlation ("r.cor")
responses <- score.sheet[,colnames(answer.key)]
responses[responses=="TRUE"] <- 1
responses$def <- all.defs$def

# All fairness definitions (DP, FNR, FPR, EO)
psych::alpha(droplevels(responses[,c(1:9)]),check.keys=TRUE)  # all fairness defintions

# Split by definition
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


#############!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#############
defs <- c("DP","FNR","FPR","EO")
questions.to.keep <- list()
for (i in c(1:length(defs))) {
  current.def <- droplevels(responses[as.character(all.defs$def)==defs[i],c(1:9)])
  current.def.iv <- psych::alpha(current.def,check.keys=TRUE)
  while (sum(current.def.iv$item.stats$r.cor<0.3)>0) {
    print(rownames(current.def.iv$response.freq)[which.min(current.def.iv$item.stats$r.cor)])
    current.def <- current.def[,-which.min(current.def.iv$item.stats$r.cor)]
    current.def.iv <- psych::alpha(current.def,check.keys=TRUE)
  }
  questions.to.keep[[i]] <- rownames(current.def.iv$response.freq)
}
#############!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#############

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
ggsave("data/figs-study-2/scores.png",height=2.2,width=3.65)

# Proportion of respondents (out of 349) answering each question correctly:
pct.correct <- data.frame(Percent=colSums(score.sheet,na.rm=TRUE)/n.respondents)
pct.correct$Percent <- percent(pct.correct$Percent)
colnames(pct.correct) <- c("Percent Correct")
pct.correct
#     Percent Correct
# Q3            45.8%
# Q4            35.8%
# Q5            55.3%
# Q6            61.0%
# Q7            43.3%
# Q8            48.1%
# Q9            75.9%
# Q10           63.6%
# Q11           61.3%

# Respondents answering each question correctly, split by definition (each column comprises all 349 respondents)
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
ggsave("data/figs-study-2/edu.png",height=2.2,width=3.65)

#library(broom)
#library(bbmle)
#library(lm.beta)
#library(rsq)

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
q12.codes <- read.table("data/study-2_full_q12coding.txt",quote="",header=TRUE,sep="\t",row.names=1)

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
ggsave("data/figs-study-2/q12.png",height=1.9,width=3)

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
ggsave("data/figs-study-2/q13.png",height=1.9,width=3)

# Spearman correlation - multiply by -1 to reverse the order of Q13 responses
# this way if those who report higher understanding (1 on Likert scale) also have higher score, there will be a positive correlation
cor.test(as.numeric(all.defs$Q13)*-1,all.defs$score,method="spearman")


# ====== RULE APPLICATION (Q14) ====== #

test.q14 <- droplevels(all.defs[!(is.na(all.defs$Q14)),])
ggplot(test.q14,aes(x=as.factor(Q14),y=score)) + geom_boxplot() + theme_bw() + scale_y_continuous("Comprehension Score",breaks=c(0,0.25,0.5,0.75,1)) + 
  scale_x_discrete("Self-report of usage (Q14)",labels=paste(c("rule\nonly","combination","personal\nnotions"),"\n(",table(test.q14$Q14),")",sep="")) + 
  theme(legend.position="none",axis.title.x=element_text(size=8),axis.title.y=element_text(size=8))
ggsave("data/figs-study-2/q14.png",height=1.9,width=3)

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
ggsave("data/figs-study-2/q15.png",height=1.9,width=3)

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
ggsave("data/figs-study-2/q16.png",height=1.9,width=3)

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
ggsave("data/figs-study-2/nc_q12q14.png",height=1.25,width=3)

# Q14 x Q13
ggplot(non.comp,aes(x=Q14)) + geom_bar(stat="count",position="fill",width=0.5,aes(fill=Q13)) + theme_bw() + 
  scale_fill_manual("Q13 rating",values=q13.colors,labels=q13.names) + scale_y_continuous("Proportion of participants") + 
  scale_x_discrete(labels=c("compliant","non-\ncompliant")) + 
  theme(legend.position="bottom",legend.box.spacing=unit(-0,"cm"),legend.key.size=unit(0.35,"cm"),
        legend.title=element_text(size=8),legend.text=element_text(size=8),legend.justification="left",
        axis.title.y=element_blank(),axis.title.x=element_text(size=8)) + coord_flip() + guides(fill=guide_legend(reverse=TRUE))
ggsave("data/figs-study-2/nc_q13q14.png",height=1.25,width=3)

# Q14 x Q15
ggplot(non.comp,aes(x=Q14)) + geom_bar(stat="count",position="fill",width=0.5,aes(fill=Q15)) + theme_bw() + 
  scale_fill_manual("Q15 rating",values=q15.colors,labels=q15.names) + scale_y_continuous("Proportion of participants") + 
  scale_x_discrete(labels=c("compliant","non-\ncompliant")) + 
  theme(legend.position="bottom",legend.box.spacing=unit(-0,"cm"),legend.key.size=unit(0.35,"cm"),
        legend.title=element_text(size=8),legend.text=element_text(size=8),legend.justification="left",
        axis.title.y=element_blank(),axis.title.x=element_text(size=8)) + coord_flip() + guides(fill=guide_legend(reverse=TRUE))
ggsave("data/figs-study-2/nc_q15q14.png",height=1.25,width=3)

# Q14 x Q16
ggplot(non.comp,aes(x=Q14)) + geom_bar(stat="count",position="fill",width=0.5,aes(fill=Q16)) + theme_bw() + 
  scale_fill_manual("Q16 rating",values=q16.colors,labels=q16.names) + scale_y_continuous("Proportion of participants") + 
  scale_x_discrete(labels=c("compliant","non-\ncompliant")) + 
  theme(legend.position="bottom",legend.box.spacing=unit(-0,"cm"),legend.key.size=unit(0.35,"cm"),
        legend.title=element_text(size=8),legend.text=element_text(size=8),legend.justification="left",
        axis.title.y=element_blank(),axis.title.x=element_text(size=8)) + coord_flip() + guides(fill=guide_legend(reverse=TRUE))
ggsave("data/figs-study-2/nc_q16q14.png",height=1.25,width=3)

chisq.test(non.comp$Q12,non.comp$Q14) #******
kruskal.test(non.comp$Q13,non.comp$Q14) #******
kruskal.test(non.comp$Q15,non.comp$Q14) #******
kruskal.test(non.comp$Q16,non.comp$Q14)


