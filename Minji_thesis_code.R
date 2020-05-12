# Step 1 - delete all nonconsenting people in excel 
# Step 2 - look at attention checks

m <- read.csv("~/Desktop/Minji_thesis.csv")


nrow(m) #410

attach(m)
#attention check
m$Check1 <- as.numeric(ERQO_General_15 == 5)
m$Check2 <- as.numeric(ERQ_7 == 2)


attach(m)

#create new variable
m$Check_Sum <- (Check1 + Check2 + Check_Blank)
hist(m$Check_Sum)

summary(as.factor(m$Check_Sum))
##method: 330 participants 

msub <- subset(m, Check_Sum == 3)
nrow(msub)

write.csv(msub, "Honors_filter.cvs")

###demographics
psych :: describe(msub$Age) #M=20.78, SD: 2.71, RANGE: 18-41
summary(as.factor(msub$Gender))/nrow(msub) #male =20%, female =80%, less than > 1% of androgenous, (female dominant test)

msub$Gender_hist = factor(msub$Gender, levels = c(1,2), labels = c("Male", "Female"))
summary(msub$Gender_hist)
ggplot(data=msub, aes(x=Gender_hist))+geom_bar()+scale_x_discrete(limits = c("Male", "Female"))+xlab("Gender")+ggtitle("Plot of Gender")

df <- data.frame(
  group = c("Male", "Female"),
  value = c(65, 263)
)
head(df)

bp<- ggplot(df, aes(x="", y=value, fill=group))+
  geom_bar(width = 1, stat = "identity")
pie <- bp + coord_polar("y", start=0)
pie+scale_fill_grey()+theme(axis.text = element_blank())+ geom_text(aes(y = value/3 + c(0, cumsum(value)[-length(value)]), 
                                                                        label = percent(value/328)), size=5)+blank_theme

blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )


library(scales)

summary(as.factor(msub$Ethnicity))/nrow(msubset)# >1% A.A, >1% American Indian, 56% Asian, 11% Latinax or Hispanics, > 1% apcific islander, 23% of White, >1% decline to answer. 8% of 

##ERQ_GEN, ERQ_REL, ERQO_GEN, ERQO_REL, POWER, REL_POWER

##### ERQ SUPPRESSION IN GENERAL #####
#2, #4, #6, #10

suppgendf =subset(msub, select = c(ERQ_2,ERQ_4,ERQ_6, ERQ_10))
psych::alpha(suppgendf) #alpha= 0.81

msub$supp_gen = rowMeans(suppgendf)
hist(msub$supp_gendf)
psych::describe(msub$supp_gen)# mean= 3.4, sd= 1.31, range 1-7

#####ERQ SUPREESION REL################
#Q2, 4, 6, 9
suppreldf =subset(msub, select = c(ERQ_Rel_2,ERQ_Rel_4,ERQ_Rel_6, ERQ_Rel_9))
psych::alpha(suppreldf) #alpha= 0.84

msub$supp_rel = rowMeans(suppreldf)
hist(msub$supp_rel) ##in the relationship, they are less likely suppres their emotion.
psych::describe(msub$supp_rel) #M=2.47, SD=1.2, Range 1-6.25

#######ERQO SUPPRESSION GENRERAL############
#Q 2,5,9,14
supp_o_gendf =subset(msub, select = c(ERQO_General_2,ERQO_General_5,ERQO_General_9, ERQO_General_14))
psych::alpha(supp_o_gendf) #alpha= 0.70

msub$supp_o_gen = rowMeans(supp_o_gendf)
hist(msub$supp_o_gen) 
psych::describe(msub$supp_o_gen) #M=2.2, SD= 0.91, Range = 1-5.25

###########ERQO SUPPRESSION RELATIONSHiP#########
##Q2,5,9,14
supp_o_reldf =subset(msub, select = c(ERQO_Rel_2,ERQO_Rel_5,ERQO_Rel_9, ERQO_Rel_14))
psych::alpha(supp_o_reldf) #alpha= 0.75

msub$supp_o_reldf = rowMeans(supp_o_reldf)
hist(msub$supp_o_reldf) 
psych::describe(msub$supp_o_reldf) #M =2.0, SD=0.98, RANGE=1-6


########power_general###################
##reverse: Q1,Q3,Q4
##Q2,Q5

SOP_1.0 =8-(msub$SOP_1)
SOP_3.0 = 8-(msub$SOP_3)
SOP_4.0 = 8-(msub$SOP_4)
msub$SOP_1_r = SOP_1.0
msub$SOP_3_r = SOP_3.0
msub$SOP_4_r = SOP_4.0
power_gendf =subset(msub, select = c(SOP_1_r,SOP_2,SOP_3_r,SOP_4_r,SOP_5))
msub$SOP_5

psych::alpha(power_gendf)#alpha = 0.83

msub$power_gendf = rowMeans(power_gendf)
hist(msub$power_gendf)
psych::describe(msub$power_gendf) #M= 4.65, SD=1.02, Range:1.8-7

##########power_rel##################
#REVERSE:Q2,Q5,Q7,Q8
#Q1,3,6,9
power_rel_2.0 = 8-(msub$Rel_Power_2)
power_rel_5.0 = 8-(msub$Rel_Power_5)
power_rel_7.0 = 8-(msub$Rel_Power_7)
power_rel_8.0 = 8-(msub$Rel_Power_8)

msub$Rel_Power_2 = power_rel_2.0
msub$Rel_Power_5 = power_rel_5.0
msub$Rel_Power_7 = power_rel_7.0
msub$Rel_Power_8 = power_rel_8.0

power_reldf = subset(msub, select = c(Rel_Power_1,Rel_Power_2,Rel_Power_3,Rel_Power_5,Rel_Power_6,Rel_Power_7,Rel_Power_8,Rel_Power_9))

psych::alpha(power_reldf) #alpha = 0.89

msub$power_reldf= rowMeans(power_reldf)
hist(msub$power_reldf)
psych::describe(msub$power_reldf) #M= 5.44, SD=0.96, Range:2.12-7


#####power and supp in general###########
mod1 = lm(scale(supp_gen)~scale(power_gendf), data=msub)
summary(mod1) # b1= -2.3, every power increase 1, decrease emotion suppression 2.284e-01, p=2.81e-05 (negative relationship)
confint(mod1)
library("ggplot2")
scatter1 = ggplot(msub, aes(power_gendf, supp_gen))

scatter1 + geom_point() + geom_smooth(method = "lm", colour = "slateblue", se = F) + labs(x = "general power", y = "suppression of self")+ geom_point() +theme_bw()


#####power and supp others in general###########
#scale = standize 
mod2 = lm(scale(supp_o_gen)~scale(power_gendf), data = msub)
summary(mod2)
#b1= -0.18, 
plot(mod2)
confint(mod2)#ci= -0.288 -0.0745 **r-squard (effect size):0.03
##effect size: 
scatter2 = ggplot(msub, aes(power_gendf, supp_o_gen))
scatter2 + geom_point() + geom_smooth(method = "lm", colour = "slateblue", se = F) + labs(x = "general power", y = "suppression of others")+ geom_point()+theme_bw()


#####power and supp in the relationship###########
mod3 = lm(scale(supp_rel)~scale(power_reldf), data =msub)
summary(mod3) #b1= -0.398608, p =4.84e-14 
confint(mod3)

scatter3 = ggplot(msub, aes(power_reldf, supp_rel))
scatter3 + geom_point() + geom_smooth(method = "lm", colour = "slateblue", se = F) + labs(x = "relational power", y = "suppression of self")+geom_point()+theme_bw()
########power and supp other in the relationship#########
mod4 = lm(scale(supp_o_reldf)~scale(power_reldf), data=msub)
summary(mod4) #b1= -0.35, p=5.31e-11 ***  **r-square: 0.12
confint(mod4)


library(ggplot2)
scatter4 = ggplot(msub, aes(power_reldf, supp_o_reldf))
scatter4 + geom_point() + geom_smooth(method = "lm", colour = "slateblue", se = F) + labs(x = "relational power", y = "suppression of others")+theme_bw()

##########Correlational Matrix###########

mydata =data.frame(msub$power_gendf, msub$power_reldf, msub$supp_gen, msub$supp_rel, msub$supp_o_gen, msub$supp_o_reldf)
library("PerformanceAnalytics")
chart.Correlation(mydata, histogram = T, pch =19)


###################Plots for the poster############
library(ggplot2)

p1 =scatter1 + geom_point() + geom_smooth(method = "lm", colour = "slateblue", se = F) + labs(x = "General Power", y = "Self-Directed Suppression")+ geom_point() +theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+ggtitle("Hypothesis 1a")
p2 =scatter3 + geom_point() + geom_smooth(method = "lm", colour = "slateblue", se = F) + labs(x = "Relational Power", y = "Self-Directed Suppression")+geom_point()+theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+ggtitle("Hypothesis 1b")
library(gridExtra)
grid.arrange(p1,p2, ncol =2, nrow =1)

p3= scatter2 + geom_point() + geom_smooth(method = "lm", colour = "slateblue", se = F) + labs(x = "General Power", y = "Other-Directed Suppression")+ geom_point()+theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+ggtitle("Hypothesis 2a")

p4= scatter4 + geom_point() + geom_smooth(method = "lm", colour = "slateblue", se = F) + labs(x = "Relational Power", y = "Other-Directed Suppression")+theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+ggtitle("Hypothesis 2b")
")

grid.arrange(p3,p4, ncol =2, nrow =1)


######controlloing age, ethnicity, and gender##########
sub$Age
msub$Gender
msub$Ethnicity
summary(as.factor(msub$Gender))
summary(as.factor(msub$Ethnicity))
levels(msub$gender_fac)

library(plyr)

names(msub$gender_fac)[names(msub) == "1"] = "male"
revalue(msub$gender_fac, c("1"="male", "2"="female", "5" = "androgenous"))

#power and self suppression in general#
msub$gender_fac= as.factor(msub$Gender)
msub$ethnicity_fac=as.factor(msub$Ethnicity)


mod1_add = lm(scale(supp_gen)~scale(power_gendf)+scale(Age)+gender_fac+ethnicity_fac, data=msub)

summary(mod1_add)


summary(mod1_add)


summary(mod1_add)
anova(mod4_add)
#power and self suppression in romantic relationship#
mod2_add = lm(scale(supp_rel)~scale(power_reldf)+scale(Age)+gender_fac+ethnicity_fac, data =msub)
summary(mod2_add)

#power and other suppression in general#
mod3_add = lm(scale(supp_o_gen)~scale(power_gendf)+scale(Age)+gender_fac+ethnicity_fac, data = msub)
summary(mod3_add)
#power and other suppression in romantic relationship#
mod4_add= lm(scale(supp_o_reldf)~scale(power_reldf)+scale(Age)++gender_fac+ethnicity_fac, data=msub)
summary(mod4_add)
