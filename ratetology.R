#download and load necessary packages

deps = c("car", "ggplot2","lme4","dplyr","tidyr","psych","plyr", "corrr");

for (dep in deps){
  if (dep %in% installed.packages()[,"Package"] == FALSE){
    install.packages(as.character(dep), quiet=TRUE);
  }
  library(dep, verbose=FALSE, character.only=TRUE)
}

#setting up data
setwd("~/R_analysis")

url <- "https://ndownloader.figshare.com/files/53281871"
download.file(url, destfile="all_sample.csv", 'libcurl')

df <- read.csv(file="all_sample.csv", sep=";", dec=",")

df$duration <- as.factor(df$duration)
df$season <- as.factor(df$season)
df$age <- as.factor(df$age)
df$ind_count <- as.numeric(df$ind_count)
df$sex <- as.factor(df$sex)



# continuous behaviour analysis - glm

cb1 <- glm(forage_pos ~ duration+age+ind_count+sex, data=df, family=gaussian) #duration
plot(cb1)
anova(cb1)
summary(cb1)
with(summary(cb1), 1 - deviance/null.deviance)

cb2 <- glm(other_pos ~ duration+age+ind_count+sex, data=df, family=gaussian) # ind_count, agejuv
plot(cb2)
anova(cb2)
summary(cb2)
with(summary(cb2), 1 - deviance/null.deviance)


cb3 <- glm(other_neg ~ duration+age+ind_count+sex, data=df, family=gaussian) # ageboth, agejuv
plot(cb3)
anova(cb3)
summary(cb3)
with(summary(cb3), 1 - deviance/null.deviance)


#graphs


df$div <- df[,7]+df[,8]+df[,9]+df[,10]+df[,11]

data1 <- gather(df, behaviour, time, leaves_pos:other_neg, factor_key=TRUE)

data1$timeperc <- (data1$time/data1$div)*100

#Fig 3A

means <- ddply(data1, c("behaviour", "age"), summarise,
               mean=mean(timeperc))

means.sem <- ddply(data1, c("behaviour", "age"), summarise,
                   mean=mean(timeperc), sem=sd(timeperc)/sqrt(length(timeperc)))
means.sem <- transform(means.sem, lower=mean-sem, upper=mean+sem)

means.sem[means.sem$behaviour=='leaves_pos',5:6] <- means.sem[means.sem$behaviour=='forage_pos',3]+
  means.sem[means.sem$behaviour=='other_pos',3]+ means.sem[means.sem$behaviour=='leaves_neg',3]+
  means.sem[means.sem$behaviour=='other_neg',3]+ means.sem[means.sem$behaviour=='leaves_pos',5:6]

means.sem[means.sem$behaviour=='forage_pos',5:6] <-means.sem[means.sem$behaviour=='other_pos',3]+ 
  means.sem[means.sem$behaviour=='leaves_neg',3]+ means.sem[means.sem$behaviour=='other_neg',3]+ 
  means.sem[means.sem$behaviour=='forage_pos',5:6]

means.sem[means.sem$behaviour=='other_pos',5:6] <- means.sem[means.sem$behaviour=='leaves_neg',3]+
  means.sem[means.sem$behaviour=='other_neg',3]+ means.sem[means.sem$behaviour=='other_pos',5:6]

means.sem[means.sem$behaviour=='leaves_neg',5:6] <-means.sem[means.sem$behaviour=='other_neg',3]+ 
  means.sem[means.sem$behaviour=='leaves_neg',5:6]


ggplot(means.sem, aes(fill=behaviour, y=mean, x=age)) + 
              geom_bar(stat="identity")+ 
              geom_errorbar(aes(ymax=upper,  ymin=lower),width=0.5)

#Fig3B

means <- ddply(data1, c("behaviour", "ind_count"), summarise,
               mean=mean(timeperc))

means.sem <- ddply(data1, c("behaviour", "ind_count"), summarise,
                   mean=mean(timeperc), sem=sd(timeperc)/sqrt(length(timeperc)))
means.sem <- transform(means.sem, lower=mean-sem, upper=mean+sem)

means.sem[means.sem$behaviour=='leaves_pos',5:6] <- means.sem[means.sem$behaviour=='forage_pos',3]+
  means.sem[means.sem$behaviour=='other_pos',3]+ means.sem[means.sem$behaviour=='leaves_neg',3]+
  means.sem[means.sem$behaviour=='other_neg',3]+ means.sem[means.sem$behaviour=='leaves_pos',5:6]

means.sem[means.sem$behaviour=='forage_pos',5:6] <-means.sem[means.sem$behaviour=='other_pos',3]+ 
  means.sem[means.sem$behaviour=='leaves_neg',3]+ means.sem[means.sem$behaviour=='other_neg',3]+ 
  means.sem[means.sem$behaviour=='forage_pos',5:6]

means.sem[means.sem$behaviour=='other_pos',5:6] <- means.sem[means.sem$behaviour=='leaves_neg',3]+
  means.sem[means.sem$behaviour=='other_neg',3]+ means.sem[means.sem$behaviour=='other_pos',5:6]

means.sem[means.sem$behaviour=='leaves_neg',5:6] <-means.sem[means.sem$behaviour=='other_neg',3]+ 
  means.sem[means.sem$behaviour=='leaves_neg',5:6]


ggplot(means.sem, aes(fill=behaviour, y=mean, x=ind_count)) + 
  geom_bar(stat="identity")+ 
  geom_errorbar(aes(ymax=upper,  ymin=lower),width=0.5)


# point behaviour - differences between classes

df$agon  <- df$chase+df$wary+df$steal+df$wrestle
df$assoc  <- df$sharing+df$smelling+df$touch+df$play

df.2 <- df %>% 
  pivot_longer(
    cols = `agon`:`assoc`, 
    names_to = "category",
    values_to = "value"
  )


point.aov <- kruskal.test(value ~ category, data=df.2)
point.aov


# point behaviour analysis


pb1 <- glm(sharing ~ duration+age+ind_count+sex, data=df, family=binomial) 
plot(pb1)
anova(pb1)
summary(pb1)
with(summary(pb1), 1 - deviance/null.deviance)


pb2 <- glm(smelling ~ duration+age+ind_count+sex, data=df, family=binomial) # age:both
plot(pb2)
anova(pb2)
summary(pb2)
with(summary(pb2), 1 - deviance/null.deviance)


pb3 <- glm(touch ~ duration+age+ind_count+sex, data=df, family=binomial) #ind_count
plot(pb3)
anova(pb3)
summary(pb3)
with(summary(pb3), 1 - deviance/null.deviance)


#no sense testing play

pb4 <- glm(meeting ~ duration+age+ind_count+sex, data=df, family=binomial) #ind_count + age
plot(pb4)
anova(pb4)
summary(pb4)
with(summary(pb4), 1 - deviance/null.deviance)


pb6 <- glm(chase ~ duration+age+ind_count+sex, data=df, family=binomial)
plot(pb2)
anova(pb2)
summary(pb6)

pb7 <- glm(wary ~ duration+age+ind_count+sex, data=df, family=binomial) 
plot(pb2)
anova(pb2)
summary(pb7)

#no sense testing steal or wrestle

#Point figures
# creating baselines with different individual count and age classes

pd_a =data.frame(ind_count=0:8, duration="20", age="adult", sex="both")
pd_b =data.frame(ind_count=0:8, duration="20", age="both", sex="both")
pd_juv =data.frame(ind_count=0:8, duration="20", age="juv", sex="both")

#Fig 4a - meeting

preds_a <- predict(pb4,newdata=pd_a, type="response", se.fit=TRUE)
pd_a$fit <- preds_a$fit
pd_a$se = preds_a$se.fit

preds_b <- predict(pb4,newdata=pd_b, type="response", se.fit=TRUE)
pd_b$fit <- preds_b$fit
pd_b$se = preds_b$se.fit

preds_juv <- predict(pb4,newdata=pd_juv, type="response", se.fit=TRUE)
pd_juv$fit <- preds_juv$fit
pd_juv$se = preds_juv$se.fit

pd <- bind_rows(pd_a, pd_b, pd_juv)

ggplot(df, aes(x = ind_count, y = smelling, color=age, fill=age)) +
  geom_point(position = position_jitter(width = 0.05, height = 0)) +
  geom_ribbon(data = pd, aes(y = fit, ymin = fit - 1.96 * se, ymax = fit + 1.96 * se), 
              alpha = 0.2, linetype=0) +
  geom_line(data = pd, aes(y = fit,color=age)) + 
  theme(panel.background = element_blank())


#Fig 4b - smelling

preds_a <- predict(pb2,newdata=pd_a, type="response", se.fit=TRUE)
pd_a$fit <- preds_a$fit
pd_a$se = preds_a$se.fit

preds_b <- predict(pb2,newdata=pd_b, type="response", se.fit=TRUE)
pd_b$fit <- preds_b$fit
pd_b$se = preds_b$se.fit

preds_juv <- predict(pb2,newdata=pd_juv, type="response", se.fit=TRUE)
pd_juv$fit <- preds_juv$fit
pd_juv$se = preds_juv$se.fit

pd <- bind_rows(pd_a, pd_b, pd_juv)

ggplot(df, aes(x = ind_count, y = smelling, color=age, fill=age)) +
  geom_point(position = position_jitter(width = 0.05, height = 0)) +
  geom_ribbon(data = pd, aes(y = fit, ymin = fit - 1.96 * se, ymax = fit + 1.96 * se), 
              alpha = 0.2, linetype=0) +
  geom_line(data = pd, aes(y = fit,color=age)) + 
  theme(panel.background = element_blank())


#Fig 5c - smelling

preds_a <- predict(pb3,newdata=pd_a, type="response", se.fit=TRUE)
pd_a$fit <- preds_a$fit
pd_a$se = preds_a$se.fit

preds_b <- predict(pb3,newdata=pd_b, type="response", se.fit=TRUE)
pd_b$fit <- preds_b$fit
pd_b$se = preds_b$se.fit

preds_juv <- predict(pb3,newdata=pd_juv, type="response", se.fit=TRUE)
pd_juv$fit <- preds_juv$fit
pd_juv$se = preds_juv$se.fit

pd <- bind_rows(pd_a, pd_b, pd_juv)

ggplot(df, aes(x = ind_count, y = smelling, color=age, fill=age)) +
  geom_point(position = position_jitter(width = 0.05, height = 0)) +
  geom_ribbon(data = pd, aes(y = fit, ymin = fit - 1.96 * se, ymax = fit + 1.96 * se), 
              alpha = 0.2, linetype=0) +
  geom_line(data = pd, aes(y = fit,color=age)) + 
  theme(panel.background = element_blank())