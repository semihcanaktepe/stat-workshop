# Cumulative Link Mixed Models

## Load the data set
hot <- read.csv("~/Desktop/stat-workshop/datasets/feelinghot.csv")


hot$frequency <- 1
agg_hot <- aggregate(frequency ~ Cold, data = hot, FUN = sum)
barplot(agg_hot$frequency, names.arg = 1:6,
        main = "Distribution of Responses", xlab = "Feeling Cold", ylab = "Frequency",
        col = "dodgerblue3", space = 1)
abline(h=0)


str(hot)

hot$subject <- as.factor(hot$ID)
hot$real_temp <- scale(hot$Temp_True)
hot$thought_temp <- scale(hot$Temp_Think)
hot$cold <- as.factor(hot$Cold)
hot$skin_exposure <- scale(hot$Openshoes + hot$Barelegs + hot$Arms + hot$Shoulders + hot$Cleavage + hot$Openback + hot$Midriff + hot$No_jacket + hot$Jean_rips)
hot$self_obj <- scale(hot$Self_Obj)

hot2 <- data.frame(subject=hot$subject, real_temp=hot$real_temp, thought_temp=hot$thought_temp,
                   skin_exposure=hot$skin_exposure, self_obj=hot$self_obj, cold=hot$cold)

install.packages("ordinal")
library(ordinal)

m_hot <- clm(cold ~ real_temp + thought_temp + skin_exposure + self_obj
              + real_temp:skin_exposure + real_temp:thought_temp
              + skin_exposure:thought_temp + self_obj:skin_exposure,
             threshold = "flexible", data = hot2)

summary(m_hot)

m_hot2 <- clm(cold ~ skin_exposure*self_obj, threshold = "flexible", data = hot2)

summary(m_hot2)

m_hot3 <- clm(cold ~ self_obj, threshold = "flexible", data = hot2)
summary(m_hot3)



lm_hot <- lm(as.numeric(cold) ~ real_temp + thought_temp + skin_exposure + self_obj
             + real_temp:skin_exposure + real_temp:thought_temp
             + skin_exposure:thought_temp + self_obj:skin_exposure, data = hot2)

summary(lm_hot)

lm_hot3 <- lm(as.numeric(cold) ~ self_obj, data = hot2)
summary(lm_hot3)










