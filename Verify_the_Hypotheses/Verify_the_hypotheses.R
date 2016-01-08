###
# For words

setwd("D:/Kile/語言分析與資料科學/Final/word_data/2015_time_series")

# create data frame
df = read.csv(list.files()[1], stringsAsFactors = F, header = F)
colnames(df) = c("V1", substr(list.files()[1],1,nchar(list.files()[1])-12))

for(i in 2:length(list.files())){
    df_temp = read.csv(list.files()[i], stringsAsFactors = F, header = F)
    colnames(df_temp)[2] = substr(list.files()[i],1,nchar(list.files()[i])-12)
    df = merge(df,df_temp,by = "V1", all.x = T)
}

df[is.na(df)] = 0

colnames(df)[1] = "Date"

df$time_ind = rep(0, nrow(df))
for(i in 1:nrow(df)){
    df$time_ind[i] = substr(df$Date[i],6,7)
}

df_month = data.frame(a = rep(0,12))
for(i in 2:32){
    df_month = cbind(df_month, as.vector(tapply(df[,i], df$time_ind, sum)))
    colnames(df_month)[i] = colnames(df)[i]
}

colnames(df_month)[1] = "Month"
df_month[1] = 1:12

# calculate the siftnig criteria, mean and coefficient of variation
mean_words = sapply(2:(ncol(df_month)), function(i) mean(df_month[,i]))
cv_words = sapply(2:(ncol(df_month)), function(i) sd(df_month[,i])/mean(df_month[,i]))

# sift the words qualified for the standards
survivor = ifelse((mean_words>=1 & cv_words<=1), 1, 0)

colnames(df)[4] = "Z>B"
colnames(df_month)[4] = "Z>B"

###
# Questionnaire outcome
setwd("D:/Kile/語言分析與資料科學/Final/word_data/hypothesis_clean")

metaphorlity = read.csv("metaphorlity.csv", stringsAsFactors = F)
sentility = read.csv("sentility.csv", stringsAsFactors = F)
sour = read.csv("sour.csv", stringsAsFactors = F)

metaphorlity[,1] = NULL
colnames(metaphorlity)[1:3] = c("2.0", "Z>B", "over my dead body")

sentility[,1] = NULL
colnames(sentility)[1:3] = c("2.0", "Z>B", "over my dead body")

sour[,1] = NULL
colnames(sour)[1:3] = c("2.0", "Z>B", "over my dead body")

name_ind = match(colnames(df)[2:32],colnames(sour))

metaphorlity = metaphorlity[name_ind]
sentility = sentility[name_ind]
sour = sour[name_ind]

metaphorlity = apply(t(metaphorlity), 1, mean)
sentility = apply(t(sentility), 1, mean)
sour = apply(t(sour), 1, mean)

df_test = cbind(metaphorlity, sentility, sour)
df_test = as.data.frame(df_test)
df_test$survivor = survivor

#tapply(df_test$metaphorlity, df_test$survivor, mean)
#tapply(df_test$sentility, df_test$survivor, mean)
#tapply(df_test$sour, df_test$survivor, mean)

#colnames(df)[2:32][survivor==1]
#colnames(df)[2:32][survivor==0]

#t.test(df_test$metaphorlity[df_test$survivor == 1])
#t.test(df_test$metaphorlity[df_test$survivor == 0])

# Implementing the two-sample t test, before each t test, we have to do the F test to check if each
# group has the same variance

var.test(df_test$metaphorlity[df_test$survivor == 1],df_test$metaphorlity[df_test$survivor == 0])
t.test(df_test$metaphorlity[df_test$survivor == 1],df_test$metaphorlity[df_test$survivor == 0], var.equal=TRUE, paired=FALSE)

var.test(df_test$sentility[df_test$survivor == 1],df_test$sentility[df_test$survivor == 0])
t.test(df_test$sentility[df_test$survivor == 1],df_test$sentility[df_test$survivor == 0], var.equal=TRUE, paired=FALSE)

var.test(df_test$sour[df_test$survivor == 1],df_test$sour[df_test$survivor == 0])
t.test(df_test$sour[df_test$survivor == 1],df_test$sour[df_test$survivor == 0], var.equal=TRUE, paired=FALSE)

###
# For sentence   
setwd("D:/Kile/語言分析與資料科學/Final/sentence_data/2015_sent_ts")

df = read.csv(list.files()[1], stringsAsFactors = F, header = F)
colnames(df) = c("V1", substr(list.files()[1],1,nchar(list.files()[1])-12))

for(i in 2:length(list.files())){
    df_temp = read.csv(list.files()[i], stringsAsFactors = F, header = F)
    colnames(df_temp)[2] = substr(list.files()[i],1,nchar(list.files()[i])-12)
    df = merge(df,df_temp,by = "V1", all = T)
}

df$a = rep(0, nrow(df))
colnames(df)[ncol(df)] = "我有一個請求，你OOO的時候不能XX，若有XX，我要跟你OO"

df[is.na(df)] = 0

colnames(df)[1] = "Date"

df$time_ind = rep(0, nrow(df))
for(i in 1:nrow(df)){
    df$time_ind[i] = substr(df$Date[i],6,7)
}

df_month = data.frame(a = rep(0,12))
for(i in 2:13){
    df_month = cbind(df_month, as.vector(tapply(df[,i], df$time_ind, sum)))
    colnames(df_month)[i] = colnames(df)[i]
}

colnames(df_month)[1] = "Month"
df_month[1] = 1:12

mean_sent = sapply(2:(ncol(df_month)), function(i) mean(df_month[,i]))
cv_sent = sapply(2:(ncol(df_month)), function(i) sd(df_month[,i])/mean(df_month[,i]))
survivor = ifelse((mean_sent>=1 & cv_sent<=1), 1, 0)

###
# Questionnaire outcome
setwd("D:/Kile/語言分析與資料科學/Final/sentence_data/hypothesis_clean")

sent_easy2know = read.csv("sent_easy2know.csv", stringsAsFactors = F)
sent_easy2makesents = read.csv("sent_easy2makesents.csv", stringsAsFactors = F)
sent_easy2talk = read.csv("sent_easy2talk.csv", stringsAsFactors = F)

sent_easy2know[,1] = NULL
sent_easy2makesents[,1] = NULL
sent_easy2talk[,1] = NULL

sent_easy2know = sent_easy2know[,c(1,2,6,12,3,8,4,5,10,11,9,7)]
colnames(sent_easy2know) = colnames(df)[2:13]

sent_easy2makesents = sent_easy2makesents[,c(1,2,6,12,3,8,4,5,10,11,9,7)]
colnames(sent_easy2makesents) = colnames(df)[2:13]

sent_easy2talk = sent_easy2talk[,c(1,2,6,12,3,8,4,5,10,11,9,7)]
colnames(sent_easy2talk) = colnames(df)[2:13]

sent_easy2know = apply(t(sent_easy2know), 1, mean)
sent_easy2makesents = apply(t(sent_easy2makesents), 1, mean)
sent_easy2talk = apply(t(sent_easy2talk), 1, mean)

df_test = cbind(sent_easy2know, sent_easy2makesents, sent_easy2talk)
df_test = as.data.frame(df_test)
df_test$survivor = survivor

#tapply(df_test$sent_easy2know, df_test$survivor, mean)
#tapply(df_test$sent_easy2makesents, df_test$survivor, mean)
#tapply(df_test$sent_easy2talk, df_test$survivor, mean)

#colnames(df)[2:13][survivor==1]
#colnames(df)[2:13][survivor==0]

# Implementing the two-sample t test, before each t test, we have to do the F test to check if each
# group has the same variance

var.test(df_test$sent_easy2know[df_test$survivor == 1],df_test$sent_easy2know[df_test$survivor == 0])
t.test(df_test$sent_easy2know[df_test$survivor == 1],df_test$sent_easy2know[df_test$survivor == 0], var.equal=TRUE, paired=FALSE)

var.test(df_test$sent_easy2makesents[df_test$survivor == 1],df_test$sent_easy2makesents[df_test$survivor == 0])
t.test(df_test$sent_easy2makesents[df_test$survivor == 1],df_test$sent_easy2makesents[df_test$survivor == 0], var.equal=TRUE, paired=FALSE)

var.test(df_test$sent_easy2talk[df_test$survivor == 1],df_test$sent_easy2talk[df_test$survivor == 0])
t.test(df_test$sent_easy2talk[df_test$survivor == 1],df_test$sent_easy2talk[df_test$survivor == 0], var.equal=TRUE, paired=FALSE)

