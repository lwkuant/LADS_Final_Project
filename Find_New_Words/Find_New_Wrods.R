### find new words generated from the 太陽花學運

# load data and combine data
word_gossiping = readLines("ptt_Gossiping_article.txt",encoding = "utf8")
word_hatepolitics = readLines("ptt_HatePolitics_article.txt", encoding = "utf8")
word = c(word_gossiping, word_hatepolitics)

# separate the lines into 4 sections and put them into the data frame
word_matrix = matrix(rep("0",length(word)*4),c(length(word), 4), byrow = T)
for(i in 1:length(word)){
    if(length(strsplit(word[i], "&&&")[[1]]) == 4){
        word_matrix[i,] = strsplit(word[i], "&&&")[[1]]
    }else{
        next
    }
}
ptt_df = as.data.frame(word_matrix, stringsAsFactors = F)

# month transform
for(i in 1:length(ptt_df[,2])){
    if(substr(ptt_df[,2][i],6,8)=="Jan"){
        ptt_df[,2][i]=gsub("Jan","01",ptt_df[,2][i])
    }else if(substr(ptt_df[,2][i],6,8)=="Feb"){
        ptt_df[,2][i]=gsub("Feb","02",ptt_df[,2][i])
    }else if(substr(ptt_df[,2][i],6,8)=="Mar"){
        ptt_df[,2][i]=gsub("Mar","03",ptt_df[,2][i])
    }else if(substr(ptt_df[,2][i],6,8)=="Apr"){
        ptt_df[,2][i]=gsub("Apr","04",ptt_df[,2][i])
    }else if(substr(ptt_df[,2][i],6,8)=="May"){
        ptt_df[,2][i]=gsub("May","05",ptt_df[,2][i])
    }else if(substr(ptt_df[,2][i],6,8)=="Jun"){
        ptt_df[,2][i]=gsub("Jun","06",ptt_df[,2][i])
    }else if(substr(ptt_df[,2][i],6,8)=="Jul"){
        ptt_df[,2][i]=gsub("Jul","07",ptt_df[,2][i])
    }else if(substr(ptt_df[,2][i],6,8)=="Aug"){
        ptt_df[,2][i]=gsub("Aug","08",ptt_df[,2][i])
    }else if(substr(ptt_df[,2][i],6,8)=="Sep"){
        ptt_df[,2][i]=gsub("Sep","09",ptt_df[,2][i])
    }else if(substr(ptt_df[,2][i],6,8)=="Oct"){
        ptt_df[,2][i]=gsub("Oct","10",ptt_df[,2][i])
    }else if(substr(ptt_df[,2][i],6,8)=="Nov"){
        ptt_df[,2][i]=gsub("Nov","11",ptt_df[,2][i])
    }else if(substr(ptt_df[,2][i],6,8)=="Dec"){
        ptt_df[,2][i]=gsub("Dec","12",ptt_df[,2][i])
    }
}

# convert the data frame into xts object
library(xts)
date = as.Date(ptt_df[,2], format = "%Y-%m-%d")
ptt_df[,2] = NULL
ptt_df = xts(ptt_df, order.by = date)

# split the xts into two parts by time(the former and the latter parts, split at 3/18)
colnames(ptt_df) = c("Title", "Article", "Comment")
ptt_df_former = ptt_df[index(ptt_df)<"2014-03-18",]
ptt_df_latter = ptt_df[index(ptt_df)>="2014-03-18",]

# use jiebaR to cut the words in Title, Content and Comment, then combine them
library(jiebaR)
cutter = worker()

# words transformation function for the former part
cut_word_and_concatenate = function(vec){
    cut_word_vec = lapply(1:length(vec), function(i) gsub("[[:punct:]]", "", vec[i]))
    #cut_word_vec = paste(unlist(cut_word_vec), collapse = "_")
    cut_word_vec
}
combine_word = function(df){
    ti = df[,1]
    article = df[,2]
    ti = cut_word_and_concatenate(ti)    
    article = cut_word_and_concatenate(article)
    comm = df[,3]
    comm = lapply(1:length(comm), function(i) strsplit(comm[i],"@@@"))
    comm = lapply(1:length(comm), function(i) substr(unlist(comm[[i]]),rep(3,length(comm[[i]])),as.vector(sapply(comm[[i]],nchar))))
    comm = lapply(1:length(comm), function(i) cut_word_and_concatenate(comm[[i]]))
    word_combined = c(unlist(ti), unlist(article), unlist(comm))
    word_combined
}

# words transformation function for the latter part
cut_word = function(vec){
    cut_word_vec = lapply(1:length(vec), function(i) gsub(" ", "", vec[i]))
    cut_word_vec = lapply(1:length(cut_word_vec), function(i) gsub("　", "", cut_word_vec[[i]]))
    cut_word_vec = lapply(1:length(cut_word_vec), function(i) cutter[cut_word_vec[[i]]])
    cut_word_vec
}

cut_and_combine = function(df){
    ti = df[,1]
    article = df[,2]
    comm = df[,3]
    comm = lapply(1:length(comm), function(i) strsplit(comm[i],"@@@"))
    comm = as.vector(lapply(1:length(comm), function(i) substr(unlist(comm[[i]]),rep(3,length(comm[[i]])),as.vector(sapply(comm[[i]],nchar)))))
    ti = cut_word(ti)
    article = cut_word(article)
    comm = unlist(comm)
    ls = cutter[comm]
    cut_word_vec = list(ti, article, ls)
    cut_word_vec
}

# transform the former and latter parts using respective function 
ptt_cut_word_former = unique(combine_word(ptt_df_former))
ptt_cut_word_latter = cut_and_combine(ptt_df_latter)

# clean some objects
rm(word, word_gossiping, word_hatepolitics, word_matrix)

# prepare the data for setting up the freq table for latter set
cut_word_latter_table = c(unlist(ptt_cut_word_latter[[1]]), unlist(ptt_cut_word_latter[[2]]),
                          unlist(ptt_cut_word_latter[[3]]))
cut_word_latter_table = table(cut_word_latter_table)[table(cut_word_latter_table) >= 0.1*nrow(ptt_df_latter)]
cut_word_latter_table = cut_word_latter_table[order(cut_word_latter_table, decreasing = T)]

# steps to finding new words
# step 1: use the table consisting of words from latter part to sift the word freq >= 10% of the articles, appearing 1 time
# step 2: use the former set to sift the word(not appear in the former set)
# step 3: use the latter set to sift the words' freq at least 60(30 days, each at morning and night)

### step 1
# set up the function serving as sifting the new words
check_gram_in_table = function(latter_set_list, check_table){
    ind = 0
    qualified_words = list()
    for(i in 1:10){
        for(j in 1:length(latter_set_list)){
            for(t in 1:length(latter_set_list[[j]])){
                if(length(latter_set_list[[j]]) < i){
                    break
                }else if(t>(t+i-1)){
                    break
                }else if(sum(latter_set_list[[j]][t:(t+i-1)] %in% names(check_table)) == i){
                    ind = ind + 1
                    qualified_words[[ind]] = latter_set_list[[j]][t:(t+i-1)]
                    #print(ind)
                }
            }
        }
        
    }
    unique(qualified_words)
}

### because the process is time_comsuming, we instead loaded the previously-run variables for displaying intention
# the two lines below are the original filtering codes to sift new words from titles and atricles from latter words' table
#title_check = check_gram_in_table(ptt_cut_word_latter[[1]], cut_word_latter_table)
#article_check = check_gram_in_table(ptt_cut_word_latter[[2]], cut_word_latter_table)
load("title_check.rda")
load("article_check.rda")

### step 2
helper_check_in_former = function(vec){
    word = paste(vec[1:(length(vec))], collapse = "")
    return(sign(sum(grepl(word, ptt_cut_word_former),na.rm = T)))
}


### because the process is time_comsuming, we instead loaded the previously-run variables for displaying intention
# the two lines below are the original filtering codes to sift new words from titles and atricles from former articles
#title_check_former = sapply(1:length(title_check), function(i) helper_check_in_former(title_check[[i]]))
#article_check_former = sapply(1:length(article_check), function(i) helper_check_in_former(article_check[[i]]))
load("title_check_former.rda")
load("article_check_former.rda")
temp_title_check_former = title_check[title_check_former == 0]
temp_article_check_former = article_check[article_check_former == 0]

### step 3
ptt_cut_word_latter_checker = combine_word(ptt_df_latter)

helper_check_in_latter = function(vec){
    word = paste(vec[1:(length(vec))], collapse = "")
    return(sum(grepl(word, ptt_cut_word_latter_checker),na.rm = T))
}

### because the process is time_comsuming, we instead loaded the previously-run variables for displaying intention
# the four lines below are the original filtering codes to sift new words from titles and atricles from latter articles
#title_check_latter = sapply(1:length(temp_title_check_former),
#                            function(i) helper_check_in_latter(temp_title_check_former[[i]]))
#article_check_latter = sapply(1:length(temp_article_check_former),
#                            function(i) helper_check_in_latter(temp_article_check_former[[i]]))
load("title_check_latter.rda")
load("article_check_latter.rda")
temp_title_check_latter = temp_title_check_former[title_check_latter >= 60]
temp_article_check_latter = temp_article_check_former[article_check_latter >= 60]

# combine the filtered new words' n-grams from the title and article sets
new_words_parts = union(temp_title_check_latter, temp_article_check_latter)
subset_ind_nchar = sapply(1:length(new_words_parts), function(i) sum(nchar(new_words_parts[[i]])))
new_words_parts = new_words_parts[subset_ind_nchar <= 5]
subset_ind_duplicate = duplicated(sapply(1:length(new_words_parts), function(i) paste(new_words_parts[[i]], collapse = "")))
new_words_parts = new_words_parts[subset_ind_duplicate == FALSE]

# rearrange the article data frame for counting the new words' appearances by time and each statement
# any appearance in each statement(including title, article and comment) is counted as appearing once 
comm = ptt_df_latter[,3]
comm = lapply(1:length(comm), function(i) strsplit(comm[i],"@@@"))
comm = as.vector(lapply(1:length(comm), function(i) substr(unlist(comm[[i]]),rep(3,length(comm[[i]])),as.vector(sapply(comm[[i]],nchar)))))
ptt_df_latter_df = as.data.frame(ptt_df_latter, stringsAsFactors = F)
ptt_df_latter_df$comment_group = lapply(1:length(comm), function(i) c(comm[[i]][1:length(comm[[i]])]))
ptt_df_latter_df$Comment = NULL
ptt_df_latter_df$Date = index(ptt_df_latter)
ptt_df_latter_df$words = lapply(1:nrow(ptt_df_latter_df),
                                function(i) c(ptt_df_latter_df[i,1],
                                              ptt_df_latter_df[i,2], unlist(ptt_df_latter_df[i,3])))
ptt_df_latter_df$Title = NULL
ptt_df_latter_df$Article = NULL
ptt_df_latter_df$comment_group = NULL

# set up the data frame representing the words' freq by time 
count_new_words_freq = function(vec, words){
    count = sum(grepl(vec, words))
    count
}

a = numeric(nrow(ptt_df_latter_df))
words_count_df = data.frame(a)

for(i in 1:length(new_words_parts)){
    temp = sapply(1:nrow(ptt_df_latter_df),
                  function(j) count_new_words_freq(paste(new_words_parts[[i]], collapse = ""), unlist(ptt_df_latter_df[j,2])))
    words_count_df = cbind(words_count_df, temp)
}

words_count_df$a = NULL
colnames(words_count_df) = sapply(1:length(new_words_parts),
                                  function(i) paste(new_words_parts[[i]], collapse = ""))

date_ind = unique(index(ptt_df_latter))
words_count_df$Date = ptt_df_latter_df[,1]

a = numeric(length(date_ind))
words_count_by_time = data.frame(a) 

for(i in 1:length(new_words_parts)){
    temp = tapply(words_count_df[,i], words_count_df$Date, sum)            
    words_count_by_time = cbind(words_count_by_time, temp)
}

words_count_by_time$a = NULL
colnames(words_count_by_time) = sapply(1:length(new_words_parts),
                                       function(i) paste(new_words_parts[[i]], collapse = ""))

words_count_by_time$date = date_ind
rownames(words_count_by_time) = seq(1,nrow(words_count_by_time))

# display the new words
new_words = colnames(words_count_df)
new_words

# plot the words' freq by time
# use ggplot2
library("reshape")
words_count_by_time_melt <- melt(words_count_by_time, id="date")

library(ggplot2)
ggplot(data = words_count_by_time_melt, aes(x = date,y = value, colour = variable))+geom_line()

# compare "太陽花", "大腸花" using ggplot2
x = words_count_by_time$date
y1 = words_count_by_time$太陽花
y2 = words_count_by_time$大腸花
plot_df = data.frame(x = rep(x, 2), y = c(y1, y2), words = as.factor(rep(c("太陽花", "大腸花"), each = 34)))
ggplot(plot_df, aes(x=x,y=y,group=words)) + geom_line(aes(colour=words, size=words)) + 
    scale_size_manual(values=c(1.2,1.2)) + scale_color_manual(values=c('dodgerblue2','firebrick2')) +
    xlab("Date") + ylab("Frequency") + theme(legend.position="bottom") +
    ggtitle("太陽花和大腸花詞頻比較")

# use base plotting system, compare "太陽花", "大腸花"
#plot(x = words_count_by_time$date,
#     y = words_count_by_time$太陽花,type = "l", lwd = 2, col = "firebrick1",
#     ylab = "詞頻", xlab = "時間", main = "太陽花和大腸花詞頻比較" )
#lines(x = words_count_by_time$date, y = words_count_by_time$大腸花, col = "gray25", lwd = 2)
#legend("topright", c("太陽花","大腸花"), lwd = c(2,2),col = c("firebrick1", "gray25"), cex = 0.7, bty= "n")

### saving time-comsuming object
#save(title_check, file = "title_check.rda")
#save(title_check_former, file = "title_check_former.rda")
#save(title_check_latter, file = "title_check_latter.rda")
#save(article_check, file = "article_check.rda")
#save(article_check_former, file = "article_check_former.rda")
#save(article_check_latter, file = "article_check_latter.rda")

