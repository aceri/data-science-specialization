rm(list = ls(all.names = TRUE))
gc()


library(corpus)
library(tm)
library(tidytext)
library(tidyverse)
library(qdapDictionaries)
library(qdapTools)
library(qdapRegex)
library(stringi)
library(sqldf)
library(caret)
library(healthcareai)
library(textclean)
library(textshape)
library(machineLearning)



q<-full[sample(1:nrow(full),1),]
q
clean_line(q,ret_words = F,min_chars = 1,use_dictionary = T,dictionary = GradyAugmented,correct = T)


clean_line <- function(line,ret_words=F,min_chars=1,use_dictionary=T,dictionary=GradyAugmented,correct=T)
{
    ret <- line
    if (correct==T) { ret <- unlist(stri_split_regex(ret," ")) ; 
                      ret <- replace_internet_slang(ret) ;    
                      ret <- replace_time(ret); 
                      ret <- replace_kern(ret) ; 
                      ret <- replace_contraction(ret) ;  
                      ret <- replace_hash(ret) ; 
                      ret <- replace_emoticon(ret);
                      ret <- replace_rating(ret) ; 
                      ret <- replace_number(ret) ; 
                      ret <- replace_tag(ret) ; 
                      ret <- replace_word_elongation(ret) ; 
                      ret <- replace_ordinal(ret) ; 
                      ret <- replace_emoticon(ret) ; 
                      ret <- replace_incomplete(ret) 
                      ret <- gsub(",","",toString(unlist(ret)))

    ret <- ret %>% tolower() %>% gsub("[[:digit:]]","",.) %>% gsub("(.)\\1{2,}","",.) %>% gsub("[^abcdefghijklmnopqrstuvwxyz' ]+","",.) %>% gsub("^[ ']+|[ ']$+","",.) %>% gsub(" +"," ",.) }
    if (ret_words == T & min_chars == 1 & use_dictionary == T) { ret <- unlist(stri_split_regex(ret," ")) ; ret <- ret[ret %in% dictionary] }
    if (ret_words == T & min_chars > 1 & use_dictionary == T)  { ret <- unlist(stri_split_regex(ret," ")) ; ret <- ret[nchar(ret)>=min_chars] ; ret <- ret[ret %in% dictionary] }
    if (ret_words == T & min_chars == 1 & use_dictionary == F) { ret <- unlist(stri_split_regex(ret," ")) }
    if (ret_words == T & min_chars > 1 & use_dictionary == F)  { ret <- unlist(stri_split_regex(ret," ")) ; ret <- ret[nchar(ret)>=min_chars] }
    if (ret_words == F & min_chars == 1 & use_dictionary == T) { ret <- unlist(stri_split_regex(ret," ")) ; ret <- ret[ret %in% dictionary] ; ret <- gsub(",","",unlist(toString(ret))) }
    if (ret_words == F & min_chars > 1 & use_dictionary == T)  { ret <- unlist(stri_split_regex(ret," ")) ; ret <- ret[nchar(ret)>=min_chars] ; ret <- ret[ret %in% dictionary] ; ret <- gsub(",","",unlist(toString(ret))) }
    # if (ret_words == F & min_chars == 1 & use_dictionary == F) <- Nothing to do.
    if (ret_words == F & min_chars > 1 & use_dictionary == F)  { ret <- unlist(stri_split_regex(ret," ")) ; ret <- ret[nchar(ret)>=min_chars] ; ret <- gsub(",","",unlist(toString(ret))) }
    return(ret)
}


clean_corpus <- function(text_tibble, min_chars=1,use_dictionary=T,correct=T,verbose=0,dictionary=GradyAugmented)
{
    # Needs a tibble of the text with each line being in a column txt
    
    txt <- text_tibble
    num_rows=nrow(txt)
    for (k in 1:num_rows)
    {
        txt[k,1] <- clean_line(txt[k,1],ret_words = F,min_chars = min_chars, use_dictionary = use_dictionary,dictionary=dictionary,correct=correct)
        if(verbose >0 & k %% 100 == 0) {print(paste("clean_text > Processed ",round(k/num_rows*100,2)," %",sep=""))}
    }
    return(txt)
}

# QUIZ 3 PREDICT THE FOLLOWING WORD    
q1<-"The guy in front of me just bought a pound of bacon, a bouquet, and a case of"
q2<-"You're the reason why I smile everyday. Can you follow me please? It would mean the"
q3<-"Hey sunshine, can you follow me and make me the"
q4<-"Very early observations on the Bills game: Offense still struggling but the"
q5<-"Go on a romantic date at the"
q6<-"Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my"
q7<-"Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some"
q8<-"After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little"
q9<-"Be grateful for the good times and keep the faith during the"
# QUIZ 3 PREDICT THE FOLLOWING WORD
q10<-"When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd"
q11<-"Guy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his"
q12<-"I'd give anything to see arctic monkeys this"
q13<-"Talking to your mom has the same effect as a hug and helps reduce your"
q14<-"When you were in Holland you were like 1 inch away from me but you hadn't time to take a"
q15<-"I'd just like all of these questions answered, a presentation of evidence, and a jury to settle the"
q16<-"I can't deal with unsymetrical things. I can't even hold an uneven number of bags of groceries in each"
q17<-"Every inch of you is perfect from the bottom to the"
q18<-"I’m thankful my childhood was filled with imagination and bruises from playing"
q19<-"I like how the same people are in almost all of Adam Sandler's"



folder <- "./data/"


blogs_str   <- paste("./Coursera-SwiftKey/final/en_US/en_US.blogs.txt"   ,sep="")
news_str    <- paste("./Coursera-SwiftKey/final/en_US/en_US.news.txt"    ,sep="")
twitter_str <- paste("./Coursera-SwiftKey/final/en_US/en_US.twitter.txt" ,sep="")

blogs_dat <- tibble(scan(blogs_str,"character",sep="\n",skipNul = T,encoding="UTF-8",quiet=T))
news_dat <- tibble(scan(news_str,"character",sep="\n",skipNul = T,encoding="UTF-8",quiet=T))
twitter_dat <- tibble(scan(twitter_str,"character",sep="\n",skipNul = T,encoding="UTF-8",quiet=T))



# THIS IS WITH LIBRARY CORPUS

full <- rbind(blogs_dat,news_dat,twitter_dat)
rm(blogs_dat,news_dat,twitter_dat,folder,blogs_str,news_str,twitter_str)
names(full) <- c("text")
full[sample(1:nrow(full),round(nrow(full),0)*0.25),]  -> full

full1 <- full %>% clean_corpus(1,T,T,verbose = 1,GradyAugmented)
full1 <- full1[full1$text!="",]
saveRDS(full1,"./data/full1.rds")
#full1 <- readRDS("./data/full1.rds")

full2 <- full1 %>% clean_corpus(3,F,F,verbose = 1,GradyAugmented)
full2 <- full2[full2$text!="",]
saveRDS(full2,"./data/full2.rds")
#full2 <- readRDS("./data/full2.rds")




full1 <- corpus_frame("full1",full1)
full2 <- corpus_frame("full2",full2)


#text_tokens(full)

#text_filter(full)
text_filter(full1)$map_case <- TRUE
text_filter(full1)$remove_ignorable <- TRUE
text_filter(full1)$drop_punct <- TRUE
text_filter(full1)$drop_symbol <- TRUE
text_filter(full2)$map_case <- TRUE
text_filter(full2)$remove_ignorable <- TRUE
text_filter(full2)$drop_punct <- TRUE
text_filter(full2)$drop_symbol <- TRUE



stop_words <- c(stopwords_fr,
                   stopwords_de,
                   stopwords_es,
                   stopwords_it,
                   stopwords_ru,
                   stopwords_sv,
                   stopwords_pt,
                   stopwords_no,
                   stopwords_hu,
                   stopwords_fi,
                   stopwords_en)



ng1 <- term_stats(full2,types=TRUE)
ng1<-ng1[ng1$support > 1 & ng1$count > 1,]
saveRDS(ng1,"./data/ng1.rds")
#ng1 <- readRDS("./data/ng1.rds")
#-------------------------------------------------------------------
#-------------------------------------------------------------------
ng2a <- term_stats(full1,types=TRUE,ngrams=2)
ng2a <- ng2a[ng2a$support>1 & ng2a$count>1,]
saveRDS(ng2a,"./data/ng2a.rds")
#ng2a <- readRDS("./data/ng2a.rds")

ng3a <- term_stats(full1,types=TRUE,ngrams=3)
ng3a <- ng3a[ng3a$support>1 & ng3a$count>1,]
saveRDS(ng3a,"./data/ng3a.rds")
#ng3a <- readRDS("./data/ng3a.rds")

ng4a <- term_stats(full1,types=TRUE,ngrams=4)
ng4a <- ng4a[ng4a$support>1 & ng4a$count>1,]
saveRDS(ng4a,"./data/ng4a.rds")
#ng4a <- readRDS("./data/ng4a.rds")

ng5a <- term_stats(full1,types=TRUE,ngrams=5)
ng5a <- ng5a[ng5a$support>1 & ng5a$count>1,]
saveRDS(ng5a,"./data/ng5a.rds")
#ng5a <- readRDS("./data/ng5a.rds")

ng6a <- term_stats(full1,types=TRUE,ngrams=6)
ng6a <- ng6a[ng6a$support>1 & ng6a$count>1,]
saveRDS(ng6a,"./data/ng6a.rds")
#-------------------------------------------------------------------
ng2b <- term_stats(full2,types=TRUE,ngrams=2)
ng2b <- ng2b[ng2b$support>1 & ng2b$count>1,]
saveRDS(ng2b,"./data/ng2b.rds")
#ng2b <- readRDS("./data/ng2b.rds")

ng3b <- term_stats(full2,types=TRUE,ngrams=3)
ng3b <- ng3b[ng3b$support>1 & ng3b$count>1,]
saveRDS(ng3b,"./data/ng3b.rds")
#ng3b <- readRDS("./data/ng3b.rds")

ng4b <- term_stats(full2,types=TRUE,ngrams=4)
ng4b <- ng4b[ng4b$support>1 & ng4b$count>1,]
saveRDS(ng4b,"./data/ng4b.rds")
#ng4b <- readRDS("./data/ng4b.rds")

ng5b <- term_stats(full2,types=TRUE,ngrams=5)
ng5b <- ng5b[ng5b$support>1 & ng5b$count>1,]
saveRDS(ng5b,"./data/ng5b.rds")
#ng5b <- readRDS("./data/ng5b.rds")

ng6b <- term_stats(full2,types=TRUE,ngrams=6)
ng6b <- ng6b[ng6b$support>1 & ng6b$count>1,]
saveRDS(ng6b,"./data/ng6b.rds")
#ng6b <- readRDS("./data/ng6b.rds")
#-------------------------------------------------------------------
text_filter(full1)$stemmer <- stem_snowball
text_filter(full2)$stemmer <- stem_snowball
#-------------------------------------------------------------------
ng2c <- term_stats(full1,types=TRUE,ngrams=2)
ng2c <- ng2c[ng2c$support>1 & ng2c$count>1,]
saveRDS(ng2c,"./data/ng2c.rds")
#ng2c <- readRDS("./data/ng2c.rds")

ng3c <- term_stats(full1,types=TRUE,ngrams=3)
ng3c <- ng3c[ng3c$support>1 & ng3c$count>1,]
saveRDS(ng3c,"./data/ng3c.rds")
#ng3c <- readRDS("./data/ng3c.rds")

ng4c <- term_stats(full1,types=TRUE,ngrams=4)
ng4c <- ng4c[ng4c$support>1 & ng4c$count>1,]
saveRDS(ng4c,"./data/ng4c.rds")
#ng4c <- readRDS("./data/ng4c.rds")

ng5c <- term_stats(full1,types=TRUE,ngrams=5)
ng5c <- ng5c[ng5c$support>1 & ng5c$count>1,]
saveRDS(ng5c,"./data/ng5c.rds")
#ng5c <- readRDS("./data/ng5c.rds")

ng6c <- term_stats(full1,types=TRUE,ngrams=6)
ng6c <- ng6c[ng6c$support>1 & ng6c$count>1,]
saveRDS(ng6c,"./data/ng6c.rds")
#ng6c <- readRDS("./data/ng6c.rds")
#-------------------------------------------------------------------
ng2d <- term_stats(full2,types=TRUE,ngrams=2)
ng2d <- ng2d[ng2d$support>1 & ng2d$count>1,]
saveRDS(ng2d,"./data/ng2d.rds")
#ng2d <- readRDS("./data/ng2d.rds")

ng3d <- term_stats(full2,types=TRUE,ngrams=3)
ng3d <- ng3d[ng3d$support>1 & ng3d$count>1,]
saveRDS(ng3d,"./data/ng3d.rds")
#ng3d <- readRDS("./data/ng3d.rds")

ng4d <- term_stats(full2,types=TRUE,ngrams=4)
ng4d <- ng4d[ng4d$support>1 & ng4d$count>1,]
saveRDS(ng4d,"./data/ng4d.rds")
#ng4d <- readRDS("./data/ng4d.rds")

ng5d <- term_stats(full2,types=TRUE,ngrams=5)
ng5d <- ng5d[ng5d$support>1 & ng5d$count>1,]
saveRDS(ng5d,"./data/ng5d.rds")
#ng5d <- readRDS("./data/ng5d.rds")

ng6d <- term_stats(full2,types=TRUE,ngrams=6)
ng6d <- ng6d[ng6d$support>1 & ng6d$count>1,]
saveRDS(ng6d,"./data/ng6d.rds")
#ng6d <- readRDS("./data/ng6d.rds")
#-------------------------------------------------------------------

###############################################



predict_word <- function(line, test=F,num_replies=1)
{
    fill_df <- function(ng,num_ng,stem=F,min_char=1)
    {
        if (nrow(ng)>0){
            df <- data.frame(ng=integer(), word=character(), count=numeric(), support=numeric(),stem=integer(),min_char=integer(),solution=character(),hit=numeric(),stringsAsFactors = F)
            df[1:nrow(ng),1] <- num_ng
            df[1:nrow(ng),2] <- ng[,(1+num_ng)]
            df[1:nrow(ng),3] <- round(ng$count/sum(ng$count)*100,2)
            df[1:nrow(ng),4] <- round(ng$support/sum(ng$support)*100,2)
            df[1:nrow(ng),5] <- ifelse(stem==F,0,1)
            df[1:nrow(ng),6] <- min_char
            df[1:nrow(ng),7] <- solution
            df[1:nrow(ng),8] <- ifelse(ng[,(1+num_ng)]==solution,round(ng$count/sum(ng$count)*100,2),0)
        } else {df <- ng}
        return(df)
    }
    
    if(test==T) {
        line=paste(line,"UNKNOWN",sep=" ") 
    }
    words <- unlist(stri_split_regex(line," "))
    solution <- words[length(words)]
    print(paste("predict_word       > ", gsub(",","",toString(words[1:(length(words)-1)]))," [ ",solution," ]",sep=""))
    
    
    words <- unlist(text_tokens(line,filter=text_filter(drop_punct=T,map_case=T,drop_symbol=T,drop_number = T,stemmer=stem_snowball)))
    num_words <- length(words)
    if(num_words>0) l0 <- words[num_words-0]
    if(num_words>1) l1 <- words[num_words-1]
    if(num_words>2) l2 <- words[num_words-2]
    if(num_words>3) l3 <- words[num_words-3]
    if(num_words>4) l4 <- words[num_words-4]
    if(num_words>5) l5 <- words[num_words-5]
    if(num_words>6) l6 <- words[num_words-6]
    #-----------------------------------------------------------------------------------------------------
    #-----------------------------------------------------------------------------------------------------    
    if(num_words>1) r_ng2a <- ng2a[ng2a$type1==l1,]    
    if(num_words>2) r_ng3a <- ng3a[ng3a$type1==l2 & ng3a$type2==l1,]
    if(num_words>3) r_ng4a <- ng4a[ng4a$type1==l3 & ng4a$type2==l2 & ng4a$type3==l1,]
    if(num_words>4) r_ng5a <- ng5a[ng5a$type1==l4 & ng5a$type2==l3 & ng5a$type3==l2 & ng5a$type4==l1,]
    if(num_words>5) r_ng6a <- ng6a[ng6a$type1==l5 & ng6a$type2==l4 & ng6a$type3==l3 & ng6a$type4==l2 & ng6a$type5==l1,]
    #-----------------------------------------------------------------------------------------------------    
    if(num_words>1) r_ng2b <- ng2b[ng2b$type1==l1,]    
    if(num_words>2) r_ng3b <- ng3b[ng3b$type1==l2 & ng3b$type2==l1,]
    if(num_words>3) r_ng4b <- ng4b[ng4b$type1==l3 & ng4b$type2==l2 & ng4b$type3==l1,]
    if(num_words>4) r_ng5b <- ng5b[ng5b$type1==l4 & ng5b$type2==l3 & ng5b$type3==l2 & ng5b$type4==l1,]
    if(num_words>5) r_ng6b <- ng6b[ng6b$type1==l5 & ng6b$type2==l4 & ng6b$type3==l3 & ng6b$type4==l2 & ng6b$type5==l1,]
    #-----------------------------------------------------------------------------------------------------    
    if(num_words>1) r_ng2c <- ng2c[ng2c$type1==l1,]        
    if(num_words>2) r_ng3c <- ng3c[ng3c$type1==l2 & ng3c$type2==l1,]
    if(num_words>3) r_ng4c <- ng4c[ng4c$type1==l3 & ng4c$type2==l2 & ng4c$type3==l1,]
    if(num_words>4) r_ng5c <- ng5c[ng5c$type1==l4 & ng5c$type2==l3 & ng5c$type3==l2 & ng5c$type4==l1,]
    if(num_words>5) r_ng6c <- ng6c[ng6c$type1==l5 & ng6c$type2==l4 & ng6c$type3==l3 & ng6c$type4==l2 & ng6c$type5==l1,]
    #-----------------------------------------------------------------------------------------------------    
    if(num_words>1) r_ng2d <- ng2d[ng2d$type1==l1,]            
    if(num_words>2) r_ng3d <- ng3d[ng3d$type1==l2 & ng3d$type2==l1,]
    if(num_words>3) r_ng4d <- ng4d[ng4d$type1==l3 & ng4d$type2==l2 & ng4d$type3==l1,]
    if(num_words>4) r_ng5d <- ng5d[ng5d$type1==l4 & ng5d$type2==l3 & ng5d$type3==l2 & ng5d$type4==l1,]
    if(num_words>5) r_ng6d <- ng6d[ng6d$type1==l5 & ng6d$type2==l4 & ng6d$type3==l3 & ng6d$type4==l2 & ng6d$type5==l1,]
    
    
    pre_final_a <- rbind(fill_df(r_ng2a,2,F,3),
                         fill_df(r_ng3a,3,F,3),
                         fill_df(r_ng4a,4,F,3),
                         fill_df(r_ng5a,5,F,3),
                         fill_df(r_ng6a,6,F,3))
    pre_final_b <- rbind(fill_df(r_ng2b,2,T,3),
                         fill_df(r_ng3b,3,T,3),
                         fill_df(r_ng4b,4,T,3),
                         fill_df(r_ng5b,5,T,3),
                         fill_df(r_ng6b,6,T,3))
    pre_final_c <- rbind(fill_df(r_ng2c,2,F,1),
                         fill_df(r_ng3c,3,F,1),
                         fill_df(r_ng4c,4,F,1),
                         fill_df(r_ng5c,5,F,1),
                         fill_df(r_ng6c,6,F,1))
    pre_final_d <- rbind(fill_df(r_ng2d,2,T,1),
                         fill_df(r_ng3d,3,T,1),
                         fill_df(r_ng4d,4,T,1),
                         fill_df(r_ng5d,5,T,1),
                         fill_df(r_ng6d,6,T,1))

    pre_final <- rbind(pre_final_a,pre_final_b,pre_final_c,pre_final_d)
    pre_final <- head(pre_final[order(-pre_final$count),],100)
    return(head(pre_final,num_replies))
}


get_random_str <- function(text_tibble,min_words=7,verbose=0)
{
    query=""
    while(query=="") 
    {
        dice <- sample(1:max(nrow(text_tibble)),1,replace=T)
        query_str <- text_tibble$text[dice] %>%
            clean_line(ret_words=T)
        num_words <- length(query_str);query_str
        if (num_words > min_words & sum(query_str %in% GradyAugmented) == num_words & nchar(query_str[num_words]) > 1)
        {
            random_start_preserving_min_words <- sample(1:(num_words-min_words),1)
            minimum_pos <- random_start_preserving_min_words
            minimum_guarantee <- (random_start_preserving_min_words+min_words)
            expendable_random <- sample(0:(num_words-minimum_guarantee),1)
            query <- gsub(",","",toString(query_str[minimum_pos:(minimum_guarantee+expendable_random)]))
        }
    }
    return(query)
}


build_training_set <- function(tr,iter=1000,verbose=0)
{
    tmp <- data.frame(ng=integer(), word=character(), count=numeric(), support=numeric(),stem=integer(),solution=character(),hit=numeric(),stringsAsFactors = F)
    for (z in 1:iter)
    {
        if (verbose >0 & z %% 100 == 0) print(paste("Iter ",z," of ",iter,sep=""))
        qq <- get_random_str(full1,8,verbose=verbose)
        tmp <- rbind(tmp,predict_word(line = qq, test = F, num_replies = 100))
    }
    tr <- rbind(tr,tmp)
    return(tr)
}


#tr <- data.frame(ng=integer(), word=character(), count=numeric(), support=numeric(),stem=integer(),solution=character(),hit=numeric(),stringsAsFactors = F)
tr <- build_training_set(tr,iter=10000,verbose=1)

saveRDS(tr,"./data/tr.rds")


prep <- tr[,c(-2,-7)]

#modFit <- train(hit~.,data=tr2,method="rf",prox=TRUE)

d <- split_train_test(d = prep,
                      outcome = hit,
                      percent_train = .7)


modFit <- machine_learn(d$train,outcome=hit)




