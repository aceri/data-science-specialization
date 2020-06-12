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

library(textclean)
library(textshape)
library(fastDummies)

library(udpipe)
dl <- udpipe_download_model(language = "english")
udmodel_english <- udpipe_load_model(dl)


pos_line <- function(line,ret_words = F)
{
    ret = as.data.frame(udpipe_annotate(udmodel_english,x=line,doc_id=seq_along(line)))$upos
    if (ret_words == F ) ret <- gsub(",","",toString(ret))
    return(ret)
}

pos_corpus <- function(text_tibble,verbose=0)
{
    # Needs a tibble of the text with each line being in a column txt

    txt <- as.data.frame(text_tibble)
    num_rows=nrow(txt)
    start <- Sys.time()    
    for (k in 1:num_rows)
    {
        txt[k,2] <- pos_line(line=as.character(txt[k,2]) ,ret_words = F)
        if(verbose >0 & k %% 100 == 0) 
            {
                chck <- Sys.time()
                diff <- chck-start
                print(paste("pos_corpus > Processed ",round(k/num_rows*100,2)," % in ",round(diff,0)," s for ",k," lines of ",num_rows," - Time Remaining : ",round((num_rows-k)*diff/k,0)," m = ",round((num_rows-k)*diff/k/3600,2)," h",sep=""))
            }
    }
    return(txt)
}


clean_line <- function(line,ret_words=F,min_chars=1,use_dictionary=T,dictionary=GradyAugmented,correct=T,verbose=0)
{
    ret <- line
    if (verbose>2) print(paste("clean_line > received line : ",ret,sep=""))
    if (correct==T) { ret <- unlist(strsplit(as.character(ret),split=" ")) ; 
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
    start <- Sys.time()    
    for (k in 1:num_rows)
    {
        txt[k,1] <- clean_line(txt[k,1],ret_words = F,min_chars = min_chars, use_dictionary = use_dictionary,dictionary=dictionary,correct=correct)
        if(verbose >0 & k %% 100 == 0) 
        {
            chck <- Sys.time()
            diff <- chck-start
            print(paste("clean_text > Processed ",round(k/num_rows*100,2)," % in ",round(diff,0)," s for ",k," lines of ",num_rows," - Time Remaining : ",round((num_rows-k)*diff/k,0)," s = ",round((num_rows-k)*diff/k/3600,2)," h",sep=""))
        }
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

blogs_dat   <- tibble(scan(blogs_str,"character",sep="\n",skipNul = T,encoding="UTF-8",quiet=T))
news_dat    <- tibble(scan(news_str,"character",sep="\n",skipNul = T,encoding="UTF-8",quiet=T))
twitter_dat <- tibble(scan(twitter_str,"character",sep="\n",skipNul = T,encoding="UTF-8",quiet=T))

####################################################################

# THIS IS WITH LIBRARY CORPUS

full <- rbind(blogs_dat,news_dat,twitter_dat)
rm(blogs_dat,news_dat,twitter_dat,folder,blogs_str,news_str,twitter_str)
names(full) <- c("text")
full[sample(1:nrow(full),round(nrow(full),0)*0.25),]  -> full

full1b <- full1 %>% clean_corpus(1,F,F,verbose = 1,Fry_1000)
full1b <- full1b[full1b$text!="",]
saveRDS(full1b,"./data/full1b.rds")

full1b <- corpus_frame("full1b",full1)

text_filter(full1b)$map_case <- TRUE
text_filter(full1b)$remove_ignorable <- TRUE
text_filter(full1b)$drop_punct <- TRUE
text_filter(full1b)$drop_symbol <- TRUE



####################################################################

ng1 <- term_stats(full1b,types=TRUE)
ng1<-ng1[ng1$support > 1 & ng1$count > 1,]
saveRDS(ng1,"./data/ng1.rds")
#-------------------------------------------------------------------
#-------------------------------------------------------------------
ng2a <- term_stats(full1b,types=TRUE,ngrams=2)
ng2a <- ng2a[ng2a$support>1 & ng2a$count>1,]
saveRDS(ng2a,"./data/ng2a.rds")

ng3a <- term_stats(full1b,types=TRUE,ngrams=3)
ng3a <- ng3a[ng3a$support>1 & ng3a$count>1,]
saveRDS(ng3a,"./data/ng3a.rds")

ng4a <- term_stats(full1b,types=TRUE,ngrams=4)
ng4a <- ng4a[ng4a$support>1 & ng4a$count>1,]
saveRDS(ng4a,"./data/ng4a.rds")

ng5a <- term_stats(full1b,types=TRUE,ngrams=5)
ng5a <- ng5a[ng5a$support>1 & ng5a$count>1,]
saveRDS(ng5a,"./data/ng5a.rds")

ng6a <- term_stats(full1b,types=TRUE,ngrams=6)
ng6a <- ng6a[ng6a$support>1 & ng6a$count>1,]
saveRDS(ng6a,"./data/ng6a.rds")
#-------------------------------------------------------------------
#-------------------------------------------------------------------
ng2b <- term_stats(full1b,types=TRUE,ngrams=2, subset = (!type1 %in% stopwords_en) & 
                                                        (!type2 %in% stopwords_en))
ng2b <- ng2b[ng2b$support>1 & ng2b$count>1,]
saveRDS(ng2b,"./data/ng2b.rds")

ng3b <- term_stats(full1b,types=TRUE,ngrams=3, subset = (!type1 %in% stopwords_en) & 
                                                        (!type2 %in% stopwords_en) & 
                                                        (!type3 %in% stopwords_en))
ng3b <- ng3b[ng3b$support>1 & ng3b$count>1,]
saveRDS(ng3b,"./data/ng3b.rds")

ng4b <- term_stats(full1b,types=TRUE,ngrams=4, subset = (!type1 %in% stopwords_en) & 
                                                        (!type2 %in% stopwords_en) & 
                                                        (!type3 %in% stopwords_en) & 
                                                        (!type4 %in% stopwords_en))
ng4b <- ng4b[ng4b$support>1 & ng4b$count>1,]
saveRDS(ng4b,"./data/ng4b.rds")

ng5b <- term_stats(full1b,types=TRUE,ngrams=5, subset = (!type1 %in% stopwords_en) & 
                                                        (!type2 %in% stopwords_en) & 
                                                        (!type3 %in% stopwords_en) & 
                                                        (!type4 %in% stopwords_en) & 
                                                        (!type5 %in% stopwords_en))
ng5b <- ng5b[ng5b$support>1 & ng5b$count>1,]
saveRDS(ng5b,"./data/ng5b.rds")

ng6b <- term_stats(full1b,types=TRUE,ngrams=6, subset = (!type1 %in% stopwords_en) & 
                                                        (!type2 %in% stopwords_en) & 
                                                        (!type3 %in% stopwords_en) & 
                                                        (!type4 %in% stopwords_en) & 
                                                        (!type5 %in% stopwords_en) & 
                                                        (!type6 %in% stopwords_en))
ng6b <- ng6b[ng6b$support>1 & ng6b$count>1,]
saveRDS(ng6b,"./data/ng6b.rds")

####################################################################
# IF DICTIONARY IS PREPARED START HERE
####################################################################

    full1b <- readRDS("./data/full1b.rds")

    full1b <- corpus_frame("full1b",full1b)    
    
    ng1  <- readRDS("./data/ng1.rds")
    ng2a <- readRDS("./data/ng2a.rds")
    ng3a <- readRDS("./data/ng3a.rds")
    ng4a <- readRDS("./data/ng4a.rds")
    ng5a <- readRDS("./data/ng5a.rds")
    ng6a <- readRDS("./data/ng6a.rds")
    ng2b <- readRDS("./data/ng2b.rds")
    ng3b <- readRDS("./data/ng3b.rds")
    ng4b <- readRDS("./data/ng4b.rds")
    ng5b <- readRDS("./data/ng5b.rds")
    ng6b <- readRDS("./data/ng6b.rds")

####################################################################

predict_word <- function(line, test=F,num_replies=1,verbose=0)
{
    fill_df <- function(line,ng,num_ng,stem=F,min_char=1,verbose=0,test=F)
    {
        l1="";l2="";l3="";l4="";l5=""
        p1="";p2="";p3="";p4="";p5=""

        words <- unlist(text_tokens(line,filter=text_filter(drop_punct=T,map_case=T,drop_symbol=T,drop_number = T,stemmer=stem_snowball)))
        num_words <- length(words)

        solution <- words[num_words]
        
        if(num_words>1) l1 <- words[num_words-1]
        if(num_words>2) l2 <- words[num_words-2]
        if(num_words>3) l3 <- words[num_words-3]
        if(num_words>4) l4 <- words[num_words-4]
        if(num_words>5) l5 <- words[num_words-5]

        pos_words     <- pos_line(line,T)
        pos_num_words <- length(pos_words)
        if(pos_num_words>1) p1 <- pos_words[pos_num_words-1]
        if(pos_num_words>2) p2 <- pos_words[pos_num_words-2]
        if(pos_num_words>3) p3 <- pos_words[pos_num_words-3]
        if(pos_num_words>4) p4 <- pos_words[pos_num_words-4]
        if(pos_num_words>5) p5 <- pos_words[pos_num_words-5]


        if (nrow(ng)>0)
        {
            df <- data.frame(ng=integer(), count=numeric(), support=numeric(),stem=integer(),min_char=integer(),
                             p5=character(),p4=character(),p3=character(),p2=character(),p1=character(),p0=character(),
                             l5=character(),l4=character(),l3=character(),l2=character(),l1=character(), 
                             word=character(),solution=character(),stringsAsFactors = F)
            df[1:nrow(ng),1]  <- num_ng
            df[1:nrow(ng),2]  <- round(ng$count/sum(ng$count)*100,2)
            df[1:nrow(ng),3]  <- round(ng$support/sum(ng$support)*100,2)
            df[1:nrow(ng),4]  <- ifelse(stem==F,0,1)
            df[1:nrow(ng),5]  <- min_char
            df[1:nrow(ng),6]  <- p5
            df[1:nrow(ng),7]  <- p4
            df[1:nrow(ng),8]  <- p3  
            df[1:nrow(ng),9]  <- p2
            df[1:nrow(ng),10] <- p1
            df[1:nrow(ng),11] <- ""
            df[1:nrow(ng),12] <- l5
            df[1:nrow(ng),13] <- l4
            df[1:nrow(ng),14] <- l3
            df[1:nrow(ng),15] <- l2
            df[1:nrow(ng),16] <- l1
            
            df[1:nrow(ng),17] <- ng[,(1+num_ng)]            
            df[1:nrow(ng),18] <- solution    
        } else {df <- ng}
        if (verbose>2)
        {
            print(paste("fill_df > ng: ",num_ng,", sw: ",ifelse(stem==F,"F","T"),", min_char: ",min_char," >> ",line,sep=""))
            head(df,5)
        }
        return(df)
    }
    #-----------------------------------------------------------------------------------------------------        
    if(test==T) line=paste(line,"UNKNOWN",sep=" ") 
    words <- unlist(stri_split_regex(line," "))
    solution <- words[length(words)]
    if (verbose>1) print(paste("predict_word       > ", gsub(",","",toString(words[1:(length(words)-1)]))," [ ",solution," ]",sep=""))
    #-----------------------------------------------------------------------------------------------------        
    words <- unlist(text_tokens(line,filter=text_filter(drop_punct=T,map_case=T,drop_symbol=T,drop_number = T,stemmer=stem_snowball)))
    num_words <- length(words)
    if(num_words>1) l1 <- words[num_words-1]
    if(num_words>2) l2 <- words[num_words-2]
    if(num_words>3) l3 <- words[num_words-3]
    if(num_words>4) l4 <- words[num_words-4]
    if(num_words>5) l5 <- words[num_words-5]
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
    #-----------------------------------------------------------------------------------------------------        
    pre_final_a <- rbind(fill_df(line,r_ng2a,2,F,1,verbose,test=test),
                         fill_df(line,r_ng3a,3,F,1,verbose,test=test),
                         fill_df(line,r_ng4a,4,F,1,verbose,test=test),
                         fill_df(line,r_ng5a,5,F,1,verbose,test=test),
                         fill_df(line,r_ng6a,6,F,1,verbose,test=test))
    #-----------------------------------------------------------------------------------------------------    
    pre_final_b <- rbind(fill_df(line,r_ng2b,2,T,1,verbose,test=test),
                         fill_df(line,r_ng3b,3,T,1,verbose,test=test),
                         fill_df(line,r_ng4b,4,T,1,verbose,test=test),
                         fill_df(line,r_ng5b,5,T,1,verbose,test=test),
                         fill_df(line,r_ng6b,6,T,1,verbose,test=test))
    #-----------------------------------------------------------------------------------------------------    
    #-----------------------------------------------------------------------------------------------------    
    pre_final <- rbind(pre_final_a,pre_final_b)
    row.names(pre_final) <- NULL
    pre_final <- pre_final[!duplicated(pre_final),]
    pre_final <- head(pre_final[order(-pre_final$count),],100)
    #-----------------------------------------------------------------------------------------------------    
    return(head(pre_final,num_replies))
}


get_random_str <- function(text_tibble,min_words=7,verbose=0,dictionary="")
{
    query=""
    while(query=="") 
    {
        dice <- sample(1:max(nrow(text_tibble)),1,replace=T)
        query_str <- text_tibble$text[dice] %>% clean_line(ret_words=T,verbose=verbose)
        num_words <- length(query_str)
        if (num_words >= min_words+1)
        {
            minimum_pos <- sample(1:(num_words-min_words),1)
            minimum_guarantee <- (minimum_pos+min_words-1)
            if (verbose>2) print(paste("get_random_str > Selecting substring from ",minimum_pos," to ",minimum_guarantee-1,sep=""))
            query_words <- query_str[minimum_pos:minimum_guarantee-1]
            query <- gsub(",","",toString(query_words))
            if (nchar(query_words[length(query_words)])<3) {if (verbose>2) print("get_random_str > rejected because solution word has less than 3 chars"); query=""}
            if (length(query_words) < min_words) {if (verbose>2) print("get_random_str > rejected because query has less than the minimum required number of words"); query=""}
            if (sum(query_words %in% dictionary) != min_words) 
                {
                    if (verbose >2) print(paste("get_random_str > rejected because the words do not conform to dictionary : ",toString(query_words[!query_words %in% dictionary]),sep=""))
                    query=""
                }
        } else {
            if (verbose >2) print(paste("get_random_str > Sentence rejectedas it does not have the minimum number of words",sep=""))
            query =""
        }
    }
    return(query)
}


build_training_set <- function(tr2,iter=500,verbose=0,test=F,save_every=100,dictionary="")
{
    tic(paste("build_training_set of ",iter," iterations",sep=""))
    tmp <- data.frame(ng=integer(), count=numeric(), support=numeric(),stem=integer(),min_char=integer(),
                     p5=character(),p4=character(),p3=character(),p2=character(),p1=character(),p0=character(),
                     l5=character(),l4=character(),l3=character(),l2=character(),l1=character(), 
                     word=character(),solution=character(),stringsAsFactors = F)
    
    tmp <- bind_rows(lapply(1:iter, function(x) predict_word(line = get_random_str(full1b,7,verbose=verbose,dictionary=dictionary), test = F, num_replies = 100,verbose=verbose) ))
    tmp[,c("term","type1","type2")]<-NULL
    tmp <- as.data.frame(tmp)
    tr2 <- rbind(tr2,tmp)
    tr2 <- tr2[!duplicated(tr2),]
    tr2 <- tr2[tr2$word %in% Fry_1000 & tr2$solution %in% Fry_1000 & tr2$l1 %in% Fry_1000 & tr2$l2 %in% Fry_1000 & tr2$l3 %in% Fry_1000 & tr2$l4 %in% Fry_1000 & tr2$l5 %in% Fry_1000,]
    tr2<-tr2[!tr2$l1==tr2$solution,]
    row.names(tr2) <- NULL
    if (verbose>0) print(paste("build_training_set > Saving ...",sep=""));saveRDS(tr2,"./data/tr2.rds")
    toc()
    return(tr2)
}

####################################################################
# tr2 <- data.frame(ng=integer(), count=numeric(), support=numeric(),stem=integer(),min_char=integer(),
#                  p5=character(),p4=character(),p3=character(),p2=character(),p1=character(),p0=character(),
#                  l5=character(),l4=character(),l3=character(),l2=character(),l1=character(), 
#                  word=character(),solution=character(),stringsAsFactors = F)
####################################################################

tr2 <- build_training_set(tr2 , iter = 30000 , verbose = 2 , test=F,save_every = 100, dictionary=GradyAugmented)
#tr2 <- readRDS("./data/tr2.rds")

# WE MIGHT HAVE A SERIOUS MEMORY PROBLEM WHEN CREATING DUMMY TABLES OF SUCH A HIGH NUMBER OF CATEGORIES LIKE THE FULL DICTIONARY.

# Let's Aggregate and count their repetitions and consider discarding those that only appear N times.


#tr2_grouped_by_word <- sqldf("select solution,count(*) as total from tr2 group by solution order by total desc")

# There are only 653 different words and they do not reduce their frquency exponentially so that is a problem

#count and support are very correlated 0.99980

tr3 <- tr2[,-3] 

# I am not using min_chart. All are 1

tr3 <- tr3[,-4]

# This should reduce the cardinality.
# uh oh. I still need to aggregate the ng1$count percentually for each "word"

require(sqldf)
ng1  <- readRDS("./data/ng1.rds")
tr3 <- sqldf("select a.ng,a.count,b.count as ng1,a.stem,a.p5,a.p4,a.p3,a.p2,a.p1,a.p0,a.l5,a.l4,a.l3,a.l2,a.l1,a.word,a.solution from tr3 a, ng1 b where a.word=b.type1")
tr3$ng1 <- round(tr3$ng1/sum(ng1$count)*100,2)


# EL TAMAÑO DE LA TABLA ES DE  77.5 MB pero aun no he creado las dummies.


# tr4 <- fastDummies::dummy_cols(tr3)

# EL TAMAÑO DE LA TABLA ES DE 10.7 GB.

# Deberia estudiar la eficiencia de los datos por rangos de COUNT. Parece que aquí está el QUID de la cuestión.


# sum(tr3$word==tr3$solution & tr3$count >90)/sum(tr3$count>90)                                         #-> 0.53         2105
# sum(tr3$word==tr3$solution & tr3$count >80 & tr3$count <90)/sum(tr3$count >80 & tr3$count <90)        #-> 0.50          142
# sum(tr3$word==tr3$solution & tr3$count >70 & tr3$count <80)/sum(tr3$count >70 & tr3$count <80)        #-> 0.46          236
# sum(tr3$word==tr3$solution & tr3$count >60 & tr3$count <70)/sum(tr3$count >60 & tr3$count <70)        #-> 0.42          426
# sum(tr3$word==tr3$solution & tr3$count >50 & tr3$count <60)/sum(tr3$count >50 & tr3$count <60)        #-> 0.35          457
# sum(tr3$word==tr3$solution & tr3$count >40 & tr3$count <50)/sum(tr3$count >40 & tr3$count <50)        #-> 0.30          858
# sum(tr3$word==tr3$solution & tr3$count >30 & tr3$count <40)/sum(tr3$count >30 & tr3$count <40)        #-> 0.21         2048    
# sum(tr3$word==tr3$solution & tr3$count >20 & tr3$count <30)/sum(tr3$count >20 & tr3$count <30)        #-> 0.17         4914
# sum(tr3$word==tr3$solution & tr3$count >10 & tr3$count <20)/sum(tr3$count >10 & tr3$count <20)        #-> 0.10        14447
# sum(tr3$word==tr3$solution & tr3$count > 0 & tr3$count <10)/sum(tr3$count > 0 & tr3$count <10)        #-> 0.014      567392    EL 95 % de las filas solo dan un 0.014% de los aciertos

# VAMOS A ELIMINAR AQUELLAS FILAS CUYO COUNT SEA MENOR A 10

tr3 <- tr3[tr3$count>10,]


# LO HEMOS DEJADO DE 77.5 MB A 3.9 MB. PREBEMOS AHORA A CREAR LA DUMMY

# tr4 <- fastDummies::dummy_cols(tr3)

# ANTES ERA DE 10.7 GB y AHORA ES 0.5 GB. La diferencia en tiempo y memoria debería ser abismal


# NECESITO UN DICCIONARIO MUCHO MAS REDUCIDO DE XNG5, XNG4, XNG3, XNG2, XNG1

# xfull1b <- full1b[sample(1:nrow(full1b),round(nrow(full1b)/20,0)),]
# xfull1b <- xfull1b[,-2]
# xfull1b <- pos_corpus(xfull1b,verbose=1)
# saveRDS(xfull1b,"./data/xfull1b.rds")

# xng1 <- term_stats(xfull1b,types=TRUE)
# xng1<-xng1[xng1$support > 1 & xng1$count > 1,]
# saveRDS(xng1,"./data/xng1.rds")
# #-------------------------------------------------------------------
# #-------------------------------------------------------------------
# xng2a <- term_stats(xfull1b,types=TRUE,ngrams=2)
# xng2a <- xng2a[xng2a$support>1 & xng2a$count>1,]
# saveRDS(xng2a,"./data/xng2a.rds")
# 
# xng3a <- term_stats(xfull1b,types=TRUE,ngrams=3)
# xng3a <- xng3a[xng3a$support>1 & xng3a$count>1,]
# saveRDS(xng3a,"./data/xng3a.rds")
# 
# xng4a <- term_stats(xfull1b,types=TRUE,ngrams=4)
# xng4a <- xng4a[xng4a$support>1 & xng4a$count>1,]
# saveRDS(xng4a,"./data/xng4a.rds")
# 
# xng5a <- term_stats(xfull1b,types=TRUE,ngrams=5)
# xng5a <- xng5a[xng5a$support>1 & xng5a$count>1,]
# saveRDS(xng5a,"./data/xng5a.rds")
# 
# xng6a <- term_stats(xfull1b,types=TRUE,ngrams=6)
# xng6a <- xng6a[xng6a$support>1 & xng6a$count>1,]
# saveRDS(xng6a,"./data/xng6a.rds")

xng1  <- readRDS("./data/xng1.rds")
xng2a <- readRDS("./data/xng2a.rds")
xng3a <- readRDS("./data/xng3a.rds")
xng4a <- readRDS("./data/xng4a.rds")
xng5a <- readRDS("./data/xng5a.rds")
xng6a <- readRDS("./data/xng6a.rds")



#sqldf("select a.ng,a.count,a.ng1,a.stem,a.p5,a.p4,a.p3,a.p2,a.p1,a.p0,a.l5,a.l4,a.l3,a.l2,a.l1,a.word,a.solution from tr3 a, xng6a b where a.ng=6 and a.stem=0 and a.p5=b.type1 and a.p4=b.type2 and a.p3=b.type3 and a.p2=b.type4 and a.p1=b.type5 and a.p0=b.type6")


# tengo que corregir p0 para que sea la categoría sintáctica de word, no de solution
for (k in 1:nrow(tr3))
{
    tr3$p0[k] <- pos_line(tr3$word[k])
    if (k %% 1000==0) print(k)
}
# Igual que tenemos una columna ng1 que nos dato el peso percentual del ngram 1 por palabra sugerida
# añadimos un xng1 que nos dá el peso percentural del ngram 1 de agrupaciones sintácticas de nivel 1 por palabra sugerida

for (k in 1:nrow(tr3))
{
    tr3$xng1[k] <- round(xng1$count[xng1$type1==tolower(tr3$p0[k])]/sum(xng1$count)*100,2)
    if (k %% 1000==0) print(k)
}

# Ahora necesito saber el valor de count() para un conjunto de pX... p0  dependiente del ng


# xng <- xng3a
# ng <- xng3a
# ngram <- 3
# verbose=1

syntax_group_count <- function(ng,tibble_num_line,verbose=0)
{
    ngram <- length(names(ng))-3
    busq <- gsub(",","",tolower(toString(tr3[tibble_num_line,(10-ngram+1):10])))
    ret <- round((ng$count[ng$term==busq]/sum(ng$count))*100,2)
    #if (verbose > 0) print(paste("syntax_group_count > ng:",ngram," ; tibble_num_line:",tibble_num_line," > ",toString(ng[ng$term == busq,c("term","count")])," = ",ret," %",sep="")) 
    if(length(ret)==0) ret=0
    return(ret)
}

# ng <- xng1
# ngram <- 1
# tibble_num_line <- 1
# 
# # parece funcionar correctamente
# # para una fila k de tr3 diremos
# k=5000
# syntax_group_count(xng1 ,k,1)
# syntax_group_count(xng2a,k,1)
# syntax_group_count(xng3a,k,1)
# syntax_group_count(xng4a,k,1)
# syntax_group_count(xng5a,k,1)
# syntax_group_count(xng6a,k,1)

# hacemos una copia por si acaso
saveRDS(tr3,"./data/tr3.rds")

tr3copy <- tr3

# ahora debería recorrer todas las filas de tr3 y asignar a una nueva columna stx_grp_count el resultado

for (k in 1: nrow(tr3))
{
    if (tr3$ng[k]==1) tr3$stx_grp_count[k] <- syntax_group_count(xng1 ,k,1)
    if (tr3$ng[k]==2) tr3$stx_grp_count[k] <- syntax_group_count(xng2a,k,1)
    if (tr3$ng[k]==3) tr3$stx_grp_count[k] <- syntax_group_count(xng3a,k,1)
    if (tr3$ng[k]==4) tr3$stx_grp_count[k] <- syntax_group_count(xng4a,k,1)
    if (tr3$ng[k]==5) tr3$stx_grp_count[k] <- syntax_group_count(xng5a,k,1)
    if (tr3$ng[k]==6) tr3$stx_grp_count[k] <- syntax_group_count(xng6a,k,1)
    if (k %% 1000==0) print(k)
}



saveRDS(tr3,"./data/tr3.rds")
#tr3 <- readRDS("./data/tr3.rds")


########################################################################################

# Ahora si, creamos las columnas dummies

#tr4 <- fastDummies::dummy_cols(tr3,remove_first_dummy=T)
tr4 <-tr3

print(paste("tr4 is ",toString(round(object.size(tr4)/1024/1024,2))," in Mb.",sep=""))

########################################################################################

# Creamos training y testing

require(caret)

inTrain <- createDataPartition(y=tr4$solution,p=0.9,list=FALSE)
training <- tr4[ inTrain,]
testing  <- tr4[-inTrain,]
dim(training);dim(testing)

# VAMOS A USAR PARALELISMO CON LA MITAD DE CORES

require(parallel)
detectCores()
cl <- makeCluster(4)
require(healthcareai)
modFit <- machine_learn(training,outcome=solution,allow_parallel = TRUE,models="rf",tune=T)

saveRDS(modFit,"./data/modFit.rds")   # This is based on hit with weight based on the weight of how 
#modFit.rds <- readRDS("./data/modFit.rds")


predictions <- predict(modFit,testing)
predictions
plot(predictions)


get_variable_importance(modFit)





#################################################################################
# PREPARING A "PRODUCTION" ENVIRONMENT FOR SINGLE SENTENCES BASED ON THE modFit #
#################################################################################

xng1  <- readRDS("./data/xng1.rds")
xng2a <- readRDS("./data/xng2a.rds")
xng3a <- readRDS("./data/xng3a.rds")
xng4a <- readRDS("./data/xng4a.rds")
xng5a <- readRDS("./data/xng5a.rds")
xng6a <- readRDS("./data/xng6a.rds")
ng1  <- readRDS("./data/ng1.rds")


pred <- function(line,num_replies=500,return=5,verbose=0)
{
    pr <- predict_word(line, test=T,num_replies=num_replies,verbose=verbose)
    pr <- pr[pr$stem==0,]
    pr$stem <- NULL
    pr3 <- pr[,c(-3,-4)] 

    require(sqldf)
    
    pr3 <- sqldf("select a.ng,a.count,b.count as ng1,a.p5,a.p4,a.p3,a.p2,a.p1,a.p0,a.l5,a.l4,a.l3,a.l2,a.l1,a.word,a.solution from pr3 a, ng1 b where a.word=b.type1")
    pr3$ng1 <- round(pr3$ng1/sum(ng1$count)*100,2)
    #pr3 <- pr3[pr3$count>10,]
    for (k in 1:nrow(pr3))
    {
        pr3$p0[k] <- pos_line(pr3$word[k])
        pr3$xng1[k] <- round(xng1$count[xng1$type1==tolower(pr3$p0[k])]/sum(xng1$count)*100,2)
        if (pr3$ng[k]==1) pr3$stx_grp_count[k] <- syntax_group_count_without_stem(xng1 ,k,1)
        if (pr3$ng[k]==2) pr3$stx_grp_count[k] <- syntax_group_count_without_stem(xng2a,k,1)
        if (pr3$ng[k]==3) pr3$stx_grp_count[k] <- syntax_group_count_without_stem(xng3a,k,1)
        if (pr3$ng[k]==4) pr3$stx_grp_count[k] <- syntax_group_count_without_stem(xng4a,k,1)
        if (pr3$ng[k]==5) pr3$stx_grp_count[k] <- syntax_group_count_without_stem(xng5a,k,1)
        if (pr3$ng[k]==6) pr3$stx_grp_count[k] <- syntax_group_count_without_stem(xng6a,k,1)
         if (k %% 1000==0) print(k)
    }
    
    pr4 <- predict(modFit,pr3)
    pr4 <- pr4$predicted_solution[pr4$l1!=pr4$predicted_solution & pr4$predicted_solution %in% Fry_1000]
    pr4 <- unlist(strsplit(toString(unlist(pr4[!duplicated(pr4)])),split=", "))
    return(pr4[1:(min(return,length(pr4)))])
}

# Con training and tune = T sin dummy obtengo un 59 % de aciertos que no está mal.

# que pasa si quito los ng=1 que que me quitan las dummy word

tr2 <- readRDS("./data/tr2.rds")

post_processing <- function(tr2)
{
    tr2 <- tr2[tr2$stem==0,]
    tr2$stem <- NULL
    tr3 <- tr2[,-3] 
    tr3 <- tr3[,-3]
    require(sqldf)
    tr3 <- sqldf("select a.ng,a.count,b.count as ng1,a.p5,a.p4,a.p3,a.p2,a.p1,a.p0,a.l5,a.l4,a.l3,a.l2,a.l1,a.word,a.solution from tr3 a, ng1 b where a.word=b.type1")
    tr3$ng1 <- round(tr3$ng1/sum(ng1$count)*100,2)
    tr3 <- tr3[tr3$count>10,]
    
    syntax_group_count_without_stem <- function(ng,tibble_num_line,verbose=0)
    {
        ngram <- length(names(ng))-3
        busq <- gsub(",","",tolower(toString(tr3[tibble_num_line,(9-ngram+1):9])))
        ret <- round((ng$count[ng$term==busq]/sum(ng$count))*100,2)
        #if (verbose > 0) print(paste("syntax_group_count > ng:",ngram," ; tibble_num_line:",tibble_num_line," > ",toString(ng[ng$term == busq,c("term","count")])," = ",ret," %",sep="")) 
        if(length(ret)==0) ret=0
        return(ret)
    }
    
    
    for (k in 1:nrow(tr3))
    {
        tr3$p0[k] <- pos_line(tr3$word[k])
        tr3$xng1[k] <- round(xng1$count[xng1$type1==tolower(tr3$p0[k])]/sum(xng1$count)*100,2)
        if (tr3$ng[k]==1) tr3$stx_grp_count[k] <- syntax_group_count_without_stem(xng1 ,k,1)
        if (tr3$ng[k]==2) tr3$stx_grp_count[k] <- syntax_group_count_without_stem(xng2a,k,1)
        if (tr3$ng[k]==3) tr3$stx_grp_count[k] <- syntax_group_count_without_stem(xng3a,k,1)
        if (tr3$ng[k]==4) tr3$stx_grp_count[k] <- syntax_group_count_without_stem(xng4a,k,1)
        if (tr3$ng[k]==5) tr3$stx_grp_count[k] <- syntax_group_count_without_stem(xng5a,k,1)
        if (tr3$ng[k]==6) tr3$stx_grp_count[k] <- syntax_group_count_without_stem(xng6a,k,1)
        
        if (k %% 1000==0) print(k)
    }
    return(tr3)
}

tr4 <- post_processing(tr3)

t1 <- data.frame(rate_sols=numeric(),rate_rows=numeric(),stringsAsFactors = F)
t1[1,1:2] <- data.frame(sum(tr4$word==tr4$solution & tr4$count >90)/sum(tr4$word==tr4$solution)  ,sum(tr4$count >90)/nrow(tr4) )
t1[2,1:2] <- data.frame(sum(tr4$word==tr4$solution & tr4$count >80)/sum(tr4$word==tr4$solution)  ,sum(tr4$count >80)/nrow(tr4) )
t1[3,1:2] <- data.frame(sum(tr4$word==tr4$solution & tr4$count >70)/sum(tr4$word==tr4$solution)  ,sum(tr4$count >70)/nrow(tr4) )
t1[4,1:2] <- data.frame(sum(tr4$word==tr4$solution & tr4$count >60)/sum(tr4$word==tr4$solution)  ,sum(tr4$count >60)/nrow(tr4) )
t1[5,1:2] <- data.frame(sum(tr4$word==tr4$solution & tr4$count >50)/sum(tr4$word==tr4$solution)  ,sum(tr4$count >50)/nrow(tr4) )
t1[6,1:2] <- data.frame(sum(tr4$word==tr4$solution & tr4$count >40)/sum(tr4$word==tr4$solution)  ,sum(tr4$count >40)/nrow(tr4) )
t1[7,1:2] <- data.frame(sum(tr4$word==tr4$solution & tr4$count >30)/sum(tr4$word==tr4$solution)  ,sum(tr4$count >30)/nrow(tr4) )
t1[8,1:2] <- data.frame(sum(tr4$word==tr4$solution & tr4$count >20)/sum(tr4$word==tr4$solution)  ,sum(tr4$count >20)/nrow(tr4) )
t1[9,1:2] <- data.frame(sum(tr4$word==tr4$solution & tr4$count >10)/sum(tr4$word==tr4$solution)  ,sum(tr4$count >10)/nrow(tr4) )
plot(t1$rate_sols,t1$rate_rows,type="l",main="relative importance of rate_solutions vs rate_rows")

inTrain <- createDataPartition(y=tr4$solution,p=0.8,list=FALSE)
training <- tr4[ inTrain,]
testing  <- tr4[-inTrain,]
dim(training);dim(testing)
# VAMOS A USAR PARALELISMO CON LA MITAD DE CORES

require(parallel)
detectCores()
cl <- makeCluster(4)
require(healthcareai)
modFit <- machine_learn(training,outcome=solution,allow_parallel = TRUE,models="rf",tune=F)

saveRDS(modFit,"./data/modFit.rds")   # This is based on hit with weight based on the weight of how 

pred(q1,return=5)
