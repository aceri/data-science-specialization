library(stopwords)
library(tidytext)
library(tidyr)
library(dplyr)
library(sqldf)
library(caret)
library(tidyverse)
library(tm)
# library(cld2)
#library(quanteda)
# library(stringi)
# library(stringr)
# library(corpus)
# library(parallel)


use_pct=0.25

######################################################################
#
#
#       F U N C T I O N S
#
#
######################################################################


sample_Corpus <- function(file_str,pct) { 
    nam <- names(file_str)
    k <- file_str[sample(1:length(file_str),round(length(file_str)*pct,0))] 
    names(k) <- nam
    return(k)
}

clean_line <- function(line,min_chars=1,filter_stop_words=F,return_words=F)
{
    require(stopwords)
    line %>% tibble() %>% tolower() %>% gsub("[[:digit:]]","",.) %>% gsub("(.)\\1{2,}","",.) %>% gsub("[^abcdefghijklmnopqrstuvwxyz' ]+","",.) %>% gsub("^[ ']+|[ ']$+","",.) %>% gsub(" +"," ",.)  -> line
    words <- unlist(stri_split_regex(line," "))
    if (min_chars > 1) words <- words[nchar(words) >= min_chars]
    if (filter_stop_words==T) words <- words[!(words %in% stopwords::stopwords(language="en",source="stopwords-iso"))]
    if (return_words==T) {ret <- words } else { ret <- gsub(", "," ",toString(words))}
    return(ret)
}

clean_text <- function(text_tibble, min_chars=1, filter_stop_words=F,verbose=0)
{
    # Needs a tibble of the text with each line being in a column txt
    text <- text_tibble
    num_rows=nrow(text)
    for (k in 1:nrow(text))
    {
        if(verbose >0 & k %% round(num_rows/20,0) == 0) print(paste("clean_text > min_chars ",min_chars," ; filter_stop_words ",filter_stop_words," > ",round(k/num_rows*100,2)," %", sep=""))
        text[k,1] <- clean_line(text[k,1],min_chars = min_chars,filter_stop_words = filter_stop_words , return_words = F)
    }
    return(text)
}



Top_N_words <- function(file_tibble,top=10,min_chars=1,filter_stop_words=F)
{
    file_tibble %>% clean_text(min_chars=min_chars,filter_stop_words=filter_stop_words) %>% unnest_tokens(word,txt) %>%
    count(word,sort=TRUE) %>% arrange(desc(n)) %>%
    mutate(word=reorder(word,n)) -> ret
    if (top>0) ret <- head(ret,top)
    return(ret)
}



save_ngram_dictionaries<-function()
{
    folder="./data/"
    saveRDS(pred_df,paste(folder,"pred_df.rds",sep=""))
    saveRDS(pred_df,paste(folder,"pred2_df.rds",sep=""))        
    saveRDS(ng1_fast,paste(folder,"ng1_fast.rds",sep=""))
}

load_ngram_dictionaries <-function()
{


    
    folder <- "./data/"

    blogs_str   <- paste(folder,"en_US.blogs.txt"   ,sep="")
    news_str    <- paste(folder,"en_US.news.txt"    ,sep="")
    twitter_str <- paste(folder,"en_US.twitter.txt" ,sep="")
    
    blogs_dat <- scan(blogs_str,"character",sep="\n",skipNul = T)
    news_dat <- scan(news_str,"character",sep="\n",skipNul = T)
    twitter_dat <- scan(twitter_str,"character",sep="\n",skipNul = T)

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
    

    ng1_fast   <- readRDS(paste(folder,"ng1_fast.rds",sep=""))   # WE NEED THIS TO GET INTERESTING WORDS FOR TRAINING/TEST SETS
    pred_df <- readRDS(paste(folder,"pred_df.rds",sep=""))
    pred2_df <- readRDS(paste(folder,"pred2_df.rds",sep=""))

    ng1_ch1_ff <- readRDS(paste(folder,"ng1_ch1_ff.rds",sep=""))
    ng1_ch1_ft <- readRDS(paste(folder,"ng1_ch1_ft.rds",sep=""))
    ng1_ch2_ff <- readRDS(paste(folder,"ng1_ch2_ff.rds",sep=""))
    ng1_ch2_ft <- readRDS(paste(folder,"ng1_ch2_ft.rds",sep=""))
    ng1_ch3_ff <- readRDS(paste(folder,"ng1_ch3_ff.rds",sep=""))
    ng1_ch3_ft <- readRDS(paste(folder,"ng1_ch3_ft.rds",sep=""))
    ng1_ch4_ff <- readRDS(paste(folder,"ng1_ch4_ff.rds",sep=""))
    ng1_ch4_ft <- readRDS(paste(folder,"ng1_ch4_ft.rds",sep=""))

    ng2_ch1_ff <- readRDS(paste(folder,"ng2_ch1_ff.rds",sep=""))
    ng2_ch1_ft <- readRDS(paste(folder,"ng2_ch1_ft.rds",sep=""))
    ng2_ch2_ff <- readRDS(paste(folder,"ng2_ch2_ff.rds",sep=""))
    ng2_ch2_ft <- readRDS(paste(folder,"ng2_ch2_ft.rds",sep=""))
    ng2_ch3_ff <- readRDS(paste(folder,"ng2_ch3_ff.rds",sep=""))
    ng2_ch3_ft <- readRDS(paste(folder,"ng2_ch3_ft.rds",sep=""))
    ng2_ch4_ff <- readRDS(paste(folder,"ng2_ch4_ff.rds",sep=""))
    ng2_ch4_ft <- readRDS(paste(folder,"ng2_ch4_ft.rds",sep=""))
    
    ng3_ch1_ff <- readRDS(paste(folder,"ng3_ch1_ff.rds",sep=""))
    ng3_ch1_ft <- readRDS(paste(folder,"ng3_ch1_ft.rds",sep=""))
    ng3_ch2_ff <- readRDS(paste(folder,"ng3_ch2_ff.rds",sep=""))
    ng3_ch2_ft <- readRDS(paste(folder,"ng3_ch2_ft.rds",sep=""))
    ng3_ch3_ff <- readRDS(paste(folder,"ng3_ch3_ff.rds",sep=""))
    ng3_ch3_ft <- readRDS(paste(folder,"ng3_ch3_ft.rds",sep=""))
    ng3_ch4_ff <- readRDS(paste(folder,"ng3_ch4_ff.rds",sep=""))
    ng3_ch4_ft <- readRDS(paste(folder,"ng3_ch4_ft.rds",sep=""))
    
    ng4_ch1_ff <- readRDS(paste(folder,"ng4_ch1_ff.rds",sep=""))
    ng4_ch1_ft <- readRDS(paste(folder,"ng4_ch1_ft.rds",sep=""))
    ng4_ch2_ff <- readRDS(paste(folder,"ng4_ch2_ff.rds",sep=""))
    ng4_ch2_ft <- readRDS(paste(folder,"ng4_ch2_ft.rds",sep=""))
    ng4_ch3_ff <- readRDS(paste(folder,"ng4_ch3_ff.rds",sep=""))
    ng4_ch3_ft <- readRDS(paste(folder,"ng4_ch3_ft.rds",sep=""))
    ng4_ch4_ff <- readRDS(paste(folder,"ng4_ch4_ff.rds",sep=""))
    ng4_ch4_ft <- readRDS(paste(folder,"ng4_ch4_ft.rds",sep=""))
    
    # DONT FORGET TO REDUCE !

}

lem <- function()
{
    # LEMMATIZATIONFOR STEMMING
    path = "./data/lemmatization-en.txt"
    #lem <- scan(path,"character",sep="\n",skipNul = T)
    lem = read.delim(path,header = FALSE,stringsAsFactors = FALSE)
    names(lem) <- c("stem","term")
    return(lem)
}

lem <-lem()

stem_list <- function(term) {
    i <- match(term, lem$term)
    if (is.na(i)) {
        stem <- term
    } else {
        stem <- lem$stem[[i]]
    }
    stem
}

stem_hunspell <- function(term) {
    # look up the term in the dictionary
    stems <- hunspell::hunspell_stem(term)[[1]]

    if (length(stems) == 0) { # if there are no stems, use the original term
        stem <- term
    } else { # if there are multiple stems, use the last one
        stem <- stems[[length(stems)]]
    }

    stem
}

stem_spellcorrect <- function(term) {
    # if the term is spelled correctly, leave it as-is
    if (hunspell::hunspell_check(term)) {
        return(term)
    }

    suggestions <- hunspell::hunspell_suggest(term)[[1]]

    # if hunspell found a suggestion, use the first one
    if (length(suggestions) > 0) {
        suggestions[[1]]
    } else {
        # otherwise, use the original term
        term
    }
}




reduce <- function(tibble_name) { return(head(tibble_name,nrow(tibble_name)/200)) }
    #return(tibble_name[tibble_name$n > mean(tibble_name$n)*1.5,]) }




get_dictionary <- function(str,verbose=0)  {
    if(verbose>2) print(paste("get_dictionary > str: ",str,sep=""))
    
    if (str=="st_ng1_ch1_ff.rds") ret <- st_ng1_ch1_ff
    if (str=="st_ng1_ch1_ft.rds") ret <- st_ng1_ch1_ft
    if (str=="st_ng1_ch2_ff.rds") ret <- st_ng1_ch2_ff
    if (str=="st_ng1_ch2_ft.rds") ret <- st_ng1_ch2_ft
    if (str=="st_ng1_ch3_ff.rds") ret <- st_ng1_ch3_ff
    if (str=="st_ng1_ch3_ft.rds") ret <- st_ng1_ch3_ft
    if (str=="st_ng1_ch4_ff.rds") ret <- st_ng1_ch4_ff
    if (str=="st_ng1_ch4_ft.rds") ret <- st_ng1_ch4_ft
    if (str=="st_ng2_ch1_ff.rds") ret <- st_ng2_ch1_ff
    if (str=="st_ng2_ch1_ft.rds") ret <- st_ng2_ch1_ft
    if (str=="st_ng2_ch2_ff.rds") ret <- st_ng2_ch2_ff
    if (str=="st_ng2_ch2_ft.rds") ret <- st_ng2_ch2_ft
    if (str=="st_ng2_ch3_ff.rds") ret <- st_ng2_ch3_ff
    if (str=="st_ng2_ch3_ft.rds") ret <- st_ng2_ch3_ft
    if (str=="st_ng2_ch4_ff.rds") ret <- st_ng2_ch4_ff
    if (str=="st_ng2_ch4_ft.rds") ret <- st_ng2_ch4_ft
    if (str=="st_ng3_ch1_ff.rds") ret <- st_ng3_ch1_ff
    if (str=="st_ng3_ch1_ft.rds") ret <- st_ng3_ch1_ft
    if (str=="st_ng3_ch2_ff.rds") ret <- st_ng3_ch2_ff
    if (str=="st_ng3_ch2_ft.rds") ret <- st_ng3_ch2_ft
    if (str=="st_ng3_ch3_ff.rds") ret <- st_ng3_ch3_ff
    if (str=="st_ng3_ch3_ft.rds") ret <- st_ng3_ch3_ft
    if (str=="st_ng3_ch4_ff.rds") ret <- st_ng3_ch4_ff
    if (str=="st_ng3_ch4_ft.rds") ret <- st_ng3_ch4_ft
    if (str=="st_ng4_ch1_ff.rds") ret <- st_ng4_ch1_ff
    if (str=="st_ng4_ch1_ft.rds") ret <- st_ng4_ch1_ft
    if (str=="st_ng4_ch2_ff.rds") ret <- st_ng4_ch2_ff
    if (str=="st_ng4_ch2_ft.rds") ret <- st_ng4_ch2_ft
    if (str=="st_ng4_ch3_ff.rds") ret <- st_ng4_ch3_ff
    if (str=="st_ng4_ch3_ft.rds") ret <- st_ng4_ch3_ft
    if (str=="st_ng4_ch4_ff.rds") ret <- st_ng4_ch4_ff
    if (str=="st_ng4_ch4_ft.rds") ret <- st_ng4_ch4_ft
    
    if(verbose==3) print(paste("get_dictionary * > dim ",toString(dim(ret))," ; typeof(ret) ",typeof(ret),sep=""))
    return(ret)
}



get_random_str <- function(text_tibble,min_words,verbose=0)
{
    if(verbose==2) print(paste("get_random_str >"))
    query=""
    while(query=="") {
        query_str <- text_tibble[sample(1:max(nrow(text_tibble),length(text_tibble)),1),1] %>%
        clean_line(min_chars=1 ,filter_stop_words=F ,return_words=T)
        num_words <- length(query_str)
        if (num_words > min_words)
        {
            random_start_preserving_min_words <- sample(1:(num_words-min_words),1)
            minimum_pos <- random_start_preserving_min_words
            minimum_guarantee <- (random_start_preserving_min_words+min_words)
            expendable_random <- sample(0:(num_words-minimum_guarantee),1)
            query <- gsub(",","",toString(query_str[minimum_pos:(minimum_guarantee+expendable_random)]))
            lastword <- query_str[num_words]
            goodword <- r_ng1_fast$word==lastword
            if (sum(goodword)==0) { query=""
            } else {
                if (r_ng1_fast$n[goodword] < mean(r_ng1_fast$n)) { query=""
                } else {
                if (verbose>1) print(paste("get_random_str > SENTENCE : [ ",query," ] ",sep=""))
                }
            }
        }
    }
    if(verbose==3) print(paste("get_random_str *"))
    return(query)
}


create_pred2_df <- function(pred_df)
{
    verbose=1
     new_df <- data.frame(ngram=integer(),
                          min_chars=integer(),
                          filter_stop_words=logical(), 
                          weight_pct=numeric(),
                          pct_ng1_ch1_ff=integer(),
                          pct_ng1_ch1_ft=integer(),
                          pct_ng1_ch2_ff=integer(),
                          pct_ng1_ch2_ft=integer(),
                          pct_ng1_ch3_ff=integer(),
                          pct_ng1_ch3_ft=integer(),
                          pct_ng1_ch4_ff=integer(),
                          pct_ng1_ch4_ft=integer(),

                          pct_ng2_ch1_ff=integer(),
                          pct_ng2_ch1_ft=integer(),
                          pct_ng2_ch2_ff=integer(),
                          pct_ng2_ch2_ft=integer(),
                          pct_ng2_ch3_ff=integer(),
                          pct_ng2_ch3_ft=integer(),
                          pct_ng2_ch4_ff=integer(),
                          pct_ng2_ch4_ft=integer(),

                          pct_ng3_ch1_ff=integer(),
                          pct_ng3_ch1_ft=integer(),
                          pct_ng3_ch2_ff=integer(),
                          pct_ng3_ch2_ft=integer(),
                          pct_ng3_ch3_ff=integer(),
                          pct_ng3_ch3_ft=integer(),
                          pct_ng3_ch4_ff=integer(),
                          pct_ng3_ch4_ft=integer(),

                          pct_ng4_ch1_ff=integer(),
                          pct_ng4_ch1_ft=integer(),
                          pct_ng4_ch2_ff=integer(),
                          pct_ng4_ch2_ft=integer(),
                          pct_ng4_ch3_ff=integer(),
                          pct_ng4_ch3_ft=integer(),
                          pct_ng4_ch4_ff=integer(),
                          pct_ng4_ch4_ft=integer(),

                          prediction=character(), 
                          solution=character(), 
                          hit1=integer(),
                          hit12=integer(),
                          hit123=integer(),
                          hit1234=integer(),
                          stringsAsFactors = F)

     
     
    num_rows=nrow(pred_df)
    for (k in 1:num_rows)
    {
         if(verbose >0 & k %% round(num_rows/100,0) == 0) print(paste("create_pred2_df > ",round(k/num_rows*100,0)," %", sep=""))
        predict <- pred_df$prediction[k]
        ngram <- pred_df$ngram[k]
        min_chars <- pred_df$min_chars[k]
        filterSW<-ifelse(pred_df$filter_stop_words[k]==F,"f","t")
        dict <- get_dictionary(str=paste("ng",ngram,"_ch",min_chars,"_f",filterSW,".rds",sep=""),verbose=0)
        v1=sum(r_ng1_ch1_ff$n[r_ng1_ch1_ff$word==predict])
        v2=sum(r_ng1_ch1_ft$n[r_ng1_ch1_ft$word==predict])
        v3=sum(r_ng1_ch2_ff$n[r_ng1_ch2_ff$word==predict])
        v4=sum(r_ng1_ch2_ft$n[r_ng1_ch2_ft$word==predict])
        v5=sum(r_ng1_ch3_ff$n[r_ng1_ch3_ff$word==predict])
        v6=sum(r_ng1_ch3_ft$n[r_ng1_ch3_ft$word==predict])
        v7=sum(r_ng1_ch4_ff$n[r_ng1_ch4_ff$word==predict])
        v8=sum(r_ng1_ch4_ft$n[r_ng1_ch4_ft$word==predict])
        
        w1=sum(r_ng2_ch1_ff$n[r_ng2_ch1_ff$word1==predict])
        w2=sum(r_ng2_ch1_ft$n[r_ng2_ch1_ft$word1==predict])
        w3=sum(r_ng2_ch2_ff$n[r_ng2_ch2_ff$word1==predict])
        w4=sum(r_ng2_ch2_ft$n[r_ng2_ch2_ft$word1==predict])
        w5=sum(r_ng2_ch3_ff$n[r_ng2_ch3_ff$word1==predict])
        w6=sum(r_ng2_ch3_ft$n[r_ng2_ch3_ft$word1==predict])
        w7=sum(r_ng2_ch4_ff$n[r_ng2_ch4_ff$word1==predict])
        w8=sum(r_ng2_ch4_ft$n[r_ng2_ch4_ft$word1==predict])
        
        x1=sum(r_ng3_ch1_ff$n[r_ng3_ch1_ff$word2==predict])
        x2=sum(r_ng3_ch1_ft$n[r_ng3_ch1_ft$word2==predict])
        x3=sum(r_ng3_ch2_ff$n[r_ng3_ch2_ff$word2==predict])
        x4=sum(r_ng3_ch2_ft$n[r_ng3_ch2_ft$word2==predict])
        x5=sum(r_ng3_ch3_ff$n[r_ng3_ch3_ff$word2==predict])
        x6=sum(r_ng3_ch3_ft$n[r_ng3_ch3_ft$word2==predict])
        x7=sum(r_ng3_ch4_ff$n[r_ng3_ch4_ff$word2==predict])
        x8=sum(r_ng3_ch4_ft$n[r_ng3_ch4_ft$word2==predict])
        
        y1=sum(r_ng4_ch1_ff$n[r_ng4_ch1_ff$word3==predict])
        y2=sum(r_ng4_ch1_ft$n[r_ng4_ch1_ft$word3==predict])
        y3=sum(r_ng4_ch2_ff$n[r_ng4_ch2_ff$word3==predict])
        y4=sum(r_ng4_ch2_ft$n[r_ng4_ch2_ft$word3==predict])
        y5=sum(r_ng4_ch3_ff$n[r_ng4_ch3_ff$word3==predict])
        y6=sum(r_ng4_ch3_ft$n[r_ng4_ch3_ft$word3==predict])
        y7=sum(r_ng4_ch4_ff$n[r_ng4_ch4_ff$word3==predict])
        y8=sum(r_ng4_ch4_ft$n[r_ng4_ch4_ft$word3==predict])
        
        
        sum_dict <-sum(dict$n)
        
        q01=ifelse(v1==0,0,round(1000000*v1/sum_dict,0))
        q02=ifelse(v2==0,0,round(1000000*v2/sum_dict,0))
        q03=ifelse(v3==0,0,round(1000000*v3/sum_dict,0))
        q04=ifelse(v4==0,0,round(1000000*v4/sum_dict,0))
        q05=ifelse(v5==0,0,round(1000000*v5/sum_dict,0))
        q06=ifelse(v6==0,0,round(1000000*v6/sum_dict,0))
        q07=ifelse(v7==0,0,round(1000000*v7/sum_dict,0))
        q08=ifelse(v8==0,0,round(1000000*v8/sum_dict,0))
        q09=ifelse(w1==0,0,round(1000000*w1/sum_dict,0))
        q10=ifelse(w2==0,0,round(1000000*w2/sum_dict,0))
        q11=ifelse(w3==0,0,round(1000000*w3/sum_dict,0))
        q12=ifelse(w4==0,0,round(1000000*w4/sum_dict,0))
        q13=ifelse(w5==0,0,round(1000000*w5/sum_dict,0))
        q14=ifelse(w6==0,0,round(1000000*w6/sum_dict,0))
        q15=ifelse(w7==0,0,round(1000000*w7/sum_dict,0))
        q16=ifelse(w8==0,0,round(1000000*w8/sum_dict,0))
        q17=ifelse(x1==0,0,round(1000000*x1/sum_dict,0))
        q18=ifelse(x2==0,0,round(1000000*x2/sum_dict,0))
        q19=ifelse(x3==0,0,round(1000000*x3/sum_dict,0))
        q20=ifelse(x4==0,0,round(1000000*x4/sum_dict,0))
        q21=ifelse(x5==0,0,round(1000000*x5/sum_dict,0))
        q22=ifelse(x6==0,0,round(1000000*x6/sum_dict,0))
        q23=ifelse(x7==0,0,round(1000000*x7/sum_dict,0))
        q24=ifelse(x8==0,0,round(1000000*x8/sum_dict,0))
        q25=ifelse(y1==0,0,round(1000000*y1/sum_dict,0))
        q26=ifelse(y2==0,0,round(1000000*y2/sum_dict,0))
        q27=ifelse(y3==0,0,round(1000000*y3/sum_dict,0))
        q28=ifelse(y4==0,0,round(1000000*y4/sum_dict,0))
        q29=ifelse(y5==0,0,round(1000000*y5/sum_dict,0))
        q30=ifelse(y6==0,0,round(1000000*y6/sum_dict,0))
        q31=ifelse(y7==0,0,round(1000000*y7/sum_dict,0))
        q32=ifelse(y8==0,0,round(1000000*y8/sum_dict,0))        

        new_df<-rbind(new_df, data.frame(
            ngram=pred_df$ngram[k],
            min_chars=pred_df$min_chars[k],
            filter_stop_words=pred_df$filter_stop_words[k],
            weight_pct=round(1000000*pred_df$weight[k]/sum_dict,0),
            pct_ng1_ch1_ff = q01 ,pct_ng1_ch1_ft = q02 , pct_ng1_ch2_ff = q03 , pct_ng1_ch2_ft = q04 , pct_ng1_ch3_ff = q05 , pct_ng1_ch3_ft = q06 , pct_ng1_ch4_ff = q07 , pct_ng1_ch4_ft = q08 ,
            pct_ng2_ch1_ff = q09 ,pct_ng2_ch1_ft = q10 , pct_ng2_ch2_ff = q11 , pct_ng2_ch2_ft = q12 , pct_ng2_ch3_ff = q13 , pct_ng2_ch3_ft = q14 , pct_ng2_ch4_ff = q15 , pct_ng2_ch4_ft = q16 ,
            pct_ng3_ch1_ff = q17 ,pct_ng3_ch1_ft = q18 , pct_ng3_ch2_ff = q19 , pct_ng3_ch2_ft = q20 , pct_ng3_ch3_ff = q21 , pct_ng3_ch3_ft = q22 , pct_ng3_ch4_ff = q23 , pct_ng3_ch4_ft = q24 ,
            pct_ng4_ch1_ff = q25 ,pct_ng4_ch1_ft = q26 , pct_ng4_ch2_ff = q27 , pct_ng4_ch2_ft = q28 , pct_ng4_ch3_ff = q29 , pct_ng4_ch3_ft = q30 , pct_ng4_ch4_ff = q31 , pct_ng4_ch4_ft = q32 ,
            prediction=pred_df$prediction[k],
            solution=pred_df$solution[k],
            hit=pred_df$hit[k],
            hit1=pred_df$hit[k]*(q01 + q02 + q03 + q04 + q05 + q06 + q07 + q08),
           hit12=pred_df$hit[k]*(q01 + q02 + q03 + q04 + q05 + q06 + q07 + q08 +
                                 q09 + q10 + q11 + q12 + q13 + q14 + q15 + q16),
           hit123=pred_df$hit[k]*(q01 + q02 + q03 + q04 + q05 + q06 + q07 + q08 +
                                  q09 + q10 + q11 + q12 + q13 + q14 + q15 + q16 +
                                  q17 + q18 + q19 + q20 + q21 + q22 + q23 + q24),
           hit1234=pred_df$hit[k]*(q01 + q02 + q03 + q04 + q05 + q06 + q07 + q08 +
                                   q09 + q10 + q11 + q12 + q13 + q14 + q15 + q16 +
                                   q17 + q18 + q19 + q20 + q21 + q22 + q23 + q24 +
                                   q25 + q26 + q27 + q28 + q29 + q30 + q31 + q32)

                                     ))
    }
    return(new_df)
}

get_weights_for_string <- function(pred_df,query_str,test=T,solution_str="",verbose=0)
{
    get_weight_and_prediction <- function(query_str,ngram=2,min_chars=1,filter_stop_words=F,verbose=0,DEBUGGING=0)
    {
        if (DEBUGGING==1)
        {
            print("=====================================================================")
            #query_str <- get_random_str(text_tibble,verbose=verbose,min_words=min_words)
            test=T
            query_str <- q1#;"award at eversheds it awards dinner tonight thanks for"
            if (test==T) query_str <- paste(query_str,"UNKNOWN",sep=" ")
            query_str_splitted <- unlist(strsplit(query_str,split=" "))
            solution_str = query_str_splitted[length(query_str_splitted)]
            if(verbose>1) print(paste("get_weight_and_prediction > DEBUGGING > Solution [ ",solution_str," ]",sep=""))
            pred_df=pred_df
            verbose=verbose
            ngram=3
            min_chars=1
            filter_stop_words=T
        }
        if(verbose>2) print(paste("get_weight_and_prediction >"))
        kk <- get_dictionary(paste("st_ng",ngram,"_ch",min_chars,"_f",ifelse(filter_stop_words==F,"f","t"),".rds",sep=""),verbose=verbose)
        if (ngram==1) names(kk)[1]<-c("word1")
        qq <- query_str
        if (verbose>2) print(paste("get_weight_and_prediction > qq = [ ",toString(qq)," ]",sep=""))
        qq_words <- unlist(strsplit(qq,split=" "))
        last1 <- gsub("'","''",qq_words[length(qq_words)-1])
        last2 <- gsub("'","''",qq_words[length(qq_words)-2]) 
        last3 <- gsub("'","''",qq_words[length(qq_words)-3]) 
        
        if (str_detect(last1,"'")) last1 <- gsub("'","\\'",last1) 
        if (str_detect(last2,"'")) last2 <- gsub("'","\\'",last2) 
        if (str_detect(last3,"'")) last3 <- gsub("'","\\'",last3) 
        if(verbose>2 & ngram>3) print(paste("get_weight_and_prediction > [ ",ngram,"-gram ] Working with last3 : ",last3,sep=""))        
        if(verbose>2 & ngram>2) print(paste("get_weight_and_prediction > [ ",ngram,"-gram ] Working with last2 : ",last2,sep=""))        
        if(verbose>2 & ngram>1) print(paste("get_weight_and_prediction > [ ",ngram,"-gram ] Working with last1 : ",last1,sep=""))
        if(verbose>2)           print(paste("get_weight_and_prediction > [ ",ngram,"-gram ] Trying to PREDICT  : ",qq_words[length(qq_words)],sep=""))

        if (ngram==1) query_n <- paste("select max(n) from kk",sep="")
        if (ngram==2) query_n <- paste("select max(n) from kk where word1 = '",last1,"'",sep="")
        if (ngram==3) query_n <- paste("select max(n) from kk where word1 = '",last2,"' and word2 = '",last1,"'",sep="")
        if (ngram==4) query_n <- paste("select max(n) from kk where word1 = '",last3,"' and word2 = '",last2,"' and word1 = '",last1,"'",sep="")
        
        if (verbose>2) print(paste('get_weight_and_prediction > QUERY: ',gsub('max\\(n\\)','*',query_n),sep=''))
        if (verbose>1) toString(sqldf(gsub('max\\(n\\)','*',query_n)))

        weight <- sqldf(query_n)[1]
        if (is.na(weight)) weight <- 0 else weight <- as.numeric(weight$`max(n)`)
        

        if (weight!=0)
        {
            
            if (ngram==1 & weight > 0) query_word <- paste("select word1 from kk where n=",as.numeric(weight),sep="")
            if (ngram==2 & weight > 0) query_word <- paste("select word2 from kk where word1 = '",last1,"' and n=",as.numeric(weight),sep="")
            if (ngram==3 & weight > 0) query_word <- paste("select word3 from kk where word1 = '",last2,"' and word2 = '",last1,"' and n=",as.numeric(weight),sep="")
            if (ngram==4 & weight > 0) query_word <- 
                paste("select word4 from kk where word1 = '",last3,"' and word2 = '",last2,"' and word1 = '",last1,"' and n=",as.numeric(weight),sep="")
            if (verbose>2) print(paste('get_weight_and_prediction > QUERY: ',gsub('^select word[123]','select *',query_word),sep=''))
            if (verbose>1) toString(sqldf(gsub('^select word[123]','select *',query_word)))

            pred <- sqldf(query_word)[1]
            pred <- unlist(lapply(pred,as.character))
            if (is.na(pred)) pred <- NA
        } else { pred <- NA }
        ret <- list("col1"=weight,"col2"=pred)
        str_ngram=""
        if (ngram==2) str_ngram=paste(last1," ",sep="")
        if (ngram==3) str_ngram=paste(last2," ",last1," ",sep="")
        if (ngram==4) str_ngram=paste(last3," ",last2," ",last1," ",sep="")
        if(verbose>1) print(paste("get_weight_and_prediction * ng",ngram,"_ch",min_chars,"_f",ifelse(filter_stop_words==F,"f","t")," * ",str_ngram,"[ ",ret$col2," ] (",ret$col1,")",sep=""))
        return(ret)
    }

    if(verbose>3) print(paste("get_weights_for_string >"))
    
    if(test==T) query_str = paste(query_str,"UNKNOWN",str="")
    if(verbose>1) print(paste("get_weights_for_string > SENTENCE : ",query_str,sep=""))
    
    new_pred_df <- data.frame(ngram=integer(),min_chars=integer(),
                      filter_stop_words=logical(), weight=numeric(), 
                      prediction=character(), 
                      solution=character(), hit=integer(),stringsAsFactors = F)
    cont=0
    for (k in 1:4) 
        { # n-grams
        for (j in 1:4) 
            { # min_chars
                for (l in 1:2)
                {
                    if (verbose>2) print(paste("get_weights_for_string > [min_chars] j: ",j," ; [ngrams] k: ",k," ; [(filter_stop_words] l: ",l,sep=""))
                    weight_and_pred <- get_weight_and_prediction(query_str=query_str,
                                                                 ngram=k,
                                                                 min_chars=j,
                                                                 filter_stop_words=ifelse(l==1,T,F),
                                                                 verbose=verbose)
                    cont=cont+1
                    new_pred_df[cont,] <- data.frame(
                        ngrams=k,
                        min_chars=j,
                        filter_stop_words=ifelse(l==1,T,F),
                        weight=weight_and_pred$col1,#[1]),
                        prediction=weight_and_pred$col2,#[2],
                        solution=ifelse(test==T,NA,solution_str),
                        hit=ifelse(as.character(weight_and_pred$col2)==as.character(ifelse(test==T,NA,solution_str)),1,0),
                        stringsAsFactors = F)
                }
            }
        }

    new_pred_df <- new_pred_df[!is.na(new_pred_df$prediction),]
    if (verbose>2) print(paste("get_weights_for_string > Duplicated Rows -> ",nrow(new_pred_df[duplicated(new_pred_df),]),sep=""))
    if(verbose>2) print(paste("get_weights_for_string *"))    
    return(new_pred_df)
}

hit_rate <- function(pred_df,verbose=1)
{
    tmp <- pred_df
    tmp$hit <- 0
    tmp[as.character(tmp$prediction) == as.character(tmp$solution),
        c("hit")] = tmp[as.character(tmp$prediction) == as.character(tmp$solution),c("weight")]
    v_cor <- round(cor(tmp$weight,tmp$hit),2)
    v_num_hits <- (sum(as.character(tmp$prediction) == as.character(tmp$solution)))
    v_pct_hits <- round(sum(as.character(tmp$prediction) == as.character(tmp$solution))/nrow(tmp)*100,2)
    if (verbose>0) print(paste("hit_rate > Correlation : ",v_cor," ; Number of Hits : ",
                               v_num_hits," ; % of Hits : ",v_pct_hits," ; Total Rows : ",nrow(tmp),sep=""))
    if (verbose>0) print(paste("hit_rate > ----------------------------------------",sep=""))
    if (verbose>0) print(paste("hit_rate > Stats - % hits 1-gram      : ",                               round(nrow(pred_df[pred_df$ngram==1 & pred_df$hit==1,])/nrow(pred_df)*100,2),sep=""))
    if (verbose>0) print(paste("hit_rate > Stats - % hits 2-gram      : ",                               round(nrow(pred_df[pred_df$ngram==2 & pred_df$hit==1,])/nrow(pred_df)*100,2),sep=""))
    if (verbose>0) print(paste("hit_rate > Stats - % hits 3-gram      : ",                               round(nrow(pred_df[pred_df$ngram==3 & pred_df$hit==1,])/nrow(pred_df)*100,2),sep=""))
    if (verbose>0) print(paste("hit_rate > Stats - % hits 4-gram      : ",                               round(nrow(pred_df[pred_df$ngram==4 & pred_df$hit==1,])/nrow(pred_df)*100,2),sep=""))
    if (verbose>0) print(paste("hit_rate > Stats - % hits min_chars 1 : ",                               round(nrow(pred_df[pred_df$min_chars==1 & pred_df$hit==1,])/nrow(pred_df)*100,2),sep=""))
    if (verbose>0) print(paste("hit_rate > Stats - % hits min_chars 2 : ",                               round(nrow(pred_df[pred_df$min_chars==2 & pred_df$hit==1,])/nrow(pred_df)*100,2),sep=""))
    if (verbose>0) print(paste("hit_rate > Stats - % hits min_chars 3 : ",                               round(nrow(pred_df[pred_df$min_chars==3 & pred_df$hit==1,])/nrow(pred_df)*100,2),sep=""))
    if (verbose>0) print(paste("hit_rate > Stats - % hits min_chars 4 : ",                               round(nrow(pred_df[pred_df$min_chars==4 & pred_df$hit==1,])/nrow(pred_df)*100,2),sep=""))
    if (verbose>0) print(paste("hit_rate > Stats - % hits filter SW T : ",                               round(nrow(pred_df[pred_df$filter_stop_words==T & pred_df$hit==1,])/nrow(pred_df)*100,2),sep=""))
    if (verbose>0) print(paste("hit_rate > Stats - % hits filter SW F : ",                               round(nrow(pred_df[pred_df$filter_stop_words==F & pred_df$hit==1,])/nrow(pred_df)*100,2),sep=""))
    if (verbose>0) print(paste("hit_rate > ----------------------------------------",sep=""))
    for (j in 1:4){
        for (k in 1:4) {
            for (l in 1:2) {
                l1=ifelse(l==1,"t","f")
                l2=ifelse(l==1,T,F)
    if (verbose>0) print(
        paste("hit_rate > Stats - % hits r_ng",j,"_ch",k,"_f",l1,": ",
            round(nrow(pred_df[pred_df$ngram==j & 
                               pred_df$min_chars==k & 
                               pred_df$filter_stop_words==l2 & 
                               pred_df$hit==1,])/nrow(pred_df)*100,2),sep=""))
            }
        }
    }
    if (verbose>0) print(paste("hit_rate > ----------------------------------------",sep=""))    
    if (verbose>0) print(paste("hit_rate > Corr - n-grams   ~ hits    : ",
                               round(cor(pred_df[,1],pred_df$hit),2),sep=""))
    if (verbose>0) print(paste("hit_rate > Corr - min_chars ~ hits    : ",
                               round(cor(pred_df[,2],pred_df$hit),2),sep=""))
    if (verbose>0) print(paste("hit_rate > Corr - filter SW ~ hits    : ",
                               round(cor(pred_df[,3],pred_df$hit),2),sep=""))
    if (verbose>0) print(paste("hit_rate > Corr - weight    ~ hits    : ",
                               round(cor(pred_df[,4],pred_df$hit),2),sep=""))
    
}

load_pred <- function() { return(pred_df <- readRDS("./data/pred_df.rds"))}
# pred_df <- load_pred()

training <- function(text_tibble, pred_df, iterations=1000,min_words=5,verbose=0,save_every=0)
{
    if(verbose==2) print(paste("training >"))
    for (b in 1:iterations)
    {
        query_str <- get_random_str(text_tibble,verbose=verbose,min_words=min_words)
        query_str_splitted <- unlist(strsplit(query_str,split=" "))
        solution_str = query_str_splitted[length(query_str_splitted)]
        if(verbose>0 & b %% save_every ==0) print(paste("training > Iteration ",b," of ",iterations,sep=""))
        pred_df <- rbind(pred_df,get_weights_for_string(pred_df=pred_df,query_str=query_str,test=F,solution_str=solution_str,verbose=verbose))
        if(verbose>0 & b %% save_every ==0) hit_rate(pred_df,verbose=verbose)
        if(verbose>1) print(paste("training >                  nrow(pred_df) : [ ", nrow(pred_df)," ]",sep=""))
        if((save_every > 0) & (b %% save_every == 0)) 
        {saveRDS(pred_df,"./data/pred_df.rds");print(paste("training > Dataframe has ",nrow(pred_df)," rows. Saving. ",sep=""))}
    }
    if(verbose==3) print(paste("training *"))
    pred_df <- pred_df[!is.na(pred_df$prediction),]
    return(pred_df)
}


 pred_df <- data.frame(ngram=integer(),min_chars=integer(),
                         filter_stop_words=logical(), weight=numeric(), 
                         prediction=character(), multiplier=numeric(),
                         solution=character(), hit=integer(), stringsAsFactors = F)

pred_df <- training(text_tibble=d_all,pred_df=pred_df,iterations=10000,min_words=5,verbose=1,save_every=100)
pred_df



get_weights_for_string(pred_df,query_str=q1,test=T,solution_str="",verbose=2)





pred2_df <- create_pred2_df(pred_df)

training <- pred2_df[,c(-22,-23)]


library(caret)



modFit <- train(hit~.,data=training,method="rf",prox=TRUE)






```