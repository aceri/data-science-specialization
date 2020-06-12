
# library(cld2)
library(stopwords)
library(tidytext)
library(tidyr)
library(dplyr)
library(sqldf)
library(qdapDictionaries)
# library(tm)
# library(tidyverse)
library(stringi)
library(stringr)
# library(ggplot2)
# library(scales)
# library(Hmisc)
# library(wordcloud)
# library(sqldf)
library(corpus)
# library(googleVis)
#op <- options(gvis.plot.tag="chart")



use_pct=1

######################################################################
#
#
#       F U N C T I O N S
#
#
######################################################################

text_filter(d_all)
text_filter(d_all) <- text_filter(drop_punct=TRUE)


is.word  <- function(x) x %in% c(GradyAugmented,Fry_1000,action.verbs,preposition,negation.words,DICTIONARY)


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
    #if (spellcorrect==T)  for (j in 1:length(words)) words[j] <- stem_spellcorrect(words[j])
    #if (filter_unknown==T) for (j in 1:length(words)) words[j] <- ifelse(!is.word(words[j]),"",words[j])
    if (filter_stop_words==T) words <- words[!(words %in% stopwords::stopwords(language="en",source="stopwords-iso"))]
    if (return_words==T) {ret <- words } else { ret <- gsub(" +"," ",gsub(", "," ",toString(words)))}
    return(ret)
}

clean_text <- function(text_tibble, min_chars=1, filter_stop_words=F,verbose=0)
{
    # Needs a tibble of the text with each line being in a column txt
    text <- text_tibble
    num_rows=nrow(text)
    for (k in 1:nrow(text))
    {
        if(verbose >0 & k %% round(num_rows/20,0) == 0) print(paste("clean_text > min_chars ",min_chars," ; filter_stop_words ",filter_stop_words," > ",round(k/num_rows*100,0)," %", sep=""))
        text[k,1] <- clean_line(text[k,1],min_chars = min_chars,filter_stop_words = filter_stop_words , return_words = F)
    }
    return(text)
}


Top_N_words <- function(file_tibble,top=10,min_chars=1,filter_stop_words=F,use_stemming=T)
{
    if (use_stemming==T) {
    file_tibble %>% clean_text(min_chars=min_chars,filter_stop_words=filter_stop_words) %>% 
            stemming(verbose=1) %>% unnest_tokens(word,txt) %>% 
            count(word,sort=TRUE) %>% arrange(desc(n)) %>% 
            mutate(word=reorder(word,n)) -> ret
    } else {
    file_tibble %>% clean_text(min_chars=min_chars,filter_stop_words=filter_stop_words) %>% 
            unnest_tokens(word,txt) %>% count(word,sort=TRUE) %>% 
            arrange(desc(n)) %>% mutate(word=reorder(word,n)) -> ret
    }
    if (top>0) ret <- head(ret,top)
    return(ret)
}



save_ngram_dictionaries<-function()
{
    folder="./data/st_"
    saveRDS(pred_df,paste(folder,"pred_df.rds",sep=""))        
    saveRDS(ng1_fast,paste(folder,"ng1_fast.rds",sep=""))
    

    saveRDS(st_ng1_ch1_ff,"./data/st_ng1_ch1_ff.rds")
    saveRDS(st_ng1_ch1_ft,"./data/st_ng1_ch1_ft.rds")
    saveRDS(st_ng1_ch2_ff,"./data/st_ng1_ch2_ff.rds")
    saveRDS(st_ng1_ch2_ft,"./data/st_ng1_ch2_ft.rds")
    saveRDS(st_ng1_ch3_ff,"./data/st_ng1_ch3_ff.rds")
    saveRDS(st_ng1_ch3_ft,"./data/st_ng1_ch3_ft.rds")
    saveRDS(st_ng1_ch4_ff,"./data/st_ng1_ch4_ff.rds")
    saveRDS(st_ng1_ch4_ft,"./data/st_ng1_ch4_ft.rds")
    saveRDS(st_ng2_ch1_ff,"./data/st_ng2_ch1_ff.rds")
    saveRDS(st_ng2_ch1_ft,"./data/st_ng2_ch1_ft.rds")
    saveRDS(st_ng2_ch2_ff,"./data/st_ng2_ch2_ff.rds")
    saveRDS(st_ng2_ch2_ft,"./data/st_ng2_ch2_ft.rds")
    saveRDS(st_ng2_ch3_ff,"./data/st_ng2_ch3_ff.rds")
    saveRDS(st_ng2_ch3_ft,"./data/st_ng2_ch3_ft.rds")
    saveRDS(st_ng2_ch4_ff,"./data/st_ng2_ch4_ff.rds")
    saveRDS(st_ng2_ch4_ft,"./data/st_ng2_ch4_ft.rds")
    saveRDS(st_ng3_ch1_ff,"./data/st_ng3_ch1_ff.rds")
    saveRDS(st_ng3_ch1_ft,"./data/st_ng3_ch1_ft.rds")
    saveRDS(st_ng3_ch2_ff,"./data/st_ng3_ch2_ff.rds")
    saveRDS(st_ng3_ch2_ft,"./data/st_ng3_ch2_ft.rds")
    saveRDS(st_ng3_ch3_ff,"./data/st_ng3_ch3_ff.rds")
    saveRDS(st_ng3_ch3_ft,"./data/st_ng3_ch3_ft.rds")
    saveRDS(st_ng3_ch4_ff,"./data/st_ng3_ch4_ff.rds")
    saveRDS(st_ng3_ch4_ft,"./data/st_ng3_ch4_ft.rds")
    saveRDS(st_ng4_ch1_ff,"./data/st_ng4_ch1_ff.rds")
    saveRDS(st_ng4_ch1_ft,"./data/st_ng4_ch1_ft.rds")
    saveRDS(st_ng4_ch2_ff,"./data/st_ng4_ch2_ff.rds")
    saveRDS(st_ng4_ch2_ft,"./data/st_ng4_ch2_ft.rds")
    saveRDS(st_ng4_ch3_ff,"./data/st_ng4_ch3_ff.rds")
    saveRDS(st_ng4_ch3_ft,"./data/st_ng4_ch3_ft.rds")
    saveRDS(st_ng4_ch4_ff,"./data/st_ng4_ch4_ff.rds")
    saveRDS(st_ng4_ch4_ft,"./data/st_ng4_ch4_ft.rds")
}

load_ngram_dictionaries <-function()
{


    
    folder <- "./data/"

    blogs_str   <- paste("./Coursera-SwiftKey/final/en_US/en_US.blogs.txt"   ,sep="")
    news_str    <- paste("./Coursera-SwiftKey/final/en_US/en_US.news.txt"    ,sep="")
    twitter_str <- paste("./Coursera-SwiftKey/final/en_US/en_US.twitter.txt" ,sep="")
    
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
    
    # STEMMING DICTIONARIES

        # ROOTS
    
        st_d_all_ch1_ff <- readRDS("./data/st_d_all_ch1_ff.rds")
        st_d_all_ch1_ft <- readRDS("./data/st_d_all_ch1_ft.rds")
        st_d_all_ch2_ff <- readRDS("./data/st_d_all_ch2_ff.rds")
        st_d_all_ch2_ft <- readRDS("./data/st_d_all_ch2_ft.rds")
        st_d_all_ch3_ff <- readRDS("./data/st_d_all_ch3_ff.rds")
        st_d_all_ch3_ft <- readRDS("./data/st_d_all_ch3_ft.rds")
        st_d_all_ch4_ff <- readRDS("./data/st_d_all_ch4_ff.rds")
        st_d_all_ch4_ft <- readRDS("./data/st_d_all_ch4_ft.rds")    

        # 1-GRAM        

        st_ng1_ch1_ff <- readRDS("./data/st_ng1_ch1_ff.rds")
        st_ng1_ch1_ft <- readRDS("./data/st_ng1_ch1_ft.rds")
        st_ng1_ch2_ff <- readRDS("./data/st_ng1_ch2_ff.rds")
        st_ng1_ch2_ft <- readRDS("./data/st_ng1_ch2_ft.rds")
        st_ng1_ch3_ff <- readRDS("./data/st_ng1_ch3_ff.rds")
        st_ng1_ch3_ft <- readRDS("./data/st_ng1_ch3_ft.rds")
        st_ng1_ch4_ff <- readRDS("./data/st_ng1_ch4_ff.rds")
        st_ng1_ch4_ft <- readRDS("./data/st_ng1_ch4_ft.rds")
        

    ng1_fast   <- readRDS(paste(folder,"ng1_fast.rds",sep=""))
    pred_df <- readRDS(paste(folder,"pred_df.rds",sep=""))
    


    st_ng2_ch1_ff <- readRDS("./data/st_ng2_ch1_ff.rds")
    st_ng2_ch1_ft <- readRDS("./data/st_ng2_ch1_ft.rds")
    st_ng2_ch2_ff <- readRDS("./data/st_ng2_ch2_ff.rds")
    st_ng2_ch2_ft <- readRDS("./data/st_ng2_ch2_ft.rds")
    st_ng2_ch3_ff <- readRDS("./data/st_ng2_ch3_ff.rds")
    st_ng2_ch3_ft <- readRDS("./data/st_ng2_ch3_ft.rds")
    st_ng2_ch4_ff <- readRDS("./data/st_ng2_ch4_ff.rds")
    st_ng2_ch4_ft <- readRDS("./data/st_ng2_ch4_ft.rds")
    st_ng3_ch1_ff <- readRDS("./data/st_ng3_ch1_ff.rds")
    st_ng3_ch1_ft <- readRDS("./data/st_ng3_ch1_ft.rds")
    st_ng3_ch2_ff <- readRDS("./data/st_ng3_ch2_ff.rds")
    st_ng3_ch2_ft <- readRDS("./data/st_ng3_ch2_ft.rds")
    st_ng3_ch3_ff <- readRDS("./data/st_ng3_ch3_ff.rds")
    st_ng3_ch3_ft <- readRDS("./data/st_ng3_ch3_ft.rds")
    st_ng3_ch4_ff <- readRDS("./data/st_ng3_ch4_ff.rds")
    st_ng3_ch4_ft <- readRDS("./data/st_ng3_ch4_ft.rds")
    st_ng4_ch1_ff <- readRDS("./data/st_ng4_ch1_ff.rds")
    st_ng4_ch1_ft <- readRDS("./data/st_ng4_ch1_ft.rds")
    st_ng4_ch2_ff <- readRDS("./data/st_ng4_ch2_ff.rds")
    st_ng4_ch2_ft <- readRDS("./data/st_ng4_ch2_ft.rds")
    st_ng4_ch3_ff <- readRDS("./data/st_ng4_ch3_ff.rds")
    st_ng4_ch3_ft <- readRDS("./data/st_ng4_ch3_ft.rds")
    st_ng4_ch4_ff <- readRDS("./data/st_ng4_ch4_ff.rds")
    st_ng4_ch4_ft <- readRDS("./data/st_ng4_ch4_ft.rds")
    
    # DONT FORGET TO REDUCE !

}

# lem <- function()
# {
#     # LEMMATIZATIONFOR STEMMING
#     path = "./data/lemmatization-en.txt"
#     #lem <- scan(path,"character",sep="\n",skipNul = T)
#     lem = read.delim(path,header = FALSE,stringsAsFactors = FALSE)
#     names(lem) <- c("stem","term")
#     return(lem)
# }

# lem <-lem()
# 
# stem_list <- function(term) {
#     i <- match(term, lem$term)
#     if (is.na(i)) {
#         stem <- term
#     } else {
#         stem <- lem$stem[[i]]
#     }
#     stem
# }

# stem_hunspell <- function(term) {
#     # look up the term in the dictionary
#     stems <- hunspell::hunspell_stem(term)[[1]]
# 
#     if (length(stems) == 0) { # if there are no stems, use the original term
#         stem <- term
#     } else { # if there are multiple stems, use the last one
#         stem <- stems[[length(stems)]]
#     }
# 
#     stem
# }

# stem_spellcorrect <- function(term) {
#     # if the term is spelled correctly, leave it as-is
#     if (hunspell::hunspell_check(term)) {
#         return(term)
#     }
# 
#     suggestions <- hunspell::hunspell_suggest(term)[[1]]
# 
#     # if hunspell found a suggestion, use the first one
#     if (length(suggestions) > 0) {
#         suggestions[[1]]
#     } else {
#         # otherwise, use the original term
#         term
#     }
# }


# stemming <- function(text,verbose=0)
# {
#     store_name <- names(text)
#     names(text)<-c("text")
#     num_rows=nrow(text)
#     for (k in 1: nrow(text))
#     {
#         if(verbose >0 & k %% round(num_rows/20,0) == 0) print(paste("stemming > ",round(k/num_rows*100,0)," %", sep=""))
#         text[k,1] <- gsub("[ ]+"," ",gsub(",","",toString(unlist(text_tokens(x=text[k,1],stemmer=stem_list))))) 
#     }
#     names(text) <- store_name
#     return(text)
# }



text_filter(d_all)


prepare_dictionary <-function(){
    
all_dat <- rbind(tibble(txt=news_dat),
                 tibble(txt=blogs_dat),
                 tibble(txt=twitter_dat))
    
#text_tokens(text_tokens(qq,stemmer=stem_spellcorrect),stemmer=stem_list)

    all_dat[sample(1:nrow(all_dat),round(nrow(all_dat),0)*0.01),]  -> d_all
    
    names(d_all) <- c("text")
    text_tokens(d_all,token_filter(stemmer="english"))
    
    
    #text_tokens(stemmer=stem_spellcorrect) %>% text_tokens(stemmer=stem_list)

    d_all %>% clean_text(1, F, 1) -> st_d_all_ch1_ff  
    d_all %>% clean_text(1, T, 1) -> st_d_all_ch1_ft  
    d_all %>% clean_text(2, F, 1) -> st_d_all_ch2_ff  
    d_all %>% clean_text(2, T, 1) -> st_d_all_ch2_ft  
    d_all %>% clean_text(3, F, 1) -> st_d_all_ch3_ff  
    d_all %>% clean_text(3, T, 1) -> st_d_all_ch3_ft  
    d_all %>% clean_text(4, F, 1) -> st_d_all_ch4_ff  
    d_all %>% clean_text(4, T, 1) -> st_d_all_ch4_ft  
    
    saveRDS(st_d_all_ch1_ff,"./data/st_d_all_ch1_ff.rds")
    saveRDS(st_d_all_ch1_ft,"./data/st_d_all_ch1_ft.rds")
    saveRDS(st_d_all_ch2_ff,"./data/st_d_all_ch2_ff.rds")
    saveRDS(st_d_all_ch2_ft,"./data/st_d_all_ch2_ft.rds")
    saveRDS(st_d_all_ch3_ff,"./data/st_d_all_ch3_ff.rds")
    saveRDS(st_d_all_ch3_ft,"./data/st_d_all_ch3_ft.rds")
    saveRDS(st_d_all_ch4_ff,"./data/st_d_all_ch4_ff.rds")
    saveRDS(st_d_all_ch4_ft,"./data/st_d_all_ch4_ft.rds")

    Top_N_words(d_all ,top=0,min_chars=1,filter_stop_words=F,use_stemming=T) -> st_ng1_ch1_ff
    Top_N_words(d_all ,top=0,min_chars=1,filter_stop_words=T,use_stemming=T) -> st_ng1_ch1_ft
    Top_N_words(d_all ,top=0,min_chars=2,filter_stop_words=T,use_stemming=T) -> st_ng1_ch2_ff
    Top_N_words(d_all ,top=0,min_chars=2,filter_stop_words=T,use_stemming=T) -> st_ng1_ch2_ft
    Top_N_words(d_all ,top=0,min_chars=3,filter_stop_words=F,use_stemming=T) -> st_ng1_ch3_ff
    Top_N_words(d_all ,top=0,min_chars=3,filter_stop_words=T,use_stemming=T) -> st_ng1_ch3_ft
    Top_N_words(d_all ,top=0,min_chars=4,filter_stop_words=F,use_stemming=T) -> st_ng1_ch4_ff
    Top_N_words(d_all ,top=0,min_chars=4,filter_stop_words=T,use_stemming=T) -> st_ng1_ch4_ft
    
    saveRDS(st_ng1_ch1_ff,"./data/st_ng1_ch1_ff.rds")
    saveRDS(st_ng1_ch1_ft,"./data/st_ng1_ch1_ft.rds")
    saveRDS(st_ng1_ch2_ff,"./data/st_ng1_ch2_ff.rds")
    saveRDS(st_ng1_ch2_ft,"./data/st_ng1_ch2_ft.rds")
    saveRDS(st_ng1_ch3_ff,"./data/st_ng1_ch3_ff.rds")
    saveRDS(st_ng1_ch3_ft,"./data/st_ng1_ch3_ft.rds")
    saveRDS(st_ng1_ch4_ff,"./data/st_ng1_ch4_ff.rds")
    saveRDS(st_ng1_ch4_ft,"./data/st_ng1_ch4_ft.rds")

    st_d_all_ch1_ff %>% unnest_ngrams(word,txt,n=2) %>% count(word) %>% arrange(desc(n)) %>% separate(word, c("word1","word2")," ")                 -> st_ng2_ch1_ff
    st_d_all_ch1_ff %>% unnest_ngrams(word,txt,n=3) %>% count(word) %>% arrange(desc(n)) %>% separate(word, c("word1","word2","word3")," ")         -> st_ng3_ch1_ff
    st_d_all_ch1_ff %>% unnest_ngrams(word,txt,n=4) %>% count(word) %>% arrange(desc(n)) %>% separate(word, c("word1","word2","word3","word4")," ") -> st_ng4_ch1_ff
    st_d_all_ch1_ft %>% unnest_ngrams(word,txt,n=2) %>% count(word) %>% arrange(desc(n)) %>% separate(word, c("word1","word2")," ")                 -> st_ng2_ch1_ft
    st_d_all_ch1_ft %>% unnest_ngrams(word,txt,n=3) %>% count(word) %>% arrange(desc(n)) %>% separate(word, c("word1","word2","word3")," ")         -> st_ng3_ch1_ft
    st_d_all_ch1_ft %>% unnest_ngrams(word,txt,n=4) %>% count(word) %>% arrange(desc(n)) %>% separate(word, c("word1","word2","word3","word4")," ") -> st_ng4_ch1_ft
    st_d_all_ch2_ff %>% unnest_ngrams(word,txt,n=2) %>% count(word) %>% arrange(desc(n)) %>% separate(word, c("word1","word2")," ")                 -> st_ng2_ch2_ff
    st_d_all_ch2_ff %>% unnest_ngrams(word,txt,n=3) %>% count(word) %>% arrange(desc(n)) %>% separate(word, c("word1","word2","word3")," ")         -> st_ng3_ch2_ff
    st_d_all_ch2_ff %>% unnest_ngrams(word,txt,n=4) %>% count(word) %>% arrange(desc(n)) %>% separate(word, c("word1","word2","word3","word4")," ") -> st_ng4_ch2_ff
    st_d_all_ch2_ft %>% unnest_ngrams(word,txt,n=2) %>% count(word) %>% arrange(desc(n)) %>% separate(word, c("word1","word2")," ")                 -> st_ng2_ch2_ft
    st_d_all_ch2_ft %>% unnest_ngrams(word,txt,n=3) %>% count(word) %>% arrange(desc(n)) %>% separate(word, c("word1","word2","word3")," ")         -> st_ng3_ch2_ft
    st_d_all_ch2_ft %>% unnest_ngrams(word,txt,n=4) %>% count(word) %>% arrange(desc(n)) %>% separate(word, c("word1","word2","word3","word4")," ") -> st_ng4_ch2_ft
    st_d_all_ch3_ff %>% unnest_ngrams(word,txt,n=2) %>% count(word) %>% arrange(desc(n)) %>% separate(word, c("word1","word2")," ")                 -> st_ng2_ch3_ff
    st_d_all_ch3_ff %>% unnest_ngrams(word,txt,n=3) %>% count(word) %>% arrange(desc(n)) %>% separate(word, c("word1","word2","word3")," ")         -> st_ng3_ch3_ff
    st_d_all_ch3_ff %>% unnest_ngrams(word,txt,n=4) %>% count(word) %>% arrange(desc(n)) %>% separate(word, c("word1","word2","word3","word4")," ") -> st_ng4_ch3_ff
    st_d_all_ch3_ft %>% unnest_ngrams(word,txt,n=2) %>% count(word) %>% arrange(desc(n)) %>% separate(word, c("word1","word2")," ")                 -> st_ng2_ch3_ft
    st_d_all_ch3_ft %>% unnest_ngrams(word,txt,n=3) %>% count(word) %>% arrange(desc(n)) %>% separate(word, c("word1","word2","word3")," ")         -> st_ng3_ch3_ft
    st_d_all_ch3_ft %>% unnest_ngrams(word,txt,n=4) %>% count(word) %>% arrange(desc(n)) %>% separate(word, c("word1","word2","word3","word4")," ") -> st_ng4_ch3_ft
    st_d_all_ch4_ff %>% unnest_ngrams(word,txt,n=2) %>% count(word) %>% arrange(desc(n)) %>% separate(word, c("word1","word2")," ")                 -> st_ng2_ch4_ff
    st_d_all_ch4_ff %>% unnest_ngrams(word,txt,n=3) %>% count(word) %>% arrange(desc(n)) %>% separate(word, c("word1","word2","word3")," ")         -> st_ng3_ch4_ff
    st_d_all_ch4_ff %>% unnest_ngrams(word,txt,n=4) %>% count(word) %>% arrange(desc(n)) %>% separate(word, c("word1","word2","word3","word4")," ") -> st_ng4_ch4_ff
    st_d_all_ch4_ft %>% unnest_ngrams(word,txt,n=2) %>% count(word) %>% arrange(desc(n)) %>% separate(word, c("word1","word2")," ")                 -> st_ng2_ch4_ft
    st_d_all_ch4_ft %>% unnest_ngrams(word,txt,n=3) %>% count(word) %>% arrange(desc(n)) %>% separate(word, c("word1","word2","word3")," ")         -> st_ng3_ch4_ft
    st_d_all_ch4_ft %>% unnest_ngrams(word,txt,n=4) %>% count(word) %>% arrange(desc(n)) %>% separate(word, c("word1","word2","word3","word4")," ") -> st_ng4_ch4_ft
    
    saveRDS(st_ng2_ch1_ff,"./data/st_ng2_ch1_ff.rds")
    saveRDS(st_ng2_ch1_ft,"./data/st_ng2_ch1_ft.rds")
    saveRDS(st_ng2_ch2_ff,"./data/st_ng2_ch2_ff.rds")
    saveRDS(st_ng2_ch2_ft,"./data/st_ng2_ch2_ft.rds")
    saveRDS(st_ng2_ch3_ff,"./data/st_ng2_ch3_ff.rds")
    saveRDS(st_ng2_ch3_ft,"./data/st_ng2_ch3_ft.rds")
    saveRDS(st_ng2_ch4_ff,"./data/st_ng2_ch4_ff.rds")
    saveRDS(st_ng2_ch4_ft,"./data/st_ng2_ch4_ft.rds")
    saveRDS(st_ng3_ch1_ff,"./data/st_ng3_ch1_ff.rds")
    saveRDS(st_ng3_ch1_ft,"./data/st_ng3_ch1_ft.rds")
    saveRDS(st_ng3_ch2_ff,"./data/st_ng3_ch2_ff.rds")
    saveRDS(st_ng3_ch2_ft,"./data/st_ng3_ch2_ft.rds")
    saveRDS(st_ng3_ch3_ff,"./data/st_ng3_ch3_ff.rds")
    saveRDS(st_ng3_ch3_ft,"./data/st_ng3_ch3_ft.rds")
    saveRDS(st_ng3_ch4_ff,"./data/st_ng3_ch4_ff.rds")
    saveRDS(st_ng3_ch4_ft,"./data/st_ng3_ch4_ft.rds")
    saveRDS(st_ng4_ch1_ff,"./data/st_ng4_ch1_ff.rds")
    saveRDS(st_ng4_ch1_ft,"./data/st_ng4_ch1_ft.rds")
    saveRDS(st_ng4_ch2_ff,"./data/st_ng4_ch2_ff.rds")
    saveRDS(st_ng4_ch2_ft,"./data/st_ng4_ch2_ft.rds")
    saveRDS(st_ng4_ch3_ff,"./data/st_ng4_ch3_ff.rds")
    saveRDS(st_ng4_ch3_ft,"./data/st_ng4_ch3_ft.rds")
    saveRDS(st_ng4_ch4_ff,"./data/st_ng4_ch4_ff.rds")
    saveRDS(st_ng4_ch4_ft,"./data/st_ng4_ch4_ft.rds")
    
    
    
    
    ng1_all <- rbind(st_ng1_ch1_ff,st_ng1_ch1_ft,st_ng1_ch2_ff,st_ng1_ch2_ft,st_ng1_ch3_ff,st_ng1_ch3_ft,st_ng1_ch4_ff,st_ng1_ch4_ft)
    ng1_fast <- sqldf("select word, sum(n) as n from ng1_all group by word order by n desc")
    ng1_fast<-ng1_fast[!duplicated(ng1_fast),]
    r_ng1_fast <- reduce(ng1_fast)
    

    r_ng1_ch1_ff <- reduce(st_ng1_ch1_ff)
    r_ng1_ch1_ft <- reduce(st_ng1_ch1_ft)
    r_ng1_ch2_ff <- reduce(st_ng1_ch2_ff)
    r_ng1_ch2_ft <- reduce(st_ng1_ch2_ft)
    r_ng1_ch3_ff <- reduce(st_ng1_ch3_ff)
    r_ng1_ch3_ft <- reduce(st_ng1_ch3_ft)
    r_ng1_ch4_ff <- reduce(st_ng1_ch4_ff)
    r_ng1_ch4_ft <- reduce(st_ng1_ch4_ft)
    r_ng2_ch1_ff <- reduce(st_ng2_ch1_ff)
    r_ng2_ch1_ft <- reduce(st_ng2_ch1_ft)
    r_ng2_ch2_ff <- reduce(st_ng2_ch2_ff)
    r_ng2_ch2_ft <- reduce(st_ng2_ch2_ft)
    r_ng2_ch3_ff <- reduce(st_ng2_ch3_ff)
    r_ng2_ch3_ft <- reduce(st_ng2_ch3_ft)
    r_ng2_ch4_ff <- reduce(st_ng2_ch4_ff)
    r_ng2_ch4_ft <- reduce(st_ng2_ch4_ft)
    r_ng3_ch1_ff <- reduce(st_ng3_ch1_ff)
    r_ng3_ch1_ft <- reduce(st_ng3_ch1_ft)
    r_ng3_ch2_ff <- reduce(st_ng3_ch2_ff)
    r_ng3_ch2_ft <- reduce(st_ng3_ch2_ft)
    r_ng3_ch3_ff <- reduce(st_ng3_ch3_ff)
    r_ng3_ch3_ft <- reduce(st_ng3_ch3_ft)
    r_ng3_ch4_ff <- reduce(st_ng3_ch4_ff)
    r_ng3_ch4_ft <- reduce(st_ng3_ch4_ft)
    r_ng4_ch1_ff <- reduce(st_ng4_ch1_ff)
    r_ng4_ch1_ft <- reduce(st_ng4_ch1_ft)
    r_ng4_ch2_ff <- reduce(st_ng4_ch2_ff)
    r_ng4_ch2_ft <- reduce(st_ng4_ch2_ft)
    r_ng4_ch3_ff <- reduce(st_ng4_ch3_ff)
    r_ng4_ch3_ft <- reduce(st_ng4_ch3_ft)
    r_ng4_ch4_ff <- reduce(st_ng4_ch4_ff)
    r_ng4_ch4_ft <- reduce(st_ng4_ch4_ft)
}





#reduce <- function(tibble_name) { return(head(tibble_name,nrow(tibble_name)/200)) }

calc_reduce<-function(tibble_name,minlines){
    k1 <- nrow(tibble_name)
    
    tibble_name[is.word(tibble_name[1:(dim(tibble_name)[2]-1)]),]

    k2 <- nrow(tibble_name[tibble_name$n>minlines,])
    s1 <- sum(tibble_name$n)
    s2 <- sum(tibble_name$n[tibble_name$n>minlines])
    if (minlines >0) print(paste("reduce > reduced from ",k1," rows to ", k2," [",100-round(k2/k1*100,0),"%]. The number of instances from ",s1," to ",s2," [",100-round(s2/s1*100,0),"%].",sep=""))    
}


reduce <- function(tibble_name,minlines) { 
    k1=nrow(tibble_name)
    s1=sum(tibble_name$n)
    ret <- tibble_name[tibble_name$n>minlines,]
    k2=nrow(ret)
    s2=sum(ret$n)
    print(paste("reduce > reduced from ",k1," rows to ", k2," [",100-round(k2/k1*100,0),"%]. The number of instances from ",s1," to ",s2," [",100-round(s2/s1*100,0),"%].",sep=""))
    return (ret)
}


check_words <- function(tibble_name,verbose=F) { 
    tib <- tibble_name
    num_rows <- nrow(tibble_name)
    num_cols <- ncol(tib)-1
    s1 <- sum(tibble_name$n)
    remove_list=c()
    for (j in 1:num_rows)
    {
        found_strange_word_in_line = 0
        for (k in 1:num_cols){
            word <- tib[j,k]
            if (!is.word(word)) { found_strange_word_in_line=1 } 
        }
        if (found_strange_word_in_line==1 & verbose==T) 
        {
            print(paste("check_words > Lines with unrecognized words : ",gsub(",","",toString(tib[j,1:num_cols])),sep=""))
            tib[j,num_cols+1] <- 1
        }    
    }
    tib<-tib[tib[,num_cols+1]==0,]
    final_rows <- nrow(tib)
    s2 <- sum(tib$n)
    print(paste("check_words > Lines with unknown words removed from ",num_rows," to ",final_rows," [",round(100-100*final_rows/num_rows,0),"%]. Instances reduced in ",s2," [",round(100-100*s2/s1,0),"%]",sep=""))
    return(tib)
}


reduce_dictionaries <- function ()
{
    ng1_all <- rbind(ng1_ch1_ff,ng1_ch1_ft,ng1_ch2_ff,ng1_ch2_ft,ng1_ch3_ff,ng1_ch3_ft,ng1_ch4_ff,ng1_ch4_ft)
    ng1_fast <- sqldf("select word, sum(n) as n from ng1_all group by word order by n desc")
    ng1_fast<-ng1_fast[!duplicated(ng1_fast),]
    r_ng1_fast <- reduce(ng1_fast)
    

    st_ng1_ch1_ff <- reduce(st_ng1_ch1_ff,F)
    st_ng1_ch1_ft <- reduce(st_ng1_ch1_ft)
    st_ng1_ch2_ff <- reduce(st_ng1_ch2_ff)
    st_ng1_ch2_ft <- reduce(st_ng1_ch2_ft)
    st_ng1_ch3_ff <- reduce(st_ng1_ch3_ff)
    st_ng1_ch3_ft <- reduce(st_ng1_ch3_ft)
    st_ng1_ch4_ff <- reduce(st_ng1_ch4_ff)
    st_ng1_ch4_ft <- reduce(st_ng1_ch4_ft)
    st_ng2_ch1_ff <- reduce(st_ng2_ch1_ff)
    st_ng2_ch1_ft <- reduce(st_ng2_ch1_ft)
    st_ng2_ch2_ff <- reduce(st_ng2_ch2_ff)
    st_ng2_ch2_ft <- reduce(st_ng2_ch2_ft)
    st_ng2_ch3_ff <- reduce(st_ng2_ch3_ff)
    st_ng2_ch3_ft <- reduce(st_ng2_ch3_ft)
    st_ng2_ch4_ff <- reduce(st_ng2_ch4_ff)
    st_ng2_ch4_ft <- reduce(st_ng2_ch4_ft)
    st_ng3_ch1_ff <- reduce(st_ng3_ch1_ff)
    st_ng3_ch1_ft <- reduce(st_ng3_ch1_ft)
    st_ng3_ch2_ff <- reduce(st_ng3_ch2_ff)
    st_ng3_ch2_ft <- reduce(st_ng3_ch2_ft)
    st_ng3_ch3_ff <- reduce(st_ng3_ch3_ff)
    st_ng3_ch3_ft <- reduce(st_ng3_ch3_ft)
    st_ng3_ch4_ff <- reduce(st_ng3_ch4_ff)
    st_ng3_ch4_ft <- reduce(st_ng3_ch4_ft)
    st_ng4_ch1_ff <- reduce(st_ng4_ch1_ff)
    st_ng4_ch1_ft <- reduce(st_ng4_ch1_ft)
    st_ng4_ch2_ff <- reduce(st_ng4_ch2_ff)
    st_ng4_ch2_ft <- reduce(st_ng4_ch2_ft)
    st_ng4_ch3_ff <- reduce(st_ng4_ch3_ff)
    st_ng4_ch3_ft <- reduce(st_ng4_ch3_ft)
    st_ng4_ch4_ff <- reduce(st_ng4_ch4_ff)
    st_ng4_ch4_ft <- reduce(st_ng4_ch4_ft)
}

prob_hist<-function(vec){
# relative frequency
h <- hist(vec,breaks=100,plot=FALSE)
h$counts=h$counts/sum(h$counts)
plot(h)
}

