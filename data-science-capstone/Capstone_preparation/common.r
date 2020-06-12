#require(sqldf)
require(qdapDictionaries)
require(dplyr)
#require(textclean)
#require(corpus)
#require(healthcareai)
require(udpipe)
dl <- "./data/english-ewt-ud-2.4-190531.udpipe"
udmodel_english <- udpipe_load_model(dl)

ng1  <- readRDS("./data/ng1.rds")
ng2a <- readRDS("./data/ng2a.rds")
ng3a <- readRDS("./data/ng3a.rds")
ng4a <- readRDS("./data/ng4a.rds")
ng5a <- readRDS("./data/ng5a.rds")
ng6a <- readRDS("./data/ng6a.rds")
xng1  <- readRDS("./data/xng1.rds")
xng2a <- readRDS("./data/xng2a.rds")
xng3a <- readRDS("./data/xng3a.rds")
xng4a <- readRDS("./data/xng4a.rds")
xng5a <- readRDS("./data/xng5a.rds")
xng6a <- readRDS("./data/xng6a.rds")



pos_line <- function(line,ret_words = F)
{
    require(udpipe)
    ret = as.data.frame(udpipe_annotate(udmodel_english,x=line,doc_id=seq_along(line)))$upos
    if (ret_words == F ) ret <- gsub(",","",toString(ret))
    return(ret)
}



predict_word <- function(line, test=F,num_replies=100,verbose=0)
{
    require(corpus)
    fill_df <- function(line,ng,num_ng,num_replies=100,verbose=0,test=F)
    {
        l1="";l2="";l3="";l4="";l5=""
        p1="";p2="";p3="";p4="";p5=""
        
        words <- unlist(text_tokens(line,filter=text_filter(drop_punct=T,map_case=T,drop_symbol=T,drop_number = T)))
        num_words <- length(words)
        num_replies <- min(num_replies,nrow(ng))
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
        
        # p_ng2a <- NA; p_ng3a <- NA; p_ng4a <- NA; p_ng5a <- NA; p_ng6a <- NA
        # if(pos_num_words>1 & num_ng > 1) png <- xng2a[xng2a$type1==tolower(p1) ,]    
        # if(pos_num_words>2 & num_ng > 2) png <- xng3a[xng3a$type1==tolower(p2) & xng3a$type2==tolower(p1) ,]
        # if(pos_num_words>3 & num_ng > 3) png <- xng4a[xng4a$type1==tolower(p3) & xng4a$type2==tolower(p2) & xng4a$type3==tolower(p1) ,]
        # if(pos_num_words>4 & num_ng > 4) png <- xng5a[xng5a$type1==tolower(p4) & xng5a$type2==tolower(p3) & xng5a$type3==tolower(p2) & xng5a$type4==tolower(p1) ,]
        # if(pos_num_words>5 & num_ng > 5) png <- xng6a[xng6a$type1==tolower(p5) & xng6a$type2==tolower(p4) & xng6a$type3==tolower(p3) & xng6a$type4==tolower(p2) & xng6a$type5==tolower(p1) ,]
        
        
        if (nrow(ng)==0) ng <- NA
        if (is.na(ng))  # y para png no hacemos lo mismo ??
        {
            df <- data.frame(ng=integer(), count=numeric(),
                             p5=character(),p4=character(),p3=character(),p2=character(),p1=character(),p0=character(),
                             l5=character(),l4=character(),l3=character(),l2=character(),l1=character(), 
                             word=character(),solution=character(),stringsAsFactors = F)
        } else {
            if (nrow(ng)>0)
            {
                df <- data.frame(ng=integer(), count=numeric(),
                                 p5=character(),p4=character(),p3=character(),p2=character(),p1=character(),p0=character(),
                                 l5=character(),l4=character(),l3=character(),l2=character(),l1=character(), 
                                 word=character(),solution=character(),stringsAsFactors = F)
                df[1:nrow(ng),1]  <- num_ng
                df[1:nrow(ng),2]  <- round(ng$count/sum(ng$count)*100,2)
                df[1:nrow(ng),3]  <- p5
                df[1:nrow(ng),4]  <- p4
                df[1:nrow(ng),5]  <- p3  
                df[1:nrow(ng),6]  <- p2
                df[1:nrow(ng),7]  <- p1
                df[1:nrow(ng),8]  <- ""
                df[1:nrow(ng),9]  <- l5
                df[1:nrow(ng),10] <- l4
                df[1:nrow(ng),11] <- l3
                df[1:nrow(ng),12] <- l2
                df[1:nrow(ng),13] <- l1
                
                df[1:nrow(ng),14] <- ng[,(1+num_ng)]
                df[1:nrow(ng),15] <- solution    
            } else {df <- ng}
        }
        if (verbose>2) print(paste("fill_df > ng: ",num_ng," >> ",line,sep=""))
        if (verbose > 2) print("fill_df *")
        return(df)
    }
    #-----------------------------------------------------------------------------------------------------        
    if(test==T) line=paste(line,"UNKNOWN",sep=" ") 
    words <- unlist(text_tokens(line,filter=text_filter(drop_punct=T,map_case=T,drop_symbol=T,drop_number = T)))
    num_words <- length(words)
    solution <- words[length(words)]
    if (verbose>1) print(paste("predict_word       > ", gsub(",","",toString(words[1:(length(words)-1)]))," [ ",solution," ]",sep=""))
    #-----------------------------------------------------------------------------------------------------
    r_ng2a <- NA; r_ng3a <- NA; r_ng4a <- NA; r_ng5a <- NA; r_ng6a <- NA
    if(num_words>1) r_ng2a <- ng2a[ng2a$type1==words[num_words-1] ,]    
    if(num_words>2) r_ng3a <- ng3a[ng3a$type1==words[num_words-2] & ng3a$type2==words[num_words-1] ,]
    if(num_words>3) r_ng4a <- ng4a[ng4a$type1==words[num_words-3] & ng4a$type2==words[num_words-2] & ng4a$type3==words[num_words-1] ,]
    if(num_words>4) r_ng5a <- ng5a[ng5a$type1==words[num_words-4] & ng5a$type2==words[num_words-3] & ng5a$type3==words[num_words-2] & ng5a$type4==words[num_words-1] ,]
    if(num_words>5) r_ng6a <- ng6a[ng6a$type1==words[num_words-5] & ng6a$type2==words[num_words-4] & ng6a$type3==words[num_words-3] & ng6a$type4==words[num_words-2] & ng6a$type5==words[num_words-1] ,]
    #-----------------------------------------------------------------------------------------------------    
    #-----------------------------------------------------------------------------------------------------        
    pre_final <- rbind(fill_df(line,r_ng2a,2,num_replies=num_replies,verbose,test=test),
                       fill_df(line,r_ng3a,3,num_replies=num_replies,verbose,test=test),
                       fill_df(line,r_ng4a,4,num_replies=num_replies,verbose,test=test),
                       fill_df(line,r_ng5a,5,num_replies=num_replies,verbose,test=test),
                       fill_df(line,r_ng6a,6,num_replies=num_replies,verbose,test=test))
    #-----------------------------------------------------------------------------------------------------    
    #-----------------------------------------------------------------------------------------------------    
    row.names(pre_final) <- NULL
    pre_final <- pre_final[!duplicated(pre_final),]
    pre_final <- head(pre_final[order(-pre_final$count),],num_replies)
    #-----------------------------------------------------------------------------------------------------    
    return(head(pre_final,num_replies))
}




get_random_str <- function(text_tibble=full1b,min_words=7,verbose=0,dictionary="")
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




post_processing <- function(tr_type)
{
    require(sqldf)
    syntax_group_count <- function(xng,tr_type,tibble_num_line,verbose=0)
    {
        ngram <- length(names(xng))-3
        busq <- gsub(",","",tolower(toString(tr_type[tibble_num_line,(9-ngram+1):9])))
        ret <- round((xng$count[xng$term==busq]/sum(xng$count))*100,2)
        if(length(ret)==0) ret=0
        return(ret)
    }      

    tr_type <- sqldf("select a.ng,a.count,b.count as ng1,a.p5,a.p4,a.p3,a.p2,a.p1,a.p0,a.l5,a.l4,a.l3,a.l2,a.l1,a.word,a.solution from tr_type a, ng1 b where a.word=b.type1")
    tr_type$ng1 <- round(tr_type$ng1/sum(ng1$count)*100,2) # ok
    
    count_aggr_by_word <- sqldf("select word,sum(count) as count from tr_type group by word")
    sum_count_aggr_by_word <- sum(count_aggr_by_word$count)
    
    for (k in 1:nrow(tr_type))
    {
        tr_type$p0[k] <- pos_line(tr_type$word[k])
        tr_type$xng1[k] <- round(xng1$count[xng1$type1==tolower(tr_type$p0[k])]/sum(xng1$count)*100,2)
        if (tr_type$ng[k]==1) tr_type$stx_grp_count[k] <- syntax_group_count(xng1 ,tr_type,k,1)
        if (tr_type$ng[k]==2) tr_type$stx_grp_count[k] <- syntax_group_count(xng2a,tr_type,k,1)
        if (tr_type$ng[k]==3) tr_type$stx_grp_count[k] <- syntax_group_count(xng3a,tr_type,k,1)
        if (tr_type$ng[k]==4) tr_type$stx_grp_count[k] <- syntax_group_count(xng4a,tr_type,k,1)
        if (tr_type$ng[k]==5) tr_type$stx_grp_count[k] <- syntax_group_count(xng5a,tr_type,k,1)
        if (tr_type$ng[k]==6) tr_type$stx_grp_count[k] <- syntax_group_count(xng6a,tr_type,k,1)
        tr_type$aggr_word[k] <- round(100*count_aggr_by_word$count[count_aggr_by_word$word==tr_type$word[k]]/sum_count_aggr_by_word,2)
        if (k %% 1000==0) print(k)
    }
    count_aggr_by_word_pos <- sqldf("select p0,sum(count) as count from tr_type group by p0")
    sum_count_aggr_by_word_pos <- sum(count_aggr_by_word$count)
    for (k in 1:nrow(tr_type))
    {
        tr_type$aggr_pos[k] <- round(100*count_aggr_by_word_pos$count[count_aggr_by_word_pos$p0==tr_type$p0[k]]/sum_count_aggr_by_word_pos,2)
        if (k %% 1000==0) print(k)
    }
    scale_tr_type <- scale(tr_type[,c("count","ng1","xng1","stx_grp_count","aggr_word","aggr_pos")])
    tr_type[,c("count","ng1","xng1","stx_grp_count","aggr_word","aggr_pos")] <- scale_tr_type[,c("count","ng1","xng1","stx_grp_count","aggr_word","aggr_pos")]
    
    return(tr_type)
}
