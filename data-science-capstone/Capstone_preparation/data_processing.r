rm(list = ls(all.names = TRUE))
gc()


##############################################################
#
#    WHERE IS THIS STORED ?
#
##############################################################

    setwd("E:/dev/data-science-specialization/data-science-capstone/Capstone_preparation")

##############################################################
#
#    READING THE SWIFTKEY FILES
#
##############################################################
    require(tibble)
    
    folder <- "./data/"
    
    blogs_str   <- paste(folder,"en_US.blogs.txt"   ,sep="")
    news_str    <- paste(folder,"en_US.news.txt"    ,sep="")
    twitter_str <- paste(folder,"en_US.twitter.txt" ,sep="")
    
    blogs_dat   <- tibble(scan(blogs_str,"character",sep="\n",skipNul = T,encoding="UTF-8",quiet=T))
    news_dat    <- tibble(scan(news_str,"character",sep="\n",skipNul = T,encoding="UTF-8",quiet=T))
    twitter_dat <- tibble(scan(twitter_str,"character",sep="\n",skipNul = T,encoding="UTF-8",quiet=T))

##############################################################
#
#    BINDING & CLEANING FILES
#
##############################################################
    require(corpus)
    library(qdapDictionaries)
    require(udpipe)
    dl <- "./data/english-ewt-ud-2.4-190531.udpipe"
    udmodel_english <- udpipe_load_model(dl);rm(dl)

    clean_line <- function(line,ret_words=F,use_dictionary=T,dictionary=Grady,correct=T,verbose=0)
    {
        if (verbose>2) print(paste("clean_line > received line : ",line,sep=""))
        if (correct==T) { line <- unlist(strsplit(as.character(line),split=" ")) ; 
        line <- replace_internet_slang(line) ;    
        line <- replace_time(line); 
        line <- replace_kern(line) ; 
        line <- replace_contraction(line) ;  
        line <- replace_hash(line) ; 
        line <- replace_rating(line) ; 
        line <- replace_number(line, remove=T) ; 
        line <- replace_tag(line) ; 
        line <- replace_word_elongation(line) ; 
        line <- replace_ordinal(line) ; 
        line <- replace_incomplete(line) 
        line <- paste(line,collapse=" ")
        
        line <- line %>% tolower() %>% gsub("[[:digit:]]","",.) %>% gsub("(.)\\1{2,}","",.) %>% gsub("[^abcdefghijklmnopqrstuvwxyz' ]+","",.) %>% gsub("^[ ']+|[ ']$+","",.) %>% gsub(" +"," ",.) }
        if (ret_words == T & use_dictionary == T) { line <- unlist(stri_split_regex(line," ")) ; line <- line[line %in% dictionary] }
        if (ret_words == F & use_dictionary == T) { line <- unlist(stri_split_regex(line," ")) ; line <- line[line %in% dictionary] ; line <- paste(line,collapse=" ") }
        return(line)
    }        
    
    clean_corpus <- function(text_tibble, use_dictionary=T,correct=T,verbose=0,dictionary=GradyAugmented,filename)
    {
        # Needs a tibble of the text with each line being in a column txt
        txt <- text_tibble
        num_rows=nrow(txt)
        start <- Sys.time()    
        for (k in 1:num_rows)
        {
            txt[k,1] <- clean_line(txt[k,1],ret_words = F,use_dictionary = use_dictionary,dictionary=dictionary,correct=correct,verbose=verbose)
            if(k %% 25000 == 0) 
            {
                chck <- Sys.time()
                diff <- chck-start
                saveRDS(text_tibble,paste("./data/",filename,"_",k,".rds",sep=""))
                if (verbose>0) print(paste("clean_text > Processed ",round(k/num_rows*100,2)," % in ",round(diff,0)," for ",k," lines of ",num_rows,
                            " - Time Remaining : ",round((num_rows-k)*diff/k,0)," = ",round((num_rows-k)*diff/k/3600,2),". Saving.",sep=""))
            }
        }
        return(txt)
    }   
    
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
            txt[k,1] <- pos_line(line=as.character(txt[k,1]) ,ret_words = F)
            if(verbose >0 & k %% 100 == 0) 
            {
                chck <- Sys.time()
                diff <- chck-start
                print(paste("pos_corpus > Processed ",round(k/num_rows*100,2)," % in ",round(diff,0)," s for ",k," lines of ",num_rows," - Time Remaining : ",round((num_rows-k)*diff/k,0)," m = ",round((num_rows-k)*diff/k/3600,2)," h",sep=""))
            }
        }
        return(txt)
    }    
    
    # We create a corpora out of the three files (blog, news and twitter)
    # later, we clean it with the functions (above) and save them.
    
    full <- rbind(blogs_dat,news_dat,twitter_dat)
    rm(blogs_dat,news_dat,twitter_dat,folder,blogs_str,news_str,twitter_str)
    names(full) <- c("text")

    full[sample(1:nrow(full),round(nrow(full),0)*0.1),] %>% 
        clean_corpus(use_dictionary = T,
                     correct = T, 
                     verbose = 1, 
                     dictionary = GradyAugmented,
                     filename = "full") -> full1
    
    full1 <- rbind()    

    full <- full1[full1$text!="",]
    rm(full1)
    saveRDS(full,"./data/full.rds")
    full <- corpus_frame("full",full)    
    text_filter(full)$map_case <- TRUE
    text_filter(full)$remove_ignorable <- TRUE
    text_filter(full)$drop_punct <- TRUE
    text_filter(full)$drop_symbol <- TRUE
    
    
    # We create a far smaller full file than the 1b because there are far 
    #    far fewer part of speech classifications
    
    xfull <- full[sample(1:nrow(full),round(nrow(full)/10,0)),]
    xfull <- xfull[c("text")]
    xfull <- pos_corpus(xfull,verbose=1)
    saveRDS(xfull,"./data/xfull.rds")    
    xfull <- corpus_frame("xfull",xfull)
    text_filter(xfull)$map_case <- TRUE
    text_filter(xfull)$remove_ignorable <- TRUE
    text_filter(xfull)$drop_punct <- TRUE
    text_filter(xfull)$drop_symbol <- TRUE
    

####################################################################
#
#    PREPARING THE WORD N-GRAM FILES
#
####################################################################    

ng1 <- term_stats(full,types=TRUE)
ng1<-ng1[ng1$support > 1 & ng1$count > 1,]
saveRDS(ng1,"./data/ng1.rds")
#-------------------------------------------------------------------
#-------------------------------------------------------------------
ng2a <- term_stats(full,types=TRUE,ngrams=2)
ng2a <- ng2a[ng2a$support>1 & ng2a$count>1,]
saveRDS(ng2a,"./data/ng2a.rds")

ng3a <- term_stats(full,types=TRUE,ngrams=3)
ng3a <- ng3a[ng3a$support>1 & ng3a$count>1,]
saveRDS(ng3a,"./data/ng3a.rds")

ng4a <- term_stats(full,types=TRUE,ngrams=4)
ng4a <- ng4a[ng4a$support>1 & ng4a$count>1,]
saveRDS(ng4a,"./data/ng4a.rds")

ng5a <- term_stats(full,types=TRUE,ngrams=5)
ng5a <- ng5a[ng5a$support>1 & ng5a$count>1,]
saveRDS(ng5a,"./data/ng5a.rds")

ng6a <- term_stats(full,types=TRUE,ngrams=6)
ng6a <- ng6a[ng6a$support>1 & ng6a$count>1,]
saveRDS(ng6a,"./data/ng6a.rds")

####################################################################
#
#    PREPARING THE PART OF SPEECH N-GRAM FILES
#
####################################################################    

xng1 <- term_stats(xfull,types=TRUE)
xng1<-xng1[xng1$support > 1 & xng1$count > 1,]
saveRDS(xng1,"./data/xng1.rds")
#-------------------------------------------------------------------
#-------------------------------------------------------------------
xng2a <- term_stats(xfull,types=TRUE,ngrams=2)
xng2a <- xng2a[xng2a$support>1 & xng2a$count>1,]
saveRDS(xng2a,"./data/xng2a.rds")

xng3a <- term_stats(xfull,types=TRUE,ngrams=3)
xng3a <- xng3a[xng3a$support>1 & xng3a$count>1,]
saveRDS(xng3a,"./data/xng3a.rds")

xng4a <- term_stats(xfull,types=TRUE,ngrams=4)
xng4a <- xng4a[xng4a$support>1 & xng4a$count>1,]
saveRDS(xng4a,"./data/xng4a.rds")

xng5a <- term_stats(xfull,types=TRUE,ngrams=5)
xng5a <- xng5a[xng5a$support>1 & xng5a$count>1,]
saveRDS(xng5a,"./data/xng5a.rds")

xng6a <- term_stats(xfull,types=TRUE,ngrams=6)
xng6a <- xng6a[xng6a$support>1 & xng6a$count>1,]
saveRDS(xng6a,"./data/xng6a.rds")



################################################################
#
#        IN CASE WE NEED TO READ THEM LATER
#
################################################################

# full <- readRDS("./data/full.rds")

# ng1  <- readRDS("./data/ng1.rds")
# ng2a <- readRDS("./data/ng2a.rds")
# ng3a <- readRDS("./data/ng3a.rds")
# ng4a <- readRDS("./data/ng4a.rds")
# ng5a <- readRDS("./data/ng5a.rds")
# ng6a <- readRDS("./data/ng6a.rds")
# xng1  <- readRDS("./data/xng1.rds")
# xng2a <- readRDS("./data/xng2a.rds")
# xng3a <- readRDS("./data/xng3a.rds")
# xng4a <- readRDS("./data/xng4a.rds")
# xng5a <- readRDS("./data/xng5a.rds")
# xng6a <- readRDS("./data/xng6a.rds")



################################################################
#
# PREPARING A FUNCTION TO GET RANDOM SAMPLES FROM THE CORPORA
#    (we will use this to get senteces for training matrix)
#
################################################################


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

# Testing a few times

for (a in 1:10) {print(get_random_str(full,sample(1:10,1),verbose=0,dictionary = GradyAugmented))}


################################################################
#
# PREPARING A FUNCTION TO GET RANDOM SAMPLES FROM THE CORPORA
#    (we will use this to get senteces for training matrix)
#
################################################################

pred2 <- function(line,verbose=0, test=F,max_lines=10,ret_lines=5)
{
    
    fill_ng <- function(eval,ng,startj,max_lines)
    {
        num_lines <- nrow2(ng)
        if (num_lines > 0)
        {
            sumcount <- sum(ng$count)
            num_ng <- dim(ng)[2]-1 
            for (j in (startj+1):(startj + min(max_lines,num_lines)))
            {
                this_count <- ng$count[j-startj]
                this_count_pct <- round(this_count/sumcount*100,2)
                eval[j,1] = ng[j-startj,dim(ng)[2]-1]  
                eval[j,2] = num_ng
                for (y in 3:7)  eval[j,y] = ifelse(num_ng==y-1,this_count,0)
                for (z in 8:12) eval[j,z] = ifelse(num_ng==z-6,this_count_pct,0)
            }
        }
        return(eval)
    }
    
    eval=""
    if (line!="")
    {
        original_line <- line
        if(test==T) line=paste(line,"UNKNOWN",sep=" ") 
        line <- clean_line(line,ret_words=F,verbose=verbose)
        words <- clean_line(line,ret_words=T,verbose=verbose)
        if (verbose>1) print(paste("pred2 > Line : ",toString(line),sep=""))
        pos_words <- pos_line(line,T)
        num_pos_words <- length(pos_words)
        num_words <- length(words)
        solution <- words[num_words]
        if (verbose>1) print(paste("pred2 > ", paste(toString(words[1:(length(words)-1)]),collapse=" ")," [ ",solution," ]",sep=""))
        #-----------------------------------------------------------------------------------------------------
        # PROPOSING WORDS
        #-----------------------------------------------------------------------------------------------------
        r_ng2a <- NA; r_ng3a <- NA; r_ng4a <- NA; r_ng5a <- NA; r_ng6a <- NA
        if(num_words>1) r_ng2a <- ng2a[ng2a$type1==words[num_words-1] ,]    
        if(num_words>2) r_ng3a <- ng3a[ng3a$type1==words[num_words-2] & ng3a$type2==words[num_words-1] ,]
        if(num_words>3) r_ng4a <- ng4a[ng4a$type1==words[num_words-3] & ng4a$type2==words[num_words-2] & ng4a$type3==words[num_words-1] ,]
        if(num_words>4) r_ng5a <- ng5a[ng5a$type1==words[num_words-4] & ng5a$type2==words[num_words-3] & ng5a$type3==words[num_words-2] & ng5a$type4==words[num_words-1] ,]
        if(num_words>5) r_ng6a <- ng6a[ng6a$type1==words[num_words-5] & ng6a$type2==words[num_words-4] & ng6a$type3==words[num_words-3] & ng6a$type4==words[num_words-2] & ng6a$type5==words[num_words-1] ,]            
        
        num_proposed_words = min(max_lines,nrow2(r_ng2a)) + 
            min(max_lines,nrow2(r_ng3a)) +
            min(max_lines,nrow2(r_ng4a)) +
            min(max_lines,nrow2(r_ng5a)) +
            min(max_lines,nrow2(r_ng6a))
        
        eval <- data.frame(word=character(),ng=integer(),w1=numeric(),w2=numeric(),w3=numeric(),
                           w4=numeric(),w5=numeric(),   pw1=numeric(),pw2=numeric(),pw3=numeric(),
                           pw4=numeric(),pw5=numeric(), x1=numeric(),x2=numeric(),x3=numeric(),
                           x4=numeric(),x5=numeric(),  px1=numeric(),px2=numeric(),px3=numeric(),
                           px4=numeric(),px5=numeric(), rep_word=integer(),rep_pos=integer(),solution=character(),points=numeric(),
                           stringsAsFactors = F)
        eval <- fill_ng(eval, ng = r_ng2a, startj = 0, max_lines=max_lines)
        eval <- fill_ng(eval, ng = r_ng3a, startj = min(max_lines,nrow2(r_ng2a)) , max_lines=max_lines)
        eval <- fill_ng(eval, ng = r_ng4a, startj = min(max_lines,nrow2(r_ng2a)) + 
                            min(max_lines,nrow2(r_ng3a)) , max_lines=max_lines)
        eval <- fill_ng(eval, ng = r_ng5a, startj = min(max_lines,nrow2(r_ng2a)) + 
                            min(max_lines,nrow2(r_ng3a)) + 
                            min(max_lines,nrow2(r_ng4a)) , max_lines=max_lines)
        eval <- fill_ng(eval, ng = r_ng6a, startj = min(max_lines,nrow2(r_ng2a)) + 
                            min(max_lines,nrow2(r_ng3a)) +
                            min(max_lines,nrow2(r_ng4a)) + 
                            min(max_lines,nrow2(r_ng5a)) , max_lines=max_lines)
        if (num_proposed_words>0)
        {
            for (k in 1:num_proposed_words)
            {
                pos_sol <- tolower(pos_line(eval[k,1]))
                
                v1 = xng1$count[  xng1$type1 == pos_sol]
                v2 = ifelse(num_pos_words>1,xng2a$count[xng2a$type1 == pos_words[num_pos_words-1] & 
                                                            xng2a$type2 == pos_sol ],0)
                v3 = ifelse(num_pos_words>2,xng3a$count[xng3a$type1 == pos_words[num_pos_words-2] & 
                                                            xng3a$type2 == pos_words[num_pos_words-1] & 
                                                            xng3a$type3 == pos_sol ],0)
                v4 = ifelse(num_pos_words>3,xng4a$count[xng4a$type1 == pos_words[num_pos_words-3] & 
                                                            xng4a$type2 == pos_words[num_pos_words-2] & 
                                                            xng4a$type3 == pos_words[num_pos_words-1] & 
                                                            xng4a$type4 == pos_sol ],0)
                v5 = ifelse(num_pos_words>4,xng5a$count[xng5a$type1 == pos_words[num_pos_words-4] & 
                                                            xng5a$type2 == pos_words[num_pos_words-3] & 
                                                            xng5a$type3 == pos_words[num_pos_words-2] & 
                                                            xng5a$type4 == pos_words[num_pos_words-1] & 
                                                            xng5a$type5 == pos_sol ],0)
                v6 = ifelse(pos_sol == pos_words[num_pos_words],-100,0)
                
                eval[k,13]  = ifelse(nrow3(v1)==0,0,v1)
                eval[k,14]  = ifelse(nrow3(v2)==0,0,v2)
                eval[k,15]  = ifelse(nrow3(v3)==0,0,v3)
                eval[k,16]  = ifelse(nrow3(v4)==0,0,v4)
                eval[k,17]  = ifelse(nrow3(v5)==0,0,v5)
                #-------------------------------------
                # percentages
                eval[k,18]  = round(eval[k,13]/sum(xng1$count)*100,2)
                eval[k,19]  = round(eval[k,14]/sum(xng2a$count)*100,2)
                eval[k,20]  = round(eval[k,15]/sum(xng3a$count)*100,2)
                eval[k,21]  = round(eval[k,16]/sum(xng4a$count)*100,2)
                eval[k,22]  = round(eval[k,17]/sum(xng5a$count)*100,2)
                #-------------------------------------
                # repeated word & repeated pos
                eval[k,23]  = ifelse(eval$word[k] == words[num_words-1],1,0)
                eval[k,24]  = ifelse(pos_sol == pos_words[num_pos_words-1],1,0)
                #-------------------------------------                
                #-------------------------------------                
                eval[k,25] = solution
                eval[k,26] = 100*eval[k,8]+43.5*eval[k,3]+25.6*eval[k,14]+22.0*eval[k,15]+17.1*eval[k,13]+15.4*eval[k,16]+13.9*eval[k,17]+
                    11.2*eval[k,18]+8.7*eval[k,19]+3.92*eval[k,20]+3.84*eval[k,21]+1.95*eval[k,22]+1.66*eval[k,24]
            }
            eval <- sqldf("select word,ng,w1,w2,w3,w4,w5,pw1,pw2,pw3,pw4,pw5,x1,x2,x3,x4,x5,px1,px2,px3,px4,px5,rep_word,rep_pos,solution,sum(points) as points from eval group by word order by points desc")
            eval <- head(eval,ret_lines)
        }
    }
    return(eval)
}

nrow2 <- function(ng)
{
    if (is.null(dim(ng))) ret = 0 else ret = nrow(ng)
    return(ret)
}

nrow3 <- function(ng)
{
    if (is.null(ng) | is.na(ng)) ret = 0 else ret = max(nrow(ng),length(ng))
    return(ret)
}



pred2(line=get_random_str(full,sample(1:10,1),verbose=3,dictionary=Grady),
      verbose=3,
      test=F,
      ret_lines = 5)

# build_training_set <- function(tr2,iter=500,verbose=0,test=F,save_every=100,dictionary="")
# {
#     require(qdapDictionaries)
#     tmp <- data.frame(ng=integer(), count=numeric(),
#                       p5=character(),p4=character(),p3=character(),p2=character(),p1=character(),p0=character(),
#                       l5=character(),l4=character(),l3=character(),l2=character(),l1=character(), 
#                       word=character(),solution=character(),stringsAsFactors = F)
#     tmp <- bind_rows(lapply(1:iter, function(x) predict_word(line = get_random_str(full1b,7,verbose=verbose,dictionary=dictionary), test = F, num_replies = 100,verbose=verbose) ))
#     tmp[,c("term","type1","type2","support")]<-NULL
#     if (verbose>0 & names(tr2)!=names(as.data.frame(tmp)))
#     {
#         print("*********************************************************")
#         print("SON DIFERENTES")        
#         print(paste(toString(names(tr2))))
#         print(paste(toString(names(as.data.frame(tmp)))))
#         print("*********************************************************")
#     }
#     if(verbose>2) print(paste("build_trining_set > binding new rows : ",nrow(as.data.frame(tmp)),sep=""))
#     tr2 <- rbind(tr2,as.data.frame(tmp))
#     tr2 <- tr2[!duplicated(tr2),]
#     Fry_1000b <- c(Fry_1000,"")
#     tr2 <- tr2[tr2$word %in% Fry_1000b & 
#                tr2$solution %in% Fry_1000b & 
#                tr2$l1 %in% Fry_1000b  & 
#                tr2$l2 %in% Fry_1000b  & 
#                tr2$l3 %in% Fry_1000b  & 
#                tr2$l4 %in% Fry_1000b  & 
#                tr2$l5 %in% Fry_1000b ,]
#     tr2<-tr2[!tr2$l1==tr2$solution,]
#     row.names(tr2) <- NULL
#     if (verbose>0) print(paste("build_training_set > Saving ...",sep=""));saveRDS(tr2,"./data/tr2.rds")
#     return(tr2)
# }
# 
# for (k in 1:1) { print(k);tr2 <- build_training_set(tr2 , iter = 200 , verbose = 1 , test=F,save_every = 100, dictionary=GradyAugmented) }
# #tr2 <- readRDS("./data/tr2.rds")
# #saveRDS(tr2,"./data/tr2.rds")
# 
# tr2 <- tr2[tr2$count>8,]  
# 
# tr2[tr2$ng==4,c("p5","l5")]<-""
# tr2[tr2$ng==3,c("p5","l5","p4","l4")]<-""
# tr2[tr2$ng==2,c("p5","l5","p4","l4","p3","l3")]<-""
# tr2[tr2$ng==1,c("p5","l5","p4","l4","p3","l3","p2","l2")]<-""
# 
# 
# tr3 <- post_processing(tr2)
# saveRDS(tr3,"./data/tr3.rds")
# #tr3<-readRDS("./data/tr3.rds")
# 
# 
# 
# print(paste("tr4 is ",toString(round(object.size(tr4)/1024/1024,2))," in Mb.",sep=""))
# 
# ########################################################################################
# 
# # Creamos training y testing
# 
# require(caret)
# 
# inTrain <- createDataPartition(y=tr4$solution,p=0.1,list=FALSE)
# training <- tr4[ inTrain,]
# testing  <- tr4[-inTrain,]
# dim(training);dim(testing)
# 
# require(parallel)
# detectCores()
# cl <- makeCluster(4)
# 
# #modFit <- machine_learn(training,outcome=solution,allow_parallel = TRUE,tune=F)
# #modFit <- machine_learn(training,outcome=solution,allow_parallel = TRUE,models="rf",tune=T)
# #modFit <- machine_learn(training,outcome=solution,allow_parallel = TRUE)
# #save_models(modFit, file = "./data/modFit.rds",sanitize_phi=T)
# #saveRDS(modFit,"./data/modFit.rds")   # This is based on hit with weight based on the weight of how 
# #modFit.rds <- readRDS("./data/modFit.rds")
# 
# 
# ######################################################
# # 
# # library(randomForest)
# # cluster <- makeCluster(detectCores() - 4) # convention to leave 1 core for OS
# # registerDoParallel(cluster)
# # 
# # fitControl <- trainControl(
# #                            #number = 5,
# #                            allowParallel = T,
# #                            verboseIter = T)
# # rf<-train(solution~.,data=training,trControl=fitControl,method="rf",metric="Accuracy",maximize=T,na.action="na.omit")
# # 
# # nnet<-train(solution~.,data=training,trControl=fitControl,method="nnet",preProcess="range")
# 
# # modFit
# # predict(modFit,testing)
# # get_variable_importance(modFit)
# print(paste("pct aciertos = ",sum(tr4$word==tr4$solution & tr4$count >=90 & tr4$count <100)/sum(tr4$word==tr4$solution)  ," ; num_rows = ",nrow(tr4[tr4$count >=90 & tr4$count <100,]),sep=""))     
# print(paste("pct aciertos = ",sum(tr4$word==tr4$solution & tr4$count >=80 & tr4$count <90)/sum(tr4$word==tr4$solution)  ," ; num_rows = ",nrow(tr4[tr4$count >=80 & tr4$count <90,]),sep=""))     
# print(paste("pct aciertos = ",sum(tr4$word==tr4$solution & tr4$count >=70 & tr4$count <80)/sum(tr4$word==tr4$solution)  ," ; num_rows = ",nrow(tr4[tr4$count >=70 & tr4$count <80,]),sep=""))     
# print(paste("pct aciertos = ",sum(tr4$word==tr4$solution & tr4$count >=60 & tr4$count <70)/sum(tr4$word==tr4$solution)  ," ; num_rows = ",nrow(tr4[tr4$count >=60 & tr4$count <70,]),sep=""))     
# print(paste("pct aciertos = ",sum(tr4$word==tr4$solution & tr4$count >=50 & tr4$count <60)/sum(tr4$word==tr4$solution)  ," ; num_rows = ",nrow(tr4[tr4$count >=50 & tr4$count <60,]),sep=""))     
# print(paste("pct aciertos = ",sum(tr4$word==tr4$solution & tr4$count >=40 & tr4$count <50)/sum(tr4$word==tr4$solution)  ," ; num_rows = ",nrow(tr4[tr4$count >=40 & tr4$count <50,]),sep=""))     
# print(paste("pct aciertos = ",sum(tr4$word==tr4$solution & tr4$count >=30 & tr4$count <40)/sum(tr4$word==tr4$solution)  ," ; num_rows = ",nrow(tr4[tr4$count >=30 & tr4$count <40,]),sep=""))     
# print(paste("pct aciertos = ",sum(tr4$word==tr4$solution & tr4$count >=20 & tr4$count <30)/sum(tr4$word==tr4$solution)  ," ; num_rows = ",nrow(tr4[tr4$count >=20 & tr4$count <30,]),sep=""))     
# print(paste("pct aciertos = ",sum(tr4$word==tr4$solution & tr4$count >=15 & tr4$count <20)/sum(tr4$word==tr4$solution)  ," ; num_rows = ",nrow(tr4[tr4$count >=15 & tr4$count <20,]),sep=""))     
# print(paste("pct aciertos = ",sum(tr4$word==tr4$solution & tr4$count >=10 & tr4$count <15)/sum(tr4$word==tr4$solution)  ," ; num_rows = ",nrow(tr4[tr4$count >=10 & tr4$count <15,]),sep=""))     
# print(paste("pct aciertos = ",sum(tr4$word==tr4$solution & tr4$count >= 0 & tr4$count <10)/sum(tr4$word==tr4$solution)  ," ; num_rows = ",nrow(tr4[tr4$count >= 0 & tr4$count <10,]),sep=""))   
# 
# 
# 
# require(healthcareai)
# tr4 <-tr3
# # tr4 <-readRDS("./data/tr3.rds")
# tr4 <- tr4[tr4$count>10,]
# #tr4 <- tr4[tr4$count>20,]
# 
# require(caret)
# inTrain <- createDataPartition(y=tr4$solution,p=0.9,list=FALSE)
# training <- tr4[ inTrain,]
# testing  <- tr4[-inTrain,]
# fitControl <- trainControl(
#     #number = 5,
#     allowParallel = T,
#     verboseIter = T)
# dim(training);dim(testing)
# require(parallel)
# detectCores()
# cl <- makeCluster(4)
# #library(randomForest)
# # training_all_but_solution <- training[!names(training)!="solution",]
# # unlist(names(training_all_but_solution))
# #rf <- train(training$solution,names(training_all_but_solution),trControl=fitControl,method="rf")
# # rf <- train(names(training[names(training)=="solution",],names(training_all_but_solution),trControl=fitControl,method="rf")
# # f <- train(solution~.,data=training,trControl=fitControl,method="rf",verbose=T)
# 
# # modFit <- machine_learn(training,outcome=solution,allow_parallel = TRUE,metric="ROC",models="RF",tune=T)
# # save_models(modFit, file = "./data/modFit_mtry10_rf_ROC.rds")
# # modFit <- machine_learn(training,outcome=solution,allow_parallel = TRUE,metric="PR",models="RF",tune=T)
# # save_models(modFit, file = "./data/modFit_mtry10_rf_PR.rds")
# # modFit <- machine_learn(training,outcome=solution,allow_parallel = TRUE,metric="Kappa",models="RF",tune=T)
# # save_models(modFit, file = "./data/modFit_mtry10_rf_Kappa.rds")
# # modFit <- machine_learn(training,outcome=solution,allow_parallel = TRUE,metric="ROC",models="glm",tune=T)
# # save_models(modFit, file = "./data/modFit_mtry10_glm_ROC.rds")
# # modFit <- machine_learn(training,outcome=solution,allow_parallel = TRUE,metric="PR",models="glm",tune=T)
# # save_models(modFit, file = "./data/modFit_mtry10_glm_PR.rds")
# # modFit <- machine_learn(training,outcome=solution,allow_parallel = TRUE,metric="Kappa",models="glm",tune=T)
# # save_models(modFit, file = "./data/modFit_mtry10_glm_Kappa.rds")
# # modFit <- machine_learn(training,outcome=solution,allow_parallel = TRUE,metric="ROC",models="RF",tune=T,tune_depth=5)
# # save_models(modFit, file = "./data/modFit_mtry5_rf_ROC.rds")
# # modFit <- machine_learn(training,outcome=solution,allow_parallel = TRUE,metric="PR",models="RF",tune=T,tune_depth=5)
# # save_models(modFit, file = "./data/modFit_mtry5_rf_PR.rds")
# # modFit <- machine_learn(training,outcome=solution,allow_parallel = TRUE,metric="Kappa",models="RF",tune=T,tune_depth=5)
# # save_models(modFit, file = "./data/modFit_mtry5_rf_Kappa.rds")
# # modFit <- machine_learn(training,outcome=solution,allow_parallel = TRUE,metric="ROC",models="glm",tune=T,tune_depth=5)
# # save_models(modFit, file = "./data/modFit_mtry5_glm_ROC.rds")
# # modFit <- machine_learn(training,outcome=solution,allow_parallel = TRUE,metric="PR",models="glm",tune=T,tune_depth=5)
# # save_models(modFit, file = "./data/modFit_mtry5_glm_PR.rds")
# # modFit <- machine_learn(training,outcome=solution,allow_parallel = TRUE,metric="Kappa",models="glm",tune=T,tune_depth=5)
# # # save_models(modFit, file = "./data/modFit_mtry5_glm_Kappa.rds")
# # modFit <- machine_learn(training,outcome=solution,allow_parallel = TRUE,tune=F)
# # save_models(modFit, file = "./data/modFit_TuneFalse_all.rds")
# # modFit <- machine_learn(training,outcome=solution,allow_parallel = TRUE,tune=T,tune_depth=5)
# # save_models(modFit, file = "./data/modFit_mtry5_all.rds")
# # modFit <- readRDS("./data/modFit_mtry5_rf_ROC.rds")
# 
# modFit <- machine_learn(training,outcome=solution,allow_parallel = TRUE,tune=T,tune_depth=3)
# save_models(modFit, file = "./data/modFit_mtry_all3.rds")
# modFit <- machine_learn(training,outcome=solution,allow_parallel = TRUE,models="RF",tune=T,tune_depth=4)
# save_models(modFit, file = "./data/modFit_mtry4_rf.rds")
# modFit <- machine_learn(training,outcome=solution,allow_parallel = TRUE,models="RF",tune=T,tune_depth=5)
# save_models(modFit, file = "./data/modFit_mtry5_rf.rds")
# modFit <- machine_learn(training,outcome=solution,allow_parallel = TRUE,models="RF",tune=T,tune_depth=6)
# save_models(modFit, file = "./data/modFit_mtry6_rf.rds")
# modFit <- machine_learn(training,outcome=solution,allow_parallel = TRUE,models="RF",tune=T,tune_depth=7)
# save_models(modFit, file = "./data/modFit_mtry7_rf.rds")
# modFit <- machine_learn(training,outcome=solution,allow_parallel = TRUE,models="RF",tune=T,tune_depth=8)
# save_models(modFit, file = "./data/modFit_mtry8_rf.rds")
# modFit <- machine_learn(training,outcome=solution,allow_parallel = TRUE,models="RF",tune=T,tune_depth=9)
# save_models(modFit, file = "./data/modFit_mtry9_rf.rds")
# modFit <- machine_learn(training,outcome=solution,allow_parallel = TRUE,models="RF",tune=T,tune_depth=10)
# save_models(modFit, file = "./data/modFit_mtry10_rf.rds")
# 
# 
# 
# training <- make_na(training,c(""))
# testing <- make_na(testing,c(""))
# prep_training <- prep_data(training, outcome = solution)
# 
# untuned2_rf <- flash_models(d = prep_training,
#                            outcome = solution,
#                            models = "RF",
#                            metric = "Accuracy",
#                            allow_parallel = T)
# save_models(untuned2_rf, file = "./data/untuned2_rf_0.47.rds")
# modFit <- readRDS("./data/untuned_rf_0.47.rds")
# 
# predict()
