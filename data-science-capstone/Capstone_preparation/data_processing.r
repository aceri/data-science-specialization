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
        line <- replace_number(line) ; 
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
    
    # We create a corpora out of the three files (blog, news and twitter)
    # later, we clean it with the functions (above) and save them.
    
    full <- rbind(blogs_dat,news_dat,twitter_dat)
    rm(blogs_dat,news_dat,twitter_dat,folder,blogs_str,news_str,twitter_str)
    names(full) <- c("text")
    # full[sample(1:nrow(full),round(nrow(full),0)*0.1),]  -> full1
    # full1b1 <- full1[1:200000,]           %>% clean_corpus(use_dictionary=T,correct=T,verbose = 1,dictionary=GradyAugmented)
    # full1b2 <- full1[300000:400000,]      %>% clean_corpus(use_dictionary=T,correct=T,verbose = 1,dictionary=GradyAugmented)
    # full1b3 <- full1[400000:600000,]      %>% clean_corpus(use_dictionary=T,correct=T,verbose = 1,dictionary=GradyAugmented)
    # full1b4 <- full1[600000:nrow(full1),] %>% clean_corpus(use_dictionary=T,correct=T,verbose = 1,dictionary=GradyAugmented)
    # rm(full)
    
    full[sample(1:nrow(full),round(nrow(full),0)*0.1),] %>% 
        clean_corpus(use_dictionary = T,
                     correct = T, 
                     verbose = 1, 
                     dictionary = GradyAugmented,
                     filename = "full") -> full1

    full <- full1[full1$text!="",]
    rm(full1)
    saveRDS(fullb,"./data/full.rds")
    xfull <- corpus_frame("full",full)    
    text_filter(full)$map_case <- TRUE
    text_filter(full)$remove_ignorable <- TRUE
    text_filter(full)$drop_punct <- TRUE
    text_filter(full)$drop_symbol <- TRUE
    
    
    # We create a far smaller full file than the 1b because there are far 
    #    far fewer part of speech classifications
    
    xfull <- full[sample(1:nrow(full),round(nrow(full)/10,0)),]
    xfull <- xfull[,-2]
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



full1b <- readRDS("./data/full1b.rds")
full1b <- corpus_frame("full1b",full1b)    


tr2 <- data.frame(ng=integer(), count=numeric(),
                 p5=character(),p4=character(),p3=character(),p2=character(),p1=character(),p0=character(),
                 l5=character(),l4=character(),l3=character(),l2=character(),l1=character(), 
                 word=character(),solution=character(),stringsAsFactors = F)


################################################################




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
