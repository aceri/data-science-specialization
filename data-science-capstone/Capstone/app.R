#######################################################################
#######################################################################
#######################################################################
# MEMORY !!!!!

#require(lobstr)  

# mem_used()
# object_size

#library(profmem)
#library(data.table)

# .ls.objects <- function (pos = 1, pattern, order.by,
#                          decreasing=FALSE, head=FALSE, n=5) {
#     napply <- function(names, fn) sapply(names, function(x)
#         fn(get(x, pos = pos)))
#     names <- ls(pos = pos, pattern = pattern)
#     obj.class <- napply(names, function(x) as.character(class(x))[1])
#     obj.mode <- napply(names, mode)
#     obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
#     obj.prettysize <- napply(names, function(x) {
#         format(utils::object.size(x), units = "auto") })
#     obj.size <- napply(names, object.size)
#     obj.dim <- t(napply(names, function(x)
#         as.numeric(dim(x))[1:2]))
#     vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
#     obj.dim[vec, 1] <- napply(names, length)[vec]
#     out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
#     names(out) <- c("Type", "Size", "PrettySize", "Length/Rows", "Columns")
#     if (!missing(order.by))
#         out <- out[order(out[[order.by]], decreasing=decreasing), ]
#     if (head)
#         out <- head(out, n)
#     out
# }
# 
# # shorthand
# lsos <- function(..., n=10) {
#     .ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
# }
# 
# lsos()


# for (i in 1:10) gc(reset = T)
# 
# gdata::ll(unit='MB',sort=T,digits=2)

#######################################################################
#######################################################################
#######################################################################
#######################################################################



require(shiny)
require(shinythemes)
require(textclean)
require(sqldf)
require(dplyr)
require(cowsay)
require(stringi)
require(pryr)

load(file="./data/all_ngs.RData")
require(udpipe)
dl <- "./data/english-ewt-ud-2.4-190531.udpipe"
udmodel_english <- udpipe_load_model(dl);rm(dl)


pos_line <- function(line,ret_words = F)
{
    ret = tolower(as.data.frame(udpipe_annotate(udmodel_english,x=line,doc_id=seq_along(line)))$upos)
    if (ret_words == F ) ret <- paste(ret,collapse=" ")
    return(ret)
}

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

nrow2 <- function(ng)
{
    if (is.null(dim(ng))) ret = 0 else ret = nrow(ng)
    return(ret)
}


pred2 <- function(line,verbose=0, test=F,max_lines=100,ret_lines=5)
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
                    
                eval[k,13]  = v1
                eval[k,14]  = v2
                eval[k,15]  = v3
                eval[k,16]  = v4
                eval[k,17]  = v5
                #-------------------------------------
                # percentages
                eval[k,18]  = round(v1/sum(xng1$count)*100,2)
                eval[k,19]  = round(v2/sum(xng2a$count)*100,2)
                eval[k,20]  = round(v3/sum(xng3a$count)*100,2)
                eval[k,21]  = round(v4/sum(xng4a$count)*100,2)
                eval[k,22]  = round(v5/sum(xng5a$count)*100,2)
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


pred2_type <- function(eval,ret_type,line,more_results="")
{
    ret = ""
    if (more_results=="" | more_results=="See only one") more_results = F else more_results = T
    ret_lines <- nrow2(eval)
    if (ret_lines == 0) ret = "Type something !!"
    else {
        eval <- as.vector(eval$word)[1:(min(ret_lines,length(eval)))]
        if(length(eval)>1) for (k in 2:length(eval)) ret = paste(ret,line, " [ ", eval[k] , " ] "," \n",sep="")
        ret <- paste("\nPREDICTED:\n",line," [ ",eval[1]," ] ",ifelse(more_results==T,paste(" \n--------------\nALTERNATIVES:\n",ret,sep=""),"\n"),sep="")
    } 
    
    return(ret)    
}

tabLines <- function(texto,num_spaces=5)
{
    lines <-unlist(strsplit(texto,split="\n"))
    for (k in 1:length(lines)) lines[k]<-paste(paste(rep(" ",num_spaces),collapse=""),lines[k],'\n',sep="")
    return(lines)
}

memory <- function()
{
    ret <- ""
    if (sample(1:10,1)==1) {print( paste("Memory used > ",round(mem_used()/1024/1024,2)," Mb.",sep=""));gc()}
}


    ui <- fluidPage(
        theme=shinytheme("darkly"),
        tags$head(tags$style('h5 {color:orange;}')),
        tags$style(type="text/css","#response {background-color:black;color:white}"),
        titlePanel("Capstone: SwiftKey - Predicting Next Word"),
        h5("David Pellon. June 10th 2020"),  
        hr(),
        div(style="width=800px",
                textInput("ti","Hi !, write some text here and i will try to predict next word ...",value="",placeholder = "Enter some text here",width='100%'),
                br(),
                radioButtons("more_results","How many results you want to see ? ",c("See only one","See alternatives too"),inline=T,selected="See only one"),
                verbatimTextOutput("response"),
                radioButtons("animals","",c("cat","owl","clippy","pig","trilobite"),inline=T),
                hr()
        )
    )

server <- function(input, output) {
    
    common_data     <- reactive({pred2(line=as.character(input$ti),verbose=0, test=T,max_lines=5,ret_lines=5)})
    memory_txt      <- reactive({memory()})
    data1           <- reactive({tabLines(say(pred2_type(common_data(),ret_type=1,line=as.character(input$ti),more_results=input$more_results),by=input$animals,type="string"),15) })    
    output$response <- renderText({data1()})
}

shinyApp(ui = ui, server = server)



