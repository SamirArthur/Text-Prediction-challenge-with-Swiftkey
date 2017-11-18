#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

options(shiny.maxRequestSize=60*1024^2)

library(shiny)
library(wordVectors)
library(magrittr)
library(tm)

load("nbi2.Rdata")
load("ntri2.Rdata")
load("nqi.Rdata")
load("nci.Rdata")

##head(nbi2)
##head(ntri2)
##head(nqi)
##head(nci)

load("model.Rdata")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$results <- renderTable({
    
    ##################################################################################
    
    model_4_lasts <- function(a,b,c,d){
      select <- (nci[which(nci$word1==a & nci$word2==b & nci$word3==c & nci$word4==d),])
      return(select[,c(1,6)])
    }
    
    model_3_lasts <- function(a,b,c){
      select <-(nqi[which(nqi$word1==a & nqi$word2==b & nqi$word3==c),])
      return(select[,c(1,5)])
    }
    
    model_2_lasts <- function(a,b){
      select <- (ntri2[which(ntri2$word1==a & ntri2$word2==b),])
      return(select[,c(1,4)])
    }
    
    model_1_lasts <- function(a){
      select <- (nbi2[which(nbi2$word1==a),])
      return(select[,c(1,3)])
    }
    
    ##################################################################################
    
    
    model_ng <- function(text,nn) {
      
      ##text <- tolower(text) ## all to lower case

      l <- length(strsplit(text," ")[[1]])
      
      if (l>=4) {
        
        a <- strsplit(text," ")[[1]][l-3]
        b <- strsplit(text," ")[[1]][l-2]
        c <- strsplit(text," ")[[1]][l-1]
        d <- strsplit(text," ")[[1]][l]
        
        ## xci
        result4 <- model_4_lasts(a,b,c,d)
        names(result4) <- c("counts","word")
        ##result4$counts <- 20*(result4$counts/mean(result4$counts))
        result4$counts <- 100*(result4$counts/mean(result4$counts))
        
        ## xqi
        result3 <- model_3_lasts(b,c,d)
        names(result3) <- c("counts","word")
        ##result3$counts <- 10*(result3$counts/mean(result3$counts))
        result3$counts <- 30*(result3$counts/mean(result3$counts))
        
        ## xtri2
        result2 <- model_2_lasts(c,d)
        names(result2) <- c("counts","word")
        ##result2$counts <- 5*(result2$counts/mean(result2$counts))
        result2$counts <- 10*(result2$counts/mean(result2$counts))
        
        result1 <- model_1_lasts(d)
        names(result1) <- c("counts","word")
        result1$counts <- result1$counts/mean(result1$counts)
        
        result <- rbind(result1,result2,result3,result4)
        
          if (sum(is.na(result))<length(result$counts))
            {
          result <- aggregate(counts ~ word, data=result,sum,na.rm=TRUE)
          result <- result[order(result$counts,decreasing = TRUE),]
          return(result[1:nn,1])
            } else  {
                    result <- result[order(result$counts,decreasing = TRUE),]
                    return(result[1:nn,2])
                    }
        
      }
      
      else if (l>=3) {
        
        b <- strsplit(text," ")[[1]][l-2]
        c <- strsplit(text," ")[[1]][l-1]
        d <- strsplit(text," ")[[1]][l]
        

        ## xqi
        result3 <- model_3_lasts(b,c,d)
        names(result3) <- c("counts","word")
        result3$counts <- 10*(result3$counts/mean(result3$counts))
        
        ## xtri2
        result2 <- model_2_lasts(c,d)
        names(result2) <- c("counts","word")
        result2$counts <- 5*(result2$counts/mean(result2$counts))
        
        result1 <- model_1_lasts(d)
        names(result1) <- c("counts","word")
        result1$counts <- result1$counts/mean(result1$counts)
        
        result <- rbind(result1,result2,result3)
        
        ##if (sum(is.na(result))<length(result$counts)){result <- aggregate(counts ~ word, data=result,sum,na.rm=TRUE)}
        ##result <- result[order(result$counts,decreasing = TRUE),]
        ##return(result[1:nn,1])
        
        if (sum(is.na(result))<length(result$counts))
        {
          result <- aggregate(counts ~ word, data=result,sum,na.rm=TRUE)
          result <- result[order(result$counts,decreasing = TRUE),]
          return(result[1:nn,1])
        } else  {
          result <- result[order(result$counts,decreasing = TRUE),]
          return(result[1:nn,2])
        }
        
      }
      
      else if (l>=2) {
        
        c <- strsplit(text," ")[[1]][l-1]
        d <- strsplit(text," ")[[1]][l]
        
        
        ## xtri2
        result2 <- model_2_lasts(c,d)
        names(result2) <- c("counts","word")
        result2$counts <- 5*(result2$counts/mean(result2$counts))
        
        result1 <- model_1_lasts(d)
        names(result1) <- c("counts","word")
        result1$counts <- result1$counts/mean(result1$counts)
        
        result <- rbind(result1,result2)
        
        ##if (sum(is.na(result))<length(result$counts)){result <- aggregate(counts ~ word, data=result,sum,na.rm=TRUE)}
        ##result <- result[order(result$counts,decreasing = TRUE),]
        ##return(result[1:nn,1])
        
        if (sum(is.na(result))<length(result$counts))
        {
          result <- aggregate(counts ~ word, data=result,sum,na.rm=TRUE)
          result <- result[order(result$counts,decreasing = TRUE),]
          return(result[1:nn,1])
        } else  {
          result <- result[order(result$counts,decreasing = TRUE),]
          return(result[1:nn,2])
        }
        
      }
      
      else if (l>=1) {
        
        d <- strsplit(text," ")[[1]][l]
        
        
        result1 <- model_1_lasts(d)
        names(result1) <- c("counts","word")
        result1$counts <- result1$counts/mean(result1$counts)
        
        ##result <- rbind(result1)
        ##result <- aggregate(counts ~ word, data=result,sum)
        result <- result1
        result <- result[order(result$counts,decreasing = TRUE),]
        return(result[1:nn,2])
        
      }
      
      
      
    } ## end model_ng to be called with nn=input$nb_res
    
    
    model_s <- function(text,nnn) {
      
      ##txt2 <- text
      ##txt2 <- removeWords(txt2, stopwords("english"))
      ##vec <- strsplit(txt2," ")[[1]]
      
      vec <- strsplit(text," ")[[1]]
      l <- length(unique(vec))
      
      ##vec <- c("food","fish","sea","car")
      ##res <- c("bus","shrimp")
      
      res <- model_ng(text,50)
      res <- res[which(!is.na(res))]
      if (length(res)==0){return("?")}
      
      stext = model[[vec,average=FALSE]]
      sres = model[[res,average=FALSE]]
      similarities = cosineSimilarity(stext,sres)
      sss <- t(similarities)
      sss <- as.data.frame(sss)
      ##class(sss)
      
      sss$max <- 0
      for (j in 1:length(sss$max)) {
        ##sss$max[j] <- max(sss[j,1:l])
        similarities <- sss[j,1:l]
        sss$max[j] <- mean(similarities[which(similarities>quantile(similarities,0.8))])
      }
      
      ##sss$semantic_match <- (rowMeans(sss)+sss$max)/2
      sss$semantic_match <- sss$max
      
      ##sss$semantic_match <- (rowMeans(sss))
      sss <- sss[order(sss$semantic_match,decreasing = TRUE),]
      for (u in 1:l) {sss <- sss[which(rownames(sss)!=vec[u]),]}
      ##sss <- sss[which(rownames(sss)!=vec[l] & rownames(sss)!=vec[l-1]),]
      return(rownames(sss[1:nnn,]))
      
    }
    
    if (input$text!="") {
    ######
    
      if (input$text =="loubia"){return(data.frame("3dss","bisandi"))}
      if (input$text =="bouzbal"){return(data.frame("dlma","kilimini"))}
      
    txt <- input$text
    txt <- tolower(txt)  
    txt <- gsub(",", "",txt)
    ##txt <- gsub(".", "",txt)
    txt <- gsub(":", "",txt)
    txt <- gsub(";", "",txt)
    
    txt <- gsub("'d"," would",txt) ## let's adjust the problem of "'d" meaning "would"
    txt <- gsub("'m"," am",txt) ## let's adjust the problem of "'d" meaning "would"
    txt <- gsub("'s"," is",txt) ## let's adjust the problem of "'d" meaning "would"
    txt <- gsub("'re"," are",txt) ## let's adjust the problem of "'d" meaning "would"
    txt <- gsub("'ve"," have",txt) ## let's adjust the problem of "'d" meaning "would"
    txt <- gsub("'nt"," not",txt) ## let's adjust the problem of "'d" meaning "would"
    
    ##d <- data.frame(model_ng(input$text,input$nb_res),model_s(input$text,input$nb_res))
    
    d <- data.frame(model_ng(txt,input$nb_res),model_s(txt,input$nb_res))
    names(d) <- c("Ngram only","Ngram+Semantic")
    
    ##d <- data.frame(model_ng(input$text,input$nb_res))
    ##names(d) <- c("Ngram only")
    
    d
    ######
    }
    
  })
  
  

})


