#Load Required Libraries
library(dplyr)
library(data.table)
library(feather)


#Load Word Lists:
matched.twograms<-read_feather(paste("data/","matched.twograms.feather",sep=""))
matched.threegrams<-read_feather(paste("data/","matched.threegrams.feather",sep=""))
matched.fourgrams<-read_feather(paste("data/","matched.fourgrams.feather",sep=""))
matched.fivegrams<-read_feather(paste("data/","matched.fivegrams.feather",sep=""))
matched.sixgrams<-read_feather(paste("data/","matched.fivegrams.feather",sep=""))


#Load Defined Functions
removepossessives<- function(x) {
  x<-unlist(strsplit(x, "('s)", perl=TRUE))
  x<-paste(x,collapse = "")
}

#Define Main Model

simplewordsuggest<-function(x){
  sentence<-x
  sentence<-unlist(removepossessives(sentence))
  sentence<-unlist((strsplit(tolower(sentence), split = " ")))
  
  if(length(sentence) > 4){
    fivegram<-paste(sentence[length(sentence)-4],sentence[length(sentence)-3],sentence[length(sentence)-2],sentence[length(sentence)-1],sentence[length(sentence)],sep = "_")    #fourgramnextword<-matched.fivegrams[which(matched.fivegrams$feature==fourgram),]
    nextword<-filter(matched.sixgrams,feature==fivegram)
    
    if(nrow(nextword)<3){
      fourgram<-paste(sentence[length(sentence)-3],sentence[length(sentence)-2],sentence[length(sentence)-1],sentence[length(sentence)],sep = "_")
      #fourgramnextword<-matched.fivegrams[which(matched.fivegrams$feature==fourgram),]
      fourgramnextword<-filter(matched.fivegrams,feature==fourgram)
      nextword<-rbind(nextword,fourgramnextword)
    }
    
    if(nrow(nextword)<3){
      threegram<-paste(sentence[length(sentence)-2],sentence[length(sentence)-1],sentence[length(sentence)],sep = "_")
      #threegramnextword<-matched.fourgrams[which(matched.fourgrams$feature==threegram),]
      threegramnextword<-filter(matched.fourgrams,feature==threegram)  
      nextword<-rbind(nextword,threegramnextword)
    }
    
    if(nrow(nextword)<3){
      twogram<-paste(sentence[length(sentence)-1],sentence[length(sentence)],sep = "_")
      #twogramnextword<-matched.threegrams[which(matched.threegrams$feature==twogram),]
      twogramnextword<-filter(matched.threegrams,feature==twogram)
      nextword<-rbind(nextword,twogramnextword)
    }
    
    if(nrow(nextword)<3){
      onegram<-sentence[length(sentence)]
      #onegramnextword<-matched.twograms[which(matched.twograms$feature==onegram),]
      onegramnextword<-filter(matched.twograms,feature==onegram)
      nextword<-rbind(nextword,onegramnextword)
    }
    
  }
  
  
  if(length(sentence) == 4){
    fourgram<-paste(sentence[length(sentence)-3],sentence[length(sentence)-2],sentence[length(sentence)-1],sentence[length(sentence)],sep = "_")
    #fourgramnextword<-matched.fivegrams[which(matched.fivegrams$feature==fourgram),]
    nextword<-filter(matched.fivegrams,feature==fourgram)
    
    if(nrow(nextword)<3){
      threegram<-paste(sentence[length(sentence)-2],sentence[length(sentence)-1],sentence[length(sentence)],sep = "_")
      #threegramnextword<-matched.fourgrams[which(matched.fourgrams$feature==threegram),]
      threegramnextword<-filter(matched.fourgrams,feature==threegram)  
      nextword<-rbind(nextword,threegramnextword)
    }
    
    if(nrow(nextword)<3){
      twogram<-paste(sentence[length(sentence)-1],sentence[length(sentence)],sep = "_")
      #twogramnextword<-matched.threegrams[which(matched.threegrams$feature==twogram),]
      twogramnextword<-filter(matched.threegrams,feature==twogram)
      nextword<-rbind(nextword,twogramnextword)
    }
    
    if(nrow(nextword)<3){
      onegram<-sentence[length(sentence)]
      #onegramnextword<-matched.twograms[which(matched.twograms$feature==onegram),]
      onegramnextword<-filter(matched.twograms,feature==onegram)
      nextword<-rbind(nextword,onegramnextword)
    }
    
  }
  
  if(length(sentence) == 3){
    threegram<-paste(sentence[length(sentence)-2],sentence[length(sentence)-1],sentence[length(sentence)],sep = "_")
    #threegramnextword<-matched.fourgrams[which(matched.fourgrams$feature==threegram),]
    nextword<-filter(matched.fourgrams,feature==threegram)
    
    if(nrow(nextword)<3){
      twogram<-paste(sentence[length(sentence)-1],sentence[length(sentence)],sep = "_")
      #twogramnextword<-matched.threegrams[which(matched.threegrams$feature==twogram),]
      twogramnextword<-filter(matched.threegrams,feature==twogram)
      nextword<-rbind(nextword,twogramnextword)
    }
    
    if(nrow(nextword)<3){
      onegram<-sentence[length(sentence)]
      #onegramnextword<-matched.twograms[which(matched.twograms$feature==onegram),]
      onegramnextword<-filter(matched.twograms,feature==onegram)
      nextword<-rbind(nextword,onegramnextword)
    }
    nextword<-nextword[order(-nextword$frequency),]
    as.character(nextword$nextword[1:3])
  }   
  
  
  if(length(sentence) == 2){
    twogram<-paste(sentence[length(sentence)-1],sentence[length(sentence)],sep = "_")
    #twogramnextword<-matched.threegrams[which(matched.threegrams$feature==twogram),]
    nextword<-filter(matched.threegrams,feature==twogram)
    
    if(nrow(nextword)<3){
      onegram<-sentence[length(sentence)]
      #onegramnextword<-matched.twograms[which(matched.twograms$feature==onegram),]
      onegramnextword<-filter(matched.twograms,feature==onegram)
      nextword<-rbind(nextword,onegramnextword)
    }
  } 
  
  if(length(sentence) == 1){
    onegram<-sentence[length(sentence)]
    #onegramnextword<-matched.twograms[which(matched.twograms$feature==onegram),]
    nextword<-filter(matched.twograms,feature==onegram)
    
  } 
  
  if(!exists("nextword")){
    nextword<-cbind.data.frame(c("the","a","an"), c("X","X","X"), as.numeric(c(1,1,1)))
    colnames(nextword)<-c("nextword","feature","frequency")
  }
  
  if(nrow(nextword)<3){
    mostcommon<-cbind.data.frame(c("the","a","an"), c("X","X","X"), as.numeric(c(1,1,1)))
    colnames(mostcommon)<-c("nextword","feature","frequency")
    nextword<-rbind(nextword,mostcommon)
  }  
  
  
  nextword<-group_by(nextword,nextword) %>% summarize(frequency = prod(frequency))
  
  nextword<-nextword[order(-nextword$frequency),]
  
  as.character(nextword$nextword[1:3])
  
}


library(shiny)

shinyServer(function(input, output,session) {

    words<-reactive({simplewordsuggest(input$caption)})
    
    output$word1 <- renderUI({
    actionButton("word1", label = words()[1])
        })
    
    output$word2 <- renderUI({
    actionButton("word2", label = words()[2])
    })
    
    output$word3 <- renderUI({
    actionButton("word3", label = words()[3])
    })
    
    
    observeEvent(input$word1, {
      name <- paste(input$caption, words()[1])
      updateTextInput(session, "caption", value=name)
    })

    observeEvent(input$word2, {
      name <- paste(input$caption, words()[2])
      updateTextInput(session, "caption",  value=name)
    })

    observeEvent(input$word3, {
      name <- paste(input$caption, words()[3])
      updateTextInput(session, "caption", value=name)

    })

    
})     


