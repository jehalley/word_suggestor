#Load Required Libraries
library(dplyr)
library(data.table)


#Load Word Lists:
matched.twograms<-readRDS(paste("data/","matched.twograms.small.rds",sep=""))
matched.threegrams<-readRDS(paste("data/","matched.threegrams.small.rds",sep=""))
matched.fourgrams<-readRDS(paste("data/","matched.fourgrams.small.rds",sep=""))
matched.fivegrams<-readRDS(paste("data/","matched.fivegrams.small.rds",sep=""))

#Load Defined Functions
firstup <- function(x) {
  substr(x, 1, 1) <- tolower(substr(x, 1, 1))
  x
}

removepossessives<- function(x) {
  x<-unlist(strsplit(x, "('s)", perl=TRUE))
  x<-paste(x,collapse = "")
}

#Define Main Model

simplewordsuggest<-function(x){
  sentence<-x
  sentence<-unlist(removepossessives(sentence))
  sentence<-unlist((strsplit(tolower(sentence), split = " ")))
  
  
  if(length(sentence) > 3){
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



if (interactive()) {
  
  ui <- fluidPage(
    textInput("caption", "Simple Word Suggestor", "Enter Text Here"),
    verbatimTextOutput("value")
  )
  server <- function(input, output) {
    
    output$value <- renderText({ simplewordsuggest(input$caption) })
  }
  shinyApp(ui, server)
}

# Run the application 
shinyApp(ui = ui, server = server)
