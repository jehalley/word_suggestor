library(data.table)
library(dplyr)
library(feather)
library(reshape)
library(quanteda)

get_tokens_from_text_files<-function(folder_path,sample_fraction){
  # combine all of the text files in a folder into a character vector
  setwd(folder_path)
  filelist <- list.files(pattern = ".*.txt")
  combined_texts<-unlist(lapply(filelist, FUN=readLines))
  #take a sample from the character vector with the size specified by user
  set.seed(333)
  sample_index<-sample(1:length(combined_texts),size=sample_fraction*length(combined_texts), replace = FALSE)
  sampled_texts<-combined_texts[sample_index]
  # Tokenize the sampled texts
  Tokens <- tokens(sampled_texts,remove_numbers = TRUE, remove_punct = TRUE,
                   remove_symbols = TRUE, remove_separators = TRUE,
                   remove_twitter = TRUE, remove_hyphens = TRUE, remove_url = TRUE)
  Tokens
}

get_nextword_list<-function(tokens_object,n_gram_size_integer){
  #tokenize tokens object to ngrams of specified size
  Ngrams<-tokens_ngrams(tokens_object, n=n_gram_size_integer)
  #construct document feature matrix
  NgramsDFM<-dfm(Ngrams, tolower =FALSE)
  rm(Ngrams)
  #remove infrequent ngrams
  NgramsDFM<-dfm_trim(NgramsDFM, min_termfreq = 2, min_docfreq = 2)
  #convert dfm to dataframe with ngram and its frequency as variables
  NgramsFREQs<-textstat_frequency(NgramsDFM)
  NgramsFREQs$docfreq<-NULL
  NgramsFREQs$group<-NULL
  #make matched file where a two gram is split into a feature (first word) and a next word
  pat <- "(.*)_(.*)"
  matched.Ngrams<-transform(NgramsFREQs, feature = sub(pat, "\\1", feature), nextword = sub(pat, "\\2", feature))
  matched.Ngrams$rank<-NULL
  #convert Ngram dataframe into datatable for keying for faster searches
  matched.Ngrams<-as.data.table(matched.Ngrams)
  setkey(matched.Ngrams,feature)
  #return final data.table
  matched.Ngrams
}

save_wordlists_as_feather<-function(){
  # make a list of all the variable names for data.tables in the global environment
  list_of_data_table_names <- Filter(function(x) is.data.table(get(x)),ls(envir = .GlobalEnv))
  # apply the feather writer to all of the data.tables in global environment
  for(i in 1:length(list_of_data_table_names)){
    write_feather(eval(parse(text = list_of_data_table_names[i])),paste(list_of_data_table_names[i],".feather", sep =""))
  }
  
}

# move to the user's home directory
setwd("~/")

# Download Data
file_url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
temp<-tempfile()
download.file(file_url,temp)
unzip(temp)

# move into the unzipped directory
setwd("~/final/en_US")

# tokenize English texts
En_Tokens<-get_tokens_from_text_files(getwd(),.2)

# make wordlists for 2-6 grams
matched.twograms<-get_nextword_list(En_Tokens,2)
matched.threegrams<-get_nextword_list(En_Tokens,3)
matched.fourgrams<-get_nextword_list(En_Tokens,4)
matched.fivegrams<-get_nextword_list(En_Tokens,5)
matched.sixgrams<-get_nextword_list(En_Tokens,6)

#weight word lists
matched.twograms$frequency<-matched.twograms$frequency*1E6
matched.threegrams$frequency<-matched.threegrams$frequency*1E7
matched.fourgrams$frequency<-matched.fourgrams$frequency*1E8
matched.fivegrams$frequency<-matched.fivegrams$frequency*1E10
matched.sixgrams$frequency<-matched.sixgrams$frequency*1E11

#trim matched ngram tables to remove infrequent ngrams
matched.twograms<-subset(matched.twograms, frequency>2e+6)
matched.threegrams<-subset(matched.threegrams, frequency>2e+7)

#make a new directory to store wordlists
dir.create("~/final/wordslists")
setwd("~/final/wordslists")

#convert wordlists to feather format for faster loading
save_wordlists_as_feather()


