# Word Suggestor
This repo contains the code to generate my predictive text web application:

https://jeffrey-halley.shinyapps.io/wordsuggester/

# Instructions
The "word_suggestor_shiny_app" folder contains everything you need to run the shiny app on a shiny server including premade word lists.

If you would like to alter the wordlists in any way or see how they were made you can use the script "get_wordlists.r".

# How it works
The wordsuggester app searches a database of commonly used phrases (wordlists) for a phrase that matches text typed by a user. The app attempts to match all of the text input by the user (up to five words long) but if no matches are found the app progressively removes words from the beginning of the phrase until a match is found. Once a match is found, the three words that most commonly follow the matched phrase are suggested to the user in a ranked order from most common to least common. If fewer than three words commonly follow a particular phrase, the app will suggest the words that do follow that phrase and then trim words from the beginning of the phrase until a total of three words can be suggested to the user.

The wordlists were made by using the quanteda package to find all of the 2-6 word phrases that occur at least twice in a collection of English texts drawn from news articles, blog posts, and tweets (853935 lines of text were analyzed in the version of the wordsuggester app linked above). All phrases of a particular length are stored in their own wordlist. The wordlist are data tables containing three columns: feature, nextword and frequency. Feature is the first word or words of a common phrase between 2 and 6 words long, and nextword is the last word of the phrase. Frequency is how many times the complete phrase (feature + nextword) occurred in the sampled text.

Frequencies are useful for ranking phrases of the same length but they are not as useful for ranking the relevances of phrases of different lengths in determining the next word. For instance, an exact match of a six-word phrase, even if it occurs only once in the sampled texts, would likely be a better predictor of the next word than a single word match that occurs 1000 times in the sampled texts. For this reason, the frequencies of all the wordlists used in the version of the app linked above have been multiplied by different factors of 10 to increase the weight of predictions for longer phrases over the predictions of shorter phrases. In the version of the app linked above, for each n-word increase in phrase length the frequency of the phrase is multiplied by 10^n. The degree of weighting used was determined through optimization with a benchmarking dataset. 

The get_wordlists.r script was used to create the wordlists used in the version of the app linked above. The features of the wordlist, such as the various weighting of different phrase lengths and the number of texts analyzed can be easily changed in the wordlist.r script to make new wordlists with different properties. 

