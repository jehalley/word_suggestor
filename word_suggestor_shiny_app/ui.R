shinyUI <- fluidPage(
  textAreaInput("caption", "Simple Word Suggester", placeholder = "Start typing and Simple Word Suggester will suggest three words to continue your sentence. Simply click one of the suggested words to add it to your sentence. ", height = '150px'),
  verbatimTextOutput("value"),
  uiOutput("word1"),
  uiOutput("word2"),
  uiOutput("word3")
)
