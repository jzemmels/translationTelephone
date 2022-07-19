#

library(shiny)
library(reticulate)
library(tidyverse)

argostranslate <- reticulate::import("argostranslate")
allTranslators_list <- reticulate::py_load_object("data/allTranslators")

allTranslators <- tibble(from_lang = allTranslators_list$from_lang,
                         to_lang = allTranslators_list$to_lang,
                         translator = allTranslators_list$translator)

load("data/englishIdioms.RData")

ui <- fluidPage(
  shinybusy::add_busy_spinner(),
  # Application title
  titlePanel("Language Translation Telephone"),
  tabsetPanel(
    tabPanel("Idiom guessing game",
             sidebarLayout(
               sidebarPanel(numericInput(inputId = "game_numLanguages",label = "Number of languages in telephone chain",value = 1,min = 1,step = 1),
                            actionButton(inputId = "game_execute",label = "Generate an idiom"),
                            textInput(inputId = "game_guess",label = "Guess the idiom:"),
                            uiOutput(outputId = "game_guessGraded"),
                            actionButton(inputId = "game_forfeit",label = "I give up")),
               mainPanel(uiOutput(outputId = "game_generatedIdiom"))
             )),
    tabPanel("Enter your own phrase",
             column(width = 4,
                    textInput(inputId = "phrase",label = "Input a phrase",value = "A bird in the hand is worth two in the bush"),
                    actionButton(inputId = "randomPhrase",label = "Choose a phrase for me")),
             column(width = 4,
                    selectInput(inputId = "translationLanguage",
                                label = "Select a Language",
                                choices = unique(c(allTranslators$from_lang,allTranslators$to_lang)),
                                selected = sample(unique(c(allTranslators$from_lang,allTranslators$to_lang)),size = 1)),
                    actionButton(inputId = "randomLanguage","Choose a language for me"),
                    uiOutput(outputId = "translatedPhrase")),
             column(width = 4,
                    uiOutput(outputId = "backToEnglishPhrase")))
  )
)


server <- function(session,input, output) {


  #################### Code for Idiom guessing game tab

  selectedIdiom <- reactiveVal()

  observeEvent(input$game_execute,{

    updateTextInput(session = session,inputId = 'game_guess',value = "")
    output$game_guessGraded <- renderUI({return("")})

    selectedIdiom(sample(englishIdioms$Idiom,size = 1))

    req(length(selectedIdiom()) > 0)

    selectedLanguages <- sample(unique(c(allTranslators$from_lang,allTranslators$to_lang)),
                                size = input$game_numLanguages)

    translatedIdiom <- selectedIdiom()
    langFrom <- "English"

    for(langTo in selectedLanguages){

      toTranslator <- allTranslators %>%
        filter(from_lang == langFrom & to_lang == langTo) %>%
        pull(translator) %>%
        .[[1]]

      translatedIdiom <- toTranslator(translatedIdiom)

      langFrom <- langTo
    }

    backToEnglishTranslator <- allTranslators %>%
      filter(from_lang == langFrom & to_lang == "English") %>%
      pull(translator) %>%
      .[[1]]

    backToEnglishIdiom <- backToEnglishTranslator(translatedIdiom)

    output$game_generatedIdiom <- renderUI({

      return(HTML(paste0("<strong>Translation Chain:</strong> ",paste0(c("English",selectedLanguages,"English"),collapse = " to "),
                         "</br>",
                         "<strong>Final Translated Idiom:</strong> ",backToEnglishIdiom)))

    })

  })

  gradeGuess <- function(guess,correct){

    if(correct){

      return(paste0("<font color = 'green'><text>",guess,"</text></font>"))

    }
    else{

      return(paste0("<font color = 'red'><text>",guess,"</text></font>"))

    }

  }

  # as the user enters a guess into the text box, highlight the words/phrases
  # that are in the correct idiom
  observe({

    req(length(selectedIdiom()) > 0)
    # req(input$guess != "")

    correctAnswer <- tolower(selectedIdiom())
    correctAnswer_split <- str_split(correctAnswer,pattern = " ")[[1]]
    correctAnswer_split <- correctAnswer_split[!is.na(correctAnswer_split) & str_length(correctAnswer_split) > 1]

    guess <- tolower(input$game_guess)
    guessSplit <- str_split(input$game_guess,pattern = " ")[[1]]
    guessSplit <- guessSplit[!is.na(guessSplit) & str_length(guessSplit) > 1]

    output$game_guessGraded <- renderUI({

      req(length(guessSplit) > 0)

      gradedGuess <- map2_chr(tolower(guessSplit),
                              1:length(guessSplit),
                              function(guess,guessInd){

                                if(str_length(guess) > 0){

                                  ret <- paste0("<font color = 'red'><text>",guess,"</text></font>")

                                  if(guess %in% correctAnswer_split){

                                    ret <- paste0("<font color = 'orange'><text>",guess,"</text></font>")

                                  }
                                  if(guess == correctAnswer_split[guessInd]){

                                    ret <- paste0("<font color = 'green'><text>",guess,"</text></font>")

                                  }

                                  return(ret)

                                }

                              }) %>%
        purrr::discard(~ length(.) == 0) %>%
        paste0(collapse = " ")

      if(guess == correctAnswer){

        return(HTML(paste0('<font color = "green">Correct! The idiom is "',correctAnswer,'."')))

      }
      else{

        return(HTML(gradedGuess))

      }

    })

  })

  observeEvent(input$game_forfeit,{

    output$game_guessGraded <- renderUI({

      return(HTML(paste0('<font color = "red"><b>The idiom was "',selectedIdiom(),'."</b></font>')))

    })

  })

  ################### Code for the Enter your own phrase tab
  observeEvent(input$randomPhrase,{

    updateTextInput(session = session,inputId = "phrase",value = sample(englishIdioms$Idiom,size = 1))

  })

  observeEvent(input$randomLanguage,{

    updateSelectInput(session = session,inputId = "translationLanguage",selected = sample(unique(c(allTranslators$from_lang,allTranslators$to_lang)),size = 1))

  })

  observe({

    toTranslator <- allTranslators %>%
      filter(from_lang == "English" & to_lang == input$translationLanguage) %>%
      pull(translator) %>%
      .[[1]]

    # phraseTo <- map_chr(str_split(input$phrase,pattern = " ")[[1]],toTranslator) %>% paste0(collapse = " ")
    phraseTo <- toTranslator(input$phrase)

    output$translatedPhrase <- renderUI({

      return(phraseTo)

    })

    fromTranslator <- allTranslators %>%
      filter(from_lang == input$translationLanguage & to_lang == "English") %>%
      pull(translator) %>%
      .[[1]]

    # phraseFrom <- map_chr(str_split(phraseTo,pattern = " ")[[1]],fromTranslator) %>% paste0(collapse = " ")
    phraseFrom <- fromTranslator(phraseTo)

    output$backToEnglishPhrase <- renderUI({

      return(phraseFrom)

    })

  })
}

# Run the application
shinyApp(ui = ui, server = server)
