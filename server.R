# Contains the shinyServer portion of the shiny App for the English word 
# predictor. It requires shiny_predictor.R to load functions and required data.
# It also requires "shinyjs' package, which is loaded in the shinyUI.
# 
# Created on 2017 by Reynaldo Vazquez for the purpose of developing an English 
# language word predictor.
# 
# Permission is granted to copy, use, or modify all or parts of this program and  
# accompanying documents for purposes of research or education, provided this 
# notice and attribution are retained, and changes made are noted.
# 
# This program and accompanying documents are distributed without any warranty,
# expressed or implied. They have not been tested to the degree that would be 
# advisable in important tasks. Any use of this program is entirely at the 
# user's own risk.
# 
source("predict_both.R")
shinyServer(
    function(input, output, session) {
        session$sendCustomMessage(type="refocus",message=list(NULL))
        n <- 0
        observeEvent(input$langEs, {
            toggle("espanol")
            toggle("english")
        })
        observeEvent(input$langEn, {
            toggle("espanol")
            toggle("english")
        })
        observeEvent(input$centerHelp, { 
            show("topHelp")
            hide("centerHelp")
            if ( h == 0){
                updateActionButton(session, "optionMenu", label ="more options")
            }
            if (n == 0) {
                updateActionButton(session, "centerHelp", label="show help")
                updateActionButton(session, "topHelp", label = "show help")
                hide("texthelp0")
                hide("texthelp")
                n <<- 1
            } else if (n == 1){
                updateActionButton(session, "centerHelp", label="hide help")
                updateActionButton(session, "topHelp", label = "hide help")
                show("texthelp0")
                show("texthelp")
                n <<- 0
            }
            session$sendCustomMessage(type="refocus",message=list(NULL))
        })
        h <- 0
        observeEvent(input$optionMenu, { 
            toggle("currents")
            toggle("preds")
            if (h == 0) {
                updateActionButton(session, "optionMenu", 
                                   label = "hide options")
                show("topHelp")
                h <<- 1
            } else if (h == 1){
                updateActionButton(session, "optionMenu", 
                                   label = "show options")
                hide("topHelp")
                h <<- 0
            }
            session$sendCustomMessage(type="refocus",message=list(NULL))
        })
        tipN <- 0
        observeEvent(input$topHelp, { 
            hide("centerHelp")
            if (n == 0) {
                updateActionButton(session, "topHelp", label = "show help")
                updateActionButton(session, "centerHelp", label="show help")
                hide("texthelp0")
                hide("texthelp")
                n <<- 1
            } else if (n == 1){
                updateActionButton(session, "topHelp", label = "hide help")
                updateActionButton(session, "centerHelp", label="hide help")
                show("texthelp")
                if (k == 0) {
                    show("texthelp0")
                }
                showModal(modalDialog(title = "Instructions",
                  HTML("To generate suggestions simply start typing in the text 
                        box <br><br> 
                        <u>Current-word suggestions</u> appear underlined
                        beneath the text box. Click on one option to complete 
                        the word. Or press ctrl+shift+[option order number],
                        i.e. ctrl+shift+1 to select the first option from left 
                        to right <br><br> 
                        <u>Next-word suggestions</u> appear in highlighted 
                        buttons. Click on one option to paste. 
                        Or press ctrl+[option order number],
                        i.e. ctrl+2 to select the option in the red highlight 
                        <br><br> 
                        Note: ctrl+ commands might not work in some system 
                        configurations")
                  ))
                tipN <<- 1
                n <<- 0
            }
            session$sendCustomMessage(type="refocus",message=list(NULL))
        })
        j <- 0
        observeEvent(input$preds, { 
            toggle("button4")
            toggle("button5")
            if (j == 0) {
                updateActionButton(session, "preds", label = "more next-words")
                j <<- 1
            } else if (j == 1){
                updateActionButton(session, "preds", label = "less next-words")
                j <<- 0
            }
            session$sendCustomMessage(type="refocus",message=list(NULL))
        })
        k <- 0
        observeEvent(input$currents, { 
            toggle("button6")
            toggle("button7")
            toggle("button8")
            if (k == 0) {
                updateActionButton(session,"currents",label="current-words too")
                hide("texthelp0")
                k <<- 1
            } else if (k == 1){
                updateActionButton(session, "currents", label="next-words only")
                k <<- 0
                if (n == 0) {
                    show("texthelp0")
                }
            }
            session$sendCustomMessage(type="refocus",message=list(NULL))
        })
        pred_both <- reactive({
            req(input$usertext)
            hide("currents")
            hide("preds")
            updateActionButton(session, "optionMenu", label ="show options")
            hide("topHelp")
            h <<- 0
            predict.both(input$usertext)
        })
        pred_next <- reactive({
            pred_both()$predictions
        })
        pred_current <- reactive({
            pred_both()$current
        })
        observeEvent(input$button1, {
            if (length(pred_next()) > 3 && tipN == 0) {
              showNotification("Tip: Try ctrl+[num]
                     to select a next-word option (i.e. ctrl+1 for the green 
                     button option).Or ctrl+shift+[num] to select a current-word
                     option.
                     (Might not work in some browsers)", 
                               duration = 20, type = "message")
                tipN <<- 1
            }
        })
        output$texthelp <- renderText({ 
            if (input$usertext == ""){
                show <- "<text style='color: white'>next-word suggestions will 
                appear here"
            } else {
                show <- "<text style='color:#737373'>click on an option to 
                paste"
            }
        })
        output$texthelp0 <- renderText({ 
            if (input$usertext == ""){
                show <- "<text style='color:#bfbfbf'>current-word suggestions 
                will appear here"
            } else if (length(pred_current())  < 1){
                show <- "<text style='color:#1c1b1a'>no current-word suggestions
                were found"
            } else {
                show <- "<text style='color:#737373'>click on an option 
                to complete current word"
            }
        })
        output$text1 <- renderText({ 
            pred_next()[1]
        })
        output$text2 <- renderText({ 
            pred_next()[2]
        })
        output$text3 <- renderText({ 
            pred_next()[3]
        })
        output$text4 <- renderText({ 
            pred_next()[4]
        })
        output$text5 <- renderText({
            show <- pred_next()[5]
        })
        output$text6 <- renderText({ 
            if (length(pred_current()) < 1){
                show <- ""
            } else {
                show <- paste("<u>",  pred_current()[1], "</u>")
            }
        })
        output$text7 <- renderText({ 
            show <- paste("<u>",  pred_current()[2], "</u>")
        })
        output$text8 <- renderText({ 
            show <- paste("<u>",  pred_current()[3], "</u>")
        })
        output$text9 <- renderText({ 
            show <- paste("<u>",  pred_current()[4], "</u>")
        })
        observeEvent(input$usertext, {
          if (length(pred_current()) < 2){
            hide("curr2"); hide("curr3"); hide("curr4");
          } else if (length(pred_current()) < 3){
            show("curr2"); hide("curr3"); hide("curr4");
          } else if (length(pred_current()) < 4){
            show("curr2"); show("curr3"); hide("curr4");
          } else {
            show("curr2"); show("curr3"); show("curr4");
          }
        })
        observeEvent(input$button6, {
            if (length(pred_current())  > 0){
                updateTextInput(session, "usertext", 
                      value = replace.last(input$usertext, pred_current()[1]))
            } 
            session$sendCustomMessage(type="refocus",message=list(NULL))
        })
        observeEvent(input$button7, {
            if (length(pred_current())  > 1){
                updateTextInput(session, "usertext", 
                      value = replace.last(input$usertext, pred_current()[2]))
            } 
            session$sendCustomMessage(type="refocus",message=list(NULL))
        })
        observeEvent(input$button8, {
            if (length(pred_current())  > 2){
                updateTextInput(session, "usertext", 
                      value = replace.last(input$usertext, pred_current()[3]))
            } 
            session$sendCustomMessage(type="refocus",message=list(NULL))
        })
        observeEvent(input$button9, {
          if (length(pred_current())  > 3){
            updateTextInput(session, "usertext", 
                            value = replace.last(input$usertext, pred_current()[4]))
          } 
          session$sendCustomMessage(type="refocus",message=list(NULL))
        })
        observeEvent(input$reset, {
            updateTextInput(session, "usertext", value = "")
            session$sendCustomMessage(type="refocus",message=list(NULL))
        })
        observeEvent(input$button1, {
            updateTextInput(session, "usertext", 
                      value = paste(input$usertext, pred_next()[1]))
            session$sendCustomMessage(type="refocus",message=list(NULL))
        })
        observeEvent(input$button2, {
            updateTextInput(session, "usertext", 
                      value = paste(input$usertext, pred_next()[2]))
            session$sendCustomMessage(type="refocus",message=list(NULL))
        })
        observeEvent(input$button3, {
            updateTextInput(session, "usertext", 
                     value = paste(input$usertext, pred_next()[3]))
            session$sendCustomMessage(type="refocus",message=list(NULL))
        })
        observeEvent(input$button4, {
            updateTextInput(session, "usertext", 
                     value = paste(input$usertext, pred_next()[4]))
            session$sendCustomMessage(type="refocus",message=list(NULL))
        })
        observeEvent(input$button5, {
            if (length(pred_next()) < 5){
                show <- ""
            } else {
                show <- pred_next()[5]
            }
            updateTextInput(session, "usertext", 
                            value = paste(input$usertext, show))
            session$sendCustomMessage(type="refocus",message=list(NULL))
        })
    }
)