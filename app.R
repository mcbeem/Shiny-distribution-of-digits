#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# load packages -------------------------------------------------------------------------------

library(shiny)
library(readtext)
library(tokenizers)
library(dplyr)
library(ggplot2)
library(DT)
library(shinythemes)
library(readxl)

# read in decimals without scientific notation
options(scipen = 9999)


# UI ------------------------------------------------------------------------------------------

ui <- fluidPage(
  theme = shinytheme("readable"),
  
  # enable MathJax (not currently used)
  withMathJax(),
  
  mainPanel(
    # format the title
    tags$head(tags$style(
      HTML("
      h1 {
        font-weight: 500;
        line-height: 1;
        font-size: 200%
      }
    ")
    )),
    
    # Application title
    headerPanel("Forensic Analysis via the Distribution of Digits"),
    
    # UI consists of multiple tab panels
    
# UI: tabsetPanel ----------------------------------------------------------------------------------
    
    tabsetPanel(
      
# UI: tabPanel data --------------------------------------------------------------------------------
      
      tabPanel(
        "Data",
        
        HTML("<br>"),
        
        radioButtons(
          "numbersType",
          "Which numbers to extract",
          inline = TRUE,
          choices = c("All numbers", "Only numbers with decimals"),
          selected = "All numbers"
        ),
        
        conditionalPanel(
          condition = "input.fileType == 'Document'",
          
          checkboxInput("removeDOIs",
                        "Try to automatically remove DOIs?",
                        value = TRUE),
          
          conditionalPanel(
            condition = "input.numbersType == 'All numbers'",
            
            checkboxInput("removeYears",
                          "Try to automatically remove years?",
                          value = TRUE),
            
            checkboxInput(
              "removePageNums",
              "Try to automatically remove page numbers?",
              value = FALSE,
              width = "400px"
            )
            
          ) # closes conditionalPanel numbersType==All numbers
        ),
        # closes conditionalPanel fileType==document
        
        conditionalPanel(
          "input.removePageNums == true",
          
          splitLayout(
            cellWidths = c("50%", "50%"),
            
            numericInput(
              "firstPage",
              "Starting page number",
              min = 1,
              max = Inf,
              step = 1,
              value = 1,
              width = '150px'
            ),
            
            numericInput(
              "lastPage",
              "Ending page number",
              min = 1,
              max = Inf,
              step = 1,
              value = 1,
              width = '150px'
            )
            
          ) # closes splitLayout
          
        ),
        #closes conditionalPanel page number
        
        
        HTML("<br>"),
        
        fileInput(
          "file",
          "File to analyze",
          multiple = FALSE,
          accept = NULL,
          width = NULL,
          buttonLabel = "Browse...",
          placeholder = "No file selected"
        ),
        
        helpText("Allowed files types: pdf, txt, html, rtf, doc, docx, xml"),
        
        radioButtons(
          "fileType",
          "Type of file selected",
          inline = TRUE,
          choices = c("Document", "Excel", "CSV"),
          selected = "Document"
        ),
        
        conditionalPanel(
          condition = "input.fileType == 'Excel'",
          
          numericInput(
            "whichSheet",
            "Select sheet for import",
            min = 1,
            max = 100,
            step = 1,
            value = 1
          )
        ),
        
        HTML("<br>"),
        
        actionButton("removeButton", "Remove selected values"),
        
        actionButton("sortButton", "Sort the values"),
        
        helpText("Click to select values for removal."),
        
        HTML("<br>"),
        
        DTOutput('tbl')
        
      ),
      # closes tabPanel "data"
      
# UI: tabPanel Leading Digit -----------------------------------------------------------------------

      tabPanel(
        "Leading digit",
        
        plotOutput("histFirstDigits"),
        
        checkboxInput(
          "firstChiSq",
          "Perform a chi-squared test? (H0: The distribution decays according to Benford's Law)",
          value = FALSE,
          width = "650px"
        ),
        
        conditionalPanel(
          condition = "input.firstChiSq == true",
          
          radioButtons(
            "firstChiType",
            "Type of chi-squared test",
            choices = c("Pearson", "Monte Carlo"),
            selected = "Pearson"
          ),
          
          helpText(
            "Note: If any of the digits occur less than five times, the Pearson",
            "chi-squared test is unreliable. In that case, use the Monte Carlo method."
          )
          
        ),
        
        conditionalPanel(
          condition = "input.firstChiSq == true & input.firstChiType=='Monte Carlo'",
          
          numericInput(
            "firstChiReps",
            "Monte Carlo repetitions",
            min = 1000,
            max = 50000,
            step = 500,
            value = 2000
          )
          
        ),
        
        conditionalPanel(condition = "input.firstChiSq == true",
                         verbatimTextOutput("chiFirstDigits"))
      ),
      
# UI: tabPanel Trailing digit ----------------------------------------------------------------------

      tabPanel(
        "Trailing digit",
        
        plotOutput("histLastDigits"),
        
        checkboxInput(
          "lastChiSq",
          "Perform a chi-squared test? (H0: the distribution is uniform according to Benford's Law)",
          value = FALSE,
          width = "750px"
        ),
        
        conditionalPanel(
          condition = "input.lastChiSq == true",
          
          checkboxInput("ignoreZero",
                        "Ignore zeros in the chi-squared test",
                        value = FALSE),
        
          radioButtons(
            "lastChiType",
            "Type of chi-squared test",
            choices = c("Pearson", "Monte Carlo"),
            selected = "Pearson"
          ),
          
          helpText(
            "Note: If any of the digits occur less than five times, the Pearson",
            "chi-squared test is unreliable. In that case, use the Monte Carlo method."
          )
          
        ), # close conditionalPanel
        
        
        conditionalPanel(
          condition = "input.lastChiSq == true & input.lastChiType=='Monte Carlo'",
          
          numericInput(
            "lastChiReps",
            "Monte Carlo repetitions",
            min = 1000,
            max = 50000,
            step = 500,
            value = 2000
          )
          
        ),
        
        conditionalPanel(condition = "input.lastChiSq == true",
                         verbatimTextOutput("chiLastDigits"))
        
      ), # closes tabPanel trailing digit
      
# UI: tabPanel histogram of all numbers  -----------------------------------------------------------
      
      tabPanel(
        "Histogram of all numbers",
        plotOutput("histAllNumbers",
                   height = "auto")
      ),
      
      tabPanel(
        "Histogram of all decimals",
        plotOutput("histAllDecimals",
                   height = "auto")
      ), # closes tabPanel hist all numbers
      
# UI: tabPanel histogram decimals by digits  -------------------------------------------------------
      
      tabPanel(
        "Histogram of decimals by number of digits",
        
        HTML("<br>"),
        
        sliderInput(
          "decimals",
          "Decimal places to plot",
          min = 1,
          max = 5,
          step = 1,
          value = 1,
          ticks = FALSE,
          round = TRUE
        ),
        
        HTML("<br>"),
        
        plotOutput("histDigits",
                   height = "auto")
      )
      
    ),
    style = 'width: 750px' # closes tabsetPanel
  ) # closes mainPanel
) # closes fluidpage


# Server -------------------------------------------------------------------------------------------

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  # show warning if fileType Excel or CSV is selected
  observeEvent(input$fileType, {
    showModal(
      modalDialog(
        title = "Warning",
        "Analysis of Excel and CSV files is not yet implemented",
        easyClose = TRUE,
        footer = NULL
      )
    )
    
    # reset radioButton to fileType=document
    updateRadioButtons(session, inputId = "fileType",
                       selected = "Document")
  }, ignoreInit = TRUE)
  
# server: define functions -------------------------------------------------------------------------

  # counttarget counts the number of times a set of values appear
  # it returns zeros, unlike table()
  counttarget <- function(x, target) {
    countone <- function(x, target) {
      length(target[target == x])
    }
    return(sapply(target, countone, x = x))
  }
  
  # mceiling is used for setting the scale of some plots
  #  it rounds up to the nearest value of 'target'
  mceiling <- function(x, base) {
    base * ceiling(x / base)
  }
  
# server: initial setup ----------------------------------------------------------------------------
  
  # specify maximum upload size: 50 mb
  options(shiny.maxRequestSize = 50 * 1024 ^ 2)
  
  # initialize the current.numbers reactive object
  current.numbers <- reactiveVal()
  

# server: observeEvent input file ------------------------------------------------------------------

  # when the user selects a file, save its path in object "file"
  observeEvent(input$file, {
    file = input$file
    if (is.null(file)) {
      return(NULL)
    }
    
    if (input$fileType == "Document") {
      # read the file using the readtext function from the readtext package
      filetext <- readtext(file = file$datapath)
      
      # tokenize the text. this makes a vector for each word in the file
      tokens <- tokenize_words(filetext$text)
      
      # vector index of number positions, based on regular expression
      if (input$numbersType == 'All numbers') {
        number.positions <-
          grepl("\\d", tokens[[1]])
      } else if (input$numbersType == "Only numbers with decimals") {
        number.positions <- grepl("\\d\\.\\d", tokens[[1]])
      }
      
      # extract the numbers
      numbers <- tokens[[1]][number.positions]
      
      numbers.for.select <- as.data.frame(t(numbers))
      
      # remove non-numeric, including dates with slashes or multiple periods
      numbers <- as.numeric(numbers)
      numbers <- numbers[!is.na(numbers)]
      numbers <- as.character(numbers)
     
# server: remove DOIs, years, and page numbers  ----------------------------------------------------

      # remove dois
      if (input$removeDOIs == TRUE) {
        #numbers <- numbers[!grepl("[1][0]\\.", numbers)]
        numbers <- numbers[!grepl("\b(10[.][0-9]{4,})", numbers)]
        
      }
      
      # remove years
      if (input$removeYears) {
        numbers <- numbers[!grepl("(19|20)\\d\\d", numbers)]
      }
      
      # remove page numbers
      if (input$removePageNums) {
        pagenums <- input$firstPage:input$lastPage
        numbers <- numbers[!numbers %in% pagenums]
        
      }
      
      # store the extracted numbers in the reactive object
      current.numbers(numbers)
      
      # display the numbers in the UI
      output$tbl = renderDT(as.matrix(current.numbers()),
                            options = list(),
                            colnames = "Number")
    }
    
  }) # closes observeevent input$file
  

# server: observeEvent remove button ---------------------------------------------------------------

  observeEvent(input$removeButton, {
    if (!is.null(input$tbl_rows_selected)) {
      # drops the selected rows from the current.numbers() reactive object
      numbers <- current.numbers()
      numbers <- numbers[-input$tbl_rows_selected]
      current.numbers(numbers)
    }
    
    # displays the updated numbers
    output$tbl = renderDT(as.matrix(current.numbers()),
                          options = list(),
                          colnames = "Number")
  }) # closes observeEvent removeButton
  

# server: observeEvent sort button ------------------------------------------------------------

  observeEvent(input$sortButton, {
    numbers <- current.numbers()
    numbers <- numbers[order(numbers)]
    current.numbers(numbers)
    
    output$tbl = renderDT(as.matrix(current.numbers()),
                          options = list(),
                          colnames = "Number")
  })
  
  
  observeEvent(
    list(
      input$numbersType,
      input$removeDOIs,
      input$removeYears,
      input$removePageNums
    ),
    {
      if (!is.null(current.numbers())) {
        showModal(
          modalDialog(
            title = "Warning",
            "You must reload the file for this change to take effect",
            easyClose = TRUE,
            footer = NULL
          )
        )
      } # closes if
      
    }
  ) # closes observeEvent numbersType
  

# server: Main processing: observeEvent removeButton or inputFile-----------------------------------

  observeEvent({
    list(input$removeButton,
         input$file)
  }, {
    numbers <- current.numbers()
    
    # get first non zero digit
    # filter leading zeros
    firstdigit <- gsub("0*", "", numbers)
    firstdigit <- gsub("\\.", "", firstdigit)
    firstdigit <- as.numeric(substr(firstdigit, 1, 1))
    
    #filter NAs
    firstdigit <- na.omit(firstdigit)
    
    # get final digit
    lastdigit <- as.numeric(substr(numbers[nchar(numbers) > 1],
                                   nchar(numbers[nchar(numbers) > 1]),
                                   nchar(numbers[nchar(numbers) > 1])))
    #filter NAs
    lastdigit <- na.omit(lastdigit)
    
    dot.locations <- regexpr(".", numbers, fixed = T)
    
    # for integers, the dot.location will be returned as -1
    #   those should have NA for the decimal portion
    decimal.vals <- ifelse(dot.locations == -1,
                           NA,
                           as.numeric(substr(
                             numbers,
                             dot.locations + 1,
                             nchar(numbers)
                           )))
    
    decimal.vals <- na.omit(decimal.vals)
    
# server: output hist first digit ------------------------------------------------------------------

    output$histFirstDigits <- renderPlot({
      # get max y
      maxy <- max(table(firstdigit))
      
      hist(
        firstdigit,
        freq = T,
        breaks = seq(0.5, 9.5, 1),
        xaxt = "n",
        ylim = c(0, max(.35 * length(firstdigit), maxy)),
        main = "",
        xlab = "Value",
        ylab = "Count"
      )
      
      axis(
        side = 1,
        at = seq(1, 10, 1),
        labels = seq(1, 10, 1),
        cex.lab = 1.4
      )
      x <- 1:9
      y <- log(1 + (1 / x), base = 10) * length(firstdigit)
      points(x, y, "b")
      
    })
    
# server: output chisquare first digit -------------------------------------------------------------    
    
    output$chiFirstDigits <- renderPrint({
      firstdigitFreqs <- counttarget(firstdigit, target = 1:9)
      
      # expected proportions (under Benford's law)
      expected  <- log(1 + (1 / (1:9)), base = 10)
      
      if (input$firstChiType == "Monte Carlo") {
        firstreps <- input$firstChiReps
        
        validate(need(
          isTRUE(all.equal(
            firstreps, as.integer(firstreps)
          )),
          "The number of repetitions must be an integer"
        ))
        
        chisq.test(
          firstdigitFreqs,
          simulate.p.value = TRUE,
          p = expected,
          B = firstreps
        )
      } else if (input$firstChiType == "Pearson") {
        chisq.test(firstdigitFreqs, p = expected)
      }
      
    })
    
# server: output hist last digit--------------------------------------------------------------------    
    
    output$histLastDigits <- renderPlot({
      #if (input$ignoreZero == FALSE) {
      hist(
        lastdigit,
        freq = T,
        breaks = seq(-0.5, 9.5, 1),
        main = "",
        xlab = "Value",
        ylab = "Count"
      )
      
      axis(
        side = 1,
        at = seq(0, 10, 1),
        labels = seq(0, 10, 1),
        cex.lab = 1.4
      )
      
      x = 0:9
      y = rep(length(lastdigit) / 10, times = length(x))
      points(x, y, "b")
      
    })
    
# server: output chisquare first digit -------------------------------------------------------------
    
    output$chiLastDigits <- renderPrint({
      if (input$ignoreZero == FALSE) {
        lastdigitFreqs <- counttarget(lastdigit, target = 0:9)
        
        # expected proportions (under uniform)
        expected  <- log(1 + (1 / (lastdigitFreqs)), base = 10)
      }
      
      if (input$ignoreZero == TRUE) {
        lastdigitFreqs <- counttarget(lastdigit, target = 1:9)
        
        # expected proportions (under uniform)
        expected  <- log(1 + (1 / (lastdigitFreqs)), base = 10)
      }
      
      if (input$lastChiType == "Monte Carlo") {
        lastreps <- input$lastChiReps
        
        validate(need(
          isTRUE(all.equal(lastreps, as.integer(lastreps))),
          "The number of repetitions must be an integer"
        ))
        
        chisq.test(lastdigitFreqs,
                   simulate.p.value = TRUE,
                   B = input$lastChiReps)
        
      } else if (input$lastChiType == "Pearson") {
        chisq.test(lastdigitFreqs)
        
      }
      
    })
    
    spacing.constant = 18
    
# server: output hist all numbers----- -------------------------------------------------------------
    
    output$histAllNumbers <- renderPlot({
      # get max to nearest 5 greater than value
      max.to.plot <- mceiling(max(table(current.numbers())),
                              5)
      
      ggplot(data = data.frame(current.numbers()), aes(x = factor(current.numbers()))) +
        geom_bar(fill = "grey80", color = "gray10") +
        coord_flip() +
        theme_bw() +
        theme(
          axis.text = element_text(size = 15),
          axis.title = element_text(size = 15),
          title = element_text(size = 20)
        ) +
        labs(x = "", y = "Count") +
        scale_y_continuous(
          limits = c(0, max.to.plot),
          breaks = seq(0, max.to.plot, by = 5),
          minor_breaks = 0:max.to.plot
        )
    }, height = function() {
      max(length(table(current.numbers())) * spacing.constant,
          600)
    })
    
# server: output hist all decimals------------------------------------------------------------------
    
    output$histAllDecimals <- renderPlot({
      validate(need(nrow(data.frame(decimal.vals)) > 0, "There are no decimal values"))
      
      # get max to nearest 5 greater than value
      max.to.plot <- mceiling(max(table(decimal.vals)),
                              5)
      
      decimal.vals <- factor(na.omit(decimal.vals))
      levels(decimal.vals) <- paste0(".", levels(decimal.vals))
      
      ggplot(data = data.frame(decimal.vals), aes(x = decimal.vals)) +
        geom_bar(fill = "grey80", color = "gray10") +
        coord_flip() +
        theme_bw() +
        theme(
          axis.text = element_text(size = 15),
          axis.title = element_text(size = 15),
          title = element_text(size = 20)
        ) +
        labs(x = "", y = "Count") +
        scale_y_continuous(
          limits = c(0, max.to.plot),
          breaks = seq(0, max.to.plot, by = 5),
          minor_breaks = 0:max.to.plot
        )
      
    }, height = function() {
      max(length(table(decimal.vals)) * spacing.constant, 500)
    })
    
# server: output hist select decimals---------------------------------------------------------------
    
    output$histDigits <- renderPlot({
      max.to.plot <-
        mceiling(max(table(decimal.vals[nchar(decimal.vals) == input$decimals])), 5)
      
      digits <- data.frame(decimal.vals) %>%
        filter(nchar(decimal.vals) == input$decimals)
      
      validate(need(
        nrow(digits) > 0,
        "There are no numbers with the specified number of digits"
      ))
      
      digits <- digits %>%
        arrange(decimal.vals) %>%
        group_by(decimal.vals) %>%
        summarise(n = n())
      
      digits$decimal.vals <- factor(digits$decimal.vals)
      levels(digits$decimal.vals) <-
        paste0(".", levels(digits$decimal.vals))
      
      ggplot(data = digits, aes(x = decimal.vals, y = n)) +
        geom_col(fill = "grey80", color = "gray10") +
        theme_bw() +
        theme(axis.text = element_text(size = 15),
              axis.title = element_text(size = 15)) +
        coord_flip() +
        labs(x = "", y = "Count") +
        scale_y_continuous(
          limits = c(0, max.to.plot),
          breaks = seq(0, max.to.plot, by = 5),
          minor_breaks = 0:max.to.plot
        )
    }, height = function() {
      max(length(table(
        filter(
          data.frame(decimal.vals),
          nchar(decimal.vals) == input$decimals
        )
      )) * spacing.constant,
      400)
    })
    
    
  }) # closes observe for file or remove
}

# run the application -----------------------------------------------------------------------------

# Run the application
shinyApp(ui = ui, server = server)