## the app won't run locally because I had to change the paths in tree-tagger-german before uploading to shinyapps.io
# to run the app locally, change pathes in tree-tagger-german and tagger-chunker-German to /home/user/quax-daf/TreeTagger/
# to run the app on shinyapps.io, change pathes in tree-tagger-german to /srv/connect/apps/quax-daf/

library(shiny)
library(tm)
library(data.table)
library(DT)
library(plotly)
library(shinyjs)
library(shinycssloaders)
library(shinyWidgets)
library(wordcloud2)
library(colorspace)
library(stringr)
library(pdftools)



LCC<-readRDS("./data/LCC.Rda") # load corpus
source('./data/Beispiele.R') # load example texts
stopwords<-append(stopwords(kind = "german"), c("dass")) # German stopwords tm package
pseudowords<-scan("./data/pseudowords.txt", character())

# Define UI ----
ui <- fluidPage(
  
  div(id = "sidebarlayout",
      
      titlePanel("QuAX-DaF"), 
      h4(HTML("<b>Qu</b>antitative <b>A</b>nalyse von Te<b>X</b>ten für den <b>DaF</b>-Unterricht <a href='https://daniel-jach.github.io/quax-daf/documentation/quax-daf-documentation.pdf' target='_blank'>(&#10137; Dokumentation)</a> <a href='https://github.com/daniel-jach/quax-daf' target='_blank'>(&#10137; Github Repository)</a>")),
      p("Fremdsprachenlehre trifft Quantitative Linguistik! QuAX-DaF analysiert deutschsprachige Texte mit Methoden der quantitativen Linguistik und erzeugt Material für Übungen zum Textverstehen und Wortschatzerwerb, angepasst an den Vokabelstand Ihrer Lernenden!"),
      
      sidebarLayout(
        
        sidebarPanel(
          
          fileInput('pdfInput', label = "1. Laden Sie hier einen Text als PDF-Datei hoch ...", multiple = TRUE, accept=c('.pdf'), buttonLabel = "Auswählen", placeholder = "Keine Datei ausgewählt"),
          
          textAreaInput("textInput", label = "... oder geben Sie hier einen Text ein.", placeholder = "Hier könnte Ihr Text stehen.", width = "100%", height = "200px"),
          
          helpText("Eine Analyse ist aus statistischen Gründen nur möglich, wenn Ihr Text eine gewisse Länge und Komplexität aufweist. Für eine Demonstration wählen Sie bitte einen Beispieltext aus."),
          
          selectizeInput("exampleInput", label = "... oder wählen Sie hier einen Beispieltext aus.", choices = c("", "Kinderzeitmaschine: Über Ritter¹" = dummyRitter, "Grimms Märchen: Rumpelstilzchen²" = dummyRumpelstilzchen, "Reportage: Zu Hause im Baumhaus³" = dummyBaumhaus, "Anne Franks Tagebuch" = dummyFrank, "Nachtgedanken von Heinrich Heine" = dummyHeine, "Deutschstunde von Siegfried Lenz" = dummyLenz, "Definition: Hermeneutischer Zirkel⁴" = dummyHermeneutik), options = list(placeholder = 'Beispieltexte', onInitialize = I('function() { this.setValue(""); }'))), # empty string in choices to default to placeholder when resetting input
          
          shinyjs::useShinyjs(),
          id = "sidePanel",
          actionButton("resetInput", "Text wieder entfernen"),
          
          br(),
          br(),
          
          tags$b("2."), actionButton("go", "Analyse beginnen", icon = icon("refresh")),
          useShinyjs(),
          
          
          br(),
          br(),
          
          tags$b("3. Ergebnisse der Analyse"),
          
          br(),
          br(),
          
          sliderInput("cutOff", label = "4. Geben Sie die höchste bekannte Häufigkeitsklasse an.", min = min(LCC$LEMMA_FREQ_CLASS_LCC), max = max(LCC$LEMMA_FREQ_CLASS_LCC), value = 12, step = 1),
          
          tags$b("5."), actionButton("goExe", "Übungsmaterial erzeugen", icon = icon("refresh"))
        ),
        
        mainPanel(
          h4("Ergebnisse der Analyse"),
          htmlOutput("n"),
          br(),
          plotlyOutput("plot") %>% withSpinner(color="#0dc5c1"),
          br(),
          helpText(HTML("Das Diagramm vergleicht den eingegebenen Text mit dem Deutschen. Die Punkte stehen für die Wörter in Ihrem Text. Wenn Sie mit der Maus über einen Punkt fahren, erscheint das entsprechende Wort, zusammen mit seiner Häufigkeitsklasse und seinem Häufigkeitsrang. Je seltener ein Wort im Deutschen vorkommt, umso höher ist seine Klasse bzw. sein Rang und umso dunkler ist der entsprechende Punkt eingefärbt. Jedes Wort wird außerdem in eine Niveaustufe des Gemeinsamen Europäischen Referenzrahmens eingeordnet und der Anteil jeder Stufe am eingegebenen Text ermittelt. Häufige grammatische Wörter, z.B. <i>aber, alle, der, die, mein, weil, würde</i> und <i>zwischen</i>, sind nicht aussagekräftig und wurden deshalb aussortiert. Sie können das Diagramm beliebig anpassen, bewegen und heranzoomen, indem sie in der Legende auf eine Niveaustufe (doppel-)klicken oder eine Funktion aus der Werkzeugleiste auswählen. Versuchen Sie einzuschätzen, welche Wörter im Text Ihren Lernenden schon bekannt sind, und bestimmen Sie die höchste bekannte Häufigkeitsklasse.")))
      )
  ),
  
  div(id = "Material",
      fluidRow(
        column(12,
               h4("Erzeugtes Übungsmaterial"), 
               helpText("Detaillierte Beschreibungen von passenden Übungen finden Sie in der Dokumentation von QuAX-DaF."),
               br(),
               h4("Vor dem Lesen: Wortwolke"),
               helpText(HTML("In der folgenden Wortwolke sind Wörter umso größer abgebildet, je häufiger sie in Ihrem Text vorkommen. Seltene Wörter, die Ihren Lernende oder QuAX-DaF unbekannt sind, werden nicht abgebildet.")),
               wordcloud2Output("wordcloud") %>% withSpinner(color="#0dc5c1"),
               br(),
               h4("Beim Lesen: Markierter Text"),
               helpText(HTML("In der folgenden Version Ihres Textes sind seltene Wörter, die Ihren Lernenden vermutlich unbekannt sind, hervorgehoben.")),
               tags$hr(style="border-color: black;"),
               htmlOutput('boldText') %>% withSpinner(color="#0dc5c1"),
               tags$hr(style="border-color: black;"),
               br(),
               h4("Nach dem Lesen: Falsche Fuffziger"),
               helpText(HTML("Die folgende Tabelle enthält zehn Wörter aus dem Text, die Ihren Lernenden vermutlich unbekannt sind, zehn bekannte Textwörter und zwanzig Pseudowörter (&bdquo;Falsche Fuffziger&ldquo;). Wenn der Text keine unbekannten Wörter enthält, wird nichts angezeigt.")),
               tableOutput("rogueWordsExe"),
               br(),
               h4("Übersichtstabelle"),
               helpText(HTML("Die Tabelle fasst einige relevante Werte der Wörter in Ihrem Text zusammen. Sie können die Tabelle nach bestimmten Einträgen durchsuchen (Suchen-Eingabe) und die Spalten absteigend oder aufsteigend sortieren. Abkürzungen: n = Häufigkeit im Text, f = Häufigkeit im Deutschen in einer Million Wörtern, Rang = Häufigkeitsrang im Deutschen, Klasse = Häufigkeitsklasse im Deutschen, GER = Gemeinsamer Europäischer Referenzrahmen für Sprachen.")),
               br(),
               downloadButton("Tabelle.csv", "Tabelle herunterladen"),
               br(),
               br(),
               DTOutput("table") %>% withSpinner(color="#0dc5c1") 
        ))
  ) %>% shinyjs::hidden(), 
  helpText("Die Häufigkeiten basieren auf den Korpora der Leipzig Corpora Collection (D. Goldhahn, T. Eckart & U. Quasthoff: Building Large Monolingual Dictionaries at the Leipzig Corpora Collection: From 100 to 200 Languages. In: Proceedings of the 8th International Language Ressources and Evaluation (LREC'12), 2012)). Für die Lemmatisierung habe ich TreeTagger verwendet (Helmut Schmid (1995): Improvements in Part-of-Speech Tagging with an Application to German. Proceedings of the ACL SIGDAT-Workshop. Dublin, Irland), heruntergeladen von https://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/ am 1. März 2020)."),
  helpText("¹ https://www.kinderzeitmaschine.de/", "² https://www.grimmstories.com/", "³ https://www.deutschlandfunknova.de", "⁴ http://www.einladung-zur-literaturwissenschaft.de"),
  helpText(HTML("Daniel Jach (Shanghai, China) - Kontakt: danieljach@pm.me - Webseite: <a href='https://daniel-jach.github.io/' target='_blank'>https://daniel-jach.github.io/</a>"))
)




# Define server logic ----
server <- function(input, output, session) {
  
  observeEvent(input$resetInput, {
    shinyjs::reset("sidePanel")
  })
  
  observe({ # example text input
    x<-input$exampleInput
    updateTextInput(session, "textInput", value = paste(x))
  })
  
  # process text
  text<-eventReactive(input$go,
                      {
                        inFile<-input$pdfInput
                        if (is.null(inFile)){
                          input<-input$textInput
                        }else{
                          input<-paste(pdf_text(inFile[[1,"datapath"]]), collapse = " ")
                          input<-gsub("-\n", "", input)
                        }

                        text<-str_trim(input, side = c("both")) # remove double and opening/trailing whitespaces from input
                        text<-str_replace_all(text, "\'", "\'\'") # double single quotation marks to avoid system command error
                        return(text)
                      })
  

  ### analyze text
  inputData<-
    eventReactive(input$go,
                  {
                    input<-text()
                    
                    input<-unlist(str_split(input, "\n")) # split input at line breaks
                    input<-input[which(input != "")] # remove empty lines
                    
                    # permissions for file execution
                    Sys.chmod("./TreeTagger/cmd/tree-tagger-german", mode = "777", use_umask = TRUE)
                    Sys.chmod("./TreeTagger/cmd/utf8-tokenize.perl", mode = "777", use_umask = TRUE)
                    Sys.chmod("./TreeTagger/bin/tree-tagger", mode = "777", use_umask = TRUE)
                    Sys.chmod("./TreeTagger/cmd/filter-german-tags", mode = "777", use_umask = TRUE)
                    
                    # parse each paragraph
                    out<-as.data.frame(matrix(ncol = 3))
                    
                    for(i in 1:length(input)){
                      cmd<-paste("echo '", input[i], "' | ./TreeTagger/cmd/tree-tagger-german", sep = "")
                      parse<-system(cmd, intern = TRUE)
                      parse<-read.table(text = parse, sep = "\t", quote = NULL)
                      out<-rbind(out, parse, c("\n", "\n", "\n"))
                    }
                    
                   input<-out[-c(1,nrow(out)),c(1,3)] # join parsed paragraphs
                    
                    input<-as.data.table(input) # create dt
                    setnames(input, c("TOKEN", "LEMMA"))
                    
                    input$LEMMA[which(input$LEMMA == "<unknown>")]<-NA

                    input<-LCC[input, on = "LEMMA"] # left join input and LCC
                    dt<-as.data.table(table(input$LEMMA[complete.cases(input$LEMMA)])) # count lemmas in input
                    setnames(dt, c("LEMMA", "LEMMA_FREQ_INPUT"))
                    
                    input<-dt[input, on = "LEMMA"]
                    
                    return(input)
                  }
    )
  
  
  
  ### generate plot
  output$plot<-renderPlotly(
    {
      df<-inputData()

      CEFR<-round(table(df$CEFR)/sum(table(df$CEFR))*100,2)
      levels(df$CEFR)<-paste(names(CEFR), " (", CEFR, "%)", sep = "")
      
      df<-df[,-c("TOKEN")]
      df<-df[complete.cases(df)] # remove NAs
      stops<-which(df$LEMMA %in% stopwords)
      if(length(stops) != 0){
        df<-df[-stops,] # remove stopwords
      }
      
      labels<-paste("Lemma:", df$LEMMA, "\n", "Häufigkeitsklasse:", df$LEMMA_FREQ_CLASS_LCC, "\n", "Häufigkeitsrang:", df$LEMMA_FREQ_RANK_LCC, "\n", "GER:", df$CEFR)
      
      df<-unique(df)
      labels<-unique(labels)
      
      plot<-ggplot(df, 
                   aes(
                     x = scale(jitter(LEMMA_FREQ_INPUT)), 
                     y = scale(jitter(LEMMA_FREQ_LCC)), 
                     color = LEMMA_FREQ_CLASS_LCC,
                     shape = CEFR,
                     text = labels)) +
        geom_point(aes(shape = CEFR)) + 
        theme_minimal() +
        scale_color_continuous(name = "",  breaks = c(min(df$LEMMA_FREQ_CLASS_LCC), max(df$LEMMA_FREQ_CLASS_LCC)), labels = c("Häufig", "Selten"), trans = "reverse") +
        scale_shape_manual(name = "", values=c(19:15)) +
        scale_x_continuous("Häufigkeit im Text") +
        scale_y_continuous("Häufigkeit im Deutschen") +
        geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "blue")
      
      ggplotly(plot, tooltip = "text") %>% 
        layout(legend = list(orientation = "v", x = 1, y = .4, itemclick = "toggle"))
    }
  )
  
  ### output n of tokens and types
  output$n<-renderText({
    df<-inputData()
    df<-df[grep("[[:punct:]]|[[:space:]]", df$TOKEN, invert = TRUE),]
    nTokens<-length(df$TOKEN)
    nTypes<-length(unique(df$TOKEN))
    df<-df[which(is.na(df$LEMMA)),]
    unknown<-paste(unique(df$TOKEN), collapse = ", ")
    if(unknown != ""){
      unknown<-paste("<br><br> Folgende Wörter sind QuAX-DaF unbekannt und waren nicht analysierbar: ", unknown, collapse = "")
    }
    paste("Ihr Text enthält ", nTokens, " Wörter (Tokens) und ", nTypes, " verschiedene Wörter (Types). ", "Das Verhältnis von Types zu Tokens in Ihrem Text beträgt ", gsub("\\.", ",", round(nTypes/nTokens,2)), ". Zum Vergleich, das Type-Token-Verhältnis in den &bdquo;Sternchentexten&ldquo; der PASCH-Initiative beträgt 0,40 (B1), 0,45 (B2), 0,49 (B2C1) bzw. 0,55 (C1).", unknown, sep = "")
    
  })
  
  
  # Toggling visability of Material on button click
  observeEvent(input$goExe, {
    shinyjs::show("Material")
  })
  

  ### boldfacing cut off words in text
  boldText<-eventReactive(input$goExe, {
    
    # call input data 
    data<-inputData() 
    data<-data[,c("TOKEN", "LEMMA_FREQ_CLASS_LCC")]

    data$LEMMA_FREQ_CLASS_LCC[grep("[[:punct:]]|\n|[[:space:]]|[[:digit:]]", data$TOKEN)]<-(-Inf) # to prevent boldfacing of punctuation, spaces, and digits
    
    boldText<-ifelse(data$LEMMA_FREQ_CLASS_LCC > input$cutOff | is.na(data$LEMMA_FREQ_CLASS_LCC), paste("<span style='color:blue;font-weight:bold;'>", as.character(data$TOKEN), "</span>", sep = ""), as.character(data$TOKEN)) # boldfacing infrequent words cut-off point or unknown
    boldText<-gsub("\n", "<br>", boldText)
    
    boldText<-paste(boldText, collapse = " ")
    
    boldText<-gsub('\\s(?=[\\,\\?\\.\\!\\:\\;])', "", boldText, perl = TRUE) # remove whitespace before punctuation
    
    
    return(boldText)
    
  })
  
  output$boldText<-renderText({return(boldText())})
  
  
    ### rogue words exercise
  rogueWordsExe<-eventReactive(input$goExe, {
    df<-unique(inputData())
    cutWords<-as.vector(df[df$LEMMA_FREQ_CLASS_LCC > input$cutOff,]$TOKEN)
    knownWords<-as.vector(df[df$LEMMA_FREQ_CLASS_LCC <= input$cutOff,]$TOKEN)
    if(length(cutWords)==0){return(NULL)}
    if(length(cutWords)>10){
      cutWords<-sample(cutWords, 10, replace = FALSE)
      }
    rogueWords<-sample(pseudowords, 20, replace = FALSE)
    knownWords<-sample(knownWords, 10, replace = TRUE)
    rogueWordsExe<-append(cutWords, append(rogueWords, knownWords))
    rogueWordsExe<-sample(rogueWordsExe) # randomize order
    rogueWordsExe<-data.frame(rogueWordsExe[1:8], rogueWordsExe[9:16], rogueWordsExe[17:24], rogueWordsExe[25:32], rogueWordsExe[33:40])
    rogueWordsExe<-rogueWordsExe[, colSums(is.na(rogueWordsExe)) != nrow(rogueWordsExe)]    
    colnames(rogueWordsExe)<-NULL
    rogueWordsExe
  })
  
  output$rogueWordsExe<-renderTable({return(rogueWordsExe())})
  
  
  ### produce table
  # create table data
  tableData<-eventReactive(input$goExe, {
    df<-inputData()
    df<-df[,c("TOKEN", "LEMMA", "LEMMA_FREQ_INPUT", "LEMMA_FREQ_RELATIVE_LCC", "LEMMA_FREQ_RANK_LCC", "LEMMA_FREQ_CLASS_LCC", "CEFR")]
    df<-df[grep("[[:punct:]]|\n", df$TOKEN, invert = TRUE),] # remove punctuation and linebreaks
    df<-as.matrix(df)
    df[is.na(df)]<-"/"
    df<-as.data.table(df)
    unique(df)
  })
  
  # render data table
  output$table<-renderDT(tableData(), 
                         rownames = FALSE,  
                         colnames = c("Wort", "Lemma", "n", "f", "Rang", "Klasse", "GER"), 
                         options = 
                           list(
                             language = 
                               list(url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/German.json")
                           )
  )
  
  # download table
  output$Tabelle.csv <- downloadHandler(
    filename = "Tabelle.csv",
    content = function(file) {
      write.csv(tableData(), file, row.names = FALSE)
    }
  )
  
  
  ### wordcloud should be last -- otherwise creates problems with reactive events for some reason
  ### wordcloud minus cut off words
  # produce wordcloud data 
  wordcloudData <- eventReactive(input$goExe, { # eventReactive 
    df<-inputData() # load input
    df<-df[df$LEMMA_FREQ_CLASS_LCC <= input$cutOff, c("LEMMA", "LEMMA_FREQ_INPUT")]
    df<-df[-which(df$LEMMA %in% stopwords),] # remove stopwords
    df<-unique(df)
    if(nrow(df)==0){return(NULL)}
    df<-df[order(-df$LEMMA_FREQ_INPUT),] # order by frequency
    df<-df[1:25,] # top25 words
    df[complete.cases(df),]
  })
  
  # create and output wordcloud
  output$wordcloud<-renderWordcloud2({
    df<-wordcloudData() # load wordcloud data
    if(is.null(df)){return(NULL)}
    wordcloud2(df, size = .75, color = sequential_hcl(10,"Blues 2", rev = FALSE), gridSize =  1)
  })
  
}




# Run the app ----
shinyApp(ui = ui, server = server)
