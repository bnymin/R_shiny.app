library(shiny)
library(readr)
library(tidyr)
library(ggplot2)
library(lattice)
library(caret)


diabetes <- read_csv("Documents/R ile Veri Analizi/diabetes.csv")
# UI tanımı
###################----------------#################
ui <- fluidPage(
  titlePanel("R SHİNY UYGULAMALARI"),
  ###################----------------#################
  sidebarLayout(    # ekrana tamamına sidebarlyout denir 
    sidebarPanel(
      fileInput("file" , "Bir Dosya Seçiniz" , TRUE ,
                accept = c(".csv", ".xlsx")),   # dosya seçimi bilgisayradan
      
      # Veri seti seçimi
      selectInput("dataset",label = "Bir Veri Seti Seçiniz"
                  , choices = c("iris","Diyabet","cars" , "pressure")),
      
      actionButton("analyze_btn", "Analizi Gerçekleştir"),
      
      
      
    ),
    
    ###################----------------#################
    mainPanel(
      tabsetPanel(
        tabPanel(title = "Veri"
                 ,verbatimTextOutput(outputId = "data_table")),
        
        
        tabPanel("Veri Yapısı", verbatimTextOutput("data_str")),
        
        tabPanel("Analiz Özeti", tableOutput("summary_table")),
        
        tabPanel("Histogram", plotOutput("dplot")),
        
        tabPanel(title = "Degişken İşlemleri ",   # degisken isimleri çıktıları 
                 br(),
                 uiOutput("bagimli"),
                 br(),
                 uiOutput("bagimsiz")),
        tabPanel("Regresyon" , verbatimTextOutput("model")) , 
        
        
        tabPanel("Train-Test" , 
                 br(),
                 uiOutput("obs") , 
                 verbatimTextOutput("new_model")),
        tabPanel("Sonuclar" , icon = icon("poll") , 
                 tableOutput("tahmin")),
       
        
        
        
      )
    )
  )
)







###################----------------#################
# Sunucu fonksiyonu
server <- function(input, output) {
  ###################----------------#################
  rv <- reactiveValues() # hepsini reactive olarak kullanarak kendini tekrar etmyeyen 
  # Veri setini okuma ve tablo çıktısı oluşturma
  output$data_str <- renderPrint({
    req(input$dataset)
    str(get(input$dataset))
  })
  
  ###################----------------#################
  # Veri seti okuma ve öset tablo oluşrutma
  observeEvent(input$analyze_btn, {
    # Veri setini seçme
    dataset <- switch(input$dataset,
                      "iris" = iris,
                      "Diyabet"=diabetes,
                      "cars" = cars,
                      "pressure" = pressure)
    
    # Özet tabloyu oluşturma
    output$summary_table <- renderTable({
      summary(dataset)
      
    })
    ###################----------------#################
  })
  
  # veriyi göstermek için kullanılır 
  # hepsini reactive olarak kullanarak kendini tekrar etmyeyen 
  data <- reactive({    #    serveren içinde gezen reactive olmasını sağlıyoruz
    req(input$dataset)  # Veri seti seçildi mi?
    
    switch(input$dataset,
           "iris" = iris,
           "Diyabet"=diabetes,
           "cars" = cars,
           "pressure" = pressure,
    )
  })
  
  output$data_table <- renderPrint({
    data()
  })
  ###################----------------#################
  # grafik çizdirmek için kulandık
  output$dplot <- renderPlot({
    req(input$dataset)
    
    df <- switch(input$dataset,
                 "iris" = iris,
                 "Diyabet"=diabetes,
                 "cars" = cars,
                 "pressure" = pressure)
    plot(df)
  })
  ###################----------------#################
  ## Regresyon modeli oluşturmak için kullandığımız kısım
  observe({
    rv$Train <- data()
  })
  
  output$bagimli <- renderUI({    # işlemi burda yapıyoruz # userinterfacen içine yazdık
    selectInput("bagimli" , h4("Bağımlı Değişkenler Seçiniz"),
                choices = names(rv$Train),
                selected = names(rv$Train)[1])
  })
  output$bagimsiz <- renderUI({
    checkboxGroupInput("bagimsiz" , h4("Bağımsız Degişkenler Seçiniz"),
                       choices = names(rv$Train)[-1],
                       selected = names(rv$Train)[-1], inline = TRUE)
  })
  output$model <- renderPrint({
    
    veri <- rv$Train
    
    form <- as.formula(paste(input$bagimli, "~", 
                             paste(input$bagimsiz, collapse = "+")))
    
    # form <-   as.formula(paste(names(data())[names(data()) %>% input$bagimli] , "~",
    #             paste(names(data())[names(data()) %>% input$bagimsiz],
    #                     collapse = "+")))
    model <- lm(form , data = veri)  # model fit edilir
    summary(model)
  })
  ###################----------------#################
  output$obs <- renderUI(
    sliderInput("obs" , label = "Test İçin Ayırmak İstediğiniz Yüzdeliğini Seçiniz" , 
                min = 0 , max = 1 , value = 0.8, width = 400)
  )
  
  ###################----------------#################
  
  
  ###  train test olarak yapılan kısımda yaptığımız analiz 
  t_i <- reactive({
    createDataPartition(y=rv$Train[,input$bagimli] , p =input$obs , list = FALSE , times=1)
  })    # obs den gelen yüzde alınır 
  egt <- reactive({    # gelen kısım eğitimiçin ayrılır
    rv$Train[t_i(),]
  })
  test <- reactive({    # tam tersi yani kalan kısım test için ayrılır 
    rv$Train[-t_i(),]
  })
  egt_x <- reactive({
    bagimli <- input$bagimli
    bagimsiz <- input$bagimsiz
    egt() %>%  dplyr::select(-bagimli) %>% dplyr::select(bagimsiz)
  })
  egt_y <- reactive({      #egitim veri setindeki veriler bagımlı bağımsız olarak ayrılır
    bagimli <- input$bagimli
    egt() %>% dplyr::select(bagimli)
  })
  
  test_x <- reactive({
    bagimli <- input$bagimli
    bagimsiz <- input$bagimsiz
    test() %>%  dplyr::select(-bagimli) %>% dplyr::select(bagimsiz)
  })
  test_y <- reactive({
    bagimli <- input$bagimli
    test() %>% dplyr::select(bagimli)
  })
  egt_tum <- reactive({
    data.frame(egt_x() , dv = egt_y())
  })
  new_formul <- reactive({
    as.formula(paste(input$bagimli , paste(input$bagimsiz ,collapse = "+")) , sep = "~")
  })
  output$new_model <- renderPrint({
    veri <- egt_tum()
    egitim_x <- egt_x()
    egitim_y <- egt_y()
    
    # form1 <- as.formula(paste(names(data())[names(data()) %>% input$bagimli] , "~",
    #                                 paste(names(data())[names(data()) %>% input$bagimsiz],
    #                                            collapse = "+")))
    form1 <- as.formula(paste(input$bagimli, "~", 
                              paste(input$bagimsiz, collapse = "+")))
    
    
    model1 <- lm(form1 , data= veri)
    summary(model1)
    
  })
  ###################----------------#################
  
  # new_model_rea ile tahmin soçlarını elde etmek için bir alt kısımda kullandık 
  new_model_rea <- reactive({
    veri <- egt_tum()
    egitim_x <- egt_x()
    egitim_y <- egt_y()
    
    # form1 <- as.formula(paste(names(data())[names(data()) %>% input$bagimli] , "~",
    #                                 paste(names(data())[names(data()) %>% input$bagimsiz],
    #                                            collapse = "+")))
    form1 <- as.formula(paste(input$bagimli, "~", 
                              paste(input$bagimsiz, collapse = "+")))
    
    
    model1 <- lm(form1 , data= veri)
    
    ###################----------------#################
  })
  
  # regresyon ile  sonuçları tahmin etmek için kullandık 
  
  pred <- reactive({
    if (!is.null(new_model_rea()) && !is.null(test_x())) {
      round(predict(new_model_rea(), test_x()), digits = 3)
    } else {
      numeric()
    }
  })
  
  output$tahmin <- renderTable({
    if (!is.null(test_y()) && !is.null(pred())) {
     
      data.frame(
        "Index" = seq_len(nrow(test_y())),
        "Gerçek" = test_y(),
        "Tahmin" = pred()
      )
    
    
    } else {
      data.frame("Index" = integer(), "Gerçek" = numeric(), "Tahmin" = numeric())
    }
    
  })
  
  
  
}

# Uygulamayı başlatma
shinyApp(ui, server)



