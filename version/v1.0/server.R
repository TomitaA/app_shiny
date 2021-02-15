#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readxl)
library(tidyverse)
library(DT)
source('preprocess.R', local = TRUE)
source('load_model.R', local = TRUE)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  observeEvent(input$file1, {
    excel_file <- reactive(read_excel(input$file1$datapath,
                                      col_names = c("area", "shop_code", "shop_name", "shop_name_sub", "shop_form", "post_code", "todohuken", 
                                                    "adress", "opening_date", "floor_num", "area_of_site", "area_of_building", "備考欄", "renewal"),
                                      skip = 1
                                      )  %>%  
      dplyr::mutate(opening_date = as.Date(opening_date),
                    renewal =  as.Date(renewal)) %>% 
      dplyr::mutate(opening_date = as.character(opening_date),
                    renewal = as.character(renewal))
    )
    output$table1 = DT::renderDataTable(excel_file(),
                                        rownames = FALSE,
                                        extensions = 'FixedColumns',
                                        options = list(
                                          dom = 't',
                                          scrollX = TRUE,
                                          scrollCollapse = TRUE))
    }
  )
  
  observeEvent(input$file2, {
    excel_file <- reactive(read_excel(input$file2$datapath))
                                      
    output$table2 = DT::renderDataTable(excel_file(),
                                        rownames = FALSE,
                                        extensions = 'FixedColumns',
                                        options = list(
                                          dom = 't',
                                          scrollX = TRUE,
                                          scrollCollapse = TRUE))
    }
  )
  
  observeEvent(input$file3, {
    excel_file <- reactive(read_excel(input$file3$datapath))
    
    output$table3 = DT::renderDataTable(excel_file(),
                                        rownames = FALSE,
                                        extensions = 'Scroller',# 'FixedColumns',
                                        options = list(
                                          deferRender = TRUE,
                                          dom = 'tfrtiS',
                                          scrollX = TRUE,
                                          scrollY = 200,
                                          scrollCollapse = TRUE))
    }
  )
  
  observeEvent(input$file4, {
    excel_file <- reactive(read_excel(input$file4$datapath))
    
    output$table4 = DT::renderDataTable(excel_file(), 
                                        rownames = FALSE,
                                        extensions = 'FixedColumns',
                                        options = list(
                                          dom = 't',
                                          scrollX = TRUE,
                                          scrollCollapse = TRUE))
    }
  )
  
  
  observeEvent(input$submit, {
    excel_file = reactive(read_excel(input$file1$datapath,
                                     col_names = c("area", "shop_code", "店名", "shop_name_sub", "shop_form", "post_code", "todohuken", 
                                                   "adress", "opening_date", "floor_num", "area_of_site", "area_of_building", "備考欄", "renewal_date"),
                                     skip = 1) %>% 
                            dplyr::mutate(opening_date = as.Date(opening_date),
                                          renewal_date =  as.Date(renewal_date)) %>% 
                            dplyr::mutate(opening_date = as.character(opening_date),
                                          renewal_date = as.character(renewal_date))
    )
    
    shop_base <- excel_file()
    shop_base <- preprocess_shop_base(shop_base)
    
    
    excel_file <- reactive(read_excel(input$file4$datapath))
    shop_faclity <- excel_file()
    
    shop_faclity <- preprocess_shop_facilty(shop_faclity)
    
    
    excel_file <- reactive(read_excel(input$file2$datapath))
    market <- excel_file()
    
    market <- preprocess_market(market)
    
    df <- preprocess_final(shop_base, shop_faclity, market)
    
    predict_table <- load_and_predict(df)
    
    output$predict_1 = DT::renderDataTable(
      predict_table,
      server = FALSE,
      rownames = FALSE,
      extensions = 'Buttons',
      options = list(
        dom = "Blfrtip",
        buttons = list(
          list(extend='csv', filename = paste0(Sys.Date(), '_predict_result')),
          list(extend='excel', filename = paste0(Sys.Date(), '_predict_result')))
      
      )
    )
    
  })
    
})
