#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(shinythemes)

shiny::shinyUI(
    shiny::navbarPage(title = span(img(src = "logo_blue_2.png",height = 28,width = 85,),"売上予測シミュレーター"),
                      theme = shinytheme("cerulean"),
                      tabPanel("Home",
                               titlePanel(h3("新規店舗の売上予測シミュレーター操作方法")),
                               br(),
                               h4("1. 候補物件の下記データを指定のフォーマットで準備"),
                               h5("    (ア) 店舗基礎データ（様式①）"),
                               h5("    (イ) 商圏データ（様式②）"),
                               h5("    (ウ)	自転車保有率および交通量データ（様式③）"),
                               h5("    (エ)	その他データ（様式④）"),
                               h5("    * 様式については別途送付済のデータフォーマットをご参照ください"),
                               br(),
                               h4("2. 上部タブ[データップロード]をクリックし、サイドバーにある各種データファイルを指定してアップロード"),
                               br(),
                               h4("3. ２のデータが正しくアップロードされたかどうかを、右側の各種データのタブをクリックし確認"),
                               br(),
                               h4("4. 上部タブ[売上予測]をクリックし、サイドバーにある[予測実行]をクリック"),
                               br(),
                               h4("5. [Download]クリックし予測結果を保存"),
                               br(),
                               helpText("サポートデスク問い合わせ先：株式会社マップル　旗持（k-hatamochi@mapple.co.jp）")
                      ),
                      
                      tabPanel("データアップロード",
                               titlePanel(h4("データアップロード")),
                               shiny::sidebarLayout(
                                   shiny::sidebarPanel(
                                       shiny::fileInput(inputId = 'file1', 
                                                        label = '店舗基礎情報データ'
                                       ),
                                       shiny::fileInput(inputId = 'file2', 
                                                        label = '商圏データ'
                                       ),
                                       shiny::fileInput(inputId = 'file3', 
                                                        label = '交通量および自転車保有率調査データ'
                                       ),
                                       shiny::fileInput(inputId = 'file4', 
                                                        label = 'その他店舗データ'
                                       )
                                   ),
                                   shiny::mainPanel(
                                       shiny::tabsetPanel(type = 'tabs',
                                                          shiny::tabPanel('店舗基礎情報', DT::dataTableOutput('table1')),
                                                          shiny::tabPanel('商圏データ', DT::dataTableOutput('table2')),
                                                          shiny::tabPanel('交通量および自転車保有率調査データ', DT::dataTableOutput('table3')),
                                                          shiny::tabPanel('その他店舗データ', DT::dataTableOutput('table4'))
                                       )
                                   )
                               )
                      ),
                      tabPanel("売上予測",
                               titlePanel(h4("売上予測")),
                               shiny::sidebarLayout(
                                   shiny::sidebarPanel(
                                       shiny::actionButton('submit', '予測実行'),
                                       shiny::downloadButton('downloadData', 'Download', position = "right")
                                   ),
                                   shiny::mainPanel(    
                                       shiny::tabsetPanel(type = 'tabs',
                                                          shiny::tabPanel('予測結果', DT::dataTableOutput('predict_1'))
                                       )
                                   )
                               )
                      ),
                      hr(),
                      footer = h5(em("©️ 2021 mapple Inc."), style="text-align:center; font-family: Arial")
    )
)
