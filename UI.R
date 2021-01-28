library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(data.table)
library(corrplot)
library(tidyr)
library(arules)
library(arulesViz)

cols<-c("blue","red","purple","deepskyblue","orange","darkgreen","darkturquoise","gold","firebrick4","violetred")
ui<-dashboardPage(skin = "purple",
                  dashboardHeader(title="WINTERFELL STORES",titleWidth=1800),
                  dashboardSidebar(
                    sidebarMenu(
                      menuItem("Home", tabName = "home", icon = icon("home")),
                      menuItem("Insights", tabName = "charts", icon=icon("bar-chart-o")),
                      menuItem("MARKET BASKET ANALYSIS", tabName = "fraud_detection", icon = icon("shopping-basket"))
                      
                      
                    )
                  ),
                  dashboardBody(
                    
                    tabItems(
                      # First tab content
                      tabItem(tabName = "home",
                              tags$img(
                                src = "https://miro.medium.com/max/5760/1*DHfQvlMVBaJCHpYmj1kmCw.png",
                                height = "100%",
                                width="100%",
                                alt = "Oopsie")
                              
                      ),
                      
                      
                      
                      tabItem(tabName = "fraud_detection",
                              mainPanel(
                                tabsetPanel(type = "tabs",
                                            tabPanel(id="tab1", title = "APRIORI",
                                                     tags$img(src="ap1.png"),
                                                     tags$img(src="ap2.jpg"),
                                                     tags$img(src="ap3.png"),
                                                     tags$img(src="ap4.png"),
                                                     tags$img(src="ap5.png"),
                                                     tags$img(src="ap6.png"))))),
                              
                      
                      tabItem(tabName="charts",
                              mainPanel(
                                #fluidRow(
                                tabsetPanel(type="tabs",
                                            tabPanel(id="tab1", title="PLOTS", 
                                                     
                                                     
                                                     selectInput("ch", label = h3("Choose one"), 
                                                                 choices = list("click for plots"=0,
                                                                                "maximum sales" = 1,
                                                                                "reorders" = 2,
                                                                                "regular customer" = 3,
                                                                                "sales by hours"=4,
                                                                                "maximum products"=5,
                                                                                "regular customers reorders"=6),
                                                                 selected=0),
                                                     
                                                     
                                                     plotOutput("f_plots")
                                            ),
                                            tabPanel(id="tab2", title="DEPARTMENTS",
                                                     tags$img(src="sales.jpg"),
                                                     tags$img(src="20200418_100818.jpg"),
                                                     tags$img(src="20200418_100648.jpg"),
                                                     tags$img(src="20200418_100531.jpg")
                                            ),
                                            tabPanel(id="tab3",title="Aisles",
                                                     tags$img(src="aisles1.jpg",height=700),
                                                     tags$img(src="aisles2.jpg",height=500)),
                                            tabPanel(id="tab5",title="ORDERS AND REORDERS",
                                                     tags$img(src="or1.jpg",width=500,heigth=500),
                                                     tags$img(src="or2.jpg",width=500,heigth=500),
                                                     tags$img(src="or3,jpg",width=500,heigth=500),
                                                     tags$img(src="or4.jpg",width=500,heigth=500),
                                                     tags$img(src="or5.jpg",width=500,heigth=500)),
                                           
                                            tabPanel(id="tab7",title="PRODUCTS",
                                                     tags$img(src="products1.jpg"),
                                                     tags$img(src="products2.jpg")))
                                
                                
                                
                                
                                
                                
                                
                                
                                # )
                                #h2("charts are not available")
                              )
                              
                      )
                      #verbatimTextOutput("Gokul")
                    )
                  )
)
server= function(input,output){
  #orders_prod=readRDS("C:/Users/GOKUL SUSEENDRAN/Documents/Market Basket Analysis(Working)/orders_products.RDs")
  
  
  hem <- reactive({
    orders=readRDS("C:/Users/GOKUL SUSEENDRAN/Documents/Market Basket Analysis(Working)/orders_products.RDs")
    orders=data.frame(orders)
    max_sales=orders %>% 
      group_by(order_dow) %>% 
      summarise(no_of_sales=n())
    max_sales_df=data.frame(max_sales)
    return(max_sales_df)
    
  })
  output$f_plots <- renderPlot({
    if (input$ch == 1)
    {n <- data.frame(order_dow =c(0:6),no_of_sales = c(6318741,5676830,4234664,3863090,3809579,4238638,4550878))
    ggplot(n, aes(x = order_dow, y = no_of_sales,color="darkred")) + theme_classic() + geom_bar(stat = "identity",fill=sample(cols,1))+geom_text(aes(x = order_dow, y = no_of_sales,label=no_of_sales), position=position_dodge(width=0.9), vjust=-0.25,color="purple")+ ggtitle("MAXIMUM SALES IN A WEEK")+xlab("DAYS IN A WEEK")+ylab("NO OF SALES")+theme(axis.text= element_text(hjust=1,color="firebrick4",face = "bold",size = 11))
    }
    else if(input$ch == 2)
    {
      n1=data.frame(reordered =c(0,1),no_of_customers=c(13400493,19291927))
      ggplot(n1, aes(x = reordered, y = no_of_customers)) + theme_classic() + geom_bar(stat = "identity",fill=sample(cols,1))+geom_text(aes(x = reordered, y = no_of_customers,label=no_of_customers), position=position_dodge(width=0.9), vjust=-0.25,color="purple")+ ggtitle("NO OF PEOPLE REORDER")+xlab("REORDERS")+ylab("NO OF CUSTOMERS")+theme(axis.text= element_text(hjust=1,color="firebrick4",face = "bold",size = 11))
      
    }
    else if(input$ch==3)
    {
      n2=data.frame(user_id=c("201268","129928","164055","186704","137629","176478","108187","33731","182401","4694"),no_of_regular_customers=c(3625,3574,3039,2910,2837,2814,2760,2719,2712,2711))
      ggplot(n2, aes(x = user_id, y = no_of_regular_customers)) + theme_classic() + geom_bar(stat = "identity",fill=sample(cols,10))+geom_text(aes(x = user_id, y =no_of_regular_customers,label=no_of_regular_customers), position=position_dodge(width=0.9), vjust=-0.25,color="purple") + ggtitle("TOP 10 REGULAR CUSTOMERS")+xlab("USER ID'S")+ylab("NO OF TIMES CUSTOMERS VISITED")+theme(axis.text= element_text(hjust=1,color="firebrick4",face = "bold",size = 11))
    }
    else if (input$ch==4)
    {n3=data.frame(order_hour_of_day=c(0:24),no_of_sales=c(220208,117351,70236,52267,53945,88832,293322,897197,1726366,2466999,2780269,2757199,2639260,2639260,2685810,2717228,2688905,2560951,2113108,1658832,1274300,983043,802113,639447,405232))
    ggplot(n3, aes(x = order_hour_of_day, y = no_of_sales)) + theme_classic() + geom_bar(stat = "identity",fill=sample(cols,1))+geom_text(aes(x = order_hour_of_day, y = no_of_sales,label=no_of_sales), position=position_dodge(width = 0.1), vjust=-0.25,angle=35,color="purple")+ggtitle("MAX SALES BY HOURS IN A DAY")+xlab("HOURS IN A DAY")+ylab("NO OF SALES")+theme(axis.text= element_text(hjust=1,color="firebrick4",face = "bold",size = 11))
    }
    else if (input$ch==5)
    {n4=data.frame(product_name=c("Banana","Bag of Organic Bananas","Organic_Strawberries","Organic_Baby_Spinach","Organic_Hass_Avocado","Organic_Avocado","Large_Lemon","Strawberries","Limes","Organic_Whole_Milk"),no_of_products=c( 475014,381704,266401,243328,213412,178129, 155453,144507,141755,138039))
    ggplot(n4, aes(x = product_name, y = no_of_products)) + theme_classic() + geom_bar(stat = "identity",fill=sample(cols,1))+geom_text(aes(x = product_name, y = no_of_products,label=no_of_products), position=position_dodge(width=0.9), vjust=-0.25,color="purple")+ggtitle("SALES OF TOP 10 PRODUCTS")+xlab("PRODUCTS")+ylab("NO OF TIMES SOLD")+theme(axis.text= element_text(angle=50,hjust=1,color="firebrick4",face = "bold",size = 11))
    }
    else if (input$ch==6)
    {n5=data.frame(user_id=c("201268","129928","164055","137629","186704","108187","176478","33731","17738","181991"),no_of_customers_reordered=c( 3194,3085,2634,2560,2533,2446,2334,2256,2204,2188))
    ggplot(n5, aes(x = user_id, y = no_of_customers_reordered)) + theme_classic() + geom_bar(stat = "identity",fill=sample(cols,10))+geom_text(aes(x = user_id, y = no_of_customers_reordered,label=no_of_customers_reordered), position=position_dodge(width=0.9), vjust=-0.25,color="purple")+ggtitle("REGULAR CUSTOMER REORDERS")+xlab("REORDERED CUSTOMERS(USER ID'S)")+ylab("NO OF TIMES CUSTOMERS REORDERED")+theme(axis.text= element_text(hjust=1,color="firebrick4",face = "bold",size = 11))
    }
    
  })
  output$Gokul <- renderImage({
    filename <- normalizePath(file.path(hemanth()))
    #(tags$img(src="20200418_100531.jpg",height=50,width=50))
    list(src=filename,
         height="300px",
         width="300px",
         alt="Oopsie")
    #    # )
    #   # }
    #   
    #   # src=hemanth(),
    #   # contentType="image/"
    #   # width=400, 
    #   # height=300,
    #   # alt="Oopsie"
  }, deleteFile = FALSE)
}
shinyApp(ui=ui,server = server)
