library(shiny)
library(tidyverse)
library(rjson)
library(ggvis)
library(dplyr)
library(xml2)
library(ggplot2)
library(readxl)
library(scales)

#####################
# SUPPORT FUNCTIONS #
#####################

# create a function to calculate eight indices, m-score, and final probability
# outputs a list
beneish = function(ticker="MMM") {
  
  is=rjson::fromJSON(file = "income_statement_clean.json")
  bs=rjson::fromJSON(file = "balance_sheet_clean.json")
  cf=rjson::fromJSON(file = "cash_flow_clean.json")
  
  dsri=(bs[[ticker]][['t']][['Net Receivables']]/is[[ticker]][['t']][['Total Revenue']])/
    (bs[[ticker]][['t_1']][['Net Receivables']]/is[[ticker]][['t_1']][['Total Revenue']])
  #dsri=(net receivalbes/sales in t=0)/(net receivables/sales in t=-1)
  
  gmi= ((is[[ticker]][['t_1']][['Total Revenue']]-is[[ticker]][['t_1']][['Cost Of Revenue']])/is[[ticker]][['t']][['Total Revenue']])/
    ((is[[ticker]][['t']][['Total Revenue']]-is[[ticker]][['t']][['Cost Of Revenue']])/is[[ticker]][['t']][['Total Revenue']])
  #gmi=((sales-COGS)/sales)in t=-1/((sales-cogs)/sales) in t=0
  
  aqi= (1-(bs[[ticker]][['t']][['Total Current Assets']]+bs[[ticker]][['t']][['Property Plant Equipment']])/bs[[ticker]][['t']][['Total Assets']])/
    (1-(bs[[ticker]][['t_1']][['Total Current Assets']]+bs[[ticker]][['t_1']][['Property Plant Equipment']])/bs[[ticker]][['t_1']][['Total Assets']])
  #aqi=[1-(current assets+PPE)/Total Assets]in t=0/[1-(current assets+PPE)/Total Assets]in t=-1
  
  sgi= is[[ticker]][['t']][['Total Revenue']]/is[[ticker]][['t_1']][['Total Revenue']]
  #sgi=sales in t=0/sales in t=-1
  
  depi= (cf[[ticker]][['t_1']][['Depreciation']]/(bs[[ticker]][['t_1']][['Property Plant Equipment']]+cf[[ticker]][['t_1']][['Depreciation']]))/
    (cf[[ticker]][['t']][['Depreciation']]/(bs[[ticker]][['t']][['Property Plant Equipment']]+cf[[ticker]][['t']][['Depreciation']]))
  #depi=(depreciation/(PPE+depreciation))in t=-1/(depreciation/(PPE+depreciation))in t=0
  
  sgai= (is[[ticker]][['t']][['Selling General Administrative']]/is[[ticker]][['t']][['Total Revenue']])/
    (is[[ticker]][['t_1']][['Selling General Administrative']]/is[[ticker]][['t_1']][['Total Revenue']])
  #sgai=(SGA/sales)in t=0/(SGA/sales)in t=-1
  
  if(is.null(bs[[ticker]][['t_1']][['Long Term Debt']])) {
    lvgi=((bs[[ticker]][['t']][['Total Current Liabilities']]+bs[[ticker]][['t']][['Long Term Debt And Capital Lease Obligation']])/bs[[ticker]][['t']][['Total Assets']])/
      ((bs[[ticker]][['t_1']][['Total Current Liabilities']]+bs[[ticker]][['t_1']][['Long Term Debt And Capital Lease Obligation']])/(bs[[ticker]][['t_1']][['Total Assets']]))
  } else {
    lvgi=((bs[[ticker]][['t']][['Total Current Liabilities']]+bs[[ticker]][['t']][['Long Term Debt']])/(bs[[ticker]][['t']][['Total Assets']]))/
      ((bs[[ticker]][['t_1']][['Total Current Liabilities']]+bs[[ticker]][['t_1']][['Long Term Debt']])/bs[[ticker]][['t_1']][['Total Assets']])
  }
  
  tata= (is[[ticker]][['t']][['Net Income From Continuing Ops']] - cf[[ticker]][['t']][['Total Cash From Operating Activities']]) / bs[[ticker]][['t']][['Total Assets']]
  
  intercept=-4.84
  
  c_dsri=0.92
  c_gmi=0.528
  c_aqi=0.404
  c_sgi=0.892
  c_depi=0.115
  c_sgai=-0.172
  c_lvgi=-0.327
  c_tata=4.679
  
  coefs=c(c_dsri,c_gmi,c_aqi,c_sgi,c_depi,c_sgai,c_lvgi,c_tata)
  vars=c(    dsri, gmi,  aqi,  sgi,  depi,  sgai,  lvgi,  tata)
  
  mscore=intercept+sum(vars*coefs)
  
  prob=pnorm(mscore,mean=0,sd=1,lower.tail = T)
  
  all=data.frame(dsri, gmi,  aqi,  sgi,  depi,  sgai,  lvgi,  tata ,mscore,prob)
  
  return(all)
}

# create a reverse search function to match a ticker
# to the full company name
findtick <- function(company){
  return(as.character(px[px['name'] == company, 1]))
}

is=rjson::fromJSON(file = "income_statement_clean.json")
bs=rjson::fromJSON(file = "balance_sheet_clean.json")
cf=rjson::fromJSON(file = "cash_flow_clean.json")
px = readxl::read_excel("px.xlsx", col_names = TRUE)
px$prob = round(px$prob * 100, 3)
stocks = read.table("stonks.csv", sep = ',', header = TRUE)
stocks$Date = as.Date(stocks$Date)

income_finder  = function(ticker) {
  # start by intialising a vector of years and incomes
  years = c('FY20', 'FY19', 'FY18', 'FY17')
  val = c()
  for (i in names(is[[ticker]])) {
    val = append(val, is[[ticker]][[i]][['Net Income']])
  }
  
  # convert two vectors and create one dataframe
  return(data.frame(years, val))
}

GProfit_finder  = function(ticker) {
  # start by intialising a vector of years and incomes
  years1 = c('FY20', 'FY19', 'FY18', 'FY17')
  val1 = c()
  for (i in names(is[[ticker]])) {
    val1 = append(val1, is[[ticker]][[i]][['Gross Profit']])
  }
  
  # convert two vectors and create one dataframe
  return(data.frame(years1, val1))
}

TRevenue_finder  = function(ticker) {
  # start by intialising a vector of years and incomes
  years2 = c('FY20', 'FY19', 'FY18', 'FY17')
  val2 = c()
  for (i in names(is[[ticker]])) {
    val2 = append(val2, is[[ticker]][[i]][['Total Revenue']])
  }
  
  # convert two vectors and create one dataframe
  return(data.frame(years2, val2))
}

################
# SERVER LOGIC #
################

shinyServer(function(input, output) {
  
  ##############################
  
  observeEvent(input$company, {
    tick=findtick(input$company)
    ben=beneish(tick)
    ms= paste0(
      as.character(format(
        round(
          ben$mscore, 3), nsmall = 3)))
    prob= paste0(
      as.character(format(
        round(
          ben$prob*100, 3), nsmall = 3)), "%")
    dsri = paste0(
      as.character(format(
        round(
          ben$dsri, 3), nsmall = 3)))
    gmi = paste0(
      as.character(format(
        round(
          ben$gmi, 3), nsmall = 3)))
    aqi = paste0(
      as.character(format(
        round(
          ben$aqi, 3), nsmall = 3)))
    depi = paste0(
      as.character(format(
        round(
          ben$depi, 3), nsmall = 3)))
    sgai = paste0(
      as.character(format(
        round(
          ben$sgai, 3), nsmall = 3)))
    lvgi = paste0(
      as.character(format(
        round(
          ben$lvgi, 3), nsmall = 3)))
    sgi = paste0(
      as.character(format(
        round(
          ben$sgi, 3), nsmall = 3)))

    output$mscore1 <- renderValueBox({valueBox(ms, HTML("<b>M-score</b><br>Model output for m-score."), icon = icon("bar-chart-o"), color = "blue")})
    output$prob <- renderValueBox({valueBox(prob, HTML("<b>Probability</b><br>Implied probability of earnings manipulation."), icon = icon("bar-chart-o"), color = "blue")})
    output$dsri <- renderValueBox({valueBox(dsri, HTML("<b>DSRI</b><br>A high DSRI could indicate revenue inflation."), icon = icon("bar-chart-o"), color = "blue")})
    output$gmi <- renderValueBox({valueBox(gmi, HTML("<b>GMI</b><br>A high GMI indicates deteriorating margins."), icon = icon("bar-chart-o"), color = "blue")})
    output$aqi <- renderValueBox({valueBox(aqi, HTML("<b>AQI</b><br>A high AQI could indicate increased cost deferrals."), icon = icon("bar-chart-o"), color = "blue")})
    output$sgi <- renderValueBox({valueBox(sgi, HTML("<b>SGI</b><br>A high SGI indicates declining sales."), icon = icon("bar-chart-o"), color = "blue")})
    output$depi <- renderValueBox({valueBox(depi, HTML("<b>DEPI</b><br>A high DEPI could indicate firms artifically extended assets useful lives."), icon = icon("bar-chart-o"), color = "blue")})
    output$sgai <- renderValueBox({valueBox(sgai, HTML("<b>SGAI</b><br>A high SGAI could indicate lower administrative efficiency, incentivizing earnings inflation."), icon = icon("bar-chart-o"), color = "blue")})
    output$lvgi <- renderValueBox({valueBox(lvgi, HTML("<b>LVGI</b><br>A high LVGI could indicate higher likelihood of defaulting on debt."), icon = icon("bar-chart-o"), color = "blue")})
    
  })

  output$ben_hist <- renderPlot(
    
    px %>%
      filter(sector == input$sector) %>%
      ggplot(aes(prob)) +
      geom_density(aes(y = ..count..), color = "darkblue", fill="lightblue") +
      xlab("Probability of earnings manipulation (%)") + 
      ylab("Number of companies") +
      ggtitle("Distribution of probabilities")
  )
  
  output$stock_price = renderPlot(
    ggplot(stocks,aes(y=get(findtick(input$company)),x=Date,group= 1)) + 
      geom_line() + scale_x_date(labels=date_format("%b-%d-%Y")) + labs(y="Price",title = "STOCK PRICES OVER THE YEARS")
  )
  
  output$net_income = renderPlot(
    ggplot(income_finder(findtick(input$company)), aes(years, val, fill=years)) +
      geom_bar(stat = "identity") +labs(y="Net Income",title = "NET INCOME PER YEAR")+
      scale_y_continuous(labels = comma)+guides(fill=guide_legend(title="YEAR"))
  )
  output$gross_profit = renderPlot(
    ggplot(GProfit_finder(findtick(input$company)), aes(years1, val1, fill=years1)) +
      geom_bar(stat = "identity") +labs(y="Gross Profit",x="Years",title = "GROSS PROFIT PER YEAR")+
      scale_y_continuous(labels = comma)+guides(fill=guide_legend(title="YEAR"))
  )
  output$total_revenue = renderPlot(
    ggplot(TRevenue_finder(findtick(input$company)), aes(years2, val2, fill=years2)) +
      geom_bar(stat = "identity") +labs(y="Total Revenue",x="Years",title = "TOTAL REVENUE PER YEAR")+
      scale_y_continuous(labels = comma)+guides(fill=guide_legend(title="YEAR"))
  )
  
  output$mytable = DT::renderDataTable({
    px %>% 
      filter(sector == input$sector) %>%
      arrange(desc(prob))
  })
})
