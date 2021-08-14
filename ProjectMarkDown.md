---
title: "Beneish M-Score Calculator"
author: "Michael Jonelis"
date: "8/11/2021"
output: html_document
---




# Welcome to the Beneish M-Score Calculator!
Prof. Daniel Beneish created the M-Score as a way to predict earnings manipulation. His model takes data from the Income Statement, Balance Sheet, and Statement of Cash Flows of a company for two consecutive years. The data are combined into eight indices that are used as input variables in a probit model. The output of the model combined with a standard normal distribution can be used to predict the probability that the company has manipulated its earnings in the most recent year.

This app allows you to select from 384 companies from the S&P500 to view their m-scores, probabilities of manipulating earnings, and descriptive graphs about their financials.
Note that the model should not be used on financial institutions, so we've removed them from our data set along with companies that were missing required line-items.

### The Model

Beneish's Model follows the form:  
$M=-4.84+0.92DSRI+0.528GMI+0.404AQI+0.892SGI+0.115DEPI-0.172SGAI+4.679TATA-0.327LVGI$
where:
<!-- **DSRI** is the Daily Sales Receivable Index -->
$DSRI (Daily Sales Receivable Index) =\frac{(Net Receivables_{t}/Sales_{t})}{(Net Receivables_{t-1}/Sales_{t-1})}$
<!-- **GMI** is the Gross Margin Index -->
$GMI (Gross Margin Index) =\frac{((Sales_{t-1}-COGS_{t-1})/Sales_{t-1}}{(Sales_{t}-COGS_{t})/Sales_{t}}$
<!-- **AQI** is the Asset Quality Index -->
$AQI (Asset Quality Index) =\frac{[1-(CurrentAssets_{t}+PPE_{t}+Securities_{t})/TotalAssets_{t}]}{[1-(CurrentAssets_{t-1}+PPE_{t-1}+Securities_{t-1})/TotalAssets_{t-1}]}$
<!-- **SGI** is the Sales Growth Index -->
$SGI (Sales Growth Index) =\frac{Sales_{t}}{Sales_{t-1}}$
<!-- **DEPI** is the Depreciation Index -->
$DePI (Depreciation Index) = \frac{Depreciation_{t-1}/(PPE_{t-1}+Depreciation_{t-1})}{Depreciation_{t}/(PPE_{t}+Depreciation_{t})}$
<!-- **SGAI** is the Selling General and Administration Index -->
$SGAI (Selling, General and Administrative Index) =\frac{(SGA Expense_{t}/Sales_{t})}{(SGA Expense_{t-1}/Sales_{t-1})}$
<!-- **TATA** is Total Accruals to Total Assets -->
$TATA (Total Accruals to Total Assets) =\frac{(NetIncome_{t}-CashFlowsFromOperations_{t}-CashFlowsFromInvestments_{t})}{TotalAssets_{t}}$
<!-- **LVGI** is the Leverage Index -->
$LVGI (Leverage Index) =\frac{[(CurrentLiabilities_{t}+TotalLongTermDebt_{t})/TotalAssets_{t}]}{[(CurrentLiabilities_{t-1}+TotalLongTermDebt_{t-1})/TotalAssets_{t-1}]}$       
   
   
To read Beneish's original paper click below.   
[Beneish M-Score](https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.195.3676&rep=rep1&type=pdf)













