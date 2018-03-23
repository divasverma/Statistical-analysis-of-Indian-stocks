#Loading required libraries
install.packages("rsconnect")
install.packages("shiny")
install.packages("corrplot")
install.packages("e1071")
install.packages("XLConnect")
install.packages("datasets")
install.packages("shinythemes")

library(shiny) #Used for running shiny
library(corrplot) #Used for plotting correlation matrix
library(e1071) #Used for plotting density plot
library(rsconnect) #Used for displaying the app 


stocks <- function() {
  #Loading the csv files for all the stocks and index
  data_gail=read.csv("./gail.csv")
  data_infy=read.csv("./infy.csv")
  data_itc=read.csv("./itc.csv")
  data_hdfc=read.csv("./hdfc.csv")
  data_maruti=read.csv("./maruti.csv")
  data_ongc=read.csv("./ongc.csv")
  data_ranbaxy=read.csv("./ranbaxy.csv")
  data_reliance=read.csv("./reliance.csv")
  data_sbin=read.csv("./sbin.csv")
  data_tcs=read.csv("./tcs.csv")
  data_nifty=read.csv("./nifty.csv")
  data_it=read.csv("./it.csv")
  data_energy=read.csv("./energy.csv")
  data_finance=read.csv("./finance.csv")
  data_fmcg=read.csv("./fmcg.csv")
  data_bank=read.csv("./bank.csv")
  data_auto=read.csv("./auto.csv")
  
  #Calculating the log returns for the stocks
  data_gail$log_return=log(data_gail$Close/data_gail$Open)
  data_infy$log_return=log(data_infy$Close/data_infy$Open)
  data_itc$log_return=log(data_itc$Close/data_itc$Open)
  data_hdfc$log_return=log(data_hdfc$Close/data_hdfc$Open)
  data_maruti$log_return=log(data_maruti$Close/data_maruti$Open)
  data_ongc$log_return=log(data_ongc$Close/data_ongc$Open)
  data_ranbaxy$log_return=log(data_ranbaxy$Close/data_ranbaxy$Open)
  data_reliance$log_return=log(data_reliance$Close/data_reliance$Open)
  data_sbin$log_return=log(data_sbin$Close/data_sbin$Open)
  data_tcs$log_return=log(data_tcs$Close/data_tcs$Open)
  data_nifty$log_return=log(data_nifty$Close/data_nifty$Open)
  data_it$log_return=log(data_it$Close/data_it$Open)
  data_energy$log_return=log(data_energy$Close/data_energy$Open)
  data_finance$log_return=log(data_finance$Close/data_finance$Open)
  data_fmcg$log_return=log(data_fmcg$Close/data_fmcg$Open)
  data_bank$log_return=log(data_bank$Close/data_bank$Open)
  data_auto$log_return=log(data_auto$Close/data_auto$Open)
  
  #Select data for a year
  data_gail=data_gail[0:253,]
  data_infy=data_infy[0:253,]
  data_itc=data_itc[0:253,]
  data_hdfc=data_hdfc[0:253,]
  data_maruti=data_maruti[0:253,]
  data_ongc=data_ongc[0:253,]
  data_ranbaxy=data_ranbaxy[0:253,]
  data_reliance=data_reliance[0:253,]
  data_sbin=data_sbin[0:253,]
  data_tcs=data_tcs[0:253,]
  data_nifty=data_nifty[0:253,]
  data_it=data_it[0:253,]
  data_energy=data_energy[0:253,]
  data_finance=data_finance[0:253,]
  data_fmcg=data_fmcg[0:253,]
  data_bank=data_bank[0:253,]
  data_auto=data_auto[0:253,]
  
  log_returns=data.frame(data_tcs$log_return,data_sbin$log_return,data_reliance$log_return,data_ranbaxy$log_return,data_ongc$log_return,
               data_maruti$log_return,data_infy$log_return,data_hdfc$log_return,data_gail$log_return,data_itc$log_return,
               data_nifty$log_return,data_it$log_return,data_energy$log_return,data_finance$log_return,data_fmcg$log_return,
               data_bank$log_return,data_auto$log_return
               )
  colnames(log_returns) = c("TCS", "SBIN", "RELIANCE", "RANBAXY", "ONGC", "MARUTI", "INFY", "HDFC", "GAIL", "ITC","NIFTY","IT","ENERGY","FINANCE","FMCG","BANK","AUTO") 
  return(log_returns) #returns the log returns for all stocks and index
}

#creating an array of ticker names 
stock_names = as.character( c("TCS", "SBIN", "RELIANCE", "RANBAXY", "ONGC", "MARUTI", "INFY", "HDFC", "GAIL", "ITC", "NIFTY", "IT", "ENERGY", "FINANCE", "FMCG", "BANK", "AUTO" ))
  
#storing the log returns for all the stocks in a dataframe
log_returns = stocks()

shinyServer(function(input, output) 
 {  #Finding the CI for mean
    output$CI_Mean_Stock = renderText({
    user_input = strtoi(input$select) #converting the user input from string to integer to point to the correct stock
    stock_one = log_returns[ , user_input] #fetching data for the selected stock
    n = length(stock_one)
    alpha = input$CI #using the selected confidence interval
    Sample_std = sqrt(var(stock_one)) #calculating std dev for the sample
    Critical_value = qt(.5 + alpha / 200, df = n - 1) #t test
    lower = round(mean(stock_one) - Critical_value * Sample_std / sqrt(n) , digits = 6) #lower bound for CI
    upper = round(mean(stock_one) + Critical_value * Sample_std / sqrt(n), digits = 6) #upper bound for CI
    paste("Confidence interval of the mean: [", lower, ",", upper, "]")
  })
  
    #Finding the CI for variance
    output$CI_Var_Stock = renderText({
    user_input = strtoi(input$select)
    stock_one = log_returns[, user_input]
    n = length(stock_one)
    alpha = input$CI
    Sample_std = sqrt(var(stock_one))
    chisqlower = qchisq(c(.5 + alpha / 200), df = n - 1) 
    chisqupper = qchisq(c(.5 - alpha / 200), df = n - 1)
    lower = round(((n - 1) * Sample_std ^ 2 / chisqlower), digits = 6) #Lower bound for CI
    upper = round(((n - 1) * Sample_std ^ 2 / chisqupper), digits = 6) #Upper bound for CI
    paste("Confidence interval of the variance: [",lower,",",upper,"]")
    })
  
    #Plotting the correlation matrix for all stocks
    output$correlation = renderPlot({
      colnames(log_returns) = stock_names[1:10]
      correl = cor(log_returns[1:10]) 
      corrplot(correl,method="color") #plotting the correlation plot
      paste("Correlation between all the stocks")
    })
    
    #Histogram plotting
    output$histogram = renderPlot({
    user_input = strtoi(input$select)
    stock_one    = log_returns[, user_input]
    n = length(stock_one)
    Sample_std = sqrt(var(stock_one))
    hist(stock_one,density = 20,breaks = 16,main = stock_names[user_input],xlab = "Daily Log returns") #plotting the histogram
    curve(dnorm(x,mean(stock_one),Sample_std),col = 'darkblue',lwd = 2, add = TRUE) #plotting the normal curve
    })
    
  
    output$qq = renderPlot({
      user_input = strtoi(input$select_6)
      stock = log_returns[ , user_input]
      qqnorm(stock)
      qqline(stock , col = 4)
    }) 

    output$dp = renderPlot({
      user_input = strtoi(input$select_7)
      stock = log_returns[ , user_input]
      plot(density(stock), main="Density Plot", ylab="Frequency", sub=paste("Skewness-", round(e1071::skewness(stock), 2)))  
      polygon(density(stock), col="blue")
    }) 
    
  
    #Linear Regression of a stock against time as the independent variable.
    output$StockDailyReg = renderPlot({
    user_input = strtoi(input$select_2)
    stock_one = log_returns[, user_input]
    n = length(stock_one)
    daily = seq(from = 1, to = n, by = 1) #making a dataframe for the number of days for which data is available
    linear_reg_model = lm(stock_one ~ daily) #linear regression with response as the log returns and terms as the days
    plot(daily, stock_one, xlab = "Day", ylab = stock_names[user_input])
    abline(linear_reg_model) #adding thr straightline - trendline
    
    # Residual plot for regression against time
    output$ResidualPlot_1 = renderPlot({
      plot(resid(linear_reg_model),ylim = c(min(resid(linear_reg_model)), max(resid(linear_reg_model))) ,main = "Graph of residuals",xlab = "",ylab = "Residual Plot")
    })
    
    })
    
  
    #Results for the stock against day regression
    output$StockDailyRegResults = renderText({
    user_input = strtoi(input$select_2)
    stock_one = log_returns[, user_input]
    n = length(stock_one)
    daily = seq(from = 1, to = n, by = 1)
    linear_reg_model = lm(stock_one ~ daily)
    plot(daily, stock_one, xlab = "Time(over 251 days)", ylab = stock_names[user_input])
    abline(linear_reg_model)
    paste(
      "Slope of regression line = ",
      round(linear_reg_model$coefficients[2], digits = 8),
      "
      ",
      "Intercept = ",
      round(linear_reg_model$coefficients[1], digits = 8),
      " R^2 of the model = ",
      round(summary(linear_reg_model)$r.squared, digits = 8)
    )
    
    })
  
  
    #Plot for two stocks regression analysis
    output$TwoStockRegression = renderPlot({
    user_input_1 = strtoi(input$select_3)
    user_input_2 = strtoi(input$select_4)
    stock_one = log_returns[, user_input_1]
    stock_two = log_returns[, user_input_2]
    n = length(stock_one)
    linear_reg_model = lm(stock_two ~ stock_one)
    plot( stock_one, stock_two, xlab = stock_names[user_input_1], ylab = stock_names[user_input_2])
    abline(linear_reg_model)
    output$ResidualPlot_2 = renderPlot({
      plot(
            resid(linear_reg_model),
            ylim = c(min(resid(linear_reg_model)), max(resid(linear_reg_model))) ,
            main = "Residuals Plot",
            xlab = "",
            ylab = "Residuals"
         )
    })
    
    })
  
    #Market index analysis
    output$NiftyIndCorr = renderPlot({
    user_input = strtoi(input$select_5)
    stock_one = log_returns[, 'NIFTY']
    stock_two = log_returns[, user_input]
    plot(stock_one, stock_two , xlab = "NIFTY", ylab = stock_names[user_input])
    correlation_coef=cor(stock_one,stock_two)
    })
  
    #Calculating the correlation coefficient between NIFTY and user selected index
    output$NiftyIndResult = renderText({
    x=input$select_5
    user_input = strtoi(input$select_5)
    stock_one = log_returns[, 'NIFTY']
    stock_two = log_returns[, user_input]
    correlation_coef=cor(stock_one,stock_two)
    paste("The correlation coefficient between NIFTY and ", stock_names[user_input]," is", correlation_coef)
    })
  
    #Results for two stock regression analysis
    output$TwoStockResult = renderText({
    user_input_1 = strtoi(input$select_3)
    user_input_2 = strtoi(input$select_4)
    stock_one = log_returns[, user_input_1]
    stock_two = log_returns[, user_input_2]
    n = length(stock_one)
    linear_reg_model = lm(stock_one ~ stock_two)
    paste("Slope of regression fitted line = ",round(linear_reg_model$coefficients[2],digits = 6)," ","Intercept = "
      ,round(linear_reg_model$coefficients[1], digits = 6)
      ," R^2 of the model = ",round(summary(linear_reg_model)$r.squared, digits = 6))
    })
  
    #Testing whether two stock means are same or not 
    output$PairedTest = renderText(
    {
      if (input$select_3 != input$select_4){
        user_input_1 = strtoi(input$select_3)
        user_input_2 = strtoi(input$select_4)
        stock_one = log_returns[,user_input_1]  
        stock_two = log_returns[,user_input_2]
        test = t.test(stock_one,stock_two,paired=TRUE,alternative = "two.sided",conf.level = 0.95)
        paste("Using t-test, Null Hypothesis is rejected for P values higher than ",round(test$p.value *100,digits=3)," %" )
      }
    })
  
})
