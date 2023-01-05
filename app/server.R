library(quantmod)
library(forecast)
library(Metrics)
library(caret)
library(dygraphs)
library(parallel)
library(moments)
library(DT)
library(dplyr)
library(tseries)
library(zoo)
library(data.table)
library(PerformanceAnalytics)
library(lpSolve)
library(psych)
library(e1071)

shinyServer(function(input, output) {
  
  #----------------------------------------------------------------------------------------------------------------
  #----------------------------------------------------------------------------------------------------------------
  #----------------------------------------------------------------------------------------------------------------
  
  #GET SYMBOLS ASSET
  
  BRKB <- getSymbols(
    'BRK-B',
    src = "yahoo",
    from = as.Date("2013-10-01"),
    to = as.Date("2018-11-30"),
    periodicity = "monthly",
    auto.assign = FALSE
  )

  BLCK <- getSymbols(
    'BLK',
    src = "yahoo",
    from = as.Date("2013-10-01"),
    to = as.Date("2018-11-30"),
    periodicity = "monthly",
    auto.assign = FALSE
  )

  AAPL <- getSymbols(
    'AAPL',
    src = "yahoo",
    from = as.Date("2013-10-01"),
    to = as.Date("2018-11-30"),
    periodicity = "monthly",
    auto.assign = FALSE
  )

  MSFT <- getSymbols(
    'MSFT',
    src = "yahoo",
    from = as.Date("2013-10-01"),
    to = as.Date("2018-11-30"),
    periodicity = "monthly",
    auto.assign = FALSE
  )

  MOOY <- getSymbols(
    'MCO',
    src = "yahoo",
    from = as.Date("2013-10-01"),
    to = as.Date("2018-11-30"),
    periodicity = "monthly",
    auto.assign = FALSE
  )

  COCA <- getSymbols(
    'KO',
    src = "yahoo",
    from = as.Date("2013-10-01"),
    to = as.Date("2018-11-30"),
    periodicity = "monthly",
    auto.assign = FALSE
  )

  GSPC <- getSymbols(
    "^GSPC",
    src = "yahoo",
    from = as.Date("2013-10-01"),
    to = as.Date("2018-11-30"),
    periodicity = "monthly",
    auto.assign = FALSE
  )
  
  #----------------------------------------------------------------------------------------------------------------
  #----------------------------------------------------------------------------------------------------------------
  #----------------------------------------------------------------------------------------------------------------
  
  #CARIMENTO DATI PER PRESENTAZIONE 
  #load ("workspace.RData")
  
  #TITOLI PER CALCOLARE IL BETA PER DECIDERE I PESI DEL PORTFOLIO
  BRKB_first_part <- BRKB['2012/201510']
  BLCK_first_part <- BLCK['2012/201510']
  AAPL_first_part <- AAPL['2012/201510']
  MSFT_first_part <- MSFT['2012/201510']
  MOOY_first_part <- MOOY['2012/201510']
  COCA_first_part <- COCA['2012/201510']
  GSPC_first_part <- GSPC['2012/201510']
  
  BRKB_second_part <- BRKB['201510/20181031']
  BLCK_second_part <- BLCK['201510/20181031']
  AAPL_second_part <- AAPL['201510/20181031']
  MSFT_second_part <- MSFT['201510/20181031']
  MOOY_second_part <- MOOY['201510/20181031']
  COCA_second_part <- COCA['201510/20181031']
  GSPC_second_part <- GSPC['201510/20181031']
  
  BRKB_third_part <- BRKB['201811/']
  BLCK_third_part <- BLCK['201811/']
  AAPL_third_part <- AAPL['201811/']
  MSFT_third_part <- MSFT['201811/']
  MOOY_third_part <- MOOY['201811/']
  COCA_third_part <- COCA['201811/']
  GSPC_third_part <- GSPC['201811/']
  
  BRKB <- BRKB_first_part
  BLCK <- BLCK_first_part
  AAPL <- AAPL_first_part
  MSFT <- MSFT_first_part
  MOOY <- MOOY_first_part
  COCA <- COCA_first_part
  GSPC <- GSPC_first_part
  
  BRKB_beta_return <- na.omit(diff(log(BRKB$'BRK-B.Adjusted')))
  BLCK_beta_return <- na.omit(diff(log(BLCK$BLK.Adjusted)))
  AAPL_beta_return <- na.omit(diff(log(AAPL$AAPL.Adjusted)))
  MSFT_beta_return <- na.omit(diff(log(MSFT$MSFT.Adjusted)))
  MOOY_beta_return <- na.omit(diff(log(MOOY$MCO.Adjusted)))
  COCA_beta_return <- na.omit(diff(log(COCA$KO.Adjusted)))
  GSPC_beta_return <- na.omit(diff(log(GSPC$GSPC.Adjusted)))
  
  BRKB_GSPC_beta_portfolio <- cov(BRKB_beta_return, GSPC_beta_return) / var(GSPC_beta_return)
  BLCK_GSPC_beta_portfolio <- cov(BLCK_beta_return, GSPC_beta_return) / var(GSPC_beta_return)
  AAPL_GSPC_beta_portfolio <- cov(AAPL_beta_return, GSPC_beta_return) / var(GSPC_beta_return)
  MSFT_GSPC_beta_portfolio <- cov(MSFT_beta_return, GSPC_beta_return) / var(GSPC_beta_return)
  MOOY_GSPC_beta_portfolio <- cov(MOOY_beta_return, GSPC_beta_return) / var(GSPC_beta_return)
  COCA_GSPC_beta_portfolio <- cov(COCA_beta_return, GSPC_beta_return) / var(GSPC_beta_return)
  
  #----------------------------------------------------------------------------------------------------------------
  #----------------------------------------------------------------------------------------------------------------
  #----------------------------------------------------------------------------------------------------------------
  
  #FORECASTING SVM
  
  svm_function <- function(stock, real_price) {
    
    x <- merge(shift(stock, 1),
               shift(stock, 2),
               shift(stock, 3),
               all = FALSE)
    colnames(x) <- c("shift1","shift2","shift3")
    
    y <- stock
  
    model <- svm(x, y, kernel = "radial")
    
    new_x <- merge(last(x$shift3),
                   last(x$shift1),
                   last(x$shift2),
                   all = FALSE)
    
    new_pred <- predict(model, new_x)
    
    rsme <- RMSE(real_price, new_pred)
    
    returnlist <- c(new_pred, rsme)
    
    return(returnlist)
  } 
  
  BRKB_real <- first(BRKB_third_part$'BRK-B.Adjusted')
  BLCK_real <- first(BLCK_third_part$BLK.Adjusted)
  AAPL_real <- first(AAPL_third_part$AAPL.Adjusted)
  MSFT_real <- first(MSFT_third_part$MSFT.Adjusted)
  MOOY_real <- first(MOOY_third_part$MCO.Adjusted)
  COCA_real <- first(COCA_third_part$KO.Adjusted)
  
  BRKB_svm <- svm_function(BRKB_second_part$'BRK-B.Adjusted', BRKB_real)
  BLCK_svm <- svm_function(BLCK_second_part$BLK.Adjusted, BLCK_real)
  AAPL_svm <- svm_function(AAPL_second_part$AAPL.Adjusted, AAPL_real)
  MSFT_svm <- svm_function(MSFT_second_part$MSFT.Adjusted, MSFT_real)
  MOOY_svm <- svm_function(MOOY_second_part$MCO.Adjusted, MOOY_real)
  COCA_svm <- svm_function(COCA_second_part$KO.Adjusted, COCA_real)
  
  BRKB_pred <- BRKB_svm[1]
  BLCK_pred <- BLCK_svm[1]
  AAPL_pred <- AAPL_svm[1]
  MSFT_pred <- MSFT_svm[1]
  MOOY_pred <- MOOY_svm[1]
  COCA_pred <- COCA_svm[1]
  
  BRKB_rmse <- BRKB_svm[2]
  BLCK_rmse <- BLCK_svm[2]
  AAPL_rmse <- AAPL_svm[2]
  MSFT_rmse <- MSFT_svm[2]
  MOOY_rmse <- MOOY_svm[2]
  COCA_rmse <- COCA_svm[2]
  
  #----------------------------------------------------------------------------------------------------------------
  #----------------------------------------------------------------------------------------------------------------
  #----------------------------------------------------------------------------------------------------------------
  
  #PORTFOLIO MANAGEMENT
  
  BRKB <- BRKB_second_part
  BLCK <- BLCK_second_part
  AAPL <- AAPL_second_part
  MSFT <- MSFT_second_part
  MOOY <- MOOY_second_part
  COCA <- COCA_second_part
  GSPC <- GSPC_second_part

  #SIMPLE RITORNI DI OGNI ASSET
  BRKB_simple_return <- na.omit(diff(BRKB$'BRK-B.Adjusted')/lag(BRKB$'BRK-B.Adjusted'))
  BLCK_simple_return <- na.omit(diff(BLCK$BLK.Adjusted)/lag(BLCK$BLK.Adjusted))
  AAPL_simple_return <- na.omit(diff(AAPL$AAPL.Adjusted)/lag(AAPL$AAPL.Adjusted))
  MSFT_simple_return <- na.omit(diff(MSFT$MSFT.Adjusted)/lag(MSFT$MSFT.Adjusted))
  MOOY_simple_return <- na.omit(diff(MOOY$MCO.Adjusted)/lag(MOOY$MCO.Adjusted))
  COCA_simple_return <- na.omit(diff(COCA$KO.Adjusted)/lag(COCA$KO.Adjusted))
  GSPC_simple_return <- na.omit(diff(GSPC$GSPC.Adjusted)/lag(GSPC$GSPC.Adjusted))
  
  #CC RITORNI DI OGNI ASSET
  BRKB_return <- na.omit(diff(log(BRKB$'BRK-B.Adjusted')))
  BLCK_return <- na.omit(diff(log(BLCK$BLK.Adjusted)))
  AAPL_return <- na.omit(diff(log(AAPL$AAPL.Adjusted)))
  MSFT_return <- na.omit(diff(log(MSFT$MSFT.Adjusted)))
  MOOY_return <- na.omit(diff(log(MOOY$MCO.Adjusted)))
  COCA_return <- na.omit(diff(log(COCA$KO.Adjusted)))
  GSPC_return <- na.omit(diff(log(GSPC$GSPC.Adjusted)))
  
  #PREZZI AL TEMPO T-1
  BRKB_t_1 <- round(as.numeric(first(BRKB$'BRK-B.Adjusted')), digits = 2)
  BLCK_t_1 <- round(as.numeric(first(BLCK$BLK.Adjusted)), digits = 2)
  AAPL_t_1 <- round(as.numeric(first(AAPL$AAPL.Adjusted)), digits = 2)
  MSFT_t_1 <- round(as.numeric(first(MSFT$MSFT.Adjusted)), digits = 2)
  MOOY_t_1 <- round(as.numeric(first(MOOY$MCO.Adjusted)), digits = 2)
  COCA_t_1 <- round(as.numeric(first(COCA$KO.Adjusted)), digits = 2)
  
  #PREZZI AL TEMPO T
  BRKB_t <- round(as.numeric(last(BRKB$'BRK-B.Adjusted')), digits = 2)
  BLCK_t <- round(as.numeric(last(BLCK$BLK.Adjusted)), digits = 2)
  AAPL_t <- round(as.numeric(last(AAPL$AAPL.Adjusted)), digits = 2)
  MSFT_t <- round(as.numeric(last(MSFT$MSFT.Adjusted)), digits = 2)
  MOOY_t <- round(as.numeric(last(MOOY$MCO.Adjusted)), digits = 2)
  COCA_t <- round(as.numeric(last(COCA$KO.Adjusted)), digits = 2)
  
  #VALORE PORTFOLIO AL TEMPO T-1 POSSEDENDO UNA SOLA AZIONE
  sum_price_portfolio_t_1 <- sum(BRKB_t_1, BLCK_t_1, AAPL_t_1, MSFT_t_1, MOOY_t_1, COCA_t_1)
  
  #PESI DEL PORTFOLIO
  BRKB_price_weight <-
    round(BRKB_t_1 / sum_price_portfolio_t_1, digits = 2)
  BLCK_price_weight <-
    round(BLCK_t_1 / sum_price_portfolio_t_1, digits = 2)
  AAPL_price_weight <-
    round(AAPL_t_1 / sum_price_portfolio_t_1, digits = 2)
  MSFT_price_weight <-
    round(MSFT_t_1 / sum_price_portfolio_t_1, digits = 2)
  MOOY_price_weight <-
    round(MOOY_t_1 / sum_price_portfolio_t_1, digits = 2)
  COCA_price_weight <-
    round(COCA_t_1 / sum_price_portfolio_t_1, digits = 2)
  
  BRKB_weight <- BRKB_price_weight + BRKB_GSPC_beta_portfolio
  BLCK_weight <- BLCK_price_weight + BLCK_GSPC_beta_portfolio
  AAPL_weight <- AAPL_price_weight + AAPL_GSPC_beta_portfolio
  MSFT_weight <- MSFT_price_weight + MSFT_GSPC_beta_portfolio
  MOOY_weight <- MOOY_price_weight + MOOY_GSPC_beta_portfolio
  COCA_weight <- COCA_price_weight + COCA_GSPC_beta_portfolio
  
  sum_weigth <-
    sum(BRKB_weight,
        BLCK_weight,
        AAPL_weight,
        MSFT_weight,
        MOOY_weight,
        COCA_weight)
  
  BRKB_weight <- BRKB_weight / sum_weigth
  BLCK_weight <- BLCK_weight / sum_weigth
  AAPL_weight <- AAPL_weight / sum_weigth
  MSFT_weight <- MSFT_weight / sum_weigth
  MOOY_weight <- MOOY_weight / sum_weigth
  COCA_weight <- COCA_weight / sum_weigth
  
  #RISOLUTORE PROBLEMA LINEARE PER IL NUMERO OTTIMO DI AZIONI DA ACQUISTARE PER MASSIMIZZARE IL VALORE DEL PORTFOLIO
  f.obj <-
    c(BRKB_weight,
      BLCK_weight,
      AAPL_weight,
      MSFT_weight,
      MOOY_weight,
      COCA_weight)
  f.con <-
    matrix(
      c(
        BRKB_t_1, BLCK_t_1, AAPL_t_1, MSFT_t_1, MOOY_t_1, COCA_t_1,
        1, 0, 0, 0, 0, 0,
        0, 1, 0, 0, 0, 0,
        0, 0, 1, 0, 0, 0,
        0, 0, 0, 1, 0, 0,
        0, 0, 0, 0, 1, 0,
        0, 0, 0, 0, 0, 1
        
      ),
      nrow = 7,
      byrow = TRUE
    )
  f.dir <- c("<=", ">=", ">=", ">=", ">=", ">=", ">=")
  f.rhs <- c(15000, 12, 12, 12, 12, 12, 12)
  linear_problem = lp("max", f.obj, f.con, f.dir, f.rhs, all.int = T)
  
  #NUMERO OTTIMO DI AZIONI PER OGNI ASSET
  BRKB_shares <- as.integer(linear_problem$solution[1])
  BLCK_shares <- as.integer(linear_problem$solution[2])
  AAPL_shares <- as.integer(linear_problem$solution[3])
  MSFT_shares <- as.integer(linear_problem$solution[4])
  MOOY_shares <- as.integer(linear_problem$solution[5])
  COCA_shares <- as.integer(linear_problem$solution[6])
  
  #VALORE PER OGNI ASSET AL TEMPO T-1
  BRKB_value_t_1 <- (BRKB_t_1 * BRKB_shares)
  BLCK_value_t_1 <- (BLCK_t_1 * BLCK_shares)
  AAPL_value_t_1 <- (AAPL_t_1 * AAPL_shares)
  MSFT_value_t_1 <- (MSFT_t_1 * MSFT_shares)
  MOOY_value_t_1 <- (MOOY_t_1 * MOOY_shares)
  COCA_value_t_1 <- (COCA_t_1 * COCA_shares)
  
  #VALORE PER OGNI ASSET AL TEMPO T
  BRKB_value_t <- (BRKB_t * BRKB_shares)
  BLCK_value_t <- (BLCK_t * BLCK_shares)
  AAPL_value_t <- (AAPL_t * AAPL_shares)
  MSFT_value_t <- (MSFT_t * MSFT_shares)
  MOOY_value_t <- (MOOY_t * MOOY_shares)
  COCA_value_t <- (COCA_t * COCA_shares)
  
  #PROFITTO PER OGNI ASSET AL TEMPO T
  BRKB_profit <- BRKB_value_t - BRKB_value_t_1
  BLCK_profit <- BLCK_value_t - BLCK_value_t_1
  AAPL_profit <- AAPL_value_t - AAPL_value_t_1
  MSFT_profit <- MSFT_value_t - MSFT_value_t_1
  MOOY_profit <- MOOY_value_t - MOOY_value_t_1
  COCA_profit <- COCA_value_t - COCA_value_t_1
  
  #RITORNI PER OGNI ASSET
  BRKB_portfolio_return <- ((BRKB_t - BRKB_t_1) / BRKB_t_1)
  BLCK_portfolio_return <- ((BLCK_t - BLCK_t_1) / BLCK_t_1)
  AAPL_portfolio_return <- ((AAPL_t - AAPL_t_1) / AAPL_t_1)
  MSFT_portfolio_return <- ((MSFT_t - MSFT_t_1) / MSFT_t_1)
  MOOY_portfolio_return <- ((MOOY_t - MOOY_t_1) / MOOY_t_1)
  COCA_portfolio_return <- ((COCA_t - COCA_t_1) / COCA_t_1)
  
  #RITORNO PORTFOLIO
  return_portfolio <- 100 * round(
      (BRKB_portfolio_return * BRKB_weight) +
      (BLCK_portfolio_return * BLCK_weight) +
      (AAPL_portfolio_return * AAPL_weight) +
      (MSFT_portfolio_return * MSFT_weight) +
      (MOOY_portfolio_return * MOOY_weight) +                   
      (COCA_portfolio_return * COCA_weight), digits = 3)
  
  #VALORE DEL PORTFOLIO AL TEMPO T-1
  value_portfolio_t_1 <-
    sum(
      BRKB_value_t_1,
      BLCK_value_t_1,
      AAPL_value_t_1,
      MSFT_value_t_1,
      MOOY_value_t_1,
      COCA_value_t_1)
  
  #VALORE DEL PORTFOLIO AL TEMPO T
  value_portfolio_t <-
    sum(BRKB_value_t,
        BLCK_value_t,
        AAPL_value_t,
        MSFT_value_t,
        MOOY_value_t,
        COCA_value_t)
  
  #PROFITTO DEL PORTFOLIO
  profit_portfolio <-
    round(value_portfolio_t - value_portfolio_t_1, digits = 2)
  
  
  #VALORE PREDETTO PER OGNI ASSET AL TEMPO T + 1
  BRKB_value_pred <- (BRKB_pred * BRKB_shares)
  BLCK_value_pred <- (BLCK_pred * BLCK_shares)
  AAPL_value_pred <- (AAPL_pred * AAPL_shares)
  MSFT_value_pred <- (MSFT_pred * MSFT_shares)
  MOOY_value_pred <- (MOOY_pred * MOOY_shares)
  COCA_value_pred <- (COCA_pred * COCA_shares)
  
  #PROFITTO PREDETTO PER OGNI ASSET AL TEMPO T + 1
  BRKB_profit_pred <- BRKB_value_pred - BRKB_value_t_1
  BLCK_profit_pred <- BLCK_value_pred - BLCK_value_t_1
  AAPL_profit_pred <- AAPL_value_pred - AAPL_value_t_1
  MSFT_profit_pred <- MSFT_value_pred - MSFT_value_t_1
  MOOY_profit_pred <- MOOY_value_pred - MOOY_value_t_1
  COCA_profit_pred <- COCA_value_pred - COCA_value_t_1
  
  #VALORE PREDETTO DEL PORTFOLIO AL TEMPO T + 1
  value_portfolio_pred <-
    round(sum(
      BRKB_value_pred,
      BLCK_value_pred,
      AAPL_value_pred,
      MSFT_value_pred,
      MOOY_value_pred,
      COCA_value_pred), digits = 2)
  
  #VALORE REALE PER OGNI ASSET AL TEMPO T + 1
  BRKB_value_real <- (BRKB_real * BRKB_shares)
  BLCK_value_real <- (BLCK_real * BLCK_shares)
  AAPL_value_real <- (AAPL_real * AAPL_shares)
  MSFT_value_real <- (MSFT_real * MSFT_shares)
  MOOY_value_real <- (MOOY_real * MOOY_shares)
  COCA_value_real <- (COCA_real * COCA_shares)
  
  #PROFITTO REALE PER OGNI ASSET AL TEMPO T + 1
  BRKB_profit_real <- BRKB_value_real - BRKB_value_t_1
  BLCK_profit_real <- BLCK_value_real - BLCK_value_t_1
  AAPL_profit_real <- AAPL_value_real - AAPL_value_t_1
  MSFT_profit_real <- MSFT_value_real - MSFT_value_t_1
  MOOY_profit_real <- MOOY_value_real - MOOY_value_t_1
  COCA_profit_real <- COCA_value_real - COCA_value_t_1
  
  #VALORE REALE DEL PORTFOLIO AL TEMPO T + 1
  value_portfolio_real <-
    round(sum(
      BRKB_value_real,
      BLCK_value_real,
      AAPL_value_real,
      MSFT_value_real,
      MOOY_value_real,
      COCA_value_real), digits = 2)
  
  #DIFFERENZA TRA VALORE REALE E VALORE PREDETTO PER OGNI ASSET
  BRKB_pred_real_diff_value <- abs(BRKB_value_real - BRKB_value_pred)
  BLCK_pred_real_diff_value <- abs(BLCK_value_real - BLCK_value_pred)
  AAPL_pred_real_diff_value <- abs(AAPL_value_real - AAPL_value_pred)
  MSFT_pred_real_diff_value <- abs(MSFT_value_real - MSFT_value_pred)
  MOOY_pred_real_diff_value <- abs(MOOY_value_real - MOOY_value_pred)
  COCA_pred_real_diff_value <- abs(COCA_value_real - COCA_value_pred)
  
  #DIFFERENZA TRA PROFITTO REALE E PROFITTO PREDETTO PER OGNI ASSET
  BRKB_pred_real_diff_profit <- abs(BRKB_profit_real - BRKB_profit_pred)
  BLCK_pred_real_diff_profit <- abs(BLCK_profit_real - BLCK_profit_pred)
  AAPL_pred_real_diff_profit <- abs(AAPL_profit_real - AAPL_profit_pred)
  MSFT_pred_real_diff_profit <- abs(MSFT_profit_real - MSFT_profit_pred)
  MOOY_pred_real_diff_profit <- abs(MOOY_profit_real - MOOY_profit_pred)
  COCA_pred_real_diff_profit <- abs(COCA_profit_real - COCA_profit_pred)
  
  #PROFITTO PREDETTO DEL PORTFOLIO A NOVEMBRE 2018
  profit_pred_portfolio <-
    round(value_portfolio_pred - value_portfolio_t_1, digits = 2)
  
  #PROFITTO REALE DEL PORTFOLIO A NOVEMBRE 2018
  profit_real_portfolio <-
    round(value_portfolio_real - value_portfolio_t_1, digits = 2)
  
  #DIFFERENZA PROFITTO REALE E PROFITTO PREDETTO 
  profit_pred_real_diff <-
    round(profit_pred_portfolio - profit_real_portfolio, digits = 2)
  
  #TABELLA RIASSUNTIVA PORTFOLIO
  output$tablePortfolio = renderTable({
    merged.tables <-
      data.frame(
        ROWS = c(
          'Beta 3 Years',
          "Price t-1",
          "Price t",
          "N Shares",
          "Weights",
          "Value t-1",
          "Value t",
          "Profit",
          "Return %"
        ),
        BRKB = c(
          BRKB_GSPC_beta_portfolio,
          BRKB_t_1,
          BRKB_t,
          as.integer(BRKB_shares),
          BRKB_weight,
          BRKB_value_t_1,
          BRKB_value_t,
          BRKB_profit,
          BRKB_portfolio_return
        ),
        BLCK = c(
          BLCK_GSPC_beta_portfolio,
          BLCK_t_1,
          BLCK_t,
          as.integer(BLCK_shares),
          BLCK_weight,
          BLCK_value_t_1,
          BLCK_value_t,
          BLCK_profit,
          BLCK_portfolio_return
        ),
        AAPL = c(
          AAPL_GSPC_beta_portfolio,
          AAPL_t_1,
          AAPL_t,
          as.integer(AAPL_shares),
          AAPL_weight,
          AAPL_value_t_1,
          AAPL_value_t,
          AAPL_profit,
          AAPL_portfolio_return
        ),
        MSFT = c(
          MSFT_GSPC_beta_portfolio,
          MSFT_t_1,
          MSFT_t,
          as.integer(MSFT_shares),
          MSFT_weight,
          MSFT_value_t_1,
          MSFT_value_t,
          MSFT_profit,
          MSFT_portfolio_return
        ),
        MOOY = c(
          MOOY_GSPC_beta_portfolio,
          MOOY_t_1,
          MOOY_t,
          as.integer(MOOY_shares),
          MOOY_weight,
          MOOY_value_t_1,
          MOOY_value_t,
          MOOY_profit,
          MOOY_portfolio_return
        ),
        COCA = c(
          COCA_GSPC_beta_portfolio,
          COCA_t_1,
          COCA_t,
          as.integer(COCA_shares),
          COCA_weight,
          COCA_value_t_1,
          COCA_value_t,
          COCA_profit,
          COCA_portfolio_return
        )
      )
  })
  
  output$tableForecasting = renderTable({
    merged.tables <-
      data.frame(
        ROWS = c(
          "Pred Nov.",
          "Real Nov.",
          "RMSE",
          "Value Pred t+1",
          "Value Real t+1",
          "Difference Value",
          "Profit Pred t+1",
          "Profit Real t+1",
          "Difference Profit"
        ),
        BRKB = c(
          BRKB_pred,
          BRKB_real,
          BRKB_rmse,
          BRKB_value_pred,
          BRKB_value_real,
          BRKB_pred_real_diff_value,
          BRKB_profit_pred,
          BRKB_profit_real,
          BRKB_pred_real_diff_profit
        ),
        BLCK = c(
          BLCK_pred,
          BLCK_real,
          BLCK_rmse,
          BLCK_value_pred,
          BLCK_value_real,
          BLCK_pred_real_diff_value,
          BLCK_profit_pred,
          BLCK_profit_real,
          BLCK_pred_real_diff_profit
        ),
        AAPL = c(
          AAPL_pred,
          AAPL_real,
          AAPL_rmse,
          AAPL_value_pred,
          AAPL_value_real,
          AAPL_pred_real_diff_value,
          AAPL_profit_pred,
          AAPL_profit_real,
          AAPL_pred_real_diff_profit
        ),
        MSFT = c(
          MSFT_pred,
          MSFT_real,
          MSFT_rmse,
          MSFT_value_pred,
          MSFT_value_real,
          MSFT_pred_real_diff_value,
          MSFT_profit_pred,
          MSFT_profit_real,
          MSFT_pred_real_diff_profit
        ),
        MOOY = c(
          MOOY_pred,
          MOOY_real,
          MOOY_rmse,
          MOOY_value_pred,
          MOOY_value_real,
          MOOY_pred_real_diff_value,
          MOOY_profit_pred,
          MOOY_profit_real,
          MOOY_pred_real_diff_profit
        ),
        COCA = c(
          COCA_pred,
          COCA_real,
          COCA_rmse,
          COCA_value_pred,
          COCA_value_real,
          COCA_pred_real_diff_value,
          COCA_profit_pred,
          COCA_profit_real,
          COCA_pred_real_diff_profit
        )
      )
  })
  
  output$txtPortfolioValue_t_1 <- renderText({
    paste("Valore Portfolio a Ottobre 2015 = ", value_portfolio_t_1, " $")
  })
  
  output$txtPortfolioValue_t <- renderText({
    paste("Valore Portfolio a Ottobre 2018 = ", value_portfolio_t, " $")
  })
  
  output$txtPortfolioReturn <- renderText({
    if (return_portfolio > 0)
      color <- "<span style=\"color:green\">"
    else if (return_portfolio < 0)
      color <- "<span style=\"color:red\">"
    else
      color <- "<span style=\"color:blue\">"
    
    paste("Ritorno Portfolio = ",
          color,
          return_portfolio,
          " %",
          "</span>")
  })
  
  output$txtPortfolioProfit <- renderText({
    if (profit_portfolio > 0)
      color <- "<span style=\"color:green\">"
    else if (profit_portfolio < 0)
      color <- "<span style=\"color:red\">"
    else
      color <- "<span style=\"color:blue\">"
    
    paste("Profitto Portfolio = ",
          color,
          profit_portfolio,
          " $",
          "</span>")
  })
  
  output$txtPortfolioValuePred <- renderText({
    paste("Valore Portfolio Predetto a Novembre 2018 = ", value_portfolio_pred, " $")
  })
  
  output$txtPortfolioValueReal <- renderText({
    paste("Valore Portfolio Reale a Novembre 2018 = ", value_portfolio_real, " $")
  })
  
  output$txtPortfolioProfitPred <- renderText({
    if (profit_pred_portfolio > 0)
      color <- "<span style=\"color:green\">"
    else if (profit_pred_portfolio < 0)
      color <- "<span style=\"color:red\">"
    else
      color <- "<span style=\"color:blue\">"
    
    paste("Profitto Predetto Novembre 2018 = ",
          color,
          profit_pred_portfolio,
          " $",
          "</span>")
  })
  
  output$txtPortfolioProfitReal <- renderText({
    if (profit_real_portfolio > 0)
      color <- "<span style=\"color:green\">"
    else if (profit_real_portfolio < 0)
      color <- "<span style=\"color:red\">"
    else
      color <- "<span style=\"color:blue\">"
    
    paste("Profitto Reale Novembre 2018 = ",
          color,
          profit_real_portfolio,
          " $",
          "</span>")
  })
  
  output$txtPortfolioProfitDifference <- renderText({
    color <- "<span style=\"color:orange\">"
    if (profit_pred_real_diff > 0){
      color <- "<span style=\"color:orange\">"
      message <- "Predizione più alta rispetto alla realtà"
    } 
    else if (profit_pred_real_diff < 0){
      color <- "<span style=\"color:yellow\">"
      message <- "Predizione più bassa rispetto alla realtà"
    }
    else
      message <- "Predizione uguale rispetto alla realtà"
    
    paste("Differenza = ",
          abs(profit_pred_real_diff),
          " $. ",
          color,
          message,
          "</span>")
  })
  
  #----------------------------------------------------------------------------------------------------------------
  #----------------------------------------------------------------------------------------------------------------
  #----------------------------------------------------------------------------------------------------------------
  
  #LISTENER EVENTO SLIDER INPUT CAPITALE INIZIALE
  
  #RISOLUTORE PROBLEMA LINEARE PER IL NUMERO OTTIMO DI AZIONI DA ACQUISTARE PER MASSIMIZZARE IL VALORE DEL PORTFOLIO
  observeEvent(input$sliderCapInit, {
    f.obj <-
      c(BRKB_weight,
        BLCK_weight,
        AAPL_weight,
        MSFT_weight,
        MOOY_weight,
        COCA_weight)
    f.con <-
      matrix(
        c(
          BRKB_t_1, BLCK_t_1, AAPL_t_1, MSFT_t_1, MOOY_t_1, COCA_t_1,
          1, 0, 0, 0, 0, 0,
          0, 1, 0, 0, 0, 0,
          0, 0, 1, 0, 0, 0,
          0, 0, 0, 1, 0, 0,
          0, 0, 0, 0, 1, 0,
          0, 0, 0, 0, 0, 1
          
        ),
        nrow = 7,
        byrow = TRUE
      )
    f.dir <- c("<=", ">=", ">=", ">=", ">=", ">=", ">=")
    f.rhs <- c(input$sliderCapInit, 12, 12, 12, 12, 12, 12)
    linear_problem = lp("max", f.obj, f.con, f.dir, f.rhs, all.int = T)
    
    #NUMERO OTTIMO DI AZIONI PER OGNI ASSET
    BRKB_shares <- as.integer(linear_problem$solution[1])
    BLCK_shares <- as.integer(linear_problem$solution[2])
    AAPL_shares <- as.integer(linear_problem$solution[3])
    MSFT_shares <- as.integer(linear_problem$solution[4])
    MOOY_shares <- as.integer(linear_problem$solution[5])
    COCA_shares <- as.integer(linear_problem$solution[6])
    
    #VALORE PER OGNI ASSET AL TEMPO T-1
    BRKB_value_t_1 <- (BRKB_t_1 * BRKB_shares)
    BLCK_value_t_1 <- (BLCK_t_1 * BLCK_shares)
    AAPL_value_t_1 <- (AAPL_t_1 * AAPL_shares)
    MSFT_value_t_1 <- (MSFT_t_1 * MSFT_shares)
    MOOY_value_t_1 <- (MOOY_t_1 * MOOY_shares)
    COCA_value_t_1 <- (COCA_t_1 * COCA_shares)
    
    #VALORE PER OGNI ASSET AL TEMPO T
    AAPL_value_t <- (AAPL_t * AAPL_shares)
    MSFT_value_t <- (MSFT_t * MSFT_shares)
    BRKB_value_t <- (BRKB_t * BRKB_shares)
    BLCK_value_t <- (BLCK_t * BLCK_shares)
    MOOY_value_t <- (MOOY_t * MOOY_shares)
    COCA_value_t <- (COCA_t * COCA_shares)
    
    #PROFITTO PER OGNI ASSET AL TEMPO T
    BRKB_profit <- BRKB_value_t - BRKB_value_t_1
    BLCK_profit <- BLCK_value_t - BLCK_value_t_1
    AAPL_profit <- AAPL_value_t - AAPL_value_t_1
    MSFT_profit <- MSFT_value_t - MSFT_value_t_1
    MOOY_profit <- MOOY_value_t - MOOY_value_t_1
    COCA_profit <- COCA_value_t - COCA_value_t_1
    
    #RITORNI PER OGNI ASSET
    BRKB_portfolio_return <- ((BRKB_t - BRKB_t_1) / BRKB_t_1)
    BLCK_portfolio_return <- ((BLCK_t - BLCK_t_1) / BLCK_t_1)
    AAPL_portfolio_return <- ((AAPL_t - AAPL_t_1) / AAPL_t_1)
    MSFT_portfolio_return <- ((MSFT_t - MSFT_t_1) / MSFT_t_1)
    MOOY_portfolio_return <- ((MOOY_t - MOOY_t_1) / MOOY_t_1)
    COCA_portfolio_return <- ((COCA_t - COCA_t_1) / COCA_t_1)
    
    #RITORNO PORTFOLIO
    return_portfolio <- 100 * round(
      (BRKB_portfolio_return * BRKB_weight) +
        (BLCK_portfolio_return * BLCK_weight) +
        (AAPL_portfolio_return * AAPL_weight) +
        (MSFT_portfolio_return * MSFT_weight) +
        (MOOY_portfolio_return * MOOY_weight) +                   
        (COCA_portfolio_return * COCA_weight), digits = 3)
    
    #VALORE DEL PORTFOLIO AL TEMPO T-1
    value_portfolio_t_1 <-
      sum(
        BRKB_value_t_1,
        BLCK_value_t_1,
        AAPL_value_t_1,
        MSFT_value_t_1,
        MOOY_value_t_1,
        COCA_value_t_1)
    
    #VALORE DEL PORTFOLIO AL TEMPO T
    value_portfolio_t <-
      sum(BRKB_value_t,
          BLCK_value_t,
          AAPL_value_t,
          MSFT_value_t,
          MOOY_value_t,
          COCA_value_t)
    
    #PROFITTO DEL PORTFOLIO
    profit_portfolio <-
      round(value_portfolio_t - value_portfolio_t_1, digits = 2)
    
    #VALORE PREDETTO PER OGNI ASSET AL TEMPO T + 1
    BRKB_value_pred <- (BRKB_pred * BRKB_shares)
    BLCK_value_pred <- (BLCK_pred * BLCK_shares)
    AAPL_value_pred <- (AAPL_pred * AAPL_shares)
    MSFT_value_pred <- (MSFT_pred * MSFT_shares)
    MOOY_value_pred <- (MOOY_pred * MOOY_shares)
    COCA_value_pred <- (COCA_pred * COCA_shares)
    
    #PROFITTO PREDETTO PER OGNI ASSET AL TEMPO T + 1
    BRKB_profit_pred <- BRKB_value_pred - BRKB_value_t_1
    BLCK_profit_pred <- BLCK_value_pred - BLCK_value_t_1
    AAPL_profit_pred <- AAPL_value_pred - AAPL_value_t_1
    MSFT_profit_pred <- MSFT_value_pred - MSFT_value_t_1
    MOOY_profit_pred <- MOOY_value_pred - MOOY_value_t_1
    COCA_profit_pred <- COCA_value_pred - COCA_value_t_1
    
    #VALORE PREDETTO DEL PORTFOLIO AL TEMPO T + 1
    value_portfolio_pred <-
      round(sum(
        BRKB_value_pred,
        BLCK_value_pred,
        AAPL_value_pred,
        MSFT_value_pred,
        MOOY_value_pred,
        COCA_value_pred), digits = 2)
    
    #VALORE REALE PER OGNI ASSET AL TEMPO T + 1
    BRKB_value_real <- (BRKB_real * BRKB_shares)
    BLCK_value_real <- (BLCK_real * BLCK_shares)
    AAPL_value_real <- (AAPL_real * AAPL_shares)
    MSFT_value_real <- (MSFT_real * MSFT_shares)
    MOOY_value_real <- (MOOY_real * MOOY_shares)
    COCA_value_real <- (COCA_real * COCA_shares)
    
    #PROFITTO REALE PER OGNI ASSET AL TEMPO T + 1
    BRKB_profit_real <- BRKB_value_real - BRKB_value_t_1
    BLCK_profit_real <- BLCK_value_real - BLCK_value_t_1
    AAPL_profit_real <- AAPL_value_real - AAPL_value_t_1
    MSFT_profit_real <- MSFT_value_real - MSFT_value_t_1
    MOOY_profit_real <- MOOY_value_real - MOOY_value_t_1
    COCA_profit_real <- COCA_value_real - COCA_value_t_1
    
    #VALORE REALE DEL PORTFOLIO AL TEMPO T + 1
    value_portfolio_real <-
      round(sum(
        BRKB_value_real,
        BLCK_value_real,
        AAPL_value_real,
        MSFT_value_real,
        MOOY_value_real,
        COCA_value_real), digits = 2)
    
    #DIFFERENZA TRA VALORE REALE E VALORE PREDETTO PER OGNI ASSET
    BRKB_pred_real_diff_value <- abs(BRKB_value_real - BRKB_value_pred)
    BLCK_pred_real_diff_value <- abs(BLCK_value_real - BLCK_value_pred)
    AAPL_pred_real_diff_value <- abs(AAPL_value_real - AAPL_value_pred)
    MSFT_pred_real_diff_value <- abs(MSFT_value_real - MSFT_value_pred)
    MOOY_pred_real_diff_value <- abs(MOOY_value_real - MOOY_value_pred)
    COCA_pred_real_diff_value <- abs(COCA_value_real - COCA_value_pred)
    
    #DIFFERENZA TRA PROFITTO REALE E PROFITTO PREDETTO PER OGNI ASSET
    BRKB_pred_real_diff_profit <- abs(BRKB_profit_real - BRKB_profit_pred)
    BLCK_pred_real_diff_profit <- abs(BLCK_profit_real - BLCK_profit_pred)
    AAPL_pred_real_diff_profit <- abs(AAPL_profit_real - AAPL_profit_pred)
    MSFT_pred_real_diff_profit <- abs(MSFT_profit_real - MSFT_profit_pred)
    MOOY_pred_real_diff_profit <- abs(MOOY_profit_real - MOOY_profit_pred)
    COCA_pred_real_diff_profit <- abs(COCA_profit_real - COCA_profit_pred)
    
    #PROFITTO PREDETTO DEL PORTFOLIO A NOVEMBRE 2018
    profit_pred_portfolio <-
      round(value_portfolio_pred - value_portfolio_t_1, digits = 2)
    
    #PROFITTO REALE DEL PORTFOLIO A NOVEMBRE 2018
    profit_real_portfolio <-
      round(value_portfolio_real - value_portfolio_t_1, digits = 2)
    
    #DIFFERENZA PROFITTO REALE E PROFITTO PREDETTO 
    profit_pred_real_diff <-
      round(profit_pred_portfolio - profit_real_portfolio, digits = 2)
    
    #TABELLA RIASSUNTIVA PORTFOLIO
    output$tablePortfolio = renderTable({
      merged.tables <-
        data.frame(
          ROWS = c(
            'Beta 3 Years',
            "Price t-1",
            "Price t",
            "N Shares",
            "Weights",
            "Value t-1",
            "Value t",
            "Profit",
            "Return %"
          ),
          BRKB = c(
            BRKB_GSPC_beta_portfolio,
            BRKB_t_1,
            BRKB_t,
            as.integer(BRKB_shares),
            BRKB_weight,
            BRKB_value_t_1,
            BRKB_value_t,
            BRKB_profit,
            BRKB_portfolio_return
          ),
          BLCK = c(
            BLCK_GSPC_beta_portfolio,
            BLCK_t_1,
            BLCK_t,
            as.integer(BLCK_shares),
            BLCK_weight,
            BLCK_value_t_1,
            BLCK_value_t,
            BLCK_profit,
            BLCK_portfolio_return
          ),
          AAPL = c(
            AAPL_GSPC_beta_portfolio,
            AAPL_t_1,
            AAPL_t,
            as.integer(AAPL_shares),
            AAPL_weight,
            AAPL_value_t_1,
            AAPL_value_t,
            AAPL_profit,
            AAPL_portfolio_return
          ),
          MSFT = c(
            MSFT_GSPC_beta_portfolio,
            MSFT_t_1,
            MSFT_t,
            as.integer(MSFT_shares),
            MSFT_weight,
            MSFT_value_t_1,
            MSFT_value_t,
            MSFT_profit,
            MSFT_portfolio_return
          ),
          MOOY = c(
            MOOY_GSPC_beta_portfolio,
            MOOY_t_1,
            MOOY_t,
            as.integer(MOOY_shares),
            MOOY_weight,
            MOOY_value_t_1,
            MOOY_value_t,
            MOOY_profit,
            MOOY_portfolio_return
          ),
          COCA = c(
            COCA_GSPC_beta_portfolio,
            COCA_t_1,
            COCA_t,
            as.integer(COCA_shares),
            COCA_weight,
            COCA_value_t_1,
            COCA_value_t,
            COCA_profit,
            COCA_portfolio_return
          )
        )
    })
    
    output$tableForecasting = renderTable({
      merged.tables <-
        data.frame(
          ROWS = c(
            "Pred Nov.",
            "Real Nov.",
            "RMSE",
            "Value Pred t+1",
            "Value Real t+1",
            "Difference Value",
            "Profit Pred t+1",
            "Profit Real t+1",
            "Difference Profit"
          ),
          BRKB = c(
            BRKB_pred,
            BRKB_real,
            BRKB_rmse,
            BRKB_value_pred,
            BRKB_value_real,
            BRKB_pred_real_diff_value,
            BRKB_profit_pred,
            BRKB_profit_real,
            BRKB_pred_real_diff_profit
          ),
          BLCK = c(
            BLCK_pred,
            BLCK_real,
            BLCK_rmse,
            BLCK_value_pred,
            BLCK_value_real,
            BLCK_pred_real_diff_value,
            BLCK_profit_pred,
            BLCK_profit_real,
            BLCK_pred_real_diff_profit
          ),
          AAPL = c(
            AAPL_pred,
            AAPL_real,
            AAPL_rmse,
            AAPL_value_pred,
            AAPL_value_real,
            AAPL_pred_real_diff_value,
            AAPL_profit_pred,
            AAPL_profit_real,
            AAPL_pred_real_diff_profit
          ),
          MSFT = c(
            MSFT_pred,
            MSFT_real,
            MSFT_rmse,
            MSFT_value_pred,
            MSFT_value_real,
            MSFT_pred_real_diff_value,
            MSFT_profit_pred,
            MSFT_profit_real,
            MSFT_pred_real_diff_profit
          ),
          MOOY = c(
            MOOY_pred,
            MOOY_real,
            MOOY_rmse,
            MOOY_value_pred,
            MOOY_value_real,
            MOOY_pred_real_diff_value,
            MOOY_profit_pred,
            MOOY_profit_real,
            MOOY_pred_real_diff_profit
          ),
          COCA = c(
            COCA_pred,
            COCA_real,
            COCA_rmse,
            COCA_value_pred,
            COCA_value_real,
            COCA_pred_real_diff_value,
            COCA_profit_pred,
            COCA_profit_real,
            COCA_pred_real_diff_profit
          )
        )
    })
    
    #PROFITTO DEL PORTFOLIO AL TEMPO T
    profit_portfolio <-
      round(value_portfolio_t - value_portfolio_t_1, digits = 2)
    
    output$txtPortfolioValue_t_1 <- renderText({
      paste("Valore Portfolio a Ottobre 2015 = ",
            value_portfolio_t_1,
            " $")
    })
    
    output$txtPortfolioValue_t <- renderText({
      paste("Valore Portfolio a Ottobre 2018 = ", value_portfolio_t, " $")
    })
    
    output$txtPortfolioReturn <- renderText({
      if (return_portfolio > 0)
        color <- "<span style=\"color:green\">"
      else if (return_portfolio < 0)
        color <- "<span style=\"color:red\">"
      else
        color <- "<span style=\"color:blue\">"
      
      paste("Ritorno Portfolio = ",
            color,
            return_portfolio,
            " %",
            "</span>")
    })
    
    output$txtPortfolioProfit <- renderText({
      if (profit_portfolio > 0)
        color <- "<span style=\"color:green\">"
      else if (profit_portfolio < 0)
        color <- "<span style=\"color:red\">"
      else
        color <- "<span style=\"color:blue\">"
      
      paste("Profitto Portfolio = ",
            color,
            profit_portfolio,
            " $",
            "</span>")
    })
    
    output$txtPortfolioValuePred <- renderText({
      paste("Valore Portfolio Predetto a Novembre 2018 = ", value_portfolio_pred, " $")
    })
    
    output$txtPortfolioValueReal <- renderText({
      paste("Valore Portfolio Reale a Novembre 2018 = ", value_portfolio_real, " $")
    })
    
    output$txtPortfolioProfitPred <- renderText({
      if (profit_pred_portfolio > 0)
        color <- "<span style=\"color:green\">"
      else if (profit_pred_portfolio < 0)
        color <- "<span style=\"color:red\">"
      else
        color <- "<span style=\"color:blue\">"
      
      paste("Profitto Predetto Novembre 2018 = ",
            color,
            profit_pred_portfolio,
            " $",
            "</span>")
    })
    
    output$txtPortfolioProfitReal <- renderText({
      if (profit_real_portfolio > 0)
        color <- "<span style=\"color:green\">"
      else if (profit_real_portfolio < 0)
        color <- "<span style=\"color:red\">"
      else
        color <- "<span style=\"color:blue\">"
      
      paste("Profitto Reale Novembre 2018 = ",
            color,
            profit_real_portfolio,
            " $",
            "</span>")
    })
    
    output$txtPortfolioProfitDifference <- renderText({
      color <- "<span style=\"color:orange\">"
      if (profit_pred_real_diff > 0){
        color <- "<span style=\"color:orange\">"
        message <- "Predizione più alta rispetto alla realtà"
      } 
      else if (profit_pred_real_diff < 0){
        color <- "<span style=\"color:yellow\">"
        message <- "Predizione più bassa rispetto alla realtà"
      }
      else
        message <- "Predizione uguale rispetto alla realtà"
      
      paste("Differenza = ",
            abs(profit_pred_real_diff),
            " $. ",
            color,
            message,
            "</span>")
    })
    
  })
  
  #----------------------------------------------------------------------------------------------------------------
  #----------------------------------------------------------------------------------------------------------------
  #----------------------------------------------------------------------------------------------------------------
  
  #LISTENER EVENTO SELECT INPUT ASSET TO SELL
  
  observeEvent(input$selectAssetToSell, {
    
    if (input$selectAssetToSell == "BRKB")
      return_of_sell <- round(BRKB_value_t - BRKB_value_t_1, digits = 2)
    
    if (input$selectAssetToSell == "BLCK")
      return_of_sell <- round(BLCK_value_t - BLCK_value_t_1, digits = 2)
    
    if (input$selectAssetToSell == "AAPL")
      return_of_sell <- round(AAPL_value_t - AAPL_value_t_1, digits = 2)
    
    if (input$selectAssetToSell == "MSFT")
      return_of_sell <- round(MSFT_value_t - MSFT_value_t_1, digits = 2)
    
    if (input$selectAssetToSell == "MOOY")
      return_of_sell <- round(MOOY_value_t - MOOY_value_t_1, digits = 2)
    
    if (input$selectAssetToSell == "COCA")
      return_of_sell <- round(COCA_value_t - COCA_value_t_1, digits = 2)
    
    if (return_of_sell > 0)
      color <- "<span style=\"color:green\">"
    else if (return_of_sell < 0)
      color <- "<span style=\"color:red\">"
    else
      color <- "<span style=\"color:blue\">"
    
    #FEE VENDITA ASSET 10%
    
    fee <- round(return_of_sell * input$sliderFeeSell, digits = 2)
    
    #PROFITTO VENDITA ASSET
    profit_of_sell <- return_of_sell - fee
    
    output$txtAssetValueSell <- renderText({
      paste(
        "Ricavo vendita asset ",
        input$selectAssetToSell,
        " = ",
        color,
        return_of_sell,
        " $",
        "</span>"
      )
    })
    
    output$txtFeeSell <- renderText({
      color <- "<span style=\"color:red\">"
      paste(
        "Fee vendita asset ",
        input$selectAssetToSell,
        " = ",
        color,
        "-",
        fee,
        " $",
        "</span>"
      )
    })
    
    output$txtProfitSell <- renderText({
      if (profit_of_sell > 0)
        color <- "<span style=\"color:green\">"
      else if (profit_of_sell < 0)
        color <- "<span style=\"color:red\">"
      else
        color <- "<span style=\"color:blue\">"
      
      paste(
        "Profitto vendita asset ",
        input$selectAssetToSell,
        " = ",
        color,
        profit_of_sell,
        " $",
        "</span>"
      )
    })
  })
  
  #----------------------------------------------------------------------------------------------------------------
  #----------------------------------------------------------------------------------------------------------------
  #----------------------------------------------------------------------------------------------------------------
  
  #LISTENER EVENTO SLIDER INPUT FEE
  
  observeEvent(input$sliderFeeSell, {
    if (input$selectAssetToSell == "BRKB")
      return_of_sell <- round(BRKB_value_t - BRKB_value_t_1, digits = 2)
    
    if (input$selectAssetToSell == "BLCK")
      return_of_sell <- round(BLCK_value_t - BLCK_value_t_1, digits = 2)
    
    if (input$selectAssetToSell == "AAPL")
      return_of_sell <- round(AAPL_value_t - AAPL_value_t_1, digits = 2)
    
    if (input$selectAssetToSell == "MSFT")
      return_of_sell <- round(MSFT_value_t - MSFT_value_t_1, digits = 2)
    
    if (input$selectAssetToSell == "MOOY")
      return_of_sell <- round(MOOY_value_t - MOOY_value_t_1, digits = 2)
    
    if (input$selectAssetToSell == "COCA")
      return_of_sell <- round(COCA_value_t - COCA_value_t_1, digits = 2)
    
    if (return_of_sell > 0)
      color <- "<span style=\"color:green\">"
    else if (return_of_sell < 0)
      color <- "<span style=\"color:red\">"
    else
      color <- "<span style=\"color:blue\">"
    
    #FEE VENDITA ASSET 10%
    
    fee <- round(return_of_sell * input$sliderFeeSell, digits = 2)
    
    #PROFITTO VENDITA ASSET
    profit_of_sell <- return_of_sell - fee
    
    output$txtAssetValueSell <- renderText({
      paste(
        "Ricavo vendita asset ",
        input$selectAssetToSell,
        " = ",
        color,
        return_of_sell,
        " $",
        "</span>"
      )
    })
    
    output$txtFeeSell <- renderText({
      color <- "<span style=\"color:red\">"
      paste(
        "Fee vendita asset ",
        input$selectAssetToSell,
        " = ",
        color,
        "-",
        fee,
        " $",
        "</span>"
      )
    })
    
    output$txtProfitSell <- renderText({
      if (profit_of_sell > 0)
        color <- "<span style=\"color:green\">"
      else if (profit_of_sell < 0)
        color <- "<span style=\"color:red\">"
      else
        color <- "<span style=\"color:blue\">"
      
      paste(
        "Profitto vendita asset ",
        input$selectAssetToSell,
        " = ",
        color,
        profit_of_sell,
        " $",
        "</span>"
      )
    })
  })
  
  
  #----------------------------------------------------------------------------------------------------------------
  #----------------------------------------------------------------------------------------------------------------
  #----------------------------------------------------------------------------------------------------------------
  
  #LISTENER EVENTO BUTTON BTN SELL
  
  observeEvent(input$btnSellAsset, {
    assetSell <- NULL
    firstAsset_t_1 <- NULL
    secondAsset_t_1 <- NULL
    thirdAsset_t_1 <- NULL
    fourthAsset_t_1 <- NULL
    fifthAsset_t_1 <- NULL
    
    firstAsset_t <- NULL
    secondAsset_t <- NULL
    thirdAsset_t <- NULL
    fourthAsset_t <- NULL
    fifthAsset_t_1 <- NULL
    
    sum_new_price_portfolio_t_1 <- NULL
    arrayTicker <- NULL
    
    if (input$selectAssetToSell == "BRKB") {
      assetSell <- "BRKB"
      firstAsset_t_1 <- BLCK_t_1
      secondAsset_t_1 <- AAPL_t_1
      thirdAsset_t_1 <- MSFT_t_1
      fourthAsset_t_1 <- MOOY_t_1
      fifthAsset_t_1 <- COCA_t_1
      
      firstAsset_t <- BLCK_t
      secondAsset_t <- AAPL_t
      thirdAsset_t <- MSFT_t
      fourthAsset_t <- MOOY_t
      fifthAsset_t <- COCA_t
      
      sum_new_price_portfolio_t_1 <- sum(BLCK_t_1, AAPL_t_1, MSFT_t_1, MOOY_t_1, COCA_t_1)
      
      arrayTicker = c('BLCK', 'AAPL', 'MSFT', 'MOOY', 'COCA')
    }
    
    if (input$selectAssetToSell == "BLCK") {
      assetSell <- "BLCK"
      firstAsset_t_1 <- BRKB_t_1
      secondAsset_t_1 <- AAPL_t_1
      thirdAsset_t_1 <- MSFT_t_1
      fourthAsset_t_1 <- MOOY_t_1
      fifthAsset_t_1 <- COCA_t_1
      
      firstAsset_t <- BRKB_t
      secondAsset_t <- AAPL_t
      thirdAsset_t <- MSFT_t
      fourthAsset_t <- MOOY_t
      fifthAsset_t <- COCA_t
      
      sum_new_price_portfolio_t_1 <- sum(BRKB_t_1, AAPL_t_1, MSFT_t_1, MOOY_t_1, COCA_t_1)
      
      arrayTicker = c('BRKB', 'AAPL', 'MSFT', 'MOOY', 'COCA')
    }
    
    if (input$selectAssetToSell == "AAPL") {
      assetSell <- "AAPL"
      firstAsset_t_1 <- BRKB_t_1
      secondAsset_t_1 <- BLCK_t_1
      thirdAsset_t_1 <- MSFT_t_1
      fourthAsset_t_1 <- MOOY_t_1
      fifthAsset_t_1 <- COCA_t_1
      
      firstAsset_t <- BRKB_t
      secondAsset_t <- BLCK_t
      thirdAsset_t <- MSFT_t
      fourthAsset_t <- MOOY_t
      fifthAsset_t <- COCA_t
      
      sum_new_price_portfolio_t_1 <- sum(BRKB_t_1, BLCK_t_1, MSFT_t_1, MOOY_t_1, COCA_t_1)
      
      arrayTicker = c('BRKB', 'BLCK', 'MSFT', 'MOOY', 'COCA')
    }
    
    if (input$selectAssetToSell == "MSFT") {
      assetSell <- "MSFT"
      firstAsset_t_1 <- BRKB_t_1
      secondAsset_t_1 <- BLCK_t_1
      thirdAsset_t_1 <- AAPL_t_1
      fourthAsset_t_1 <- MOOY_t_1
      fifthAsset_t_1 <- COCA_t_1
      
      firstAsset_t <- BRKB_t
      secondAsset_t <- BLCK_t
      thirdAsset_t <- AAPL_t
      fourthAsset_t <- MOOY_t
      fifthAsset_t <- COCA_t
      
      sum_new_price_portfolio_t_1 <- sum(BRKB_t_1, BLCK_t_1, AAPL_t_1, MOOY_t_1, COCA_t_1)
      
      arrayTicker = c('BRKB', 'BLCK', 'AAPL', 'MOOY', 'COCA')
    }
    
    if (input$selectAssetToSell == "MOOY") {
      assetSell <- "MOOY"
      firstAsset_t_1 <- BRKB_t_1
      secondAsset_t_1 <- BLCK_t_1
      thirdAsset_t_1 <- AAPL_t_1
      fourthAsset_t_1 <- MSFT_t_1
      fifthAsset_t_1 <- COCA_t_1
      
      firstAsset_t <- BRKB_t
      secondAsset_t <- BLCK_t
      thirdAsset_t <- AAPL_t
      fourthAsset_t <- MSFT_t
      fifthAsset_t <- COCA_t
      
      sum_new_price_portfolio_t_1 <- sum(BRKB_t_1, BLCK_t_1, AAPL_t_1, MSFT_t_1, COCA_t_1)
      
      arrayTicker = c('BRKB', 'BLCK', 'AAPL', 'MSFT', 'COCA')
    }
    
    if (input$selectAssetToSell == "COCA") {
      assetSell <- "COCA"
      firstAsset_t_1 <- BRKB_t_1
      secondAsset_t_1 <- BLCK_t_1
      thirdAsset_t_1 <- AAPL_t_1
      fourthAsset_t_1 <- MSFT_t_1
      fifthAsset_t_1 <- MOOY_t_1
      
      firstAsset_t <- BRKB_t
      secondAsset_t <- BLCK_t
      thirdAsset_t <- AAPL_t
      fourthAsset_t <- MSFT_t
      fifthAsset_t <- MOOY_t
      
      sum_new_price_portfolio_t_1 <- sum(BRKB_t_1, BLCK_t_1, AAPL_t_1, MSFT_t_1, MOOY_t_1)
      
      arrayTicker = c('BRKB', 'BLCK', 'AAPL', 'MSFT', 'MOOY')
    }
    
    firstAsset_weight <-
      round(firstAsset_t_1 / sum_new_price_portfolio_t_1, digits = 2)
    secondAsset_weight <-
      round(secondAsset_t_1 / sum_new_price_portfolio_t_1, digits = 2)
    thirdAsset_weight <-
      round(thirdAsset_t_1 / sum_new_price_portfolio_t_1, digits = 2)
    fourthAsset_weight <-
      round(fourthAsset_t_1 / sum_new_price_portfolio_t_1, digits = 2)
    fifthAsset_weight <-
      round(fifthAsset_t_1 / sum_new_price_portfolio_t_1, digits = 2)
    
    
    #RISOLUTORE PROBLEMA LINEARE PER IL NUMERO OTTIMO DI AZIONI DA ACQUISTARE PER MASSIMIZZARE IL VALORE DEL PORTFOLIO
    f.obj <-
      c(firstAsset_weight,
        secondAsset_weight,
        thirdAsset_weight,
        fourthAsset_weight,
        fifthAsset_weight)
    f.con <-
      matrix(
        c(firstAsset_t_1, secondAsset_t_1, thirdAsset_t_1, fourthAsset_t_1, fifthAsset_t_1,
          1, 0, 0, 0, 0,
          0, 1, 0, 0, 0,
          0, 0, 1, 0, 0,
          0, 0, 0, 1, 0,
          0, 0, 0, 0, 1
        ),
        nrow = 6,
        byrow = TRUE
      )
    f.dir <- c("<=", ">=", ">=", ">=", ">=", ">=")
    f.rhs <- c(input$sliderCapInit, 12, 12, 12, 12, 12)
    linear_problem = lp("max", f.obj, f.con, f.dir, f.rhs, all.int = T)
    
    #NUMERO OTTIMO DI AZIONI PER OGNI ASSET
    firstAsset_shares <- as.integer(linear_problem$solution[1])
    secondAsset_shares <- as.integer(linear_problem$solution[2])
    thirdAsset_shares <- as.integer(linear_problem$solution[3])
    fourthAsset_shares <- as.integer(linear_problem$solution[4])
    fifthAsset_shares <- as.integer(linear_problem$solution[5])
    
    firstTicker <- arrayTicker[1]
    secondTicker <- arrayTicker[2]
    thirdTicker <- arrayTicker[3]
    fourthTicker <- arrayTicker[4]
    fifthTicker <- arrayTicker[5]
    
    #TABELLA RIASSUNTIVA PORTFOLIO
    output$tablePortfolio = renderTable({
      myTable <-
        data.frame(
          ROWS = c("Price t-1",
                   "Price t",
                   "N Shares",
                   "Weights"),
          firstTicker = c(
            firstAsset_t_1,
            firstAsset_t,
            as.integer(firstAsset_shares),
            firstAsset_weight
          ),
          secondTicker = c(
            secondAsset_t_1,
            secondAsset_t,
            as.integer(secondAsset_shares),
            secondAsset_weight
          ),
          thirdTicker = c(
            thirdAsset_t_1,
            thirdAsset_t,
            as.integer(thirdAsset_shares),
            thirdAsset_weight
          ),
          fourthTicker = c(
            fourthAsset_t_1,
            fourthAsset_t,
            as.integer(fourthAsset_shares),
            fourthAsset_weight
          ),
          fifthTicker = c(
            fifthAsset_t_1,
            fifthAsset_t,
            as.integer(fifthAsset_shares),
            fifthAsset_weight
          )
        )
      
    })
  })
  
  
  #----------------------------------------------------------------------------------------------------------------
  #----------------------------------------------------------------------------------------------------------------
  #----------------------------------------------------------------------------------------------------------------
  
  #DATA SUMMARY
  
  #----------------------------------------------------------------------------------------------------------------
  #----------------------------------------------------------------------------------------------------------------
  #----------------------------------------------------------------------------------------------------------------
  
  #LISTENER EVENTO RADIO BUTTON PRICE SUMMARY
  
  observeEvent(input$rbPriceReturnSummary,
               {
                 
                 if (input$rbPriceReturnSummary == 1) {
                   choice <- "Price"
                   
                   output$plotDataSummary1 <- renderDygraph({
                     dygraph(BRKB$'BRK-B.Adjusted', main = paste("BERKSHIRE ", choice)) %>%
                       dyAxis("y", label = choice) %>%
                       dyAnnotation("2018-1-1", text = "D", tooltip = "Utili 2017 in calo") %>%
                       dyOptions(
                         stackedGraph = TRUE,
                         axisLabelColor = "white",
                         axisLineColor = "white",
                         gridLineColor = "white",
                         colors = c("red")
                       ) %>%
                       dyRangeSelector(height = 30)
                   })
                   
                   output$plotDataSummary2 <- renderDygraph({
                     dygraph(BLCK$BLK.Adjusted, main = paste("BLACKROCK ", choice)) %>%
                       dyAnnotation("2018-1-1", text = "D", tooltip = "Stop ETF") %>%
                       dyAxis("y", label = choice) %>%
                       dyOptions(
                         stackedGraph = TRUE,
                         axisLabelColor = "white",
                         axisLineColor = "white",
                         gridLineColor = "white",
                         colors = c("red")
                       ) %>%
                       dyRangeSelector(height = 30)
                   })
                   
                   output$plotDataSummary3 <- renderDygraph({
                     dygraph(AAPL$AAPL.Adjusted, main = paste("APPLE ", choice)) %>%
                       dyAxis("y", label = choice) %>%
                       dyAnnotation("2018-8-1", text = "U", tooltip = "Cap. Mille miliardi") %>%
                       dyOptions(
                         stackedGraph = TRUE,
                         axisLabelColor = "white",
                         axisLineColor = "white",
                         gridLineColor = "white",
                         colors = c("red")
                       ) %>%
                       dyRangeSelector(height = 30)
                   })
                   
                   output$plotDataSummary4 <- renderDygraph({
                     dygraph(MSFT$MSFT.Adjusted, main = paste("MICROSOFT ", choice)) %>%
                       dyAnnotation("2018-6-1", text = "U", tooltip = "Acquisto Github") %>% 
                       dyAxis("y", label = choice) %>%
                       dyOptions(
                         stackedGraph = TRUE,
                         axisLabelColor = "white",
                         axisLineColor = "white",
                         gridLineColor = "white",
                         colors = c("red")
                       ) %>%
                       dyRangeSelector(height = 30)
                   })
                   
                   output$plotDataSummary5 <- renderDygraph({
                     dygraph(MOOY$MCO.Adjusted, main = paste("MOODY'S ", choice)) %>%
                       dyAxis("y", label = choice) %>%
                       dyOptions(
                         stackedGraph = TRUE,
                         axisLabelColor = "white",
                         axisLineColor = "white",
                         gridLineColor = "white",
                         colors = c("red")
                       ) %>%
                       dyRangeSelector(height = 30)
                   })
                   
                   output$plotDataSummary6 <- renderDygraph({
                     dygraph(COCA$KO.Adjusted, main = paste("COCA COLA ", choice)) %>%
                       dyAnnotation("2018-8-1", text = "U", tooltip = "Acquisto Caffè Costa") %>% 
                       dyAxis("y", label = choice) %>%
                       dyOptions(
                         stackedGraph = TRUE,
                         axisLabelColor = "white",
                         axisLineColor = "white",
                         gridLineColor = "white",
                         colors = c("red")
                       ) %>%
                       dyRangeSelector(height = 30)
                   })
                 }
               })
  
  #----------------------------------------------------------------------------------------------------------------
  #----------------------------------------------------------------------------------------------------------------
  #----------------------------------------------------------------------------------------------------------------
  
  #LISTENER EVENTO RADIO BUTTON RETURN SUMMARY
  
  observeEvent(input$rbPriceReturnSummary,
               {
                 if (input$rbPriceReturnSummary == 2) {
                   choice <- "Return"
                   
                   BRKB_simple_CC_return <- merge(BRKB_return, BRKB_simple_return)
                   colnames(BRKB_simple_CC_return) <- c('CC Return','Simple Return')
                   
                   output$plotDataSummary1 <- renderDygraph({
                     dygraph(BRKB_simple_CC_return, main = paste("BERKSHIRE ", choice)) %>%
                       dySeries(name = "CC Return",
                                label = "BRKB CC Return",
                                color = "red") %>%
                       dySeries(name = "Simple Return",
                                label = "BRKB Simple Return",
                                color = "orange") %>%
                       dyAxis("y", label = choice) %>%
                       dyOptions(
                         stackedGraph = TRUE,
                         axisLabelColor = "white",
                         axisLineColor = "white",
                         gridLineColor = "white",
                         colors = c("red", "orange")
                       ) %>%
                       dyRangeSelector(height = 30)
                   })
                   
                   BLCK_simple_CC_return <- merge(BLCK_return, BLCK_simple_return)
                   colnames(BLCK_simple_CC_return) <- c('CC Return','Simple Return')
                   
                   output$plotDataSummary2 <- renderDygraph({
                     dygraph(BLCK_simple_CC_return, main = paste("BLACKROCK ", choice)) %>%
                       dySeries(name = "CC Return",
                                label = "BLCK CC Return",
                                color = "red") %>%
                       dySeries(name = "Simple Return",
                                label = "BLCK Simple Return",
                                color = "orange") %>%
                       dyAxis("y", label = choice) %>%
                       dyOptions(
                         stackedGraph = TRUE,
                         axisLabelColor = "white",
                         axisLineColor = "white",
                         gridLineColor = "white",
                         colors = c("red", "orange")
                       ) %>%
                       dyRangeSelector(height = 30)
                   })
                   
                   AAPL_simple_CC_return <- merge(AAPL_return, AAPL_simple_return)
                   colnames(AAPL_simple_CC_return) <- c('CC Return','Simple Return')
                   
                   output$plotDataSummary3 <- renderDygraph({
                     dygraph(AAPL_simple_CC_return, main = paste("APPLE ", choice)) %>%
                       dySeries(name = "CC Return",
                                label = "AAPL CC Return",
                                color = "red") %>%
                       dySeries(name = "Simple Return",
                                label = "AAPL Simple Return",
                                color = "orange") %>%
                       dyAxis("y", label = choice) %>%
                       dyOptions(
                         stackedGraph = TRUE,
                         axisLabelColor = "white",
                         axisLineColor = "white",
                         gridLineColor = "white",
                         colors = c("red", "orange")
                       ) %>%
                       dyRangeSelector(height = 30)
                   })
                   
                   MSFT_simple_CC_return <- merge(MSFT_return, MSFT_simple_return)
                   colnames(MSFT_simple_CC_return) <- c('CC Return','Simple Return')
                   
                   output$plotDataSummary4 <- renderDygraph({
                     dygraph(MSFT_simple_CC_return, main = paste("MICROSOFT ", choice)) %>%
                       dySeries(name = "CC Return",
                                label = "MSFT CC Return",
                                color = "red") %>%
                       dySeries(name = "Simple Return",
                                label = "MSFT Simple Return",
                                color = "orange") %>%
                       dyAxis("y", label = choice) %>%
                       dyOptions(
                         stackedGraph = TRUE,
                         axisLabelColor = "white",
                         axisLineColor = "white",
                         gridLineColor = "white",
                         colors = c("red", "orange")
                       ) %>%
                       dyRangeSelector(height = 30)
                   })
                   
                   MOOY_simple_CC_return <- merge(MOOY_return, MOOY_simple_return)
                   colnames(MOOY_simple_CC_return) <- c('CC Return','Simple Return')
                   
                   output$plotDataSummary5 <- renderDygraph({
                     dygraph(MOOY_simple_CC_return, main = paste("MOODY'S ", choice)) %>%
                       dySeries(name = "CC Return",
                                label = "MOOY CC Return",
                                color = "red") %>%
                       dySeries(name = "Simple Return",
                                label = "MOOY Simple Return",
                                color = "orange") %>%
                       dyAxis("y", label = choice) %>%
                       dyOptions(
                         stackedGraph = TRUE,
                         axisLabelColor = "white",
                         axisLineColor = "white",
                         gridLineColor = "white",
                         colors = c("red", "orange")
                       ) %>%
                       dyRangeSelector(height = 30)
                   })
                   
                   COCA_simple_CC_return <- merge(COCA_return, COCA_simple_return)
                   colnames(COCA_simple_CC_return) <- c('CC Return','Simple Return')
                   
                   output$plotDataSummary6 <- renderDygraph({
                     dygraph(COCA_simple_CC_return, main = paste("COCA COLA ", choice)) %>%
                       dySeries(name = "CC Return",
                                label = "COCA CC Return",
                                color = "red") %>%
                       dySeries(name = "Simple Return",
                                label = "COCA Simple Return",
                                color = "orange") %>%
                       dyAxis("y", label = choice) %>%
                       dyOptions(
                         stackedGraph = TRUE,
                         axisLabelColor = "white",
                         axisLineColor = "white",
                         gridLineColor = "white",
                         colors = c("red", "orange")
                       ) %>%
                       dyRangeSelector(height = 30)
                   })
            
                 }
               })
  
  #----------------------------------------------------------------------------------------------------------------
  #----------------------------------------------------------------------------------------------------------------
  #----------------------------------------------------------------------------------------------------------------
  
  #CANDLESTICK
  
  output$plotCandlestick1 <- renderPlot({
    chartSeries(BRKB, theme = "black", TA = "addBBands()")
  })
  
  output$plotCandlestick2 <- renderPlot({
    chartSeries(BLCK, theme = "black", TA = "addBBands()")
  })
  
  output$plotCandlestick3 <- renderPlot({
    chartSeries(AAPL, theme = "black", TA = "addBBands()")
  })
  
  output$plotCandlestick4 <- renderPlot({
    chartSeries(MSFT, theme = "black", TA = "addBBands()")
  })
  
  output$plotCandlestick5 <- renderPlot({
    chartSeries(MOOY, theme = "black", TA = "addBBands()")
  })
  
  output$plotCandlestick6 <- renderPlot({
    chartSeries(COCA, theme = "black", TA = "addBBands()")
  })
  
  #------------------------------------------------------------------------------------------------
  #------------------------------------------------------------------------------------------------
  #------------------------------------------------------------------------------------------------
  
  #RITORNI ASSET - INDICE
  
  BRKB_GSPC_return <- merge(BRKB_return, GSPC_return)
  BLCK_GSPC_return <- merge(BLCK_return, GSPC_return)
  AAPL_GSPC_return <- merge(AAPL_return, GSPC_return)
  MSFT_GSPC_return <- merge(MSFT_return, GSPC_return)
  MOOY_GSPC_return <- merge(MOOY_return, GSPC_return)
  COCA_GSPC_return <- merge(COCA_return, GSPC_return)
  
  colnames(BRKB_GSPC_return) <- c("BRKB_return", "GSPC_return")
  colnames(BLCK_GSPC_return) <- c("BLCK_return", "GSPC_return")
  colnames(AAPL_GSPC_return) <- c("AAPL_return", "GSPC_return")
  colnames(MSFT_GSPC_return) <- c("MSFT_return", "GSPC_return")
  colnames(MOOY_GSPC_return) <- c("MOOY_return", "GSPC_return")
  colnames(COCA_GSPC_return) <- c("COCA_return", "GSPC_return")
  
  output$plotReturnAssetIndex1 <- renderDygraph({
    dygraph(BRKB_GSPC_return, main = "BERKSHIRE vs SP500 Return") %>%
      dySeries(name = "BRKB_return",
               label = "BRKB Return",
               color = "red") %>%
      dySeries(name = "GSPC_return",
               label = "SP500 Return",
               color = "white") %>%
      dyAxis("y", label = "Return") %>%
      dyOptions(
        stackedGraph = TRUE,
        axisLabelColor = "white",
        axisLineColor = "white",
        gridLineColor = "white",
        colors = c("red", "orange")
      ) %>%
      dyRangeSelector(height = 30)
  })
  
  output$plotReturnAssetIndex2 <- renderDygraph({
    dygraph(BLCK_GSPC_return, main = "BLACKROCK vs SP500 Return") %>%
      dySeries(name = "BLCK_return",
               label = "BLCK Return",
               color = "red") %>%
      dySeries(name = "GSPC_return",
               label = "SP500 Return",
               color = "white") %>%
      dyAxis("y", label = "Return") %>%
      dyOptions(
        stackedGraph = TRUE,
        axisLabelColor = "white",
        axisLineColor = "white",
        gridLineColor = "white",
        colors = c("red", "orange")
      ) %>%
      dyRangeSelector(height = 30)
  })
  
  output$plotReturnAssetIndex3 <- renderDygraph({
    dygraph(AAPL_GSPC_return, main = "APPLE vs SP500 Return") %>%
      dySeries(name = "AAPL_return",
               label = "AAPL Return",
               color = "red") %>%
      dySeries(name = "GSPC_return",
               label = "SP500 Return",
               color = "white") %>%
      dyAxis("y", label = "Return") %>%
      dyOptions(
        stackedGraph = TRUE,
        axisLabelColor = "white",
        axisLineColor = "white",
        gridLineColor = "white",
        colors = c("red", "orange")
      ) %>%
      dyRangeSelector(height = 30)
  })
  
  output$plotReturnAssetIndex4 <- renderDygraph({
    dygraph(MSFT_GSPC_return, main = "MICROSOFT vs SP500 Return") %>%
      dySeries(name = "MSFT_return",
               label = "MSFT Return",
               color = "red") %>%
      dySeries(name = "GSPC_return",
               label = "SP500 Return",
               color = "white") %>%
      dyAxis("y", label = "Return") %>%
      dyOptions(
        stackedGraph = TRUE,
        axisLabelColor = "white",
        axisLineColor = "white",
        gridLineColor = "white",
        colors = c("red", "orange")
      ) %>%
      dyRangeSelector(height = 30)
  })
  
  output$plotReturnAssetIndex5 <- renderDygraph({
    dygraph(MOOY_GSPC_return, main = "MOODY'S vs SP500 Return") %>%
      dySeries(name = "MOOY_return",
               label = "MOOY Return",
               color = "red") %>%
      dySeries(name = "GSPC_return",
               label = "SP500 Return",
               color = "white") %>%
      dyAxis("y", label = "Return") %>%
      dyOptions(
        stackedGraph = TRUE,
        axisLabelColor = "white",
        axisLineColor = "white",
        gridLineColor = "white",
        colors = c("red", "orange")
      ) %>%
      dyRangeSelector(height = 30)
  })
  
  output$plotReturnAssetIndex6 <- renderDygraph({
    dygraph(COCA_GSPC_return, main = "COCA COLA vs SP500 Return") %>%
      dySeries(name = "COCA_return",
               label = "COCA Return",
               color = "red") %>%
      dySeries(name = "GSPC_return",
               label = "SP500 Return",
               color = "white") %>%
      dyAxis("y", label = "Return") %>%
      dyOptions(
        stackedGraph = TRUE,
        axisLabelColor = "white",
        axisLineColor = "white",
        gridLineColor = "white",
        colors = c("red", "orange")
      ) %>%
      dyRangeSelector(height = 30)
  })
  
  
  #------------------------------------------------------------------------------------------------
  #------------------------------------------------------------------------------------------------
  #------------------------------------------------------------------------------------------------
  
  #VOLUMI
  
  output$plotVolumes1 <- renderPlot({
    chartSeries(BRKB, main = "Volume BERKSHIRE")
  })
  
  output$plotVolumes2 <- renderPlot({
    chartSeries(BLCK, main = "Volume BLACKROCK")
  })
  
  output$plotVolumes3 <- renderPlot({
    chartSeries(AAPL, main = "Volume APPLE")
  })
  
  output$plotVolumes4 <- renderPlot({
    chartSeries(MSFT, main = "Volume MICROSOFT")
  })
  
  output$plotVolumes5 <- renderPlot({
    chartSeries(MOOY, main = "Volume MOODY'S")
  })
  
  output$plotVolumes6 <- renderPlot({
    chartSeries(COCA, main = "Volume COCA COLA")
  })
  
  #------------------------------------------------------------------------------------------------
  #------------------------------------------------------------------------------------------------
  #------------------------------------------------------------------------------------------------
  
  #STATISTICHE PER OGNI ASSET
  
  #MEDIA DI OGNI ASSET
  mean_BRKB <- mean(BRKB_return)
  mean_BLCK <- mean(BLCK_return)
  mean_AAPL <- mean(AAPL_return)
  mean_MSFT <- mean(MSFT_return)
  mean_MOOY <- mean(MOOY_return)
  mean_COCA <- mean(COCA_return)
  
  #VARIANZA DI OGNI ASSET
  variance_BRKB <- var(BRKB_return)
  variance_BLCK <- var(BLCK_return)
  variance_AAPL <- var(AAPL_return)
  variance_MSFT <- var(MSFT_return)
  variance_MOOY <- var(MOOY_return)
  variance_COCA <- var(COCA_return)
  
  #DEVIAZIONE STANDARD DI OGNI ASSET
  std_BRKB <- sd(BRKB_return)
  std_BLCK <- sd(BLCK_return)
  std_AAPL <- sd(AAPL_return)
  std_MSFT <- sd(MSFT_return)
  std_MOOY <- sd(MOOY_return)
  std_COCA <- sd(COCA_return)
  
  #KURTOSI DI OGNI ASSET
  kurtosis_BRKB <- kurtosis(BRKB_return)
  kurtosis_BLCK <- kurtosis(BLCK_return)
  kurtosis_AAPL <- kurtosis(AAPL_return)
  kurtosis_MSFT <- kurtosis(MSFT_return)
  kurtosis_MOOY <- kurtosis(MOOY_return)
  kurtosis_COCA <- kurtosis(COCA_return)
  
  #SKEWNESS DI OGNI ASSET
  skewness_BRKB <- skewness(BRKB_return)
  skewness_BLCK <- skewness(BLCK_return)
  skewness_AAPL <- skewness(AAPL_return)
  skewness_MSFT <- skewness(MSFT_return)
  skewness_MOOY <- skewness(MOOY_return)
  skewness_COCA <- skewness(COCA_return)
  
  #QUANTILI DI OGNI ASSET
  quantile_BRKB <- quantile(BRKB_return)
  BRKB_0 <- quantile_BRKB[1]
  BRKB_25 <- quantile_BRKB[2]
  BRKB_50 <- quantile_BRKB[3]
  BRKB_75 <- quantile_BRKB[4]
  BRKB_100 <- quantile_BRKB[5]
  
  quantile_BLCK <- quantile(BLCK_return)
  BLCK_0 <- quantile_BLCK[1]
  BLCK_25 <- quantile_BLCK[2]
  BLCK_50 <- quantile_BLCK[3]
  BLCK_75 <- quantile_BLCK[4]
  BLCK_100 <- quantile_BLCK[5]
  
  quantile_AAPL <- quantile(AAPL_return)
  AAPL_0 <- quantile_AAPL[1]
  AAPL_25 <- quantile_AAPL[2]
  AAPL_50 <- quantile_AAPL[3]
  AAPL_75 <- quantile_AAPL[4]
  AAPL_100 <- quantile_AAPL[5]
  
  quantile_MSFT <- quantile(MSFT_return)
  MSFT_0 <- quantile_MSFT[1]
  MSFT_25 <- quantile_MSFT[2]
  MSFT_50 <- quantile_MSFT[3]
  MSFT_75 <- quantile_MSFT[4]
  MSFT_100 <- quantile_MSFT[5]
  
  quantile_MOOY <- quantile(MOOY_return)
  MOOY_0 <- quantile_MOOY[1]
  MOOY_25 <- quantile_MOOY[2]
  MOOY_50 <- quantile_MOOY[3]
  MOOY_75 <- quantile_MOOY[4]
  MOOY_100 <- quantile_MOOY[5]
  
  quantile_COCA <- quantile(COCA_return)
  COCA_0 <- quantile_COCA[1]
  COCA_25 <- quantile_COCA[2]
  COCA_50 <- quantile_COCA[3]
  COCA_75 <- quantile_COCA[4]
  COCA_100 <- quantile_COCA[5]
  
  
  #STATISTICHE PER OGNI ASSET
  merged.tables <-
    data.frame(
      ROWS = c(
        "Mean",
        "Variance",
        "Std",
        "Skewness",
        "Kurtosis",
        "0 % Quantile",
        "25 % Quantile",
        "50 % Quantile",
        "75 % Quantile",
        "100 % Quantile"
      ),
      BRKB = c(
        mean_BRKB,
        variance_BRKB,
        round(std_BRKB, digits = 4),
        round(skewness_BRKB, digits = 4),
        round(kurtosis_BRKB, digits = 4),
        round(BRKB_0, digits = 4),
        round(BRKB_25, digits = 4),
        round(BRKB_50, digits = 4),
        round(BRKB_75, digits = 4),
        round(BRKB_100, digits = 4)
      ),
      BLCK = c(
        mean_BLCK,
        variance_BLCK,
        round(std_BLCK, digits = 4),
        round(skewness_BLCK, digits = 4),
        round(kurtosis_BLCK, digits = 4),
        round(BLCK_0, digits = 4),
        round(BLCK_25, digits = 4),
        round(BLCK_50, digits = 4),
        round(BLCK_75, digits = 4),
        round(BLCK_100, digits = 4)
      ),
      AAPL = c(
        mean_AAPL,
        variance_AAPL,
        round(std_AAPL, digits = 4),
        round(skewness_AAPL, digits = 4),
        round(kurtosis_AAPL, digits = 4),
        round(AAPL_0, digits = 4),
        round(AAPL_25, digits = 4),
        round(AAPL_50, digits = 4),
        round(AAPL_75, digits = 4),
        round(AAPL_100, digits = 4)
      ),
      MSFT = c(
        mean_MSFT,
        variance_MSFT,
        round(std_MSFT, digits = 4),
        round(skewness_MSFT, digits = 4),
        round(kurtosis_MSFT, digits = 4),
        round(MSFT_0, digits = 4),
        round(MSFT_25, digits = 4),
        round(MSFT_50, digits = 4),
        round(MSFT_75, digits = 4),
        round(MSFT_100, digits = 4)
      ),
      MOOY = c(
        mean_MOOY,
        variance_MOOY,
        round(std_MOOY, digits = 4),
        round(skewness_MOOY, digits = 4),
        round(kurtosis_MOOY, digits = 4),
        round(MOOY_0, digits = 4),
        round(MOOY_25, digits = 4),
        round(MOOY_50, digits = 4),
        round(MOOY_75, digits = 4),
        round(MOOY_100, digits = 4)
      ),
      COCA = c(
        mean_COCA,
        variance_COCA,
        round(std_COCA, digits = 4),
        round(skewness_COCA, digits = 4),
        round(kurtosis_COCA, digits = 4),
        round(COCA_0, digits = 4),
        round(COCA_25, digits = 4),
        round(COCA_50, digits = 4),
        round(COCA_75, digits = 4),
        round(COCA_100, digits = 4)
      )
    )
  
  
  output$tableStatisticsMatrix = renderTable(
    merged.tables, digits = 4, align = "c"
  )
  
  #------------------------------------------------------------------------------------------------
  #------------------------------------------------------------------------------------------------
  #------------------------------------------------------------------------------------------------
  
  #MAX AND MIN STATISTICHE
  
  output$txtMaxMean <- renderText({
    max <- max(mean_BRKB, mean_BLCK, mean_AAPL, mean_MSFT, mean_MOOY, mean_COCA)
    
    if(max == mean_BRKB)
      ticker <- "BRBK"
    if(max == mean_BLCK)
      ticker <- "BLCK"
    if(max == mean_AAPL)
      ticker <- "AAPL"
    if(max == mean_MSFT)
      ticker <- "MSFT"
    if(max == mean_MOOY)
      ticker <- "MOOY"
    if(max == mean_COCA)
      ticker <- "COCA"
    
    paste("Massima Media è di ", ticker, " = ", max)
  })
  
  output$txtMinMean <- renderText({
    min <- min(mean_BRKB, mean_BLCK, mean_AAPL, mean_MSFT, mean_MOOY, mean_COCA)
    
    if(min == mean_BRKB)
      ticker <- "BRBK"
    if(min == mean_BLCK)
      ticker <- "BLCK"
    if(min == mean_AAPL)
      ticker <- "AAPL"
    if(min == mean_MSFT)
      ticker <- "MSFT"
    if(min == mean_MOOY)
      ticker <- "MOOY"
    if(min == mean_COCA)
      ticker <- "COCA"
    
    paste("Minima Media è di ", ticker, " = ", min)
  })
  
  
  
  output$txtMaxStd <- renderText({
    max <- max(std_BRKB, std_BLCK, std_AAPL, std_MSFT, std_MOOY, std_COCA)
    
    if(max == std_BRKB)
      ticker <- "BRBK"
    if(max == std_BLCK)
      ticker <- "BLCK"
    if(max == std_AAPL)
      ticker <- "AAPL"
    if(max == std_MSFT)
      ticker <- "MSFT"
    if(max == std_MOOY)
      ticker <- "MOOY"
    if(max == std_COCA)
      ticker <- "COCA"
    
    paste("Massima Dev. Standard è di ", ticker, " = ", max)
  })
  
  output$txtMinStd <- renderText({
    min <- min(std_BRKB, std_BLCK, std_AAPL, std_MSFT, std_MOOY, std_COCA)
    
    if(min == std_BRKB)
      ticker <- "BRBK"
    if(min == std_BLCK)
      ticker <- "BLCK"
    if(min == std_AAPL)
      ticker <- "AAPL"
    if(min == std_MSFT)
      ticker <- "MSFT"
    if(min == std_MOOY)
      ticker <- "MOOY"
    if(min == std_COCA)
      ticker <- "COCA"
    
    paste("Minima Dev. Standard è di ", ticker, " = ", min)
  })
  
  #------------------------------------------------------------------------------------------------
  #------------------------------------------------------------------------------------------------
  #------------------------------------------------------------------------------------------------
  
  #SCATTER PLOT E MATRICE DI CORRELAZIONE
  
  output$plotCorrelationMatrix <- renderPlot({
    pairs.panels(
      cbind(
        as.numeric(BRKB_return),
        as.numeric(BLCK_return),
        as.numeric(AAPL_return),
        as.numeric(MSFT_return),
        as.numeric(MOOY_return),
        as.numeric(COCA_return)
      ),
      method = 'pearson',
      hist.col = "#00AFBB",
      density = TRUE,
      ellipses = TRUE,
      main = "Correlazione tra tutti gli asset"
    )
  })
  
  #------------------------------------------------------------------------------------------------
  #------------------------------------------------------------------------------------------------
  #------------------------------------------------------------------------------------------------
  
  #MATRICE DI COVARIANZA
  
  covariance_matrix <- cov(cbind(
    BRKB_return,
    BLCK_return,
    AAPL_return,
    MSFT_return,
    MOOY_return,
    COCA_return
  ))
  
  colnames(covariance_matrix) <- c("BRKB","BLCK","AAPL","MSFT","MOOY","COCA")
  
  output$tableCovarianceMatrix <- renderTable(
    covariance_matrix, align = "c", digits = 8
  )
  
  #------------------------------------------------------------------------------------------------
  #------------------------------------------------------------------------------------------------
  #------------------------------------------------------------------------------------------------
  
  #ISTOGRAMMI DEI RITORNI
  
  output$plotHistogram1 <- renderPlot({
    hist(BRKB_return, freq = FALSE,  main = "Distribuzione dei CC Return di BERKSHIRE", xlab = "Ritorni BERKSHIRE")
    BRKB_density <- density(BRKB_return)
    points(BRKB_density, type = "l", col = "blue")
    x <- seq(from = min(BRKB_return),
             to = max(BRKB_return))
    y <- dnorm(x,
               mean = mean(BRKB_return),
               sd = sd(BRKB_return))
    lines(x, y, col = "blue", lwd = 2)
    
  })
  
  output$plotHistogram2 <- renderPlot({
    hist(BLCK_return, freq = FALSE,  main = "Distribuzione dei CC Return di BLACKROCK", xlab = "Ritorni BLACKROCK")
    BLCK_density <- density(BLCK_return)
    points(BLCK_density, type = "l", col = "blue")
    x <- seq(from = min(BLCK_return),
             to = max(BLCK_return))
    y <- dnorm(x,
               mean = mean(BLCK_return),
               sd = sd(BLCK_return))
    lines(x, y, col = "blue", lwd = 2)
  })
  
  output$plotHistogram3 <- renderPlot({
    hist(AAPL_return, freq = FALSE,  main = "Distribuzione dei CC Return di APPLE", xlab = "Ritorni APPLE")
    AAPL_density <- density(AAPL_return)
    points(AAPL_density, type = "l", col = "blue")
    x <- seq(from = min(AAPL_return),
             to = max(AAPL_return))
    y <- dnorm(x,
               mean = mean(AAPL_return),
               sd = sd(AAPL_return))
    lines(x, y, col = "blue", lwd = 2)
  })
  
  output$plotHistogram4 <- renderPlot({
    hist(MSFT_return, freq = FALSE,  main = "Distribuzione dei CC Return di MICROSOFT", xlab = "Ritorni MICROSOFT")
    MSFT_density <- density(MSFT_return)
    points(MSFT_density, type = "l", col = "blue")
    x <- seq(from = min(MSFT_return),
             to = max(MSFT_return))
    y <- dnorm(x,
               mean = mean(MSFT_return),
               sd = sd(MSFT_return))
    lines(x, y, col = "blue", lwd = 2)
  })
  
  output$plotHistogram5 <- renderPlot({
    hist(MOOY_return, freq = FALSE,  main = "Distribuzione dei CC Return di MOODY'S", xlab = "Ritorni MOODY'S")
    MOOY_density <- density(MOOY_return)
    points(MOOY_density, type = "l", col = "blue")
    x <- seq(from = min(MOOY_return),
             to = max(MOOY_return))
    y <- dnorm(x,
               mean = mean(MOOY_return),
               sd = sd(MOOY_return))
    lines(x, y, col = "blue", lwd = 2)
  })
  
  output$plotHistogram6 <- renderPlot({
    hist(COCA_return, freq = FALSE,  main = "Distribuzione dei CC Return di COCA COLA", xlab = "Ritorni COCA COLA")
    COCA_density <- density(COCA_return)
    points(COCA_density, type = "l", col = "blue")
    x <- seq(from = min(COCA_return),
             to = max(COCA_return))
    y <- dnorm(x,
               mean = mean(COCA_return),
               sd = sd(COCA_return))
    lines(x, y, col = "blue", lwd = 2)
  })
  
  #------------------------------------------------------------------------------------------------
  #------------------------------------------------------------------------------------------------
  #------------------------------------------------------------------------------------------------
  
  #BARPLOT

  output$plotBox1 <- renderPlot({
    chart.Boxplot(BRKB_return, outlier.symbol = "O", main = "Barplot dei CC Return di BERKSHIRE")
  })
  
  output$plotBox2 <- renderPlot({
    chart.Boxplot(BLCK_return, outlier.symbol = "O", main = "Barplot dei CC Return di BLACKROCK")
  })
  
  output$plotBox3 <- renderPlot({
    chart.Boxplot(AAPL_return, outlier.symbol = "O", main = "Barplot dei CC Return di APPLE")
  })
  
  output$plotBox4 <- renderPlot({
    chart.Boxplot(MSFT_return, outlier.symbol = "O", main = "Barplot dei CC Return di MICROSOFT")
  })
  
  output$plotBox5 <- renderPlot({
    chart.Boxplot(MOOY_return, outlier.symbol = "O", main = "Barplot dei CC Return di MOODY'S")
  })
  
  output$plotBox6 <- renderPlot({
    chart.Boxplot(COCA_return, outlier.symbol = "O", main = "Barplot dei CC Return di COCA COLA")
  })
  
  output$plotBoxAll <- renderPlot({
    chart.Boxplot(merge(BRKB_return, BLCK_return, AAPL_return, MSFT_return, MOOY_return, COCA_return), outlier.symbol = "O", main = "Barplot dei CC Return")
  })
  
  #------------------------------------------------------------------------------------------------
  #------------------------------------------------------------------------------------------------
  #------------------------------------------------------------------------------------------------
  
  #QUANTILE PLOT
  
  output$plotQuantile1 <- renderPlot({
    q = quantile(BRKB_return)
    hist(BRKB_return, main = "Quartili dei CC Return di BERKSHIRE")
    abline(v = q[1], col = "red", lwd = 2) # 0% (min)
    abline(v = q[2], col = "blue", lwd = 2) # 1st quartile 25%
    abline(v = q[3], col = "green", lwd = 2.5) # Median value distribution 50%
    abline(v = q[4], col = "blue", lwd = 2) # 3rd quartile 75%
    abline(v = q[5], col = "red", lwd = 2) # 100% (max)
  })
  
  output$plotQuantile2 <- renderPlot({
    q = quantile(BLCK_return)
    hist(BLCK_return, main = "Quartili dei CC Return di BLACK ROCK")
    abline(v = q[1], col = "red", lwd = 2) # 0% (min)
    abline(v = q[2], col = "blue", lwd = 2) # 1st quartile 25%
    abline(v = q[3], col = "green", lwd = 2.5) # Median value distribution 50%
    abline(v = q[4], col = "blue", lwd = 2) # 3rd quartile 75%
    abline(v = q[5], col = "red", lwd = 2) # 100% (max)
  })
  
  output$plotQuantile3 <- renderPlot({
    q = quantile(AAPL_return)
    hist(AAPL_return, main = "Quartili dei CC Return di APPLE")
    abline(v = q[1], col = "red", lwd = 2) # 0% (min)
    abline(v = q[2], col = "blue", lwd = 2) # 1st quartile 25%
    abline(v = q[3], col = "green", lwd = 2.5) # Median value distribution 50%
    abline(v = q[4], col = "blue", lwd = 2) # 3rd quartile 75%
    abline(v = q[5], col = "red", lwd = 2) # 100% (max)
  })
  
  output$plotQuantile4 <- renderPlot({
    q = quantile(MSFT_return)
    hist(MSFT_return, main = "Quartili dei CC Return di MICROSOFT")
    abline(v = q[1], col = "red", lwd = 2) # 0% (min)
    abline(v = q[2], col = "blue", lwd = 2) # 1st quartile 25%
    abline(v = q[3], col = "green", lwd = 2.5) # Median value distribution 50%
    abline(v = q[4], col = "blue", lwd = 2) # 3rd quartile 75%
    abline(v = q[5], col = "red", lwd = 2) # 100% (max)
  })
  
  output$plotQuantile5 <- renderPlot({
    q = quantile(MOOY_return)
    hist(MOOY_return, main = "Quartili dei CC Return di MOODY'S")
    abline(v = q[1], col = "red", lwd = 2) # 0% (min)
    abline(v = q[2], col = "blue", lwd = 2) # 1st quartile 25%
    abline(v = q[3], col = "green", lwd = 2.5) # Median value distribution 50%
    abline(v = q[4], col = "blue", lwd = 2) # 3rd quartile 75%
    abline(v = q[5], col = "red", lwd = 2) # 100% (max)
  })
  
  output$plotQuantile6 <- renderPlot({
    q = quantile(COCA_return)
    hist(COCA_return, main = "Quartili dei CC Return di COCA COLA")
    abline(v = q[1], col = "red", lwd = 2) # 0% (min)
    abline(v = q[2], col = "blue", lwd = 2) # 1st quartile 25%
    abline(v = q[3], col = "green", lwd = 2.5) # Median value distribution 50%
    abline(v = q[4], col = "blue", lwd = 2) # 3rd quartile 75%
    abline(v = q[5], col = "red", lwd = 2) # 100% (max)
  })
  
  #------------------------------------------------------------------------------------------------
  #------------------------------------------------------------------------------------------------
  #------------------------------------------------------------------------------------------------
  
  #QQ-PLOT
  
  output$plotQQ1 <- renderPlot({
    qqnorm(BRKB_return, main = "QQ Plot dei CC Return di BERKSHIRE", col = "blue")
    qqline(BRKB_return)
  })
  
  output$plotQQ2 <- renderPlot({
    qqnorm(BLCK_return, main = "QQ Plot dei CC Return di BLACKROCK", col = "blue")
    qqline(BLCK_return)
  })
  
  output$plotQQ3 <- renderPlot({
    qqnorm(AAPL_return, main = "QQ Plot dei CC Return di APPLE", col = "blue")
    qqline(AAPL_return)
  })
  
  output$plotQQ4 <- renderPlot({
    qqnorm(MSFT_return, main = "QQ Plot dei CC Return di MICROSOFT", col = "blue")
    qqline(MSFT_return)
  })
  
  output$plotQQ5 <- renderPlot({
    qqnorm(MOOY_return, main = "QQ Plot dei CC Return di MOODY'S", col = "blue")
    qqline(MOOY_return)
  })
  
  output$plotQQ6 <- renderPlot({
    qqnorm(COCA_return, main = "QQ Plot dei CC Return di COCA COLA", col = "blue")
    qqline(COCA_return)
  })
  
  #------------------------------------------------------------------------------------------------
  #------------------------------------------------------------------------------------------------
  #------------------------------------------------------------------------------------------------
  
  #PLOT ASSET - ASSET PRICE
  
  observeEvent(input$selectAssetPricePlot1, {
    asset1 <- NULL
    asset2 <- NULL
    
    if (input$selectAssetPricePlot1 == "BRKB")
      asset1 = BRKB$'BRK-B.Adjusted'
    if (input$selectAssetPricePlot1 == "BLCK")
      asset1 = BLCK$BLK.Adjusted
    if (input$selectAssetPricePlot1 == "AAPL")
      asset1 = AAPL$AAPL.Adjusted
    if (input$selectAssetPricePlot1 == "MSFT")
      asset1 = MSFT$MSFT.Adjusted
    if (input$selectAssetPricePlot1 == "MOOY")
      asset1 = MOOY$MCO.Adjusted
    if (input$selectAssetPricePlot1 == "COCA")
      asset1 = COCA$KO.Adjusted
    
    if (input$selectAssetPricePlot2 == "BRKB")
      asset2 = BRKB$'BRK-B.Adjusted'
    if (input$selectAssetPricePlot2 == "BLCK")
      asset2 = BLCK$BLK.Adjusted
    if (input$selectAssetPricePlot2 == "AAPL")
      asset2 = AAPL$AAPL.Adjusted
    if (input$selectAssetPricePlot2 == "MSFT")
      asset2 = MSFT$MSFT.Adjusted
    if (input$selectAssetPricePlot2 == "MOOY")
      asset2 = MOOY$MCO.Adjusted
    if (input$selectAssetPricePlot2 == "COCA")
      asset2 = COCA$KO.Adjusted
    
    asset <- merge(asset1, asset2)
    colnames(asset) <- c("asset1","asset2")
    
    output$plotAssetAssetPrice <- renderDygraph({
      dygraph(asset, main = paste(input$selectAssetPricePlot1, "vs", input$selectAssetPricePlot2, " Price")) %>%
        dySeries(name = "asset1",
                 axis = "y",
                 label = paste(input$selectAssetPricePlot1, " Price"),
                 color = "red") %>%
        dySeries(name = "asset2",
                 axis = "y2",
                 label = paste(input$selectAssetPricePlot2, " Price"),
                 color = "white") %>%
        dyAxis("y", label = paste("Price ", input$selectAssetPricePlot1), valueRange = c(min(asset1),max(asset1)), independentTicks = TRUE)%>%
        dyAxis("y2", label = paste("Price ", input$selectAssetPricePlot2), valueRange = c(min(asset2),max(asset2)), independentTicks = TRUE)%>%
        dyOptions(
          stackedGraph = TRUE,
          axisLabelColor = "white",
          axisLineColor = "white",
          gridLineColor = "white",
          colors = c("red", "orange")
        ) %>%
        dyRangeSelector(height = 30)
    })
  })
  
  observeEvent(input$selectAssetPricePlot2, {
    asset1 <- NULL
    asset2 <- NULL
    
    if (input$selectAssetPricePlot1 == "BRKB")
      asset1 = BRKB$'BRK-B.Adjusted'
    if (input$selectAssetPricePlot1 == "BLCK")
      asset1 = BLCK$BLK.Adjusted
    if (input$selectAssetPricePlot1 == "AAPL")
      asset1 = AAPL$AAPL.Adjusted
    if (input$selectAssetPricePlot1 == "MSFT")
      asset1 = MSFT$MSFT.Adjusted
    if (input$selectAssetPricePlot1 == "MOOY")
      asset1 = MOOY$MCO.Adjusted
    if (input$selectAssetPricePlot1 == "COCA")
      asset1 = COCA$KO.Adjusted
    
    if (input$selectAssetPricePlot2 == "BRKB")
      asset2 = BRKB$'BRK-B.Adjusted'
    if (input$selectAssetPricePlot2 == "BLCK")
      asset2 = BLCK$BLK.Adjusted
    if (input$selectAssetPricePlot2 == "AAPL")
      asset2 = AAPL$AAPL.Adjusted
    if (input$selectAssetPricePlot2 == "MSFT")
      asset2 = MSFT$MSFT.Adjusted
    if (input$selectAssetPricePlot2 == "MOOY")
      asset2 = MOOY$MCO.Adjusted
    if (input$selectAssetPricePlot2 == "COCA")
      asset2 = COCA$KO.Adjusted
    
    asset <- merge(asset1, asset2)
    colnames(asset) <- c("asset1","asset2")
    
    output$plotAssetAssetPrice <- renderDygraph({
      dygraph(asset, main = paste(input$selectAssetPricePlot1, "vs", input$selectAssetPricePlot2, " Price")) %>%
        dySeries(name = "asset1",
                 axis = "y",
                 label = paste(input$selectAssetPricePlot1, " Price"),
                 color = "red") %>%
        dySeries(name = "asset2",
                 axis = "y2",
                 label = paste(input$selectAssetPricePlot2, " Price"),
                 color = "white") %>%
        dyAxis("y", label = paste("Price ", input$selectAssetPricePlot1), valueRange = c(min(asset1),max(asset1)), independentTicks = TRUE)%>%
        dyAxis("y2", label = paste("Price ", input$selectAssetPricePlot2), valueRange = c(min(asset2),max(asset2)), independentTicks = TRUE)%>%
        dyOptions(
          stackedGraph = TRUE,
          axisLabelColor = "white",
          axisLineColor = "white",
          gridLineColor = "white",
          colors = c("red", "orange")
        ) %>%
        dyRangeSelector(height = 30)
    })
  })
  
  #------------------------------------------------------------------------------------------------
  #------------------------------------------------------------------------------------------------
  #------------------------------------------------------------------------------------------------
  
  #PLOT ASSET - ASSET RETURN
  
  observeEvent(input$selectAssetReturnPlot1, {
    asset1 <- NULL
    asset2 <- NULL
    
    if (input$selectAssetReturnPlot1 == "BRKB")
      asset1 = BRKB_return
    if (input$selectAssetReturnPlot1 == "BLCK")
      asset1 = BLCK_return
    if (input$selectAssetReturnPlot1 == "AAPL")
      asset1 = AAPL_return
    if (input$selectAssetReturnPlot1 == "MSFT")
      asset1 = MSFT_return
    if (input$selectAssetReturnPlot1 == "MOOY")
      asset1 = MOOY_return
    if (input$selectAssetReturnPlot1 == "COCA")
      asset1 = COCA_return
    
    if (input$selectAssetReturnPlot2 == "BRKB")
      asset2 = BRKB_return
    if (input$selectAssetReturnPlot2 == "BLCK")
      asset2 = BLCK_return
    if (input$selectAssetReturnPlot2 == "AAPL")
      asset2 = AAPL_return
    if (input$selectAssetReturnPlot2 == "MSFT")
      asset2 = MSFT_return
    if (input$selectAssetReturnPlot2 == "MOOY")
      asset2 = MOOY_return
    if (input$selectAssetReturnPlot2 == "COCA")
      asset2 = COCA_return
    
    asset <- merge(asset1, asset2)
    colnames(asset) <- c("asset1","asset2")
    
    output$plotAssetAssetReturn <- renderDygraph({
      dygraph(asset, main = paste(input$selectAssetReturnPlot1, "vs", input$selectAssetReturnPlot2, " Return")) %>%
        dySeries(name = "asset1",
                 label = paste(input$selectAssetReturnPlot1, " Return"),
                 color = "red") %>%
        dySeries(name = "asset2",
                 label = paste(input$selectAssetReturnPlot2, " Return"),
                 color = "orange") %>%
        dyAxis("y", label = "Return") %>%
        dyOptions(
          stackedGraph = TRUE,
          axisLabelColor = "white",
          axisLineColor = "white",
          gridLineColor = "white",
          colors = c("red", "orange")
        ) %>%
        dyRangeSelector(height = 30)
    })
  })
  
  observeEvent(input$selectAssetReturnPlot2, {
    asset1 <- NULL
    asset2 <- NULL
    
    if (input$selectAssetReturnPlot1 == "BRKB")
      asset1 = BRKB_return
    if (input$selectAssetReturnPlot1 == "BLCK")
      asset1 = BLCK_return
    if (input$selectAssetReturnPlot1 == "AAPL")
      asset1 = AAPL_return
    if (input$selectAssetReturnPlot1 == "MSFT")
      asset1 = MSFT_return
    if (input$selectAssetReturnPlot1 == "MOOY")
      asset1 = MOOY_return
    if (input$selectAssetReturnPlot1 == "COCA")
      asset1 = COCA_return
    
    if (input$selectAssetReturnPlot2 == "BRKB")
      asset2 = BRKB_return
    if (input$selectAssetReturnPlot2 == "BLCK")
      asset2 = BLCK_return
    if (input$selectAssetReturnPlot2 == "AAPL")
      asset2 = AAPL_return
    if (input$selectAssetReturnPlot2 == "MSFT")
      asset2 = MSFT_return
    if (input$selectAssetReturnPlot2 == "MOOY")
      asset2 = MOOY_return
    if (input$selectAssetReturnPlot2 == "COCA")
      asset2 = COCA_return
    
    asset <- merge(asset1, asset2)
    colnames(asset) <- c("asset1","asset2")
    
    output$plotAssetAssetReturn <- renderDygraph({
      dygraph(asset, main = paste(input$selectAssetReturnPlot1, "vs", input$selectAssetReturnPlot2, " Return")) %>%
        dySeries(name = "asset1",
                 label = paste(input$selectAssetReturnPlot1, " Return"),
                 color = "red") %>%
        dySeries(name = "asset2",
                 label = paste(input$selectAssetReturnPlot2, " Return"),
                 color = "orange") %>%
        dyAxis("y", label = "Return") %>%
        dyOptions(
          stackedGraph = TRUE,
          axisLabelColor = "white",
          axisLineColor = "white",
          gridLineColor = "white",
          colors = c("red", "orange")
        ) %>%
        dyRangeSelector(height = 30)
    })
  })
  
  #------------------------------------------------------------------------------------------------
  #------------------------------------------------------------------------------------------------
  #------------------------------------------------------------------------------------------------
  
  #ARIMA PRICE FORECASTING
  
  observeEvent(input$selectAssetToPriceForecast, {
    asset_price <- NULL
    arma.predictions <- NULL
    arma.forecast <- NULL
    
    if (input$selectAssetToPriceForecast == "BRKB")
      asset_price = BRKB$'BRK-B.Adjusted'
    if (input$selectAssetToPriceForecast == "BLCK")
      asset_price = BLCK$BLK.Adjusted
    if (input$selectAssetToPriceForecast == "AAPL")
      asset_price = AAPL$AAPL.Adjusted
    if (input$selectAssetToPriceForecast == "MSFT")
      asset_price = MSFT$MSFT.Adjusted
    if (input$selectAssetToPriceForecast == "MOOY")
      asset_price = MOOY$MCO.Adjusted
    if (input$selectAssetToPriceForecast == "COCA")
      asset_price = COCA$KO.Adjusted
    
    returnTrain <- asset_price[1:(0.90 * length(asset_price))]
    returnTest <- asset_price[(0.90 * length(asset_price) + 1):length(asset_price)]
    
    fit <- arima(returnTrain, order = c(2, 0, 4))
    arma.predictions <- predict(fit, n.ahead = (length(asset_price) - (0.90 * length(asset_price))))$pred
    arma.forecast <- forecast(fit, h = length(returnTest), level = c(95, 85))
    
    output$plotPriceForecast <- renderPlot({
      plot(arma.forecast, main = paste(input$selectAssetToPriceForecast, " ARIMA FORECAST"))
      lines(arma.predictions, col = "blue")
    })
    
    rmse <- round(RMSE(arma.predictions, returnTest), digits = 5)
    
    output$txtPriceForecastRSME <- renderText({
      paste("RMSE ", input$selectAssetToPriceForecast, " = ", rmse)
    })
    
    output$plotPricePeriod <- renderPlot({
      asset_tseries <- ts(asset_price, frequency = 12)
      fitRet <- stl(asset_tseries[,1], s.window="period")
      plot(fitRet)
    })
  })
  
  
  observeEvent(input$btnUpdatePriceForecast, {
    asset_price <- NULL
    arma.forecast <- NULL
    arma.predictions <- NULL
    
    if (input$selectAssetToPriceForecast == "BRKB")
      asset_price = BRKB$'BRK-B.Adjusted'
    if (input$selectAssetToPriceForecast == "BLCK")
      asset_price = BLCK$BLK.Adjusted
    if (input$selectAssetToPriceForecast == "AAPL")
      asset_price = AAPL$AAPL.Adjusted
    if (input$selectAssetToPriceForecast == "MSFT")
      asset_price = MSFT$MSFT.Adjusted
    if (input$selectAssetToPriceForecast == "MOOY")
      asset_price = MOOY$MCO.Adjusted
    if (input$selectAssetToPriceForecast == "COCA")
      asset_price = COCA$KO.Adjusted
    
    returnTrain <- asset_price[1:(0.90 * length(asset_price))]
    returnTest <- asset_price[(0.90 * length(asset_price) + 1):length(asset_price)]
    
    fit <- arima(returnTrain, order = c(input$sliderPriceAR, 0, input$sliderPriceMA))
    arma.predictions <- predict(fit, n.ahead = (length(asset_price) - (0.90 * length(asset_price))))$pred
    arma.forecast <- forecast(fit, h = length(returnTest), level = c(95, 85))

    output$plotPriceForecast <- renderPlot({
      plot(arma.forecast,
           main = paste(input$selectAssetToPriceForecast, " ARIMA FORECAST"))
      lines(arma.predictions, col = "blue")
    })
    
    rmse <- round(RMSE(arma.predictions, returnTest), digits = 5)
    
    output$txtPriceForecastRSME <- renderText({
      paste("RMSE ", input$selectAssetToPriceForecast, " = ", rmse)
    })
    
    output$plotPricePeriod <- renderPlot({
      asset_tseries <- ts(asset_price, frequency = 12)
      fitRet <- stl(asset_tseries[,1], s.window="period")
      plot(fitRet)
    })
  })
  
  #------------------------------------------------------------------------------------------------
  #------------------------------------------------------------------------------------------------
  #------------------------------------------------------------------------------------------------
  
  #ARIMA RETURN FORECASTING
  
  observeEvent(input$selectAssetToReturnForecast, {
    asset_return <- NULL
    arma.predictions <- NULL
    arma.forecast <- NULL
    
    if (input$selectAssetToReturnForecast == "BRKB")
      asset_return = BRKB_return
    if (input$selectAssetToReturnForecast == "BLCK")
      asset_return = BLCK_return
    if (input$selectAssetToReturnForecast == "AAPL")
      asset_return = AAPL_return
    if (input$selectAssetToReturnForecast == "MSFT")
      asset_return = MSFT_return
    if (input$selectAssetToReturnForecast == "MOOY")
      asset_return = MOOY_return
    if (input$selectAssetToReturnForecast == "COCA")
      asset_return = COCA_return
    
    returnTrain <- asset_return[1:(0.90 * length(asset_return))]
    returnTest <- asset_return[(0.90 * length(asset_return) + 1):length(asset_return)]
    
    fit <- arima(returnTrain, order = c(2, 0, 4))
    
    arma.predictions <- predict(fit, n.ahead = (length(asset_return) - (0.90 * length(asset_return))))$pred
    arma.forecast <- forecast(fit, h = length(returnTest), level = c(95, 85))
    
    output$plotReturnForecast <- renderPlot({
      plot(arma.forecast, main = paste(input$selectAssetToReturnForecast, " ARIMA FORECAST"))
      lines(arma.predictions)
    })
    
    rmse <- round(RMSE(arma.predictions, returnTest), digits = 5)
    
    output$txtReturnForecastRSME <- renderText({
      paste("RMSE ", input$selectAssetToReturnForecast, " = ", rmse)
    })
    
    output$plotReturnPeriod <- renderPlot({
      asset_tseries <- ts(asset_return, frequency = 12)
      fitRet <- stl(asset_tseries[,1], s.window="period")
      plot(fitRet)
    })
  })
  
  
  observeEvent(input$btnUpdateReturnForecast, {
    asset_return <- NULL
    arma.predictions <- NULL
    arma.forecast <- NULL
    
    if (input$selectAssetToReturnForecast == "BRKB")
      asset_return = BRKB_return
    if (input$selectAssetToReturnForecast == "BLCK")
      asset_return = BLCK_return
    if (input$selectAssetToReturnForecast == "AAPL")
      asset_return = AAPL_return
    if (input$selectAssetToReturnForecast == "MSFT")
      asset_return = MSFT_return
    if (input$selectAssetToReturnForecast == "MOOY")
      asset_return = MOOY_return
    if (input$selectAssetToReturnForecast == "COCA")
      asset_return = COCA_return
    
    returnTrain <- asset_return[1:(0.90 * length(asset_return))]
    returnTest <- asset_return[(0.90 * length(asset_return) + 1):length(asset_return)]
    
    fit <- arima(returnTrain, order = c(input$sliderReturnAR, 0, input$sliderReturnMA))
    arma.predictions <- predict(fit, n.ahead = (length(asset_return) - (0.90 * length(asset_return))))$pred
    arma.forecast <- forecast(fit, h = length(returnTest), level = c(95, 85))
    
    output$plotReturnForecast <- renderPlot({
      plot(arma.forecast, main = paste(input$selectAssetToReturnForecast, " ARIMA FORECAST"))
      lines(arma.predictions, col = "blue")
    })
    
    rmse <- round(RMSE(arma.predictions, returnTest), digits = 5)
    
    output$txtReturnForecastRSME <- renderText({
      paste("RMSE ", input$selectAssetToReturnForecast, " = ", rmse)
    })
    
    output$plotReturnPeriod <- renderPlot({
      asset_tseries <- ts(asset_return, frequency = 12)
      fitRet <- stl(asset_tseries[,1], s.window="period")
      plot(fitRet)
    })
    
  })
  
  #
  #------------------------------------------------------------------------------------------------
  #------------------------------------------------------------------------------------------------
  #------------------------------------------------------------------------------------------------
  
  #BETA
  
  #BETA PER OGNI ASSET
  beta_BRKB_GSPC <- cov(BRKB_return, GSPC_return) / var(GSPC_return)
  beta_BLCK_GSPC <- cov(BLCK_return, GSPC_return) / var(GSPC_return)
  beta_AAPL_GSPC <- cov(AAPL_return, GSPC_return) / var(GSPC_return)
  beta_MSFT_GSPC <- cov(MSFT_return, GSPC_return) / var(GSPC_return)
  beta_MOOY_GSPC <- cov(MOOY_return, GSPC_return) / var(GSPC_return)
  beta_COCA_GSPC <- cov(COCA_return, GSPC_return) / var(GSPC_return)
  
  beta_function <- function(stock, market_index) {
    beta <- cov(stock, market_index) / var(market_index)
    return(beta)
  }
  
  BRKB_betas.xts <- NULL
  BLCK_betas.xts <- NULL
  AAPL_betas.xts <- NULL
  MSFT_betas.xts <- NULL
  MOOY_betas.xts <- NULL
  COCA_betas.xts <- NULL
  
  delta_t <- 10
  length_period = dim(GSPC_return)[1]
  
  start <- delta_t + 1
  
  for (i in start:length_period) {
    beta_val_BRKB <-
      beta_function(BRKB_return[(i - delta_t):(i - 1)], GSPC_return[(i - delta_t):(i - 1)])
    beta_val_BLCK <-
      beta_function(BLCK_return[(i - delta_t):(i - 1)], GSPC_return[(i - delta_t):(i - 1)])
    beta_val_AAPL <-
      beta_function(AAPL_return[(i - delta_t):(i - 1)], GSPC_return[(i - delta_t):(i - 1)])
    beta_val_MSFT <-
      beta_function(MSFT_return[(i - delta_t):(i - 1)], GSPC_return[(i - delta_t):(i - 1)])
    beta_val_MOOY <-
      beta_function(MOOY_return[(i - delta_t):(i - 1)], GSPC_return[(i - delta_t):(i - 1)])
    beta_val_COCA <-
      beta_function(COCA_return[(i - delta_t):(i - 1)], GSPC_return[(i - delta_t):(i - 1)])
    
    beta_xts_BRKB <-
      as.xts(beta_val_BRKB, order.by = index(BRKB_return[(i - 1)]))
    beta_xts_BLCK <-
      as.xts(beta_val_BLCK, order.by = index(BLCK_return[(i - 1)]))
    beta_xts_AAPL <-
      as.xts(beta_val_AAPL, order.by = index(AAPL_return[(i - 1)]))
    beta_xts_MSFT <-
      as.xts(beta_val_MSFT, order.by = index(MSFT_return[(i - 1)]))
    beta_xts_MOOY <-
      as.xts(beta_val_MOOY, order.by = index(MOOY_return[(i - 1)]))
    beta_xts_COCA <-
      as.xts(beta_val_COCA, order.by = index(COCA_return[(i - 1)]))
    
    if (is.null(BRKB_betas.xts) && 
        is.null(BLCK_betas.xts) && 
        is.null(AAPL_betas.xts) && 
        is.null(MSFT_betas.xts) && 
        is.null(MOOY_betas.xts) && 
        is.null(COCA_betas.xts)){
      BRKB_betas.xts <- beta_xts_BRKB
      BLCK_betas.xts <- beta_xts_BLCK
      AAPL_betas.xts <- beta_xts_AAPL
      MSFT_betas.xts <- beta_xts_MSFT
      MOOY_betas.xts <- beta_xts_MOOY
      COCA_betas.xts <- beta_xts_COCA
    }
    else{
      BRKB_betas.xts <- rbind(BRKB_betas.xts, beta_xts_BRKB)
      BLCK_betas.xts <- rbind(BLCK_betas.xts, beta_xts_BLCK)
      AAPL_betas.xts <- rbind(AAPL_betas.xts, beta_xts_AAPL)
      MSFT_betas.xts <- rbind(MSFT_betas.xts, beta_xts_MSFT)
      MOOY_betas.xts <- rbind(MOOY_betas.xts, beta_xts_MOOY)
      COCA_betas.xts <- rbind(COCA_betas.xts, beta_xts_COCA)
    }
  }
  
  BRKB_beta <-
    as.xts(c(rep(NA, delta_t), as.numeric(BRKB_betas.xts)), order.by = index(BRKB_return))
  BLCK_beta <-
    as.xts(c(rep(NA, delta_t), as.numeric(BLCK_betas.xts)), order.by = index(BLCK_return))
  AAPL_beta <-
    as.xts(c(rep(NA, delta_t), as.numeric(AAPL_betas.xts)), order.by = index(AAPL_return))
  MSFT_beta <-
    as.xts(c(rep(NA, delta_t), as.numeric(MSFT_betas.xts)), order.by = index(MSFT_return))
  MOOY_beta <-
    as.xts(c(rep(NA, delta_t), as.numeric(MOOY_betas.xts)), order.by = index(MOOY_return))
  COCA_beta <-
    as.xts(c(rep(NA, delta_t), as.numeric(COCA_betas.xts)), order.by = index(COCA_return))
  
  observeEvent(input$selectAssetBeta, {
    asset_name <- NULL
    asset_beta <- NULL
    asset_beta_txt <- NULL
    asset_price <- NULL
    asset_index <- GSPC$GSPC.Adjusted
    
    if (input$selectAssetBeta == "BRKB") {
      asset_name <- "Berkshire"
      asset_beta <- BRKB_beta
      asset_beta_txt <- beta_BRKB_GSPC
      asset_price <- BRKB$'BRK-B.Adjusted'
    }
    
    if (input$selectAssetBeta == "BLCK") {
      asset_name <- "Black Rock"
      asset_beta <- BLCK_beta
      asset_beta_txt <- beta_BLCK_GSPC
      asset_price <- BLCK$BLK.Adjusted
    }
    
    if (input$selectAssetBeta == "AAPL") {
      asset_name <- "Apple"
      asset_beta <- AAPL_beta
      asset_beta_txt <- beta_AAPL_GSPC
      asset_price <- AAPL$AAPL.Adjusted
    }
    
    if (input$selectAssetBeta == "MSFT") {
      asset_name <- "Microsoft"
      asset_beta <- MSFT_beta
      asset_beta_txt <- beta_MSFT_GSPC
      asset_price <- MSFT$MSFT.Adjusted
    }
    
    if (input$selectAssetBeta == "MOOY") {
      asset_name <- "Moody's"
      asset_beta <- MOOY_beta
      asset_beta_txt <- beta_MOOY_GSPC
      asset_price <- MOOY$MCO.Adjusted
    }
    
    if (input$selectAssetBeta == "COCA") {
      asset_name <- "Coca Cola"
      asset_beta <- COCA_beta
      asset_beta_txt <- beta_COCA_GSPC
      asset_price <- COCA$KO.Adjusted
    }
    
    asset <- merge(asset_price, asset_index)
    colnames(asset) <- c("asset_price", "asset_index")
    
    output$plotPriceAssetIndex <- renderDygraph({
      dygraph(asset, main = paste("Price ", input$selectAssetBeta, " - SP500")) %>%
        dySeries(name = "asset_price",
                 axis = ('y'),
                 label = paste("Price ", input$selectAssetBeta),
                 color = "red") %>%
        dySeries(name = "asset_index",
                 axis = ('y2'),
                 label = "Price SP500",
                 color = "white") %>%
          dyAxis("y", label = "Price Asset", valueRange = c(min(asset_price),max(asset_price)), independentTicks = TRUE)%>%
          dyAxis("y2", label = "Price Index", valueRange = c(min(asset_index),max(asset_index)), independentTicks = TRUE)%>%
        dyOptions(
          stackedGraph = FALSE,
          axisLabelColor = "white",
          axisLineColor = "white",
          gridLineColor = "white",
          colors = c("red", "orange")
        ) %>%
        dyRangeSelector(height = 30)
    })
   
    output$plotBeta <- renderDygraph({
      dygraph(asset_beta, main = paste("Beta ", input$selectAssetBeta)) %>%
        dyAxis("y", label = "Beta") %>%
        dyLimit(1, strokePattern = "solid", col = "orange") %>%
        dyOptions(
          stackedGraph = TRUE,
          axisLabelColor = "white",
          axisLineColor = "white",
          gridLineColor = "white",
          colors = c("red")
        ) %>%
        dyRangeSelector(height = 30)
    })
    
    output$txtBeta <- renderText({
      paste("Beta ", asset_name, " : ", round(asset_beta_txt, digits = 3))
    })
  })
})
