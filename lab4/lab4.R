###########    CSN     #######
##########    Lab 4    #######
#########   Xin & Raul #######


########### Information Extraction #############
##Extract information form datasets
get_data <- function(file, lang) {
  language <- read.table(file, header = FALSE)
  colnames(language) <- c("vertices","degree_2nd_moment", "mean_length")
  k2 <- language$degree_2nd_moment
  d <- language$mean_length
  n<- length(language$vertices)
  vertices <- language$vertices
  mu_n <- mean(language$vertices)
  sigma_n <- sd(language$vertices)
  mu_x <- mean(language$mean_length)
  sigma_x <- sd(language$mean_length)
  validate(lang,vertices,k2,d)
  return(c(lang,n,mu_n,sigma_n,mu_x,sigma_x))
}


## Data validation
validate <- function(lang, v, k2, d){
  valid <- TRUE
  for (i in length(v)) {
    if (!((4 - 6 / v[i] <= k2[i]) && (k2[i] <= v[i] - 1) &&
          (v[i] / (8 * (v[i] - 1)) * k2[i] + 0.5 <= d[i]) && (d[i] <= v[i] - 1))) {
      valid <- FALSE
      break
    }
  }
  
  if (valid) {
    print(paste("Language ", lang, "is valid"))
  } else{
    print(paste("Language ", lang, "is not valid"))
  }
}

## Summary function
summarize <- function(data){
  df <- data.frame(data)
  colnames(df) <- c("Language", "N", "mu_n", "sigma_n","mu_x", "sigma_x")
  print.data.frame(df, right=FALSE, row.names=FALSE)
}

summarize_tables <- function(data,languages){
  df <- data.frame(data)
  rownames(df) = c(languages)
  
  # s table
  df_1 = subset(df, select=c(1,4,7,10,13,16,19,22,25,28,31))
  colnames(df_1) <- c('0','1','2','3','4','1+','2+','3+','4+','4e','4+e')
  
  # aic table
  df_2 = subset(df, select=c(2,5,8,11,14,17,20,23,26,29,32))
  colnames(df_2) <- c('0','1','2','3','4','1+','2+','3+','4+','4e','4+e')
  
  # aic differences table
  df_3 = subset(df, select=c(3,6,9,12,15,18,21,24,27,30,33))
  colnames(df_3) <- c('0','1','2','3','4','1+','2+','3+','4+','4e','4+e')
  
  # model tables 
  df_4 = subset(df, select=c(24,25,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56))
  colnames(df_4) <- c('1b','2a','2b','3a','3c','4a','4ea','4eb','4ec','1+b','1+d','2+a','2+b','2+d','3+a','3+c','3+d','4+a','4+d','4+ea','4+eb','4+ec','4+ed')
  
  print("==== Table of s ====")
  print.data.frame(df_1, right=FALSE)
  print("==== Table of AIC ====")
  print.data.frame(df_2, right=FALSE)
  print("==== Table of AIC differences ====")
  print.data.frame(df_3, right=FALSE)
  print("==== Model table ====")
  print.data.frame(df_4, right=FALSE)
}

########### Information Visualization #############
visualize_data <- function(file, lang){
  language = read.table(file, header = FALSE)
  
  colnames(language) = c("vertices","degree_2nd_moment", "mean_length")
  language = language[order(language$vertices), ]
  mean_language = aggregate(language, list(language$vertices), mean)
  plot(language$vertices, language$degree_2nd_moment,main = paste('2nd moment of ',lang),
      xlab = "vertices", ylab = "degree 2nd moment")
  lines(mean_language$vertices,mean_language$degree_2nd_moment, col = "green")
  lines(language$vertices,
       (1 - 1/language$vertices)*(5 - 6/language$vertices), col = "red")
  lines(language$vertices,4-6/language$vertices, col = "blue")
  lines(language$vertices,language$vertices-1, col = "blue")
  plot(log(language$vertices), log(language$degree_2nd_moment),main = paste('Preliminary plot (Log) 2nd moment of ',lang),
    xlab = "log(vertices)", ylab = "log(degree_2nd_moment)")
  lines(log(mean_language$vertices),log(mean_language$degree_2nd_moment), col = "green")
  lines(log(language$vertices),
        log((1 - 1/language$vertices)*(5 - 6/language$vertices)), col = "red")
  lines(log(language$vertices),log(4-6/language$vertices), col = "blue")
  lines(log(language$vertices),log(language$vertices-1), col = "blue")
  plot(log(mean_language$vertices), log(mean_language$degree_2nd_moment),main = paste('Preliminary plot (Average Log) scale 2nd moment of ',lang),
       xlab = "log(vertices)", ylab = "log(degree_2nd_moment)")
}


########### Models #############

##Homocesdasticity checking
is_homocesdastic <- function(data){
  variances <- aggregate(data, list(data$vertices), var)
  #remove zeros and NA values
  variances <- variances[!apply(variances[, 3:4] == 0, 1, FUN = any, na.rm = TRUE),]
  variances <- na.omit(variances) 
  
  f_max <- max(variances$degree_2nd_moment) / min(variances$degree_2nd_moment)
  delta_f <- 0.5
  
  return (f_max <= 1 + delta_f)
}

model_fitting <- function(file, lang){
  language <- read.table(file, header = FALSE)
  colnames(language) <- c("vertices","degree_2nd_moment", "mean_length")
  language <- language[order(language$vertices), ]
  
  if(is_homocesdastic(language)){
    cat("The data set of ",lang," is homocesdastic")
  }else{
    cat("The data set of ",lang," is not homocesdastic\n")
    language <- aggregate(language, list(language$vertices), mean)
  }
  variances <- aggregate(language, list(language$vertices), var)
  #remove zeros and NA values
  variances <- variances[!apply(variances[, 3:4] == 0, 1, FUN = any, na.rm = TRUE),]
  variances <- na.omit(variances) 
  
  
  ## Model 0 - null model
  rss0 <- sum((1-1/(language$vertices))*(5-6/(language$vertices)))
  n0 <- length(language$vertices)
  p0 <- 0
  s0 <- sqrt(rss0/(n0 - p0))
  aic0 <- n0*log(2*pi) + n0*log(rss0/n0) + n0 + 2*(p0 + 1)

  
  ## Model 1
  lm1 <- lm(log(degree_2nd_moment) ~ log(vertices) , language)
  b_initial1 <- coef(lm1)[2]
  
  nlm1 <- nls(degree_2nd_moment~(vertices/2)^b,data=language,
           start = list(b = b_initial1), trace = TRUE)
  
  rss1 <- deviance(nlm1)
  aic1 <- AIC(nlm1)
  s1 <- sqrt(rss1/df.residual(nlm1))

  ## Model 2
  lm2 <- lm(log(degree_2nd_moment) ~ log(vertices) , language)
  a_initial2 <- exp(coef(lm2)[1])
  b_initial2 <- coef(lm2)[2]
  
  nlm2 <- nls(degree_2nd_moment~(a*vertices^b),data=language,
             start = list(a = a_initial2,b = b_initial2), trace = TRUE)
  rss2 <- deviance(nlm2)
  aic2 <- AIC(nlm2)
  s2 <- sqrt(rss2/df.residual(nlm2))

  ## Model 3
  lm3 <- lm(log(degree_2nd_moment) ~ vertices , language)
  a_initial3 <- exp(coef(lm3)[1])
  c_initial3 <- coef(lm3)[2]

  nlm3 <- nls(degree_2nd_moment~(a*exp(c*vertices)),data=language,
             start = list(a = a_initial3,c = c_initial3), trace = TRUE)
  rss3 <- deviance(nlm3)
  aic3 <- AIC(nlm3)
  s3 <- sqrt(rss3/df.residual(nlm3))

  ## Model 4
  lm4 <- lm(log(degree_2nd_moment) ~ log(vertices) , language)
  a_initial4 <- exp(coef(lm4)[1])
  nlm4 <- nls(degree_2nd_moment~a*log(vertices),data=language,
              start = list(a = a_initial4), trace = TRUE)
  rss4 <- deviance(nlm4)
  aic4 <- AIC(nlm4)
  s4 <- sqrt(rss4/df.residual(nlm4))

  ## Model 1+ 
  lm1p <- lm(log(degree_2nd_moment) ~ log(vertices) , language)
  b_initial1p <- coef(lm1p)[2]
  d_initial1p<- 0
  
  nlm1p <- nls(degree_2nd_moment~((vertices/2)^b+d),data=language,
              start = list(b = b_initial1p,d = d_initial1p), trace = TRUE)
  rss1p <- deviance(nlm1p)
  aic1p <- AIC(nlm1p)
  s1p <- sqrt(rss1p/df.residual(nlm1p))

  ## Model 2+
  lm2p <- lm(log(degree_2nd_moment) ~ log(vertices) , language)
  a_initial2p <- exp(coef(lm2p)[1])
  b_initial2p <- coef(lm2p)[2]
  d_initial2p<- 1
  ## using Port's nl2sol algorithm
  nlm2p <- nls(degree_2nd_moment~a*vertices^b+d,data=language,
              start = list(a = a_initial2p,b = b_initial2p, d = d_initial2p), 
              trace = TRUE,
              algorithm = "port",
              lower = 0)
  rss2p <- deviance(nlm2p)
  aic2p <- AIC(nlm2p)
  s2p <- sqrt(rss2p/df.residual(nlm2p))

  
  ## Model 3+
  lm3p <- lm(log(degree_2nd_moment) ~ vertices , language)
  a_initial3p <- exp(coef(lm3p)[1])
  c_initial3p <- coef(lm3p)[2]
  d_initial3p <- 1
  ## using Port's nl2sol algorithm
  nlm3p <- nls(degree_2nd_moment~ a * exp(c * vertices) + d,data=language,
              start = list(a = a_initial3p,c = c_initial3p, d = d_initial3p),
              algorithm = "port",
              lower = 0,
              trace = TRUE)
  rss3p <- deviance(nlm3p)
  aic3p <- AIC(nlm3p)
  s3p <- sqrt(rss3p/df.residual(nlm3p))

  
  ## Model 4+ 
  lm4p <- lm(log(degree_2nd_moment) ~ log(vertices) , language)
  a_initial4p <- exp(coef(lm4p)[1])
  d_initial4p <- 0
  nlm4p <- nls(degree_2nd_moment~a*log(vertices)+d,data=language,
              start = list(a = a_initial4p,d = d_initial4p),
              trace = TRUE)
  rss4p <- deviance(nlm4p)
  aic4p <- AIC(nlm4p)
  s4p <- sqrt(rss4p/df.residual(nlm4p))
  
  ## Model extra 4
  lm4e <- lm(log(degree_2nd_moment) ~ vertices , language)
  a_initial4e <- exp(coef(lm4e)[1])
  b_initial4e <- coef(lm4e)[1]  
  c_initial4e <- coef(lm4e)[2]
  nlm4e <- nls(degree_2nd_moment~(a*(vertices)^b)*exp(c*vertices),data=language,
               start = list(a = a_initial4e, b = b_initial4e, c= c_initial4e ), 
               algorithm = "port",
               control = nls.control(warnOnly = TRUE),
               lower = -1,
               trace = TRUE)
  rss4e <- deviance(nlm4e)
  aic4e <- AIC(nlm4e)
  s4e <- sqrt(rss4e/df.residual(nlm4e))

  
  ## Model extra 4+
  lm4pe <- lm(log(degree_2nd_moment) ~ vertices , language)
  a_initial4pe <- exp(coef(lm4pe)[1])
  b_initial4pe <- coef(lm4pe)[1]  
  c_initial4pe <- coef(lm4pe)[2]
  d_initial4pe <- 1
  nlm4pe <- nls(degree_2nd_moment~(a*(vertices)^b)*exp(c*vertices)+d,data=language,
                start = list(a = a_initial4pe, b = b_initial4pe, c= c_initial4pe,d = d_initial4pe), 
                control = nls.control(warnOnly = TRUE),
                algorithm = "port",
                lower = -1,
                trace = TRUE)
  rss4pe <- deviance(nlm4pe)
  aic4pe <- AIC(nlm4pe)
  s4pe <- sqrt(rss4pe/df.residual(nlm4pe))
  
  plot(log(language$vertices), log(language$degree_2nd_moment), main = paste("Language: ",lang, " - ", "Model 4 extra"),
       xlab = "log(vertices)", ylab = "log(degree_2nd_moment)")
  lines(log(language$vertices), log(fitted(nlm4e)), col = "green")
  
  
  plot(log(language$vertices), log(language$degree_2nd_moment), main = paste("Language: ",lang, " - ", "Model 4+ extra"),
       xlab = "log(vertices)", ylab = "log(degree_2nd_moment)")
  lines(log(language$vertices), log(fitted(nlm4pe)), col = "green")
  
  
  ## Plot best model
  min_aic <- min(c(aic0,aic1,aic2,aic3,aic4,aic1p,aic2p,aic3p,aic4p))
  best_model <- which.min(c(aic0,aic1,aic2,aic3,aic4,aic1p,aic2p,aic3p,aic4p))
  switch (as.character(best_model),
    "1" = {
      plot(log(language$vertices), log(language$degree_2nd_moment), main = paste("Language: ",lang, " - ", "Model 0"),
           xlab = "log(vertices)", ylab = "log(degree_2nd_moment)")
      lines(log(language$vertices),
            log((1 - 1/language$vertices)*(5 - 6/language$vertices)), col = "green")
    },
    "2" = {
      plot(log(language$vertices), log(language$degree_2nd_moment), main = paste("Language: ",lang, " - ", "Model 1"),
           xlab = "log(vertices)", ylab = "log(degree_2nd_moment)")
      lines(log(language$vertices), log(fitted(nlm1)), col = "green")
    },
    "3" = {
      plot(log(language$vertices), log(language$degree_2nd_moment), main = paste("Language: ",lang, " - ", "Model 2"),
           xlab = "log(vertices)", ylab = "log(degree_2nd_moment)")
      lines(log(language$vertices), log(fitted(nlm2)), col = "green")
    },
    "4" = {
      plot(log(language$vertices), log(language$degree_2nd_moment), main = paste("Language: ",lang, " - ", "Model 3"),
           xlab = "log(vertices)", ylab = "log(degree_2nd_moment)")
      lines(log(language$vertices), log(fitted(nlm3)), col = "green")
    },
    "5" = {
      plot(log(language$vertices), log(language$degree_2nd_moment), main = paste("Language: ",lang, " - ", "Model 4"),
           xlab = "log(vertices)", ylab = "log(degree_2nd_moment)")
      lines(log(language$vertices), log(fitted(nlm4)), col = "green")
    },
    "6" = {
      plot(log(language$vertices), log(language$degree_2nd_moment), main = paste("Language: ",lang, " - ", "Model 1+"),
           xlab = "log(vertices)", ylab = "log(degree_2nd_moment)")
      lines(log(language$vertices), log(fitted(nlm1p)), col = "green")
    },
    "7" = {
      plot(log(language$vertices), log(language$degree_2nd_moment), main = paste("Language: ",lang, " - ", "Model 2+"),
           xlab = "log(vertices)", ylab = "log(degree_2nd_moment)")
      lines(log(language$vertices), log(fitted(nlm2p)), col = "green")
    },
    "8" = {
      plot(log(language$vertices), log(language$degree_2nd_moment), main = paste("Language: ",lang, " - ", "Model 3+"),
           xlab = "log(vertices)", ylab = "log(degree_2nd_moment)")
      lines(log(language$vertices), log(fitted(nlm3p)), col = "green")
    },
    "9" = {
      plot(log(language$vertices), log(language$degree_2nd_moment), main = paste("Language: ",lang, " - ", "Model 4+"),
           xlab = "log(vertices)", ylab = "log(degree_2nd_moment)")
      lines(log(language$vertices), log(fitted(nlm4p)), col = "green")
    }
    ,{
      print("None best model selected")
    }
  )
  x <- c(s0, aic0, aic0-min_aic,
         s1, aic1, aic1-min_aic,
         s2, aic2, aic2-min_aic,
         s3, aic3, aic3-min_aic,
         s4, aic4, aic4-min_aic,
         s1p, aic1p, aic1p-min_aic,
         s2p, aic2p, aic2p-min_aic,
         s3p, aic3p, aic3p-min_aic,
         s4p, aic4p, aic4p-min_aic,
         s4e, aic4e, aic4e-min_aic,
         s4pe, aic4pe, aic4pe-min_aic,
         coef(nlm1)[1], 
         coef(nlm2)[1], coef(nlm2)[2], 
         coef(nlm3)[1], coef(nlm3)[2], 
         coef(nlm4)[1],
         coef(nlm4e)[1], coef(nlm4e)[2], coef(nlm4e)[3],
         coef(nlm1p)[1], coef(nlm1p)[2], 
         coef(nlm2p)[1], coef(nlm2p)[2], coef(nlm2p)[3], 
         coef(nlm3p)[1], coef(nlm3p)[2], coef(nlm3p)[3], 
         coef(nlm4p)[1], coef(nlm4p)[2],
         coef(nlm4pe)[1], coef(nlm4pe)[2], coef(nlm4pe)[3],coef(nlm4pe)[4]
  )
  r<- formatC(x,digits = 4, format = "f")
  
  return(r)
  

}
#######  Result Analysis ##########
## Generate summarize table and models
analyze <- function(list_file) {
  files <- read.table(list_file, header = TRUE, as.is=c("language","file"))
  result <- NULL
  for (x in 1:nrow(files)) {
    result <- rbind(result, get_data(files$file[x],files$language[x]))
  }
  summarize(result)
  
  for (x in 1:nrow(files)) {
    visualize_data(files$file[x],files$language[x])
  }
  
  fitted_models <-NULL
  for (x in 1:nrow(files)) {
    fitted_models <- rbind(fitted_models,model_fitting(files$file[x],files$language[x]))
  }
  summarize_tables(fitted_models,result[,1])
}

analyze("list_languages.txt")
