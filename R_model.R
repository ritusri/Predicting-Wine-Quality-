knitr::opts_chunk$set(echo = FALSE, tidy.opts = list(width.cutoff = 45), 
    tidy = TRUE, comment = "##") 
library(knitr) 
library(tidyverse) 
library(xtable) 
library(MASS) 
library(psych) 
library(reshape2) 
library(randomForest) 
library(rpart) 
rm(list = ls()) 
d1 <- read.csv("./casestudy/winequality-red.csv", 
    sep = ";") 
d2 <- read.csv("./casestudy/winequality-white.csv", 
    sep = ";") 
d1.r <- dplyr::rename(d1, `Fixed Acidity` = fixed.acidity, `
     Volatile Acidity` = volatile.acidity, `Citric Acid` = citric.acid, `
     Residual Sugar` = residual.sugar, Chlorides = chlorides, `
     Free Sulphurdioxide` = free.sulfur.dioxide, `
     Total Sulfurdioxide` = total.sulfur.dioxide, 
     Density = density, Sulphates = sulphates, 
     Alcohol = alcohol, Quality = quality)

d2.r <- dplyr::rename(d2, `Fixed Acidity` = fixed.acidity, `
     Volatile Acidity` = volatile.acidity, `Citric Acid` = citric.acid, `
     Residual Sugar` = residual.sugar, Chlorides = chlorides, `
     Free Sulphurdioxide` = free.sulfur.dioxide, `
     Total Sulfurdioxide` = total.sulfur.dioxide, 
     Density = density, Sulphates = sulphates, 
     Alcohol = alcohol, Quality = quality)

head(d1.r[, 1:6]) %>% kable(caption = "Red Wine Sample Data (Columns: 1-6)", 
    booktabs = TRUE, longtable = TRUE) 
head(d1.r[, 7:12]) %>% kable(caption = "Red Wine Sample Data (Columns: 7-12)",
    booktabs = TRUE, longtable = TRUE) 
head(d2.r[, 1:6]) %>% kable(caption = "White Wine Sample Data (Columns: 1-6)", 
    booktabs = TRUE, longtable = TRUE) 
head(d2.r[, 7:12]) %>% kable(caption = "White Wine Sample Data (Columns: 7-12)", b
   ooktabs = TRUE, longtable = TRUE) 
var <- colnames(d1.r)[-ncol(d1.r)] 
num <- 1:length(var)

df <- data.frame(S.No. = num, Predictor = var)

kable(df, caption = "Predictor Variables", booktabs = TRUE, 
      longtable = TRUE)
multi.hist(d1.r, main = NA, dcol = c("blue", "red"), 
      dlty = c("solid", "solid"), bcol = "linen") 
d1.b <- d1.r 
d1.b[, 12] <- as.factor(d1.b[, 12]) 
lh <- c(4, 5, 7, 10) 
bw <- c(0.05, 0.003, 0.6, 0.01) 
for (i in 1:4) { 
  base <- ggplot(d1.b) 
  layering.h <- geom_histogram(aes(x = d1.b[, 
      lh[i]], fill = Quality), binwidth = bw[i]) 
  layering.l <- geom_vline(aes(xintercept = mean(d1.b[, 
      lh[i]])), color = "indianred4") 
  theming.x <- xlab(colnames(d1.b)[lh[i]]) 
  theming.y <- ylab("Frequency") 
  theming.t <- ggtitle(paste("Histogram and Mean of ", 
      colnames(d1.b)[lh[i]], "")) 
  themed <- base + layering.h + layering.l + 
    theming.x + theming.y + theming.t 
  print(themed) 
}

qboxp <- function(df) { 
  d1.b <- df 
  d1.b[, 12] <- as.factor(d1.b[, 12]) 
  df.m <- melt(d1.b, id.var = "Quality") 
  base <- ggplot(data = df.m, aes(x = variable, 
      y = value)) 
  layering <- geom_boxplot(aes(fill = Quality), 
      outlier.shape = 1, outlier.color = "indianred3") 
  scaling <- scale_x_discrete(breaks = NULL) 
  faceting <- facet_wrap(~variable, scales = "free") 
  theming.x <- xlab("Predictor Variables") 
  theming.y <- ylab("Physico-chemical Values") 
  theming.title <- ggtitle("Quality Boxplots for Each Predictor") 
  theming.facet <- theme(strip.text = element_text(size = 7), 
      axis.text.x = element_blank(), axis.ticks.x = element_blank()) 
  themed <- base + layering + scaling + faceting + 
    theming.x + theming.y + theming.title + 
    theming.facet 
  themed 
} 
qboxp(d1.r)

outtona <- function(vl, df) { 
    for (i in vl) { outlier <- boxplot.stats(df[, i])$out 
    df[, i] <- ifelse(df[, i] %in% outlier, 
        NA, df[, i])

  } 
  return(df)
}

outliereff <- function(varlist, df) { 
  for (i in varlist) { 
    total = length(df[, i]) 
    par(mfrow = c(2, 2), oma = c(0, 0, 3, 
        0)) 
    boxplot(df[, i], main = "With Outliers") 
    hist(df[, i], main = "With Outliers", 
         xlab = NA, ylab = NA, prob = TRUE) 
    df <- outtona(i, df) 
    boxplot(df[, i], main = "Without outliers") 
    hist(df[, i], main = "Without outliers", 
         xlab = NA, ylab = NA, prob = TRUE) 
    out <- sum(is.na(df[, i])) 
    per <- round((out)/total * 100, 1) 
    title(paste("Effect of", out, "(", per, 
        "%)", "Outliers on", colnames(df)[i], 
        sep = " "), outer = TRUE) 
    } 
  } 
outliereff(4, d1.r) 
d1.on <- outtona(1:11, d1.r) 
d1.f <- d1.on[complete.cases(d1.on), ] 
d2.on <- outtona(1:11, d2.r) 
d2.f <- d2.on[complete.cases(d2.on), ] 
qboxp(d1.f) 
qboxp(d2.f) 
imppred <- randomForest(quality ~ ., data = d1, 
    ntree = 100, keep.forest = FALSE, importance = TRUE) 
importance(imppred, type = 1) 
imppred <- randomForest(quality ~ ., data = d2, 
    ntree = 100, keep.forest = FALSE, importance = TRUE)
importance(imppred, type = 1) 
symnum(cor(d1.r)) 
symnum(cor(d2.r)) 
lrmodel.red <- lm(Quality ~ ., data = d1.r) 
summary(lrmodel.red) 
kable(anova(lrmodel.red), booktabs = T) 
lrmodel.red2 <- update(lrmodel.red, . ~ . - `Citric Acid` `
     Residual Sugar`) 
summary(lrmodel.red2) 
rtmodel.red <- rpart(Quality ~ ., data = d1.r) 
plot(rtmodel.red, uniform = T, branch = 1, margin = 0.05, 
     cex = 0.9) 
text(rtmodel.red, cex = 0.7) 
lrm.pred.red <- predict(lrmodel.red, d1.r) 
rt.pred.red <- predict(rtmodel.red, d1.r) 
mae.lrm.red <- mean(abs(lrm.pred.red - d1.r[, 12])) 
mae.rt.red <- mean(abs(rt.pred.red - d1.r[, 12]))
mae.lrm.red 
mae.rt.red 
(mse.lrm.red <- mean((lrm.pred.red - d1.r[, 12])^2)) 
(mse.rt.red <- mean((rt.pred.red - d1.r[, 12])^2))
multi.hist(d2.r, main = NA, dcol = c("blue", "red"), 
    dlty = c("solid", "solid"), bcol = "grey95") 
qboxp(d2.r) 
outliereff(5, d1.r) 
outliereff(10, d1.r) 
outliereff(1, d2.r) 
outliereff(5, d2.r) 
outliereff(6, d2.r) 
outliereff(8, d2.r) 
outliereff(10, d2.r)