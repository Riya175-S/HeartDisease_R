df <- read.csv("heart.csv", stringsAsFactors = FALSE)
df
table(df $ target)
df $ target <- factor(df$target , levels = c ("0", "1"), labels=c("Not Present","Present"))
table(df $ target)

#normalization function
norm <- function (x)
{
  return ((x-min(x))/(max(x)-min(x)))
}
df_n <- as.data.frame(lapply(df[1:13], norm))
table(df $ target)

#splitting the dataset into training and testing
df_train <- df_n[1:900,]
df_test <- df_n[901:1025,]

df_train_labels <- df[1:900,14]
df_test_labels <- df[901:1025,14]
library("class")
df
df_n
df
df_train_pred <-knn(train= df_train, test=df_test, cl=df_train_labels,k=15)
install.packages('gmodels')
table(df $ target)
df
df_train_labels
CrossTable(x=df_test_labels, y=df_train_pred, prop.chisq=FALSE)
