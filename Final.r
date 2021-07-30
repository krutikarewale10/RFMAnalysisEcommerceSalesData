#Path for working directory
getwd
setwd("C:\\Users\\praka\\OneDrive\\Desktop\\METRO\\R\\EDA_PROJECTS\\SALES")
#--------------------------------------------------------------------------------------------------------------------------------------------------------------
#Load the dataset
data <- read.csv("train.csv")
data

#--------------------------------------------------------------------------------------------------------------------------------------------------------------

#structure of data
str(data)         #Structure of data (Datatypes in dataset)
dim(data)         #Dimensions of data
colnames(data)    #Column names in data
any(is.na(data)) #Finding NA values in the data frame
sum(is.na(data)) #Determining the number of NA values
head(data)       #Initial 6 columns of data
tail(data)       #Last 6 columns of data

df1$Order.Date <- as.Date(df1$Order.Date, format = "%d/%m/%Y")
df1$Ship.Date <- as.Date(df1$Ship.Date, format = "%d/%m/%Y")
str(df1)

#--------------------------------------------------------------------------------------------------------------------------------------------------------------

#1) Distribution of Sales data
df1 = data
summary(df1$Sales)

library(pastecs)
stat.desc(df1$Sales)

library(Hmisc)
describe(df1$Sales)

library(psych)
describe(df1$Sales)


hist(df1$Sales,
     main = "Histogram of Sales",
     xlab = "Sales Distribution")

library(moments)
skewness(df1$Sales)
kurtosis(df1$Sales) #Leptokurtic

df=df1
boxplot(df$Sales,
        main = toupper("Boxplot of Sales"),
        ylab = "Sales Distribution",
        col = "blue")

OutVals = boxplot(df$Sales)$out
OutVals

#df$Sales[df$Sales %in% boxplot(df)$out] <- median(df$Sales)


notout<-function(x){
  print("summary before applying this method ")
  print(summary(x))  
  M1<-mean(x,na.rm = TRUE)
  S1<-sd(x,na.rm=TRUE)
  low1<-M1-3*S1
  up1<-M1+3*S1
  x[x<low1]<-NA #Method 2:x[x<low1]<-low1
  x[x>up1]<-NA  #mwthod 2:x[x>up1]<-up1 
  print("summary after applying this method ")
  print(summary(x)) 
  return(x)
}
df$Sales<-notout(df$Sales)
boxplot(df$Sales,
        main = toupper("Boxplot of Sales"),
        ylab = "Sales Distribution",
        col = "blue")

#--------------------------------------------------------------------------------------------------------------------------------------------------------------

#2) Univariate Analysis on Categorical variable Sub.Category

sum(is.na(df$Sub.Category))

tbl<-table(df1$Sub.Category)
tbl

tbl<-aggregate(df1$Sub.Category,list(df1$Sub.Category),length)
tbl


# Pie Chart with Percentages
count<-table(df1$Sub.Category)
count
freq1 <- c(count[1:17])
freq1
lbls <- c("Accessories","Appliances","Art","Binders","Bookcases","Chairs","Copiers","Envelopes","Fasteners","Furnishings",
          "Labels","Machines","Paper","Phones","Storage","Supplies","Tables")
pct <- round(freq1/sum(freq1)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(freq1,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of Sub.Category")
#as you can see we have 75% fully paid and 25% charged off, so we are dealing with unbalaced data

# Simple Bar Plot
counts <- table(df$Sub.Category)
counts
barplot(c (counts[1:17]),las=2,cex.names = .5, 
        main="Categories of Product",
        ylab="Number",col = rainbow(length(lbls)),horiz = FALSE)

barplot(sample_curve, las=2, cex.names=.5) 

#--------------------------------------------------------------------------------------------------------------------------------------------------------------
#3) Is there any relationship between SubCategory and Sales

df = df1
df$Sub.Category <- as.factor(df$Sub.Category)
levels(df$Sub.Category)
Number_of_Orders=table(df$Sub.Category)

df$Sub.Category <- ordered((df1$Category),levels = c("Furniture", "Office Supplies", "Technology"))

library(dplyr)
group_by(df1, Category) %>%
  summarise(
    count = n(),
    mean = mean(Sales, na.rm = TRUE),
    sd = sd(Sales, na.rm = TRUE)
  )

library(devtools)
library(ggpubr)

ggboxplot(df1, x = "Category", y = "Sales", 
          color = "Category", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("Furniture", "Office Supplies", "Technology"),
          ylab = "Sales", xlab = "Category")

#NULL HYPOTHESIS : Category of products does not affects Sales

# Compute the analysis of variance
res.aov <- aov(Sales ~ Category, data = df1)
# Summary of the analysis
summary(res.aov)

#--------------------------------------------------------------------------------------------------------------------------------------------------------------

#4) Is there any relationship between Shipmode and Category of products bought
df= df1
library(MASS)
category_shipmode <- table(df1$Ship.Mode,df1$Category)
category_shipmode

tbl = table(df1$Ship.Mode,df1$Category)
prop.table(table(df1$Ship.Mode,df1$Category))*100
chisq.test(category_shipmode)

library(vcd)
mosaic(tbl)
mosaic(category_shipmode, shade=TRUE) 

#or
#Association Plots
assoc(category_shipmode, shade=TRUE)


#As the p-value is = 0.7088 ,more  than the significance level 0.05, 
#we accept the Null Hypothesis that Category is independent of Shipmode(does not affects Shipmode) 
counts <- table(df1$Ship.Mode,df1$Category)
barplot(counts, main="Shipmode and Category",
        xlab="Shipmode Type", col=c("green","red","yellow","blue"),
        legend = rownames(counts))

#--------------------------------------------------------------------------------------------------------------------------------------------------------------
#5)Monthly Sales

df3 <- df2 %>%
  mutate(month = month(Order.Date))%>%
  group_by(month) %>%
  summarise(sum_sales = sum(Sales))

write.csv(df3,"C:\\Users\\praka\\OneDrive\\Desktop\\METRO\\R\\EDA_PROJECTS\\SALES\\monthly_sales.csv", row.names = FALSE)

df3 %>%
  ggplot(aes(x = month, y = sum_sales)) +
  geom_line(color = "purple") +
  labs(title = "Sales Data",
       subtitle = "Data plotted by month",
       y = "Monthly",
       x = "Sales") + theme_bw(base_size = 15)


df1$Order.Date <- as.Date(df1$Order.Date, format = "%d/%m/%Y")
str(df1)
df2 <- df1
str(df2)


df3=df2 %>%
  mutate(year = year(Order.Date))%>%
  mutate(month = month(Order.Date))%>%
  group_by(year,month) %>%
  summarise(sum_sales = sum(Sales), .groups = "drop")
write.csv(df3,"C:\\Users\\praka\\OneDrive\\Desktop\\METRO\\R\\EDA_PROJECTS\\SALES\\monthly_yearly_sales.csv", row.names = FALSE)


df3 %>%
  ggplot(aes(x = month, y = sum_sales)) +
  geom_bar(stat = "identity", fill = "purple") +
  facet_wrap(~ year) +
  labs(title = "Total Monthly Sales",
       subtitle = "Data plotted by year",
       y = "Sales",
       x = "Month") + theme_bw(base_size = 15)

df4=df2 %>%
  mutate(year = year(Order.Date))%>%
  group_by(year) %>%
  summarise(sum_sales = sum(Sales), .groups = "drop")
#--------------------------------------------------------------------------------------------------------------------------------------------------------------

#6) IS there any relationship between Shipmode and City
#Null Hypothesis : City is independent of Shipmode
df= df1
library(MASS)
city_shipmode <- table(df$Ship.Mode,df$City)
city_shipmode

tbl = table(df$Ship.Mode,df$City)
prop.table(table(df$Ship.Mode,df$City))*100
chisq.test(city_shipmode)

library(vcd)
mosaic(tbl)
mosaic(city_shipmode, shade=TRUE) 

#or
#Association Plots
assoc(city_shipmode, shade=TRUE)


#As the p-value is = 2.2e-16 ,less  than the significance level 0.05, 
#we reject the Null Hypothesis that City is independent of Shipmode(does not affects Shipmode) 
counts <- table(df$Ship.Mode,df$City)
barplot(counts, main="Shipmode and Category",
        xlab="Shipmode Type", col=c("green","red","yellow","blue"),
        legend = rownames(counts))



