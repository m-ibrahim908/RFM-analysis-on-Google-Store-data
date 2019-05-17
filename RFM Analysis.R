df_storeData <- read.csv("pakistanAi/rfmData.csv", stringsAsFactors = FALSE)
View(df_storeData)
library(Hmisc)

#recency
df_storeData1 <- separate(df_storeData,InvoiceDate, into = c("Date", "Time"), sep = " ")
df_storeData1$Date <- as.Date(df_storeData1$Date, format = "%m/%d/%Y")
class(df_storeData1$Date)
df_storeData1 %>% mutate(subTotal= Quantity * UnitPrice) -> df_storeData1
df_storeData1 %>% filter(complete.cases(CustomerID)) %>% group_by(CustomerID) %>% summarise(n = n_distinct(InvoiceNo), lastPurchaseDate = max(Date), monetaryValue = sum(subTotal)) %>% arrange(desc(n), desc(lastPurchaseDate),desc(monetaryValue)) -> df_storeData1
max(df_storeData1$lastPurchaseDate)
thisDate <- as.Date("2011-12-31")
df_storeData1$recencyScore <- thisDate-df_storeData1$lastPurchaseDate
min(df_storeData1$recencyScore)
class(df_storeData1$recencyScore)
df_storeData1$recencyScore <- as.numeric(df_storeData1$recencyScore)
cut2(df_storeData1$recencyScore, g=5)
#Levels: [ 22, 34) [ 34, 54) [ 54, 94) [ 94,201) [201,395]

#frequency
df_storeData %>% filter(complete.cases(CustomerID)) %>%  group_by(CustomerID) %>% summarise(n = n_distinct(InvoiceNo)) %>% arrange(desc(n)) -> df_frequency
cut2(df_frequency$n, g = 5)
#Levels: 1 2 [3,  5) [5,  8) [8,248]

#monetary
df_storeData %>% mutate(subtotal = Quantity*UnitPrice) %>% group_by(InvoiceNo) %>% summarise(sum(subtotal))%>%  View()
cut2(df_storeData1$monetaryValue, g=5)
#Levels: [-4288,   235) [  235,   466) [  466,   910) [  910,  2004) [ 2004,279489]

