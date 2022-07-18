library(readxl)
library(ggplot2)

df <- read_excel("C:/Users/Hürü/Downloads/EVDS (3).xlsx", 
                 range = "A1:B24")

df2 <- read_excel("C:/Users/Hürü/Downloads/EVDS (2).xlsx", 
                      range = "A1:B24")



#Table Columns are re-named in more understandable format.
colnames(df) <- c("Year","GDP_in_Thousand_TL")
colnames(df2) <- c("Year","Chain_Linked_Volume_by_Expenditure_Approach_in_Thousand_TL")

#Average Yearly GDP Grow Rate

cat("From 1999 to 2021 Average GDP Growth %:",
    ((df$GDP_in_Thousand_TL[23]-df$GDP_in_Thousand_TL[1])/df$GDP_in_Thousand_TL[1])*100/22)
year <- 1999
data_gdp <- c()
#Growth Data
for (val in 1: 22)
{
  year <- year+1
  x <- df$GDP_in_Thousand_TL[val]
  y <- df$GDP_in_Thousand_TL[val+1]
  
  cat("Annual GDP Growth from year",year-1,"to",year,"is %",((y-x)/x)*100)
  data_gdp = append(data_gdp,((y-x)/x)*100)
  
  
}

cat("From 1999 to 2021 total GDP Growth %:",
    ((df$GDP_in_Thousand_TL[23]-df$GDP_in_Thousand_TL[1])/df$GDP_in_Thousand_TL[1])*100)

#Average Yearly Expenditure Rate

cat("From 1999 to 2021 Average annual total consumption Expenditure growth of resident households:% ",
    ((df2$Chain_Linked_Volume_by_Expenditure_Approach_in_Thousand_TL[23]-
        df2$Chain_Linked_Volume_by_Expenditure_Approach_in_Thousand_TL[1])/
       df2$Chain_Linked_Volume_by_Expenditure_Approach_in_Thousand_TL[1])*100/22)

gdp_av = ((df$GDP_in_Thousand_TL[23]-df$GDP_in_Thousand_TL[1])/df$GDP_in_Thousand_TL[1])*100/22
data_exp <- c()
#Expenditure Growth Data
year <- 1999
for (val in 1: 22)
{
  year <- year+1
  x <- df2$Chain_Linked_Volume_by_Expenditure_Approach_in_Thousand_TL[val]
  y <- df2$Chain_Linked_Volume_by_Expenditure_Approach_in_Thousand_TL[val+1]
  
  cat("Final consumption Expenditure of resident households annual growth ",year-1,"to",year,"is %",((y-x)/x)*100)
  data_exp = append(data_exp,((y-x)/x)*100)
 
}

cat("From 1999 to 2021 total consumption Expenditure growth of resident households:% ",
    ((df2$Chain_Linked_Volume_by_Expenditure_Approach_in_Thousand_TL[23]-
        df2$Chain_Linked_Volume_by_Expenditure_Approach_in_Thousand_TL[1])/
        df2$Chain_Linked_Volume_by_Expenditure_Approach_in_Thousand_TL[1])*100)

exp_av = ((df2$Chain_Linked_Volume_by_Expenditure_Approach_in_Thousand_TL[23]-
             df2$Chain_Linked_Volume_by_Expenditure_Approach_in_Thousand_TL[1])/
            df2$Chain_Linked_Volume_by_Expenditure_Approach_in_Thousand_TL[1])*100/22

#Creating new data frames



df3 <- data.frame(data_exp,data_gdp)
ggplot(df3, aes(x = data_gdp, y = data_exp)) + 
  geom_point() +  
  labs(x = " GDP Growth Rate", y = "Consumption Growth Rate")+
  ggtitle("GDP Growth Rate vs  Consumption Growth Rate From 1999 to 2021 ")+
  scale_x_continuous()+
  scale_y_continuous()+
  theme_bw()

#corrolation_coeff = 
cor(data_gdp, data_exp) 
#covairance
cov(data_gdp, data_exp) 


df3 <- c(0,gdp_av,gdp_av*2,gdp_av*3,gdp_av*4,gdp_av*5,gdp_av*6,gdp_av*7,gdp_av*8,
         gdp_av*9,gdp_av*10,gdp_av*11,gdp_av*12,gdp_av*13,gdp_av*14,gdp_av*15,gdp_av*16
         ,gdp_av*17,gdp_av*18,gdp_av*19,gdp_av*20,gdp_av*21,gdp_av*22,gdp_av*23)

df4 <- c(0,exp_av,exp_av*2,exp_av*3,exp_av*4,exp_av*5,exp_av*6,exp_av*7,exp_av*8,
         exp_av*9,exp_av*10,exp_av*11,exp_av*12,exp_av*13,exp_av*14,exp_av*15,exp_av*16
         ,exp_av*17,exp_av*18,exp_av*19,exp_av*20,exp_av*21,exp_av*22,exp_av*23)

