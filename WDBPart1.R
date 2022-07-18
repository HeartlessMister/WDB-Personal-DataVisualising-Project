#Installation packages: WDI and ggplot2

#install.packages("WDI")
#install.packages("ggplot2")

#WDI & ggplot2 packages are required.

#Question 1 Part A:

#Libraries

library(WDI)

library(ggplot2)

#I assigned the desired terms to the variables using their indexes.

Gdp_Per_Capita <- WDIsearch("gdp.*capita.*US")[1]
    
Gdp_Per_Person_Employed <- WDIsearch("gdp.*person")[1]
    
Consumer_Price_Index_Base2010 <- WDIsearch("consumer.*price.*index")[6]
    
wdi_dat <- WDI(indicator = c(Gdp_Per_Capita,Gdp_Per_Person_Employed,Consumer_Price_Index_Base2010),
                                                  start = 2012, end = 2012)


names(wdi_dat)

#Table Columns are re-named in more understandable format.

names(wdi_dat)[which(names(wdi_dat) == Gdp_Per_Capita)] <- "Gdp_Per_Capita"
names(wdi_dat)[which(names(wdi_dat) == Gdp_Per_Person_Employed)] <- "Gdp_Per_Person_Employed"
names(wdi_dat)[which(names(wdi_dat) == Consumer_Price_Index_Base2010)] <- "Consumer_Price_Index_Base2010"

#50 Countries are randomly selected.

wdi_dat <- subset(wdi_dat, country %in% c("United States", "Rwanda", "Mongolia","Romania",
                                          "Pakistan", "Lao PDR", "Bhutan", "Malaysia", 
                                          "Brazil", "Ireland", "Japan", "Sweden", "Netherlands",
                                          "Algeria","Albania,","Angola","Azerbaijan","Banladesh",
                                          "Australia","Austria","Belgium","Benin","Bhutan",
                                          "Bolivia", "Brazil", "Bulgaria", "Burund", "Cameroon", "Canada", "Chile"
                                          ,"China","Denmark", "Czech Republic", "Cyprus", "Ethiopia","Finland","France",
                                          "Georgia","Germany","Greece","Hungary","Iceland","India","Iraq",
                                          "Israel","Italy","Kazakhstan","Kenya","Jordan","Kosova","Latvia","Luxembourg",
                                          "Madagascar","Mexico","Norway","Turkey","Russia"))


#Question 1 Part B:

#Mean value calculations for year 2012, Per Person Employed and Consumer Price Index(2010 based):
mean(wdi_dat$Gdp_Per_Person_Employed)
mean(wdi_dat$Consumer_Price_Index_Base2010)


#Median value calculations for year 2012, Per Person Employed and Consumer Price Index(2010 based):

median(wdi_dat$Gdp_Per_Person_Employed)
median(wdi_dat$Consumer_Price_Index_Base2010)

#First Quantile calculations for year 2012, Per Person Employed and Consumer Price Index(2010 based):
quantile(wdi_dat$Gdp_Per_Person_Employed,0.25)
quantile(wdi_dat$Consumer_Price_Index_Base2010,0.25)


#Third Quantile calculations for year 2012, Per Person Employed and Consumer Price Index(2010 based):
quantile(wdi_dat$Gdp_Per_Person_Employed,0.75)
quantile(wdi_dat$Consumer_Price_Index_Base2010,0.75)

#The standard deviation calculations for year 2012, Per Person Employed and Consumer Price Index(2010 based):
sd(wdi_dat$Gdp_Per_Person_Employed)
sd(wdi_dat$Consumer_Price_Index_Base2010)


#Question 1 Part C:

# Histogram Plot of GDP Per Capita

ggplot(wdi_dat, aes(Gdp_Per_Capita, fill = country))+
  geom_histogram(binwidth = 1000)+
  ggtitle("GDP Per Capita")+
  labs(x = "Gdp Per Capita", y = "Frequency")

# Histogram Plot of GDP Per Person Employed

ggplot(wdi_dat, aes(Gdp_Per_Person_Employed, fill = country))+
  geom_histogram(binwidth = 1000)+
  ggtitle("GDP Per Person Employed")+
  labs(x = "Gdp Per Person Employed", y = "Frequency")+
  theme_minimal()
  

#Question 1 Part D:


# Logarithmic Plot of Gdp_Per_Person_Employed vs GDP
ggplot(wdi_dat, aes(x = Gdp_Per_Capita, y = Gdp_Per_Person_Employed)) + 
  geom_point() +  
  labs(x = "GDP Per Capita", y = "GDP Per Person Employed ")+
  ggtitle("Gdp Per Person Employed vs GDP Per Capita")+
  geom_text(aes(label = country), size=3, nudge_y = 0.05) +
  scale_x_continuous(trans='log')+
  scale_y_continuous()+
  theme_bw()

# Logarithmic Plot of Consumer_Price_Index_Base2010 vs GDP
ggplot(wdi_dat, aes(x = Gdp_Per_Capita, y = Consumer_Price_Index_Base2010)) + 
  geom_point() +  
  labs(x = "GDP Per Capita", y = "Consumer Price Index (Base 2010)")+
  ggtitle("Consumer Price Index  vs GDP Per Capita")+
  geom_text(aes(label = country), size=3, nudge_y = 0.01)+
  scale_x_continuous(trans='log')+
  scale_y_continuous()+
  theme_bw()




