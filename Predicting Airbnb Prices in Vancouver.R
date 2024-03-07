# Predict AirBnB Price

```{r}
# Load standard libraries
library("tidyverse")

# Load data
airbnb <- read.csv('airbnb-vancouver-bc-listings.csv.bz2')

# Select needed variables
airbnb_clean <- airbnb %>% 
  select(price, bedrooms, room_type, accommodates)
summary(airbnb_clean)

# Convert price variable from character to numeric
airbnb_clean<-airbnb_clean %>% 
  mutate(price = as.numeric(gsub("\\$", "", price)))

# Remove missing values in *price* 
airbnb_clean<-airbnb_clean %>% 
  drop_na(price)
summary(airbnb_clean)


# Observe accommodates values with missing bedrooms number
airbnb_clean %>% 
  filter(is.na(bedrooms)) %>% 
  group_by(accommodates) %>% 
  summarize(n())

# Find average accommodates in relation to number of bedrooms
airbnb_clean %>% 
  filter(!is.na(bedrooms)) %>% 
  group_by(bedrooms) %>% 
  summarize(acc_room = round(mean(accommodates), 0))

# Fill in the missing values based on the findings above by dividing  *accommodates* number by 2 and rounding up to get the *bedrooms* value.
airbnb_clean <- airbnb_clean %>%
  mutate(bedrooms = ifelse(is.na(bedrooms), ceiling(accommodates / 2), bedrooms))

# Check price distribution
airbnb_clean %>% 
  ggplot(aes(x=price))+
  geom_bar()+
  labs(title="Airbnb Price Distribution")

# Convert bedrooms number into categories as new variable 
airbnb_clean<-airbnb_clean %>% 
  mutate(RoomGroup = cut(airbnb_clean$bedrooms, breaks =c(0, 1, 2, Inf), labels=c("1","2","3+")))

# Create linear regression model outputs, y= 'price'
lm_output <- lm(price ~ RoomGroup, data=airbnb_clean)
summary(lm_output)

# Create linear regression model outputs, y= 'log(price)'
lm_output_log <- lm(log(price) ~ RoomGroup, data=airbnb_clean)
summary(lm_output_log)


# Create lookup table for room_type
rmtype_table<- c("Entire home/apt" = "Entire home/apt", "Private room" ="Private room", "Hotel room" = "Others", "Shared room" = "Others")

# Convert room_type and accommodates into categories
airbnb_clean<-airbnb_clean %>% 
  mutate(RtypeGroup = rmtype_table[room_type]) %>% 
  mutate(AccGroup = cut(accommodates, breaks =c(0, 1, 2, Inf), labels=c("1","2","3 or more")))

# Create linear regression model outputs
lm_output2 <- lm(log(price) ~ RtypeGroup + AccGroup, data=airbnb_clean)
summary(lm_output2)

# Predict (log) price for all listings in the data
prediction <-predict(lm_output2)
head(prediction)

# Calculate RMSE of prediction
sqrt(mean((prediction - log(airbnb_clean$price))^2))

# Run prediction
newdata <-data.frame(RtypeGroup="Entire home/apt", AccGroup="3 or more")
predict(lm_output2, newdata, type="response")

```

