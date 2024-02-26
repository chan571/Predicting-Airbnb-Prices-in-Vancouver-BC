## Data Inspection and Cleaning
```
# Load data
airbnb <- read.csv('airbnb-vancouver-bc-listings.csv.bz2')

# Select needed variables
airbnb_clean <- airbnb %>% 
  select(price, bedrooms, room_type, accommodates)
summary(airbnb_clean)
```
<img width="601" alt="Screenshot 2024-02-13 at 8 39 43 PM" src="https://github.com/cp571/Predicting-Airbnb-Prices-in-Vancouver-BC/assets/157858508/a05f3442-4611-4f6d-9200-f205302be717">

The *price* variable is character when it should be numeric for analysis. 
Additionally, the *bedroom* variable contains missing values.

```
# Convert price variable from character to numeric
airbnb_clean<-airbnb_clean %>% 
  mutate(price = as.numeric(gsub("\\$", "", price)))

# Remove missing values in *price*
airbnb_clean<-airbnb_clean %>% 
  drop_na(price)
summary(airbnb_clean)
```
Next, fill in the missing values in *bedrooms*. 
Upon investigating the relationship between the 'bedrooms' and 'accommodates' variables, I have found that listings with fewer than 13 bedrooms typically accommodate an average of 2 guests per room. 
The maximum value of accommodates with missing bedrooms is 10, which should align well with this rule.

```
# Observe accommodates values with missing bedrooms number
airbnb_clean %>% 
  filter(is.na(bedrooms)) %>% 
  group_by(accommodates) %>% 
  summarize(n())
```
<img width="206" alt="Screenshot 2024-02-13 at 8 51 47 PM" src="https://github.com/cp571/Predicting-Airbnb-Prices-in-Vancouver-BC/assets/157858508/97ea9447-d46b-4038-a1b0-705c8a8252cf">

```
# Find average accommodates in relation to number of bedrooms
airbnb_clean %>% 
  filter(!is.na(bedrooms)) %>% 
  group_by(bedrooms) %>% 
  summarize(acc_room = round(mean(accommodates), 0))
```
<img width="199" alt="Screenshot 2024-02-13 at 8 52 21 PM" src="https://github.com/cp571/Predicting-Airbnb-Prices-in-Vancouver-BC/assets/157858508/3d3831ff-9340-47b4-ad20-eb5e1da22cc7">

```
#Fill in the missing values based on the findings above by dividing accommodates number by 2 and rounding up to get the bedrooms value

airbnb_clean <- airbnb_clean %>%
  mutate(bedrooms = ifelse(is.na(bedrooms), ceiling(accommodates / 2), bedrooms))


summary(airbnb_clean)
```
<img width="558" alt="Screenshot 2024-02-13 at 8 54 05 PM" src="https://github.com/cp571/Predicting-Airbnb-Prices-in-Vancouver-BC/assets/157858508/6fa78a31-4140-470d-a700-ad3056be54b1">


## Data Analysis

The price distribution does not resemble a normal distribution. 
It has a long thin tail for Airbnb that are higher price, but the majority of listings are below $250. 
It is suggested to do a log-transformation because it is highly skewed.

```
airbnb_clean %>% 
  ggplot(aes(x=price))+
  geom_bar()+
  labs(title="Airbnb Price Distribution")
```
![image](https://github.com/cp571/Predicting-Airbnb-Prices-in-Vancouver-BC/assets/157858508/6d5f1137-7806-4e09-ac90-64bcb5a1e0ae)

Next, we examine how the number accommodates and room type predict the (log) price of Airbnb listings in Vancouver, BC.
Before proceeding, the room type is being converted into three categories: combining Hotel and Shared Room, as they do not have many listings.
```
# Check what kind of values inside *room_type* 
acc_count<- table(airbnb_clean$accommodates)
acc_count
```
<img width="657" alt="Screenshot 2024-02-13 at 9 18 42 PM" src="https://github.com/cp571/Predicting-Airbnb-Prices-in-Vancouver-BC/assets/157858508/994e67a8-ef0e-4ac1-a85f-e35ce2b998fc">

```
Check what kind of values inside *accommodates*
roomtype_count<-table(airbnb_clean$room_type)
roomtype_count
```
<img width="546" alt="Screenshot 2024-02-13 at 9 19 47 PM" src="https://github.com/cp571/Predicting-Airbnb-Prices-in-Vancouver-BC/assets/157858508/cd2dd654-9ad3-48f3-bc0c-0e91e0b4327f">

```
# Create lookup table for room_type
rmtype_table<- c("Entire home/apt" = "Entire home/apt", "Private room" ="Private room", "Hotel room" = "Others", "Shared room" = "Others")

# Convert room_type and accommodates into categories
airbnb_clean<-airbnb_clean %>% 
  mutate(RtypeGroup = rmtype_table[room_type]) %>% 
  mutate(AccGroup = cut(accommodates, breaks =c(0, 1, 2, Inf), labels=c("1","2","3 or more")))
```

Run linear regression model to explain log price with predictors, room type and accomodates. 
Here are the findings:
The relevant reference categories are room type ‘Entire home/apt’ and accommodates =1. 
All variables are statistical significance except room type ‘Others’. 
‘Others’ is not statistically significant suggests that there isn’t sufficient evidence to demonstrate that shared room and hotel rooms have an effect on the price. 
It could indicate a higher correlation between price and other variables. 
Another possibility is due to small sample size, as there are only 12 of them compared to the other groups having 3556 and 849 of listings.
Price of room type ‘Entire home/apt’ is higher than the others on average. 
More accommodates allow, the price of Airbnb is higher. The R squared value is 0.3405 for this multiple regression model.

```
# Create linear regression model outputs
lm_output2 <- lm(log(price) ~ RtypeGroup + AccGroup, data=airbnb_clean)
summary(lm_output2)
```
<img width="616" alt="Screenshot 2024-02-13 at 9 25 03 PM" src="https://github.com/cp571/Predicting-Airbnb-Prices-in-Vancouver-BC/assets/157858508/2f5efa09-fb99-4164-8d61-7473cf522e8d">

## Price Prediction

Now we use the model above to predict (log) price for each listing in the data. 
```
# Predict (log) price for all listings in the data
prediction <-predict(lm_output2)
head(prediction)
```
<img width="464" alt="Screenshot 2024-02-13 at 9 27 35 PM" src="https://github.com/cp571/Predicting-Airbnb-Prices-in-Vancouver-BC/assets/157858508/9c7705c2-3c8d-4d9a-9625-f1525fa76230">

Check RMSE of my predictions
```
# Calculate RMSE of prediction
sqrt(mean((prediction - log(airbnb_clean$price))^2))
```
<img width="158" alt="Screenshot 2024-02-13 at 9 28 28 PM" src="https://github.com/cp571/Predicting-Airbnb-Prices-in-Vancouver-BC/assets/157858508/08559b23-6414-47e6-a259-aec3c6473995">

Predict log price for a 2-bedroom apartment that accomodates 4 people. 
```
newdata <-data.frame(RtypeGroup="Entire home/apt", AccGroup="3 or more")
predict(lm_output2, newdata, type="response")
```
<img width="95" alt="Screenshot 2024-02-13 at 9 29 30 PM" src="https://github.com/cp571/Predicting-Airbnb-Prices-in-Vancouver-BC/assets/157858508/344dfb25-fcf5-4ece-a0f7-8cd45db85adf">

Note: We can use the exponential function exp() to convert it back to the original scale. For a 2-bedroom apartment that accommodates 4 people, a log price of 5.179181 would be approximately $177.

