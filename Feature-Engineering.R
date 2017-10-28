library(DescTools)
library(randomForest)
library(corrplot)
library(tidyverse)
rm(list=ls())

### Load Data 
Sessions=read.csv("sessions.csv")
UserTrain=read.csv("train_users_2.csv")
Countries=read.csv("countries.csv")
AgeBucket=read.csv("age_gender_bkts.csv")

head(Sessions,5)
str(UserTrain)

#### Format Training Data

Train01=UserTrain
str(Train01)
Train01$date_account_created=as.Date(Train01$date_account_created)
Train01$timestamp_first_active=as.Date(as.character(Train01$timestamp_first_active), format='%Y%m%d%H%M%S')
Train01$date_first_booking=as.Date(Train01$date_first_booking, format='%Y-%m-%d')

##########################################
##########Apply basic cleaning############
##########################################


# create Final booking valriable
Train01$Booking=1
Train01$Booking[Train01$country_destination=="NDF"]=0
prop.table(table(Train01$Booking_US))

# Create varible for people booking in US
Train01$Booking_US=0
Train01$Booking_US[Train01$country_destination=="US"]=1
table(Train01$Booking_US)


# Check lag in dates
Train01$lag_crt_acc = Train01$date_account_created - Train01$timestamp_first_active ##mostly accounts are created first
summary(Train01$lag_crt_acc)
tapply(Train01$lag_crt_acc, Train01$Booking, mean) ##lag is double for people who make a booking

# Check percentage of NA in each variable
na_count=sapply(Train01, function(x) sum(is.na(x)))
na_ratio=na_count/nrow(Train01)*100
sort(na_ratio)

# Gender - Replave blank with NA
Train01$gender=as.factor(Train01$gender)
Train01$gender[Train01$gender=="-unknown-"]=NA
(prop.table(table(Train01$gender, Train01$Booking))*100)
# Visualize whether Booking patterns vary for Genders
ggplot(data=Train01, aes(x=gender, fill=Booking))+geom_bar(position="identity", alpha=0.5)

# Age - Replace erroneous values in Age with NA 
summary(Train01$age)
Train01$age[Train01$age>150]=NA
Desc(Train01$age[Train01$Booking==1], plotit = T)
table(Train01$Booking)
# Visualize whether Booking patterns vary for Genders
ggplot(data=Train01, aes(x=age, fill=Booking)) + geom_bar(position = "identity", alpha=0.5)

# signup method
summary(Train01$signup_method)

# signup flow
table(Train01$signup_flow)
Train01$signup_flow=as.factor(Train01$signup_flow)

ggplot(data = Train01, aes(x = signup_flow, fill=Booking)) + geom_bar(position = "identity", alpha=0.5)

# Language - does booking patterns vary for languages in destination vs at origin?
summary(Train01$language)
ggplot(data = Train01, aes(x=language, fill=Booking)) + geom_bar(position = "identity", alpha=0.6)

# Check association between market affliation variables
Train01$first_affiliate_tracked[(Train01$first_affiliate_tracked=="")]=NA
Train01$first_affiliate_tracked=as.factor(as.character(Train01$first_affiliate_tracked))

summary(Train01$affiliate_channel)
sort(table(Train01$affiliate_provider))
table(Train01$affiliate_channel, Train01$affiliate_provider)
summary(Train01$first_affiliate_tracked) 
table(Train01$affiliate_channel, Train01$first_affiliate_tracked)####how does untracked links to other variables

# Distribution of customers across different signup apps
summary(Train01$signup_app)
summary(Train01$first_device_type)
summary(Train01$first_browser)
prop.table(table(Train01$signup_method))

#### Visual Inspection of Trends

# How does the booking pattern vary with first device used by users
round(prop.table(table(Train01$first_device_type))*100) # windows desktop - 34%, mac desktop - 42%, iphone+ipad - 16%
ggplot(Train01, aes(x=first_device_type, fill=Booking)) + geom_bar(position = "identity", alpha=0.5)

# How does the booking pattern vary for people who signed up using iOS devices vs web?
round(prop.table(table(Train01$signup_app))*100) ## web - 85%, ios-9%
prop.table(table(Train01$signup_app, Train01$Booking))*100 ##web - 37%, ios - 2.6%
ggplot(Train01, aes(x=signup_app, fill=Booking)) + geom_bar(position = "identity", alpha=0.5)

# Is there a variaton in pattern of people using Chrome vs Firefox vs Safari vs IE(who uses that really?)
round(sort(prop.table(table(Train01$first_browser))*100)) ##chrome - 30%, safari - 21%, firefox - 15%, IE-9.8%, mobile safari - 9%
ggplot(Train01, aes(x=first_browser, fill=Booking)) + geom_bar(position = "identity", alpha=0.5) +theme(axis.text.x=element_text(angle=90,hjust = 1))

# Browser vs Devices
round(prop.table(table(Train01$first_browser, Train01$first_device_type))*100,1)

# How does signup app relates to device type?
round(prop.table(table(Train01$first_device_type, Train01$signup_app))*100,0) ##iphones related to ios but ipads and mac related to web
ggplot(Train01, aes(x=first_device_type, fill=signup_app)) + geom_bar(position = "identity", alpha=0.5) + theme(axis.text.x=element_text(angle=90))
table(Train01$signup_method, Train01$signup_app) ##google is signup method only for android

# Sign up app vs Sign up method
prop.table(table(Train01$signup_app, Train01$signup_method))*100

# How does browser relates to device type?
ggplot(Train01, aes(x=first_browser, fill=first_device_type)) + geom_bar() + theme(axis.text.x=element_text(angle=90))

#####
##### How does target varibale vary with these inputs
#####

# Browser vs Booking
round(prop.table(table(Train01$first_browser, Train01$Booking))*100) ##categorize browsers and combine safari and mobile safari. Similar distribution b/w chrome, firefox. IE, Safari users are less likly to book

# Sign up Method vs Booking
round(prop.table(table(Train01$signup_method, Train01$Booking))*100,0) ##almost similar distribution b/w basic and fb. Google is rarely used as signup method

# First Device type vs Booking
round(prop.table(table(Train01$first_device_type, Train01$Booking))*100,0) ##Win dekstop , Mac dekstop, iphone+ipad. MAc dekstop users have max conversion ratio, followed by ipad and win dekstop
(round(prop.table(table(Train01$first_browser[Train01$Booking==1], Train01$first_device_type[Train01$Booking==1]))*100))

# Sign up flow vs Booking
round(prop.table(table(Train01$signup_flow, Train01$Booking))*100)

###
### Exploring and Visualizing Patterns in date variables
###

Desc(Train01$date_account_created, plotit = T) #### number of accounts are increasing with time - Airbnb popularity increasing
ggplot(Train01, aes(x= date_account_created, fill=Booking)) + geom_bar(position = "identity", alpha=0.5)

Desc(Train01$date_first_booking, plotit = T) ###booking increasing for first 3 qtr and then dipping - explains holiday season effect
ggplot(Train01, aes(x= date_first_booking, fill=Booking)) + geom_bar(position = "identity", alpha=0.5)

Desc(Train01$timestamp_first_active, plotit = T) ##same trend as date first booking and date account created
Desc(Month(Train01$date_account_created[Year(Train01$date_account_created)!="2014"]), plotit = T)

table(Month(Train01$date_account_created[Year(Train01$date_account_created)!="2014"])) ###account creation increases steadily from 1-9 month and dips slightly in 10-12 months. An important parameter cab be that exact monthly trend is getting overshadowed by increase in Airbnb popularity

## Extracting months and days from dates
Train01$month_acc_created=Month(Train01$date_account_created)
Train01$month_first_Active=Month(Train01$timestamp_first_active)
Train01$month_first_booking=Month(Train01$date_first_booking)

Train01$day_acc_created=Day(Train01$date_account_created)
Train01$day_first_Active=Day(Train01$timestamp_first_active)
Train01$day_first_booking=Day(Train01$date_first_booking)

# Is delay in clicking confirmation link affecting the booking pattern?
Train01$lag_crt_acc=as.numeric(Train01$lag_crt_acc)
tapply(Train01$lag_crt_acc, Train01$Booking, mean) ###lag is double for people who book the room


tapply(Train01$date_account_created - Train01$date_first_booking, Train01$Booking, mean)
ggplot(Train01, aes(x= month_first_Active, fill=Booking)) + geom_bar(position = "identity", alpha=0.5)

### Visualize market affiliation variables

# Affiliate channel vs Booking
ggplot(Train01, aes(x=affiliate_channel, fill=Booking)) + geom_bar(position = "identity", alpha=0.5)

# Affiliate Provider vs Booking
ggplot(Train01, aes(x=affiliate_provider, fill=Booking)) + geom_bar(position = "identity", alpha=0.5) + theme(axis.text.x=element_text(angle=90))

# Affiliate tracked vs Booking
ggplot(Train01, aes(x=first_affiliate_tracked, fill=Booking)) + geom_bar(position = "identity", alpha=0.5) + theme(axis.text.x=element_text(angle=90))

# Time stamp first active vs Booking
ggplot(Train01, aes(x=timestamp_first_active, fill=Booking)) + geom_bar(position = "identity")


### Lets explore train data some more!

library(tidyverse)
summary(Train01)

PlotMiss(Train01) # plot of missing values
colMeans(is.na(Train01))*100 #age and genger has >40% data missing

Desc(Train01$country_destination) #58% users never made a booking and 30% made first booking in US

Train01 %>% group_by(country_destination) %>% summarise(perct=mean(is.na(age))) ## 55% times Age is NA for users who do not make a booking
Train01 %>% group_by(country_destination) %>% summarise(perct=mean(is.na(gender))) ## 55% times gender is NA for NDF ## date first booking is NA for country as NDF (no booking). It must be removed from model?

### Step by step variable summary

Desc(Month(Train01$date_account_created), plotit=T) ## Max accounts are created in first 6 months. Can it be related to bookings?
Desc(Month(Train01$date_first_booking), plotit=T) ## very similar trend, these variables must be corelated. ### How to check corelation??? ### read DescTools for 2 variable plots

summary(Train01$gender) ## almost equal distribution between males and females

summary(Train01$age)  ##MAx age as 2014 --- incorrect
Desc(Train01$age[Train01$age < 150], plotit = T) ## almost 780 records with incorrect data ## average age of customer ~ 30

Desc(Train01$signup_method, plotit = T) # basic and fb gets max user 
table(Train01$signup_method, Train01$country_destination)

### Make Booking Variable
Train01$Booking = Train01$country_destination !="NDF"

Desc(Train01$signup_flow, plotit=T) ##max at page # 0 . Does it have relation with booking?
Desc(table(Train01$signup_flow,Train01$Booking), plotit=T) ###how to interpret?

Desc(Train01$language, plotit = T) ##english

Desc(Train01$affiliate_channel, plotit = T) ##direct channel accounts fo 64%
Desc(Train01$affiliate_channel ~ Train01$Booking, plotit = T) ##equally distributed as per freq

Desc(Train01$affiliate_provider) #direct and google provide max
Desc(Train01$affiliate_provider~ Train01$Booking, plotit = T)

Desc(Train01$first_affiliate_tracked) ##should it be related to signup ?##read more about this variable

Desc(Train01$signup_app, plotit = T) ##web and ios lead with Android contributing very less 
Desc(Train01$signup_app~ Train01$Booking, plotit = T)

Desc(Train01$first_browser, plotit = T) ##chrome and safari major contributor. Safari mostly used with ios, this should mean that Mac users are significant contributors. Can be looked using device type?
Desc(Train01$first_device_type, plotit = T) ## device type has similar distribution. How does it corelate to booking?

plot(Train01$first_device_type)
ggplot(Train01, aes(x=first_device_type, fill=Booking)) + geom_bar(position = "identity", alpha=0.5) ##booking proportion seems to be highest in MAc then windows desktop. However desktop other have a very small percentage in overall booking, their proportion is quite high 

table(Train01$first_device_type, Train01$Booking) ##safari is exclusively related to iOS. Chrome and firefox evenly distributed in MAC and windows desktop. IE exclusive to windows Desktop.

### Lets now explore Web sessions of users

library(tidyverse)
library(data.table)
library(stringr)
webSession=fread("sessions.csv")

str(webSession)
View(webSession)

# Merging to bring in target variable from tarin dataset
webSessionBooking= merge(webSession, Train01[c("id", "Booking")], by.x = "user_id", by.y = "id", all.x = T)

summary(webSessionBooking$Booking) ##a hell lot of NAs

# Lets check for unique users from train file who have records in WebSessions data
length(unique(webSessionBooking$user_id[!is.na(webSessionBooking$Booking)])) ##73815
length(unique(webSessionBooking$user_id[is.na(webSessionBooking$Booking)])) ##61669
# We do have session data available for 45% of users 

# remove records with NA
webSessionBooking=webSessionBooking[!is.na(webSessionBooking$Booking),]

# What are some high and low frequency actions performed by users?
HighFreqAction=as.data.frame(table(webSessionBooking$action, webSessionBooking$Booking)) %>% 
  group_by(Var1) %>% mutate(pct=Freq/sum(Freq)*100) %>% filter(Var2==1)

LowFreqAction = as.data.frame(table(webSessionBooking$action, webSessionBooking$Booking)) %>% 
  group_by(Var1) %>% mutate(pct=Freq/sum(Freq)*100) %>% filter(Var2==0)

head(HighFreqAction)
head(LowFreqAction)

# Lets visualize and save this data. May come in handy at a later stage!
ggplot(HighFreqAction, aes(x=Var1, y=pct)) + geom_bar(stat="identity")

View(LowFreqAction)
View(HighFreqAction) ##few actions strongly co-relate with booking
write.csv(x = HighFreqAction, "HighFreqAction.csv", row.names = F)

###
### Clean up sessions data for modeling
###


webSessionBooking=as.data.table(webSessionBooking)
webSessionBooking$flag=1 #create flag for mean etc
row_number(head(webSessionBooking))
webSessionBooking[, rank := sequence(.N), by = c("user_id")]
webSessionBooking[, rev_rank := rev(sequence(.N)), by=c("user_id")]

webSessionBooking %>% head() %>% View()

### Create summarize statistics to be used for processing

# Mean secs spent at each action by each users

webSessionBookingActionMean=webSessionBooking %>% group_by(user_id, action) %>% summarise(feature_value = mean(secs_elapsed, na.rm=T)) %>% mutate(feature="action_mean_secs_elapsed", feature_detail=action)

webSessionBookingActionTypeMean=webSessionBooking %>% group_by(user_id, action_type) %>% summarise(feature_value = mean(secs_elapsed, na.rm=T)) %>% mutate(feature="action_type_mean_secs_elapsed", feature_detail=action_type)

webSessionBookingActionDetailMean=webSessionBooking %>% group_by(user_id, action_detail) %>% summarise(feature_value = mean(secs_elapsed, na.rm=T)) %>% mutate(feature="action_detail_mean_secs_elapsed", feature_detail=action_detail)

webSessionBookingDeviceMean=webSessionBooking %>% group_by(user_id, device_type) %>% summarise(feature_value = mean(secs_elapsed, na.rm=T)) %>% mutate(feature="device_mean_secs_elapsed", feature_detail=device_type)


# Total time spent at each action by each user

webSessionBookingActionSum=webSessionBooking %>% group_by(user_id, action) %>% summarise(feature_value = sum(secs_elapsed, na.rm=T)) %>% mutate(feature="action_sum_secs_elapsed", feature_detail=action)

webSessionBookingActionTypeSum=webSessionBooking %>% group_by(user_id, action_type) %>% summarise(feature_value = sum(secs_elapsed, na.rm=T)) %>% mutate(feature="action_type_sum_secs_elapsed", feature_detail=action_type)

webSessionBookingActionDetailSum=webSessionBooking %>% group_by(user_id, action_detail) %>% summarise(feature_value = sum(secs_elapsed, na.rm=T)) %>% mutate(feature="action_detail_sum_secs_elapsed", feature_detail=action_detail)


webSessionBookingDeviceSum=webSessionBooking %>% group_by(user_id, device_type) %>% summarise(feature_value = sum(secs_elapsed, na.rm=T)) %>% mutate(feature="device_sum_secs_elapsed", feature_detail=device_type)

webSessionBookingActionDetailSum %>% head() %>% View()


# Standard deviation of secs elapsed at each action by each user

webSessionBookingActionSd=webSessionBooking %>% group_by(user_id, action) %>% summarise(feature_value = sd(secs_elapsed, na.rm=T)) %>% mutate(feature="action_sd_secs_elapsed", feature_detail=action)

webSessionBookingActionTypeSd=webSessionBooking %>% group_by(user_id, action_type) %>% summarise(feature_value = sd(secs_elapsed, na.rm=T)) %>% mutate(feature="action_type_sd_secs_elapsed", feature_detail=action_type)

webSessionBookingActionDetailSd=webSessionBooking %>% group_by(user_id, action_detail) %>% summarise(feature_value = sd(secs_elapsed, na.rm=T)) %>% mutate(feature="action_detail_sd_secs_elapsed", feature_detail=action_detail)

webSessionBookingDeviceSd=webSessionBooking %>% group_by(user_id, device_type) %>% summarise(feature_value = sd(secs_elapsed, na.rm=T)) %>% mutate(feature="device_sd_secs_elapsed", feature_detail=device_type)


# Number of sessions for each user

webSessionBookingActionNumber=webSessionBooking %>% group_by(user_id, action) %>% summarise(feature_value = sum(flag, na.rm=T)) %>% mutate(feature="action_number_flag", feature_detail=action)

webSessionBookingActionTypeNumber=webSessionBooking %>% group_by(user_id, action_type) %>% summarise(feature_value = sum(flag, na.rm=T)) %>% mutate(feature="action_type_number_flag", feature_detail=action_type)

webSessionBookingActionDetailNumber=webSessionBooking %>% group_by(user_id, action_detail) %>% summarise(feature_value = sum(flag, na.rm=T)) %>% mutate(feature="action_detail_number_flag", feature_detail=action_detail)

webSessionBookingDeviceNumber=webSessionBooking %>% group_by(user_id, device_type) %>% summarise(feature_value = sum(flag, na.rm=T)) %>% mutate(feature="device_number_flag", feature_detail=device_type)

webSessionBookingActionDetailNumber %>% head() %>% View()


# Weighted mean of seconds elapsed as per rank of actions

webSessionBookingActionWtMean=webSessionBooking %>% group_by(user_id, action) %>% summarise(feature_value = weighted.mean(secs_elapsed,w =1/rank, na.rm=T)) %>% mutate(feature="action_wt_mean_secs_elapsed", feature_detail=action)

webSessionBookingActionTypeWtMean=webSessionBooking %>% group_by(user_id, action_type) %>% summarise(feature_value = weighted.mean(secs_elapsed,w =1/rank, na.rm=T)) %>% mutate(feature="action_type_wt_mean_secs_elapsed", feature_detail=action_type)

webSessionBookingActionDetailWtMean=webSessionBooking %>% group_by(user_id, action_detail) %>% summarise(feature_value = weighted.mean(secs_elapsed,w =1/rank, na.rm=T)) %>% mutate(feature="action_detail_wt_mean_secs_elapsed", feature_detail=action_detail)

webSessionBookingDeviceWtMean=webSessionBooking %>% group_by(user_id, device_type) %>% summarise(feature_value = weighted.mean(secs_elapsed,w =1/rank, na.rm=T)) %>% mutate(feature="device_wt_mean_secs_elapsed", feature_detail=device_type)


# Weighted mean of seconds elapsed as per reverse rank of actions


webSessionBookingActionWtMeanRev=webSessionBooking %>% group_by(user_id, action) %>% summarise(feature_value = weighted.mean(secs_elapsed,w =1/rev_rank, na.rm=T)) %>% mutate(feature="action_wt_mean_rev_secs_elapsed", feature_detail=action)

webSessionBookingActionTypeWtMeanRev=webSessionBooking %>% group_by(user_id, action_type) %>% summarise(feature_value = weighted.mean(secs_elapsed,w =1/rev_rank, na.rm=T)) %>% mutate(feature="action_type_wt_mean_rev_secs_elapsed", feature_detail=action_type)

webSessionBookingActionDetailWtMeanRev=webSessionBooking %>% group_by(user_id, action_detail) %>% summarise(feature_value = weighted.mean(secs_elapsed,w =1/rev_rank, na.rm=T)) %>% mutate(feature="action_detail_wt_mean_rev_secs_elapsed", feature_detail=action_detail)

webSessionBookingDeviceWtMeanRev=webSessionBooking %>% group_by(user_id, device_type) %>% summarise(feature_value = weighted.mean(secs_elapsed,w =1/rev_rank, na.rm=T)) %>% mutate(feature="device_wt_mean_rev_secs_elapsed", feature_detail=device_type)


# Combine all data to proceed for modeling

SessionsProcessed=rbind(
  webSessionBookingActionDetailMean[,-2],
  webSessionBookingActionDetailNumber[,-2],
  webSessionBookingActionDetailSd[,-2],
  webSessionBookingActionDetailSum[,-2],
  webSessionBookingActionDetailWtMean[,-2],
  webSessionBookingActionDetailWtMeanRev[,-2],
  webSessionBookingActionMean[,-2],
  webSessionBookingActionNumber[,-2],
  webSessionBookingActionSd[,-2],
  webSessionBookingActionSum[,-2],
  webSessionBookingActionTypeMean[,-2],
  webSessionBookingActionTypeNumber[,-2],
  webSessionBookingActionTypeSd[,-2],
  webSessionBookingActionTypeSum[,-2],
  webSessionBookingActionTypeWtMean[,-2],
  webSessionBookingActionTypeWtMeanRev[,-2],
  webSessionBookingActionWtMean[,-2],
  webSessionBookingActionWtMeanRev[,-2],
  webSessionBookingDeviceMean[,-2],
  webSessionBookingDeviceNumber[,-2],
  webSessionBookingDeviceSd[,-2],
  webSessionBookingDeviceSum[,-2],
  webSessionBookingDeviceWtMean[,-2],
  webSessionBookingDeviceWtMeanRev[,-2]
)

SessionsProcessed$Booking[SessionsProcessed$user_id %in% webSessionBooking$user_id[webSessionBooking$Booking==1]]=1

write.csv(SessionsProcessed, file = "SessionsProcessed.csv", row.names = F)

###
### Feature engineering on Sessions data continues!
###

sessions_actions<-data.frame(sessions$user_id,sessions$action)
names(sessions_actions)<-c("user_id","action")
sessions_actions<-sessions_actions %>% group_by(user_id,action) %>% summarize(num = n())
sessions_actions<-spread(sessions_actions, action, num)

temp_actions<-sessions_actions[,c(2:ncol(sessions_actions))]
temp_actions[is.na(temp_actions)]<-0
temp_actions[colnames(temp_actions[colSums(temp_actions)<10])]<-NULL

Action_users<-sessions_actions$user_id
temp_actions %>% head() %>% View()
sessions_actions<-data.frame(Action_users,temp_actions)
ncol(temp_actions)
ncol(sessions_actions)

sessions_actions%>% head() %>% View()

sessions_action_detail<-data.frame(sessions$user_id,sessions$action_detail)
names(sessions_action_detail)<-c("user_id","action_detail")
sessions_action_detail<-sessions_action_detail %>% group_by(user_id,action_detail) %>% summarize(num = n())
sessions_action_detail<-spread(sessions_action_detail, action_detail, num)
temp_action_detail<-sessions_action_detail[,c(2:ncol(sessions_action_detail))]
temp_action_detail[is.na(temp_action_detail)]<-0
temp_action_detail[colnames(temp_action_detail[colSums(temp_action_detail)<10])]<-NULL
Action_detail_users<-sessions_action_detail$user_id
ncol(temp_action_detail)
ncol(sessions_action_detail)
sessions_action_detail<-data.frame(Action_detail_users,temp_action_detail)

sessions_action_detail%>% head() %>% View()


colnames(sessions_actions) <- paste("action", colnames(sessions_actions), sep = "")
colnames(sessions_action_detail) <- paste("action_detail", colnames(sessions_action_detail), sep = "")

sessions_actions$user_id<-sessions_actions$actionAction_users
sessions_action_detail$user_id<-sessions_action_detail$action_detailAction_detail_users


final_sessions<-merge(sessions_actions,sessions_action_detail,by="user_id")

final_sessions%>% head() %>% View()

nrow(final_sessions)

write.csv(train_final_sessions,"train_final_sessions.csv")
df_train<-read.csv("train_users_2.csv")
df_train$user_id<-df_train$id
train_final_sessions<-merge(final_sessions,df_train, by="user_id")

# use columns that are relevant to booking
colsForBooking <- c("actionapply_coupon_click_success", "actionapply_reservation", "actionbook",  
                    "actionchange_availability", "actionchange_currency", "actioncoupon_code_click", 
                    "actionpay", "actionpayment_methods", "actionprint_confirmation", "actionrate", 
                    "actionreceipt", "actionrecent_reservations", "action_detailapply_coupon", 
                    "action_detailapply_coupon_click", "action_detailapply_coupon_click_success",
                    "action_detailapply_coupon_error", #"action_detailbooking",
                    "action_detailbook_it",
                    "action_detailchange_availability", "action_detailchange_or_alter", 
                    "action_detailcreate_payment_instrument", "action_detailmodify_reservations")




# some columns only apply to non-English speaking destinations
colForNonEnglish <- c("actionajax_google_translate", "actionajax_google_translate_description", 
                      "actionajax_google_translate_reviews","actionchange_currency",
                      "actioncountry_options", #"actionsouth.america", "actionsouthern.europe", 
                      "actionspoken_languages", "action_detailtranslate_listing_reviews", 
                      "action_detailtranslations","actionlanguages_multiselect","actionspoken_languages",
                      "action_detailuser_languages")


train_final_sessions$BookingDone <- rowSums(train_final_sessions[,colsForBooking], na.rm = TRUE)
train_final_sessions$OtherThanEnglish <- rowSums(train_final_sessions[,colForNonEnglish], na.rm = TRUE)

colfinal1=colnames(final_sessions)
write.csv(colfinal1,"colfinal1.csv")
setdiff(colForNonEnglish,intersect(colfinal, colForNonEnglish))
setdiff(colsForBooking,intersect(colfinal, colsForBooking))

###
### Under and over sampling techniques for people booking outside US
###


library(ROSE)
library(tidyverse)

final_booking= read.csv("D:/Projects/R/Kaggle/Airbnb/AirBnB/Final_Booking.csv", stringsAsFactors = F)
summary(final_booking)

final_booking$Booking_New %>% table() %>% prop.table()

# splitting to test and rest
booking_test=final_booking[final_booking$Validation==2,]
booking=final_booking[final_booking$Validation!=2,]

booking %>%  dim()
booking$Booking_Non.US %>% table() ###this variable will be sampled
booking$index=seq.int(nrow(booking)) ###create row index

# separte data for sampling
booking %>% colnames()
data_sample=booking[c(87, 85)]
data_sample %>% head()
data_sample$Booking_Non.US =factor(data_sample$Booking_Non.US)


# sampling

data_sample_under=ovun.sample(Booking_Non.US~., data = data_sample, method = "under", N=46000)$data
data_sample_under$Booking_Non.US %>% table()

data_sample$Booking_Non.US %>% table()
data_sample_over=ovun.sample(Booking_Non.US~., data = data_sample, method = "over", N=220000)$data
data_sample_over %>% head() %>% View()
data_sample_over$Booking_Non.US %>% table()

# getting sampled data
rm(booking_data_over)

booking_data_under=booking[data_sample_under$index,]
booking_data_under %>% dim()

booking_data_over=booking[data_sample_over$index,]
booking_data_over$Booking_New %>% table()

# remove indices
booking_data_over$index=NULL
booking_data_under$index=NULL

booking_data_over$Booking_New %>% table() %>% prop.table()

# concat with test dataset
booking_over = rbind(booking_data_over, booking_test)
booking_under = rbind(booking_data_under, booking_test)

booking_under$Booking_New %>% table()
booking_over$Booking_New %>% table() 

booking_over$Booking_New %>% table() %>% prop.table()
booking_under$Validation %>% table() %>% prop.table()

# write data to files
write.csv(booking_over, "booking_data_over.csv", row.names = F)
write.csv(booking_under, "booking_data_under.csv", row.names = F)