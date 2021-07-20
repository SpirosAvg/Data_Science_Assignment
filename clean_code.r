## Clean Spitogatos Code


## libraries and data load 

library(ggplot2)
library(plotly)
library(groupedstats)
library(plyr)
library(DT)
library(doBy)
library(caret)
library(naniar)
library(ggpubr)
library(mlbench)
library(Hmisc)


dat<-read.csv("C:/Users/SpirosAvgoustatos/Downloads/assignment_rev2.csv")
dat_raw<-dat

## Part 1 


## Part 1 

## check under construction
listings_under_construction<-nrow(table(dat[dat$year_of_construction==2155,]$price))
under_construction_percentage<-listings_under_construction/nrow(dat)



# check NAs in price
price_missing<-sum(is.na(dat$price))
price_missing_percentage<-price_missing/nrow(dat)


## remove under construction cause its future price  remove prices below 1000?
price_freq<-as.data.frame(table(dat$price))
names(price_freq)<-c("price","count")
dat<-dat[dat$year_of_construction!=2155,]
dat<-dat[dat$price>1000,]
## price outliers total
## this seems to remove 11% of northern which seems unfair since prices depend on geo


outs<-boxplot(dat$price, plot=FALSE)$out

dat_no_outliers<-dat[-which(dat$price %in% outs),]
dat_only_outliers<-dat[which(dat$price %in% outs),]

geo_out<-as.data.frame(table(dat_only_outliers$geography_name))
geo_count<-as.data.frame(table(dat$geography_name))

names(geo_out)<-c("geography_name","count_outliers")
names(geo_count)<-c("geography_name","count")

merged_geo_out<-merge(geo_out,geo_count,by="geography_name")

merged_geo_out$removed_percentage<-merged_geo_out$count_outliers/merged_geo_out$count

no_out_len<-nrow(dat_no_outliers)
no_outliers_keep_percentage<-no_out_len/nrow(dat)


# remove outliers per geo 
## these seems more balanced

geo_names<-as.data.frame(table(dat$geography_name))
names(geo_names)<-c("geography_name","count")
geo_names<-geo_names[,1]

dat_per_geo<-list()
dat_per_geo_no_outliers<-list()
dat_per_geo_only_outliers<-list()
merged_geo_per_geo<-list()

for (i in 1:length(geo_names)) {
  
  dat_per_geo[[i]]<-dat[dat$geography_name==geo_names[i],]
  
  outs_temp<-boxplot(dat_per_geo[[i]]$price, plot=FALSE)$out
  
  dat_per_geo_no_outliers[[i]]<-dat_per_geo[[i]][-which(dat_per_geo[[i]]$price %in% outs_temp),]
  dat_per_geo_only_outliers[[i]]<-dat_per_geo[[i]][which(dat_per_geo[[i]]$price %in% outs_temp),]
  
  geo_out_temp<-as.data.frame(table(dat_per_geo_only_outliers[[i]]$geography_name))
  geo_count_temp<-as.data.frame(table(dat_per_geo[[i]]$geography_name))
  
  names(geo_out_temp)<-c("geography_name","count_outliers")
  names(geo_count_temp)<-c("geography_name","count")
  
  merged_geo_per_geo[[i]]<-merge(geo_out_temp,geo_count_temp,by="geography_name")
  
  merged_geo_per_geo[[i]]$removed_percentage<-merged_geo_per_geo[[i]]$count_outliers/merged_geo_per_geo[[i]]$count
  
}

merged_geo_per_geo<-do.call("rbind",merged_geo_per_geo)
dat_per_geo_no_outliers<-do.call("rbind",dat_per_geo_no_outliers)
final_percentage_kept<-nrow(dat_per_geo_no_outliers)/nrow(dat)
final_percentage_kept_raw<-nrow(dat_per_geo_no_outliers)/nrow(dat_raw)

names(merged_geo_out)<-c("Geography Name","Count of Outliers","Count of Total Listings","Removed Listings Percentage")

names(merged_geo_per_geo)<-c("Geography Name","Count of Outliers","Count of Total Listings","Removed Listings Percentage")










## Group Summaries
both_groups<-grouped_summary(
  data = dat_per_geo_no_outliers,
  grouping.vars = c(subtype,geography_name),
  measures = price,
  measures.type = "numeric"
)

subtype_group<-grouped_summary(
  data = dat_per_geo_no_outliers,
  grouping.vars = c(subtype),
  measures = price,
  measures.type = "numeric"
)

geo_group<-grouped_summary(
  data = dat_per_geo_no_outliers,
  grouping.vars = c(geography_name),
  measures = price,
  measures.type = "numeric"
)


both_groups<-both_groups[,c(1,2,7,8,9,11,13,14)]
subtype_group<-subtype_group[,c(1,6,7,8,10,12,13)]
geo_group<-geo_group[,c(1,6,7,8,10,12,13)]


## Part 2


dat2<-dat_raw[dat_raw$price>1000,]




### per area total
ad_freq_per_geo<-data.frame(table(dat2$geography_name, dat2$ad_type))
names(ad_freq_per_geo)<-c("geography_name","ad_type","count")

ad_freq<-data.frame(table(dat2$geography_name))
names(ad_freq)<-c("geography_name","count_of_ad")

ad_freq_merged<-merge(ad_freq_per_geo,ad_freq,by="geography_name")
ad_freq_merged$percent_in_geo<-ad_freq_merged$count/ad_freq_merged$count_of_ad
average_ranking_per_geo_ad_type<-summaryBy(ranking_score ~ geography_name + ad_type, dat2, FUN=median)
comp_tables<-merge(ad_freq_merged,average_ranking_per_geo_ad_type,by=c("geography_name","ad_type"))

comp_tables$ad_type_ordinal<-0
comp_tables[comp_tables$ad_type=="up",]$ad_type_ordinal<-1
comp_tables[comp_tables$ad_type=="premium",]$ad_type_ordinal<-2
comp_tables[comp_tables$ad_type=="star",]$ad_type_ordinal<-3

comp_tables$vs_score<-1-comp_tables$percent_in_geo


comp_tables_per_geo<-split(comp_tables,ad_freq_merged$geography_name)

area_score_table<-list()

for (i in 1:length(comp_tables_per_geo)){
  
  temp<-comp_tables_per_geo[[i]][comp_tables_per_geo[[i]]$ad_type_ordinal==0,]$ranking_score.median
  comp_tables_per_geo[[i]]$vs_score2<-temp/comp_tables_per_geo[[i]]$ranking_score.median-1
  
  comp_tables_per_geo[[i]]<-comp_tables_per_geo[[i]][comp_tables_per_geo[[i]]$ad_type_ordinal!=0,]
  
  comp_tables_per_geo[[i]]$ad_type_opportunity_score<-comp_tables_per_geo[[i]]$vs_score/abs(comp_tables_per_geo[[i]]$vs_score2)
  
  comp_tables_per_geo[[i]]$area_opportunity_score<-mean(comp_tables_per_geo[[i]]$ad_type_opportunity_score)
  
  area_score_table[[i]]<-as.data.frame(cbind(as.character(unique(comp_tables_per_geo[[i]]$geography_name)),comp_tables_per_geo[[i]]$area_opportunity_score[1]))
  
  names(area_score_table[[i]])<-c("geography_name","area_score")
}



comp_tables_per_geo<-do.call("rbind",comp_tables_per_geo)
area_score_table<-do.call("rbind",area_score_table)
area_score_table$area_score<-as.numeric(as.character(area_score_table$area_score))
## Part 3

## prepare data for ml 

mldat<-dat_no_outliers[dat_no_outliers$year_of_construction!=0,]
mldat<-mldat[mldat$sq_meters<20000,]
mldat<-mldat[mldat$year_of_construction>1920,]
mldat<-mldat[mldat$sq_meters<1500,]


dat_no_outliers[dat_no_outliers$energy_class=="",]$energy_class<-NA


# subset data for ml after missing analysis

mldat<-mldat[,c(6,4,5,7,8,10,11,13,14,16,17,18)] ## maybe to use ? 16:56

#recode floor 

mldat[mldat$floor=="basement",]$floor<-1
mldat[mldat$floor=="semi-basement",]$floor<-0.5
mldat[mldat$floor=="ground-floor",]$floor<-0
mldat[mldat$floor=="mezzanine",]$floor<-0.5

# remove nas from renovation
mldat[is.na(mldat$renovation_year)==TRUE,]$renovation_year<-0

mldat$floor<-as.numeric(mldat$floor)



dmy <- dummyVars(" ~ .", data = mldat,fullRank = T) 
mldat_transformed <- data.frame(predict(dmy, newdata = mldat))



inTraining<-createDataPartition(1:nrow(mldat_transformed), p = 0.80,list = FALSE)
training<-mldat_transformed[ inTraining,]
test<-mldat_transformed[-inTraining,]




subsets <- c(2:14)
control <- rfeControl(functions=lmFuncs, method="repeatedcv",repeats=5,number=10)
choose_vars<-rfe(x=mldat_transformed[,c(2:14)], y=mldat_transformed$price,sizes=subsets,
                 rfeControl = control)


training<-training[,c("price",choose_vars$optVariables)]
test<-test[,c("price",choose_vars$optVariables)]

fitControl <-trainControl(method="repeatedcv", repeats=5,number=10)

### exclude less important variables

training<-training[,-c(9,11)]
test<-test[,-c(9,11)]

model <- train(price~.,data =training, method='lm',preProcess=c("center","scale"),trControl= fitControl,na.action = na.omit)

importance<-varImp(model,value="rss")


## test evaluation 
test<-test[complete.cases(test),]
test$predictions<- predict(model, test[,-1])

test$absolute_error<-(abs((test$predictions/test$price)-1))*100



## in training evaluation


training$predictions<- predict(model, training[,-1])

training$absolute_error<-(abs((training$predictions/training$price)-1))*100


mape<-mean(test$absolute_error)
mape_t<-mean(training$absolute_error)


temp1<-rep("Actuals",length(test$price))
temp2<-rep("Predictions",length(test$price))


temp3<-data.frame(test$price,temp1,1:length(test$price))
temp4<-data.frame(test$predictions,temp2,1:length(test$price))

names(temp3)<-c("Price","Type","x_axis")
names(temp4)<-c("Price","Type","x_axis")

test_to_plot<-rbind(temp3,temp4)




