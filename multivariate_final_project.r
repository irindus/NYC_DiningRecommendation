attach(party_in_nyc)
install.packages("dplyr")
library(dplyr)

#Filtering only in Manhattan for plotting
party_nyc_plot = dplyr::filter(party_in_nyc,Borough=='MANHATTAN')

#Filtering only in Manhattan
party_nyc = dplyr::filter(party_in_nyc,Borough=='MANHATTAN')

#Adding count column
party_nyc$noise_count=1

#Keeping only necessary columns
party_nyc=party_nyc[c('Incident.Zip','noise_count','Latitude','Longitude')]

#Grouping by zip code and summing their incident counts
party_nyc1 = party_nyc %>% group_by(Incident.Zip) %>% mutate(noise_count = sum(noise_count))

#Removing duplicates
party_nyc1=unique(party_nyc1)

#renaming incident.zip to postal_code
party_nyc1$postal_code=party_nyc1$Incident.Zip

#dropping columns: incident.zip, Latitude, Longitude
party_nyc2=party_nyc1[c('postal_code','noise_count')]

#merging restaurant week df and party nyc df on postal_code
party_restaurants=merge(x=restaurant_week,y=party_nyc2,by='postal_code',all.x=TRUE)

#dropping duplicate rows
party_restaurants=unique(party_restaurants)

#dropping rows with NA postal code
party_restaurants = dplyr::filter(party_restaurants, postal_code != 'NA')

#Replacing noise counts with NA to 0
party_restaurants$noise_count[is.na(party_restaurants$noise_count)] = 0


#Categories in price range
catergories = unique(party_restaurants$price_range)
levels(party_restaurants$price_range)


#Assign a price to price ranges
party_restaurants$Price = ifelse(price_range == '$30 and under' , 1, ifelse(price_range=='$31 to $50',2,3))

#Assign Noisy or not 
party_restaurants$noisy = ifelse(noise_count>1000,1,0)


################################################
#Classification
attach(party_restaurants)
install.packages('tree')
install.packages('rpart.plot')
install.packages('ggplot2')
install.packages('ggfortify')
library(tree)
library(rpart)
library(rpart.plot)
library(ggplot2)
library(ggfortify)

mytreehouse=rpart(restaurant_main_type~postal_code+food_review+ambience_review+value_review, cp=0.00001)
rpart.plot(mytreehouse)

printcp(mytreehouse)
plotcp(mytreehouse)
mytreehouse$cptable[which.min(mytreehouse$cptable[,"xerror"]),"CP"]

dev.new()
mytreehouse=rpart(restaurant_main_type~food_review+ambience_review+value_review+price_range, cp=0.007)
rpart.plot(mytreehouse)


#Running PCA
rest_review = party_restaurants[c('average_review','food_review','service_review','ambience_review','value_review','star_1','star_2','star_3','star_4','star_5','noise_count')]
pca = prcomp(rest_review, scale = TRUE)
pca

dev.new()
autoplot(pca, data=rest_review, loadings = TRUE, col=ifelse(rest_review$star_5 > '66', 'green', 'orange'),loadings.label = TRUE,main='Average # of 5-star ratings')

#Clustering

attach(party_restaurants)

rests_man = dplyr::filter(party_restaurants,postal_code<'10050')
party_restaurants_k=rests_man[,c(1,9)]

rownames(party_restaurants_k)=rests_man$name
plot(party_restaurants_k)

party_restaurants_k$'Zip Code'=party_restaurants_k$postal_code
party_restaurants_k$'Average Review'=party_restaurants_k$average_review
party_restaurants_k=party_restaurants_k[c('Zip Code','Average Review')]

km.3=kmeans(party_restaurants_k, 3) #4 clusters
dev.new()
plot(party_restaurants_k, col=(km.3$cluster))
legend("bottomleft",title='Average Reviews by Zip Code' ,legend=c("Lower Manhattan", "Midtown Manhattan","Upper Manhattan"),col=c("green",'black',"red",type='p'),cex=0.8,pch=1)

########################################
#Mapping
########################################
install.packages('maps')
install.packages('mapdata')
library(maps)
library(mapdata)

states <- map_data("state")
manhattan<- subset(states, subregion =="manhattan")

ggplot(data = manhattan) + geom_polygon(aes(x = long, y = lat), fill = "palegreen", color = "black")  + coord_fixed(1.0)

######################
install.packages('leaflet')
library(leaflet)
install.packages('htmltools')
library(htmltools)

df=party_nyc_plot[c('Latitude','Longitude')]
#df=unique(df)
df2=df[c(1:64172),c(1:2)]
df2 = dplyr::filter(df2,Latitude >	40.70)
df2$noise='noise'

leaflet() %>% addTiles() %>% setView(-74.00, 40.74, zoom = 12)
leaflet(df2) %>% addTiles() %>% setView(-74.00, 40.74, zoom = 12) %>% addMarkers(~Longitude, ~Latitude, label = ~htmlEscape(noise),clusterOptions = markerClusterOptions())

#############
#Restaurant week locations
#############

party_restaurants_plot = party_restaurants

#Users choose to filter by price range (or not)
#1 = $30 and under
#2 = #31 to $50
#3 = $50 and over
party_restaurants_plot = dplyr::filter(party_restaurants_plot,Price==3)

#Users choose to filter by Noisiness (or not)
#0 = quiet
#1 = noisy
party_restaurants_plot = dplyr::filter(party_restaurants_plot,noisy==1)

#Users choose to filter by restaurant type (or not)
#Steakhouse, American, Italian, Japanese, Mexican, Indian, French, etc...
party_restaurants_plot = dplyr::filter(party_restaurants_plot,restaurant_type=='Steakhouse')

#Ploting
df3=party_restaurants_plot[c('name','latitude','longitude')]
leaflet(df3) %>% addTiles() %>% setView(-74.00, 40.74, zoom = 12) %>% addMarkers(~longitude, ~latitude, label = ~htmlEscape(name))#, clusterOptions = markerClusterOptions())

