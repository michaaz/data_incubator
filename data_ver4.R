library("dplyr")
library("data.table")
library("tidyr")
library(scales) #coloring
library(ggplot2)

# The script checks if the by_state1.txt file with preprocessed data is already present
# If not the script will check if the raw data if present from which the by_state1.txt
# file willbe generated  ina deterministic way (the resultof preprocessing willbe always the same)
# If raw data is not present then:
# 1.zipfiles with the data will be downloaded from https://freddiemac.embs.com/FLoan/Data/
# one zip file per eachquarter per year, for each year between 2000 and 2015. The download
# may take up to several hours depending on connection speed
# 2. Immediately after downloading each zip file will be unzipped and data for that quarter
# will be stored in a separate folder the total size of the data is about 60GB
# 3. Immediately after the zip file is unzipped, the zipfilee itself is deleted
# 4. The relevant data from  each folder is extracted into a data table
# 5. Complete cases from all data tables are combined into one data table and 
# stored in the text file by_state1.txt for furteher processing


if(!file.exists("by_state1.txt")){
  
  url <- "https://freddiemac.embs.com/FLoan/Data/"
  
  for (y in 2000:2015){
    for(q in 1:4){
      folder <- paste("Q",as.character(q),as.character(y), sep="")
      if(!file.exists(folder)){
        zipfile <- paste("historical_data1_", folder , ".zip", sep="")
        download.file(paste(url, zipfile, sep=""), zipfile)
        unzip(zipfile, exdir = folder)
        unlink(zipfile)
      }
    }
  }
  
  for (y in 2000:2015){
    for(q in 1:4){
      folder <- paste("Q",as.character(q),as.character(y), sep="")
      file <- paste(folder, "/historical_data1_Q", q, y, ".txt", sep="")
      origclass <- c('integer','integer','character', 'integer', 'character', 'real', 'integer',
                     'character','real','integer','integer','integer','real','character','character','character','character',
                     'character','character','character','character', 'integer', 'integer','character','character' ,'character' )
      
      if(file.exists(file)){
        #df <- as.data.table(read.table(file, sep="|", header=FALSE, colClasses=origclass))
        df <- fread(file, sep="|", header=FALSE, select=c(1,3,10,12,17,18))
        orig_names<-c('fico','dt_first_pi','flag_fthb','dt_matr','cd_msa',"mi_pct",'cnt_units','occpy_sts','cltv'
                      ,'dti','orig_upb','ltv','int_rt','channel','ppmt_pnlty','prod_type','st', 'prop_type','zipcode','id_loan','loan_purpose',
                      'orig_loan_term','cnt_borr','seller_name','servicer_name', 'flag_sc') 
        names(df) <- orig_names[c(1,3,10,12,17,18)]
        
        df <- df[complete.cases(df),]
        #grouping by state, property type, first buyer flag
        new_by_state <- df %>% 
          group_by(st, flag_fthb, prop_type) %>%
          summarize(count = n(), fico_avg = mean(fico,na.rm=TRUE), avg_ltv = mean(ltv,na.rm=TRUE),
                    avg_dti = mean(dti,na.rm=TRUE)) %>%
          mutate(year =  as.character(y), quart = as.character(q))
        if(exists("by_state")){
          by_state <- rbind(by_state, new_by_state)
        }else{
          by_state <- new_by_state
        }
      }else{
        print("No such file")
      }
    }
  }
  write.table(by_state, "by_state1.txt")
}else{
  data <- read.table("by_state1.txt")
}

# averaging  data over quarters to eliminate seasonality
by_year<- data %>% group_by(year, st, flag_fthb,prop_type) %>%
  summarise(fico_avg=weighted.mean(fico_avg,  count, na.rm=TRUE), weighted.mean(avg_ltv, count,na.rm=TRUE),
            weighted.mean(avg_dti, count,na.rm=TRUE), count=sum(count))

# we extract single family  houses

# the data with siffix N contains single family houses where the buyer is Not a first time buyer
# the data is also reformatted into a wide form for easier plotting by state
single_fam_total_N <- by_year %>%
  filter(prop_type=="SF" & flag_fthb == "N") %>%
  group_by(year, st)%>%
  summarize(count = sum(count))%>%
  spread(st, count)
setnames(single_fam_total_N, names(single_fam_total_N)[-1], paste(names(single_fam_total_N)[-1], "N", sep="_"))

# the data with siffix N contains single family houses where the buyer is a first time buyer
single_fam_total_Y<- by_year %>%
  filter(prop_type=="SF" & flag_fthb == "Y") %>%
  group_by(year, st)%>%
  summarize(count = sum(count))%>%
  spread(st, count)
setnames(single_fam_total_Y, names(single_fam_total_Y)[-1], paste(names(single_fam_total_Y)[-1], "Y", sep="_"))
single_fam_total <- merge(single_fam_total_Y, single_fam_total_N)

years <- single_fam_total$year
single_fam_total<-as.data.table(single_fam_total)
single_fam_total[,year:=NULL]
# For all instances (1 in this case) where no data for a given year was present we put the value 0
single_fam_total[is.na(single_fam_total)] <- 0;

# In order to remove the total size of the given market we normalize by dividing
# by the sum of all mortgages in that market for teh period analyzed
# We found that  this approach worked better than other niormalizations
single_fam_total_norm<-data.frame(lapply(single_fam_total,function(x) x/sum(x)))

# A draft plot of all the markets is created for first visual analysys 

plot(years, single_fam_total_norm[,1], 
     type='l', xlab="Year", 
     ylab="Sf housses", 
     col = 1, 
     ylim=c(0,0.3),
     main="Single family houses by state and first time buyer (Y/N)")
for (i in seq(2:ncol(single_fam_total_norm))) {
  lines(years, single_fam_total_norm[,i],
        col = i,
        lw=2)
}
legend(x=2010, y=0.3, 
       lty=1, cex = 0.4,
       ncol = 7,
       col=1:22,
       legend=colnames(single_fam_total_norm))



#   We perform the PCA analysys on the normalized (divided by the sum) data
pca_sf_total <- prcomp(t(single_fam_total_norm), scale. = TRUE)
plot(pca_sf_total)
 
# use the firsrt two pc to represent the data
scores_sf_total_df <-as.data.frame(pca_sf_total$x)
head(scores_sf_total_df[1:2])
#   
#   plotting the principal components
#   coloring approximately by the loadings of the first PCA
ramp <- colorRamp(c("blue", "red"))
colors_by_low_high <- rgb(
  ramp( as.vector(rescale(apply(single_fam_total_norm,2,
                                function(x) abs((sum(x[1],x[12:16]))-sum(x[2:11]))),c(0,1)))),max = 255)
plot(PC1~PC2, data=scores_sf_total_df,
     main= "Single Family Homes",
     cex = .1, lty = "solid", col=colors_by_low_high)
text(PC1~PC2, data=scores_sf_total_df,
     labels=colnames(single_fam_total),
     cex=.8, col=colors_by_low_high)

# An attempt to create the plot with ggplot. For some reason 
# the coloring does not seem to reflect the separation
# In this case will stick to the default plotting methods

qplot(PC2,PC1,data=scores_sf_total_df,
      main = "Single Family homes markets",
      geom="point", color=colors_by_low_high) +
  theme(legend.position="none") +
  geom_text(aes(label=colnames(single_fam_total)),hjust=0, vjust=0)



#   plotting the principal components
#   coloring approximately by the standard deviation of each of the markets
ramp <- colorRamp(c("blue", "red"))
colors_by_sd <- rgb(
  ramp( as.vector(rescale(apply(single_fam_total_norm,2,function(x) sd(x)),c(0,1)))),
  max = 255)
plot(PC1~PC2, data=scores_sf_total_df,
     main= "Single Family Homes",
     cex = .1, lty = "solid", col=colors_by_sd)
text(PC1~PC2, data=scores_sf_total_df,
     labels=colnames(single_fam_total),
     cex=.8, col=colors_by_sd)
#   
  

#kMeans We run the kMeans classification algorithm


t_single_fam_total <- t(single_fam_total_norm)
wss <- (nrow(t_single_fam_total)-1)*sum(apply(t_single_fam_total,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(t_single_fam_total,
                                     centers=i, iter.max=1000)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# we visualize teh clusterimg with 4 clusters
n_centers <- 4
#set.seed(1)
single_fam_clustering <- kmeans(t_single_fam_total, centers = n_centers)


single_fam_cluster_groups <- single_fam_clustering$cluster
plot(PC1~PC2, data=scores_sf_total_df, 
     main= "Single Family Homes",
     cex = .1, lty = "solid", col=single_fam_cluster_groups+1)
text(PC1~PC2, data=scores_sf_total_df, 
     labels=colnames(single_fam_total),
     cex=.8, col=single_fam_cluster_groups+1)


# Finally we plot the first graph this time with coloring depending on the cluster classification
clusters <- single_fam_clustering$cluster
states_by_cluster<-sapply(1:n_centers, function(x) which(clusters==x))
single_fam_total<-data.frame(single_fam_total)
single_fam_total_norm <- data.frame(single_fam_total_norm)

plot( years, single_fam_total_norm[,1], 
     type='l', xlab="Year", 
     ylab="Single Family housses", 
     col = NULL,
     ylim=c(0,0.3),
     main="Trend in sales of houses for different states")
for(clust in 1:n_centers){
  for(i in 1:length(unlist(states_by_cluster[clust]))){
    lines(years, single_fam_total_norm[,states_by_cluster[[clust]][i]],
          col = clust + 1, 
          lw = 2)
  }
} 

