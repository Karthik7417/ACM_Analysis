library(plyr)

#You can add more topics for analysis here
link_list <- c("Big Data","Data Analytics","Cloud Computing")
for(b in 1:length(link_list)){
   topic = link_list[b]
   
   
   Main_links <- read.csv(file =paste0(topic,"_doc_links.csv"), stringsAsFactors = F)
   Main_data <- read.csv(file =paste0(topic,"_data.csv"), stringsAsFactors = F)
   
   Main_links <- Main_links[!duplicated(Main_links),]
   Main_data <- Main_data[!duplicated(Main_data),]
   
   flag = 0
   
   Main_links <- Main_links[Main_links$authors != "",]
   Main_links <- Main_links[!duplicated(Main_links$title, Main_links$publication_date),]
   broken_date <-  read.table(text = Main_links$publication_date, sep = " ", colClasses = "character")
   names(broken_date) <- c("Month","Year")
   
   broken_date$Month_no <-sprintf("%02d", as.integer(factor(broken_date$Month, levels = c("January","February","March","April","May","June","July","August","September","October","November","December"))))
   broken_date$Date <- paste("01", broken_date$Month_no, broken_date$Year, sep = "-")
   Main_links$date <- as.Date(broken_date$Date,"%d-%m-%Y") 
   
   
   #Searching for the terms University, Univ, Institute, Academy, School, College
   is.univ <- grep(pattern = "University|Univ|Institute|Academy|School|College", x = Main_data$author_affliation,ignore.case = T)
   
   Main_data$is.university <- "No"
   Main_data$is.university[is.univ] <- "Yes"
   
   for(i in 1:nrow(Main_links)){
      gephi_aut <- sort(unlist(strsplit(x = Main_links$authors[i], split = " , ")))
      if(length(gephi_aut) == 1){
         gephi_source = Main_data$author_affliation[Main_data$author_name == gephi_aut][1]
         gephi_target = Main_data$author_affliation[Main_data$author_name == gephi_aut][1]
         entry = data.frame(gephi_source, gephi_target, Main_links$title[i], Main_links$date[i] ,stringsAsFactors = F)
         gephi_node <- rbind(gephi_node, entry)
         next
      }
      
      
      for(j in 1:(length(gephi_aut)-1)){
         gephi_source <- Main_data$author_affliation[Main_data$author_name == gephi_aut[j]][1]
         for(k in j+1:(length(gephi_aut))){
            gephi_target <- Main_data$author_affliation[Main_data$author_name == gephi_aut[k]][1]
            entry <- data.frame(gephi_source, gephi_target, Main_links$title[i],Main_links$date[i], stringsAsFactors = F)
            
            if(flag == 0){
               gephi_node <- entry
               flag = 1
            }
            else{
               gephi_node <- rbind(gephi_node, entry)
            }
         }
      } 
   }
   
   gephi_node <- gephi_node[gephi_node$gephi_source != "-" & gephi_node$gephi_target != "-",]
   edgelist <- gephi_node[!is.na(gephi_node$gephi_target),]
   names(edgelist) <- c("Source","Target","Label", "Timestamp")
   edgelist$Timestamp <- as.Date(edgelist$Timestamp,"%d-%m-%Y")
   
   nodelist <- data.frame(unique(c(edgelist$Source, edgelist$Target)), unique(c(edgelist$Source, edgelist$Target)), stringsAsFactors = F)
   names(nodelist) <- c("Id", "Label")
   
   for(k in 1:nrow(nodelist)){
      nodelist$Publications_in_current_topic[k] <- length(unique(Main_data$title[Main_data$author_affliation == nodelist$Label[k]]))
      }
   
   for(a in 1:nrow(nodelist)){
      nodelist$Timestamp[a] <- paste0("<[",paste(format(unique(c(sort(edgelist$Timestamp[edgelist$Source == nodelist$Id[a] | edgelist$Target == nodelist$Id[a]]))), "%d-%m%-%Y"), collapse = ", "), "]>")
      nodelist$Start_Date[a]<- format(unique(c(sort(edgelist$Timestamp[edgelist$Source == nodelist$Id[a] | edgelist$Target == nodelist$Id[a]]))), "%d-%m%-%Y")[1]
      nodelist$End_Date[a] <- format(unique(c(sort(edgelist$Timestamp[edgelist$Source == nodelist$Id[a] | edgelist$Target == nodelist$Id[a]]))), "%d-%m%-%Y")[length(unique(c(sort(edgelist$Timestamp[edgelist$Source == nodelist$Id[a] | edgelist$Target == nodelist$Id[a]]))))]
   }
   
   nodelist <- nodelist[complete.cases(nodelist),]
   nodelist$Start_Date <- as.Date(nodelist$Start_Date,"%d-%m-%Y")
   nodelist$End_Date <- as.Date(nodelist$End_Date,"%d-%m-%Y")
   
   
   edgelist_final <- data.frame(count(edgelist, vars = c("Source", "Target")), stringsAsFactors = F)
   names(edgelist_final)[3] <- "Weight"
   edgelist_final$Type = "Undirected"
   
   
   
   write.csv(nodelist, file = paste0(topic, "_affliation_gephi_nodelist.csv"), row.names = F)
   write.csv(edgelist_final, file = paste0(topic,"_affliation_gephi_edgelist.csv"), row.names = F)
   
}
