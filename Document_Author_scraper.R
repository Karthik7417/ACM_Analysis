library(rvest)
library(magrittr)
library(gsubfn)

key_words <- c("Data Analytics")

for(j in 1:length(key_words)){
   #flag=0
   topic <- key_words[j]
   page_number = 0
   
   url <- paste0("http://dl.acm.org/results.cfm?query=acmdlTitle%3A%28%252B%22", gsub(" ", "%20",topic), "%22%29%20OR%20recordAbstract%3A%28%252B%22", gsub(" ", "%20",topic), "%22%29%20OR%20acmdlCCS%3A%28%252B%22", gsub(" ", "%20",topic), "%22%29&start=", page_number,"&filtered=acmPubGroups%2EacmPubGroup%3DProceeding&within=owners%2Eowner%3DGUIDE&dte=&bfr=&srt=_score")
   
   doc <- read_html(url)
   
   totalproceedingCount <- doc %>% html_node("#searchtots strong") %>% html_text(trim =T) 
   totalproceedingCount<- gsub(",","", totalproceedingCount)
   
   page_range <- c(1:ceiling(as.numeric(totalproceedingCount)/20))
   
   detail_nodes <- doc %>% html_nodes('.details')
   
   for(k in 1:length(detail_nodes)){
      
      title <- detail_nodes[k] %>% html_nodes(".title a") %>% html_text(trim = T) 
      
      link <- detail_nodes[k] %>% html_nodes(".title a") %>% html_attr("href")
      
      authors <- paste(detail_nodes[k] %>% html_nodes(".authors a") %>% html_text(trim=T), collapse = " , ")
      if(length(authors) == 0){
         authors = "-"
      }
      
      citations <- detail_nodes[k] %>% html_nodes(".citedCount") %>% html_text(trim=T)
      if(length(citations) == 0){
         citations = "-"
      }
      
      downloads <- detail_nodes[k] %>% html_nodes(".downloadAll") %>% html_text(trim=T)
      if(length(downloads) == 0){
         downloads = "-"
      }
      
      key_topics <- detail_nodes[k] %>% html_nodes(".kw") %>% html_text(trim=T)
      if(length(key_topics) == 0){
         key_topics = "-"
      }
      
      publication_name <- detail_nodes[k] %>% html_nodes(".publicationDate+ span") %>% html_text(trim=T)
      if(length(publication_name) == 0){
         publication_name = "-"
      }
      
      
      publication_date <- detail_nodes[k] %>% html_nodes(".publicationDate") %>% html_text(trim=T)
      if(length(publication_date) == 0){
         publication_date = "-"
      }
      
      entry <- data.frame(topic, title, link, authors, citations,downloads, key_topics, publication_name, publication_date, stringsAsFactors = F)
      
      if(flag == 0){
         main = entry
         flag = 1
      }
      else{
         main <- rbind(main, entry)
      }
   }
   
   Sys.sleep(4.25)
   
   for(i in 40:length(page_range)){
      
      url1 <- paste0("http://dl.acm.org/results.cfm?query=acmdlTitle%3A%28%252B%22", gsub(" ", "%20",topic), "%22%29%20OR%20recordAbstract%3A%28%252B%22", gsub(" ", "%20",topic), "%22%29%20OR%20acmdlCCS%3A%28%252B%22", gsub(" ", "%20",topic), "%22%29&start=", 20*page_range[i],"&filtered=acmPubGroups%2EacmPubGroup%3DProceeding&within=owners%2Eowner%3DGUIDE&dte=&bfr=&srt=_score")
      
      doc1 <- read_html(url1)
      
      detail_nodes1 <- doc1 %>% html_nodes('.details')
      
      for (l in 1:length(detail_nodes1)){
         title <- detail_nodes1[l] %>% html_nodes(".title a") %>% html_text(trim = T) 
         title <- title[1]
         
         link <- detail_nodes1[l] %>% html_nodes(".title a") %>% html_attr("href")
         
         authors <- paste(detail_nodes1[l] %>% html_nodes(".authors a") %>% html_text(trim=T), collapse = " , ")
         if(length(authors) == 0){
            authors = "-"
         }
       
         
         citations <- detail_nodes1[l] %>% html_nodes(".citedCount") %>% html_text(trim=T)
         if(length(citations) == 0){
            citations = "-"
         }
           
         downloads <- detail_nodes1[l] %>% html_nodes(".downloadAll") %>% html_text(trim=T)
         if(length(downloads) == 0){
            downloads = "-"
         }
         
         
         key_topics <- detail_nodes1[l] %>% html_nodes(".kw") %>% html_text(trim=T)
         if(length(key_topics) == 0){
            key_topics = "-"
         }
         
         publication_name <- detail_nodes1[l] %>% html_nodes(".publicationDate+ span") %>% html_text(trim=T)
         if(length(publication_name) == 0){
            publication_name = "-"
         }
         
         
         publication_date <- detail_nodes1[l] %>% html_nodes(".publicationDate") %>% html_text(trim=T)
         if(length(publication_date) == 0){
            publication_date = "-"
         }
         
         
         entry <- data.frame(topic, title, link,authors,citations,downloads, key_topics, publication_name,publication_date, stringsAsFactors = F)
         
         
         main <- rbind(main, entry)
         
         Sys.sleep(4.25)
         
      }
   }
   
   main$link <- paste0("http://dl.acm.org/", main$link)
   write.csv(main, file = paste0(topic, "_doc_links.csv"), row.names = F)
}