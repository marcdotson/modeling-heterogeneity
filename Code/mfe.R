library(tidyverse)
library(readxl)

# load in data
final_data = read_csv(here::here("Data", "218329_Final_Excel_050619.csv"))
dummy_design = read_csv(here::here("Data", "dummy_design.csv"))
# final_data = read_excel("/Users/adamsmith/Dropbox/Research/geolocation/final_data.xlsx")
# dummy_design = read_csv("/Users/adamsmith/Dropbox/Research/geolocation/dummy_design.csv")

# geolocation data
geo = final_data %>%
  select(pid,465:485) %>%
  mutate(visit_count=apply(select(.,-pid),1,sum),visit=ifelse(visit_count>0,"yes","no")) %>%
  select(pid,visit,visit_count)

# geolocation data (with brand columns)
geobrand = final_data %>% 
  select(pid,465:485)

# brand map
brandmap = dummy_design %>% 
  select(.,c("version","task","alt",contains("brand"))) %>% 
  mutate(brand=apply(select(.,-c("version","task","alt")),1,which.max)+1) %>%
  select(version,task,alt,brand) %>%
  rename(choice=alt) %>%
  as_tibble()

# price map
pricemap = dummy_design %>% 
  select(.,c("version","task","alt",contains("price"))) %>% 
  mutate(price=apply(select(.,-c("version","task","alt")),1,which.max)+1) %>%
  select(version,task,alt,price) %>%
  rename(choice=alt) %>%
  as_tibble()

# brand choices across choice tasks
choices = final_data %>%
  select(pid,contains("Q3")) %>%
  gather(.,"task","choice",-c(pid,Q3_Version)) %>% 
  mutate(task=as.integer(str_remove(task,"Q3_"))) %>%
  rename(version=Q3_Version) %>% 
  left_join(.,brandmap,by=c("version","task","choice")) %>%
  arrange(pid,task) %>%
  mutate(brand=ifelse(is.na(brand),99,brand)) %>%
  select(pid,brand,task) %>%
  spread(.,task,brand,fill=0,sep="")

# prices chosen across choice tasks
prices = final_data %>%
  select(pid,contains("Q3")) %>%
  gather(.,"task","choice",-c(pid,Q3_Version)) %>% 
  mutate(task=as.integer(str_remove(task,"Q3_"))) %>%
  rename(version=Q3_Version) %>% 
  left_join(.,pricemap,by=c("version","task","choice")) %>%
  arrange(pid,task) %>%
  filter(!is.na(price)) %>%
  select(pid,price,task) %>%
  mutate(price=(price-1)*5+20) %>%
  spread(.,task,price,fill=0,sep="")
  
  
  
# distribution of dealership visits
choices %>%
  mutate(out=apply(.,1,function(x)sum(x==99)/12)) %>%
  select(pid,out) %>%
  inner_join(.,geo,by="pid") %>%
  filter(visit_count<50) %>%
  ggplot(aes(x=as.factor(visit_count))) + 
  stat_count(geom = "bar") + 
  labs(x="number of dealership visits")

# differences in share of outside good
choices %>%
  mutate(out=apply(.,1,function(x)sum(x==99)/12)) %>%
  select(pid,out) %>%
  inner_join(.,geo,by="pid") %>%
  ggplot(aes(y=out,x=visit,fill=visit)) + 
  geom_boxplot(alpha=.75) + 
  labs(y="Choice Share of Outside Good") + 
  theme(legend.position="none")

# differences in price of chosen alternative
prices %>%
  mutate(meanprice=apply(.,1,function(x)sum(x[-1])/length(which(x[-1]>0)))) %>%
  select(pid,meanprice) %>%
  inner_join(.,geo,by="pid") %>% 
  ggplot(aes(y=meanprice,x=visit,fill=visit)) + 
  geom_boxplot(alpha=.75) + 
  labs(y="Average Price of Chosen Alternative") + 
  theme(legend.position="none")


# differences in amount expecting to pay
final_data %>%
  select(c("pid",starts_with("Q2x8"))) %>%
  inner_join(.,geo,by="pid") %>%
  filter(Q2x8<500) %>%
  ggplot(.,aes(x=Q2x8,y=..density..,fill=visit)) + 
  geom_histogram(alpha=.75,bins=50) + 
  labs(x="Amount Expecting to Pay for New Vehicle")

# dealerships visited
geobrand %>%
  select(-pid) %>%
  mutate(total=rowSums(.)) %>%
  filter(total<50) %>%
  select(-total) %>%
  summarise_all(.,sum) %>%
  gather() %>%
  ggplot(.,aes(x=reorder(key,-value),y=value)) +
  geom_bar(stat="identity") + 
  labs(x="",y="Number of Visits") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# dealership visits by gender
final_data %>%
  select(c("pid","Q4x1")) %>%
  inner_join(.,geo,by="pid") %>%
  filter(Q4x1!=3) %>%
  group_by(Q4x1,visit) %>% summarise (n = n()) %>% 
  group_by(Q4x1) %>% mutate(freq = n / sum(n)) %>% filter(visit=="yes") %>%
  ggplot(.,aes(x=Q4x1,y=freq)) + 
  geom_bar(stat="identity") + 
  scale_x_discrete(limits=c("Female","Male")) + 
  labs(x="Gender",y="Proportion Making a Dealership Visit")

# dealership visits by likelihood of purchasing used
final_data %>%
  select(c("pid","Q2x2")) %>%
  inner_join(.,geo,by="pid") %>%
  group_by(Q2x2,visit) %>% summarise (n = n()) %>% 
  group_by(Q2x2) %>% mutate(freq = n / sum(n)) %>% filter(visit=="yes") %>%
  ggplot(.,aes(x=Q2x2,y=freq)) + 
  geom_bar(stat="identity") + 
  scale_x_discrete(limits=c("very likely","somewhat likely","somewhat unlikely","very unlikely")) + 
  labs(x="Likelihood of Purchasing a Used Vehicle",y="Proportion Making a Dealership Visit") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# dealership visits by number of vehicles currently owned
final_data %>%
  select(c("pid","Q2x4")) %>%
  inner_join(.,geo,by="pid") %>%
  group_by(Q2x4,visit) %>% summarise (n = n()) %>% 
  group_by(Q2x4) %>% mutate(freq = n / sum(n),count=sum(n)) %>% filter(visit=="yes") %>%
  ggplot(.,aes(x=Q2x4,y=freq)) + 
  geom_bar(stat="identity") + 
  geom_text(aes(label=count),vjust=-1,size=3) +
  scale_x_discrete(limits=c(0:5)) +
  labs(x="Number of Vehicles Currently Owned",y="Proportion Making a Dealership Visit")

# dealership visits by whether replacing current vehicle
final_data %>%
  select(c("pid","Q2x5")) %>%
  inner_join(.,geo,by="pid") %>%
  group_by(Q2x5,visit) %>% summarise (n = n()) %>% 
  group_by(Q2x5) %>% mutate(freq = n / sum(n)) %>% filter(visit=="yes") %>%
  ggplot(.,aes(x=Q2x5,y=freq)) + 
  geom_bar(stat="identity") + 
  scale_x_discrete(limits=c("No","Yes")) +
  labs(x="Replacing a Current Vehicle",y="Proportion Making a Dealership Visit")

# average income by dealership
final_data %>%
  select(c("pid","Q4x4")) %>%
  inner_join(.,geobrand,by="pid") %>%
  select(-pid) %>%
  mutate(total=rowSums(select(.,-Q4x4))) %>%
  filter(total<50) %>%
  mutate_at(vars(-Q4x4,-total),~1*(.>0)) %>%
  mutate_at(vars(-Q4x4,-total),~.*Q4x4) %>%
  select(-c("Q4x4","total")) %>%
  summarise_all(.,list(~sum(.)/sum(.>0),~sum(.>0))) %>%
  gather(.,"brand") %>%
  mutate(fun=rep(c("mean","count"),each=nrow(.)/2)) %>%
  rowwise %>% mutate(brand=str_replace(brand, "_.*","")) %>% ungroup() %>%
  spread(fun,value) %>%
  ggplot(.,aes(x=reorder(brand,-mean),y=mean)) +
  geom_bar(stat="identity") + 
  geom_text(aes(label=count),vjust=-1,size=3) +
  labs(x="",y="Average Income of Visitors") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

