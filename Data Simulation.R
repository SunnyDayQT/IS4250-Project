#====Sheet One: Basic Info======
#This part is used to generate Figure 1 & Figure 2
id=c(1:431)
gender=c(rep("M",274),rep("F",157)) 

#cannot generate the exact distribution as original data, 
#the median and sd will have differences but won't be huge
age_m=c(0.3,floor(runif(36,0.3,13)),floor(runif(87,13,21)),floor(runif(75,21,31)),floor(runif(33,31,41)),floor(runif(26,41,51)),floor(runif(11,51,61)),floor(runif(4,61,71)),82)
age_f=c(floor(runif(19,0.3,12)),floor(runif(34,13,21)),floor(runif(44,21,31)),floor(runif(29,31,41)),floor(runif(17,41,51)),floor(runif(10,51,61)),floor(runif(2,61,71)),floor(runif(2,71,82)))
age=c(age_m,age_f)

#combine id,gender,age into a dataframe
basic_info=data.frame(id,gender,age)
names(basic_info)=c("ID","Gender","Age")

#shuffle the dataframe and add in location info
basic_info=basic_info[sample(nrow(basic_info)),]
location=c(rep("Adityapur",28),rep("Kadma",66),rep("Mango",100),rep("Bagbera",26),rep("Agrico",18),
           rep("Baridih",12),rep("Bistupur",15),rep("Gamaria",12),rep("Sakchi",45),rep("Sonari",15),
           rep("Parsudih",18),rep("TELCO",12),rep("Others",64))
basic_info[,"Location"]=location

#sort the dataframe by id
basic_info=basic_info[with(basic_info,order(ID)),]

#write the datafarme into excel
library(xlsx)
write.xlsx(basic_info,file="/Users/sunny/Desktop/Dengue Epidemic.xlsx",sheetName="Basic Info",row.names = FALSE)






#===================================================
#======Sheet Two:Thrombocytopenia & Major Bleed=====
#This is used to generate Table 1 and Table 3
t_mb=data.frame(id,gender,age)
names(t_mb)=c("ID","Gender","Age")

#create platelet count and major bleed
no_plat=rep(">50,000-1L",431)
m_b=rep(FALSE,431)
no_plat[c(2,40:47,275,277,295:298)]="<=20,000"
no_plat[c(10,21,34,48:67,126:143,202:215,235:242,330:339,375:379,405,418:421)]=">20,000-50,000"
t_mb[,"Platelet count Lakh/s cumm(on admission)"]=no_plat
t_mb[,"Major Bleed (Y/N)"]=m_b

#shuffle t_mb
t_mb=t_mb[sample(nrow(t_mb)),]

#add in missing platelet count
n=1
count=0
while (count <150) {
  if(t_mb[n,4]==">50,000-1L"){
    t_mb[n,4]=">1.0L"
    count=count+1
  }
  n=n+1
}

#change major bleed
c1=0
c2=0
c3=0
c4=0

n=1
while (c1<5){
    if(t_mb[n,4]=="<=20,000"){
      t_mb[n,5]=TRUE
      c1=c1+1
    }
      n=n+1
}

n=1
while (c2<12){
    if(t_mb[n,4]==">20,000-50,000"){
      t_mb[n,5]=TRUE
      c2=c2+1
    }
      n=n+1
}

n=1
while (c3<27){
    if(t_mb[n,4]==">50,000-1L"){
      t_mb[n,5]=TRUE
      c3=c3+1
    }
      n=n+1
}

n=1
while (c4<2){
    if(t_mb[n,4]==">1.0L"){
      t_mb[n,5]=TRUE
      c4=c4+1
    }
      n=n+1
}
  
#write the datafarme into excel
library(xlsx)
write.xlsx(t_mb,file="/Users/sunny/Desktop/Dengue Epidemic.xlsx",sheetName="Thrombocytopenia & Major Bleed",row.names= FALSE,append=TRUE)
  
  
  

#===================================================
#======Clinical Subtypes & Manifestations=====

