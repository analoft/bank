library(pdftools)
source("bankSupport.R")
library(plyr)
library(stringr)
library(zoo)
# Bank name, Customer name,
# Bank statement period, 
# total monthly debit,
# total monthly credit,
# total monthly balance,
# overall balance

list_pdf<-grep(pattern = ".pdf",list.files("data/",full.names = T),fixed = T,value = T)

#eco bank sample 1
readPDF<-function(path){

text<-pdftools::pdf_text(path)
text2 <- strsplit(text, "\r\n")

if(trimws(text2[[1]][1])=="ACCOUNT HISTORY WITH RUNNING BALANCE"){

  information<-list()
  tab<-list()
  invisible(lapply(1:length(text2),function(x){
    print(x)
  Y<-trimws(text2[[x]])
  st<-grep(pattern = "ACCOUNT HISTORY WITH RUNNING BALANCE",x = Y)
  en<-grep(pattern = "Statement Period",x = Y)
  #information
 if(x==1){ 
  info<-Y[st:en]
  
  acc<-unlist(strsplit(info[grep(x = info,pattern = "Account No")],split = "\\s\\s+"))
  stm<-unlist(strsplit(info[grep(x = info,pattern = "Statement Period")],split = "\\s\\s+"))
  stm<-trimws(gsub(pattern = "To",replacement = "",x = stm))
  stm1<-c("Statment start Date",stm[2])
  stm2<-c("Statment end Date",stm[3])
  depT<-unlist(strsplit(info[grep(x = info,pattern = "Total Deposits")],split = "\\s\\s+"))
  depT<-c("Total Deposits",depT[2])
  
  withT<-trimws(gsub(pattern = ".*(Total Withdrawals)","",info[grep(pattern = "Total Withdrawals",x = info)]))
  withT<-c("Total withdrawals",withT)
  
  Balance<-trimws(gsub(pattern = ".*(Closing Balance)","",info[grep(pattern = "Closing Balance ",x = info)]))
  Balance<-c("Closeing Balance",Balance)
  
  OBalance<-trimws(gsub(pattern = ".*(Opening Balance)","",info[grep(pattern = "Opening Balance",x = info)]))
  OBalance<-c("Opening Balance",OBalance)
  
  nam1<-trimws(gsub(pattern = "(Currency).*","",info[grep(pattern = "Currency",x = info)]))
  nam2<-trimws(gsub(pattern = "(Opening Balance).*","",info[grep(pattern = "Opening Balance",x = info)]))
  nam3<-trimws(gsub(pattern = "(Closing Balance).*","",info[grep(pattern = "Closing Balance",x = info)]))
  nam<-c("Client",paste0(nam1,nam2,nam3))
  bank<-c("Bank","ECO BANK")
  
  dt<-as.data.frame(rbind(bank,nam,acc,OBalance,Balance,stm1,stm2,depT,withT),row.names = F)
  colnames(dt)<-c("Key" ,"Value")
  information[[1]]<<-dt
 }
  
  # table
  dt_reg<-"([0-9]{1,2}[/\\.-](JAN|FEB|MAR|MAY|APR|JUL|JUN|AUG|OCT|SEP|NOV|DEC)[/\\.-][0-9]{2,4})"
  tb<-Y[-c(st:en)][grep(pattern = dt_reg,Y[-c(st:en)])]
  date_<-unlist(lapply(tb,function(x){str_extract_all(x,pattern=dt_reg)[[1]][2]}))
  amt<-strsplit(x = trimws(gsub(x = tb,replacement = "",pattern = paste0(".*",dt_reg))),"\\s+\\s")
  amt<-lapply(amt,function(x){x<-unlist(x);if(length(x)==1){
    rt<-c(NA,strsplit(x,split = "\\s")[[1]])
    return(rt)
  }else{
    rt<-c(x[1],NA,x[2])
    return(rt)
  }})
  amt<-as.data.frame(cbind(date_,ldply(amt,rbind)))
  if(nrow(amt)!=0){
    colnames(amt)<-c("Date","Debit","credit","Balance")
    tab[[x]]<<-amt}
}))
  
  information<-information[[1]]
 tab<- ldply(tab,data.frame)
}



#eco bank sample 2
if(length(grep("(EcoBank \\:)|(\\@ecobank)",x = unlist(list(text2[[length(text2)-1]],text2[[length(text2)]]))))>0){
  information<-list()
  tab<-list()
  
  invisible(lapply(1:length(text2),function(x){
    print(x)
    Y<-trimws(text2[[x]])
    #information
    
    if(x==1){ 
      en<-grep(pattern = "Account Statement",x = Y)
      
      info<-Y[1:en]
      
      acc<-unlist(strsplit(info[grep(x = info,pattern = "Account Number")],split = "\\s\\s+"))
      acc<-c("Account No",acc[2])
      stm1<-c("Statment start Date",NA)
      stm2<-c("Statment end Date",NA)
      depT<-c("Total Deposits",NA)
      withT<-c("Total withdrawals",NA)
      Balance<-c("Closeing Balance",NA)
      
      OBalance<-trimws(gsub(pattern = ".*(Available Balance)","",info[grep(pattern = "Available Balance",x = info)]))
      OBalance<-c("Opening Balance",OBalance)
      nam<-unlist(strsplit(info[grep(pattern = "Name",x = info)],"\\s+\\s"))[2]
      bank<-c("Bank","ECO BANK")
      
      dt<-as.data.frame(rbind(bank,nam,acc,OBalance,Balance,stm1,stm2,depT,withT),row.names = F)
      colnames(dt)<-c("Key" ,"Value")
      information[[1]]<<-dt
    }
    
    # table
    dt_reg<-"([0-9]{1,2}[/\\.-](JAN|FEB|MAR|MAY|APR|JUL|JUN|AUG|OCT|SEP|NOV|DEC)[/\\.-][0-9]{2,4})"
    tb<-Y[grep(pattern =paste0("^",dt_reg),Y)]
    date_<-unlist(lapply(tb,function(x){str_extract_all(x,pattern=dt_reg)[[1]][2]}))
    
    
    amt<-strsplit(x = trimws(gsub(x = tb,replacement = "",pattern = paste0(".*",dt_reg))),"\\s")
    amt<-lapply(amt,function(x){x<-unlist(x);
    len<-length(x)
    bal<-trimws(x[len])
    dep<-ifelse(paste0(trimws(x[(len-4):(len-1)]),collapse = "")=="",NA,paste0(trimws(x[(len-4):(len-1)]),collapse = ""))
    x<-x[-((len-1):len)]
    x<-x[x!=""]
    with<-ifelse(is.na(dep),trimws(x[length(x)]),NA)
    c(with,dep,bal)
    })
    amt<-as.data.frame(cbind(date_,ldply(amt,rbind)))
    if(nrow(amt)!=0){
    colnames(amt)<-c("Date","Debit","credit","Balance")
    tab[[x]]<<-amt}
  }))
  
  information<-information[[1]]
  tab<- ldply(tab,data.frame)
}

#GUARANTY TRUST BANK PLC
if(trimws(text2[[1]][1])=="GUARANTY TRUST BANK PLC"){
  
  information<-list()
  tab<-list()
  
  invisible(lapply(1:length(text2),function(x){
    print(x)
    Y<-trimws(text2[[x]])
    #information
    if(x==1){ 
      st<-grep(pattern = "CUSTOMER STATEMENT",x = Y)
      en<-grep(pattern = "Trans Date",x = Y)
      
      info<-Y[st:en]
      
      acc<-trimws(unlist(strsplit(info[grep(x = info,pattern = "Account No:")],split = "\\:")))
      stm<-as.character(as.Date(trimws(unlist(strsplit(gsub(pattern = "Period\\:","",info[grep(x = info,pattern = "Period:")]),split = "To"))),format = "%d/%B/%Y"))
      #stm<-trimws(gsub(pattern = "To",replacement = "",x = stm))
      stm1<-c("Statment start Date",stm[1])
      stm2<-c("Statment end Date",stm[2])
      
      #depT<-unlist(strsplit(info[grep(x = info,pattern = "Total Deposits")],split = "\\s\\s+"))
      depT<-c("Total Deposits",NA)
      
      #withT<-trimws(gsub(pattern = ".*(Total Withdrawals)","",info[grep(pattern = "Total Withdrawals",x = info)]))
      withT<-c("Total withdrawals",NA)
      
      #Balance<-trimws(gsub(pattern = ".*(Closing Balance)","",info[grep(pattern = "Closing Balance ",x = info)]))
      Balance<-c("Closeing Balance",NA)
      
      OBalance<-trimws(gsub(pattern = "Opening Balance\\:","",info[grep(pattern = "Opening Balance",x = info)]))
      OBalance<-c("Opening Balance",OBalance)
      
      nam1<-trimws(gsub(pattern = "Account Name\\:","",info[grep(pattern = "Account Name",x = info)]))
      nam<-c("Client",nam1)
      bank<-c("Bank","GUARANTY TRUST BANK PLC")
      
      dt<-as.data.frame(rbind(bank,nam,acc,OBalance,Balance,stm1,stm2,depT,withT),row.names = F)
      colnames(dt)<-c("Key" ,"Value")
      information[[1]]<<-dt
    }
    
    # table
    dt_reg<-"([0-9]{1,2}[/\\.-](JAN|FEB|MAR|MAY|APR|JUL|JUN|AUG|OCT|SEP|NOV|DEC|Jan|Feb|Mar|May|Apr|Jul|Jun|Aug|Oct|Sep|Nov|Dec)[/\\.-][0-9]{2,4})"
    tb<-Y[grep(pattern = paste0("^",tolower(dt_reg)),tolower(Y))]
    date_<-unlist(lapply(tb,function(x){str_extract_all(x,pattern=dt_reg)[[1]][2]}))
    
    amt<-strsplit(x = trimws(gsub(x = tb,replacement = "",pattern = paste0(".*",dt_reg))),"\\s")
    amt<-lapply(amt,function(x){x<-unlist(x);
    if(length(tail(which(x==""),1))!=0){
    x<-x[1:(tail(which(x==""),1)+1)]
    if(length(x)>4){
      rt<-c(x[1],NA,tail(x,1))
    }else{
      rt<-c(NA,x[1],tail(x,1))
    }
    
    
    
    }else{
      x<-x[1:2]
      rt<-c(NA,x[1],x[2])
    }
    
   return(rt)}) 
    
    #print(rt)
   
    amt<-as.data.frame(cbind(date_,ldply(amt,rbind)))
    if(nrow(amt)!=0){
      colnames(amt)<-c("Date","Debit","credit","Balance")
      tab[[x]]<<-amt}
  }))
  
  information<-information[[1]]
  tab<- ldply(tab,data.frame)
}

#Plaris bank
if(trimws(text2[[1]][1])=="STATEMENT OF ACCOUNTS"){
  
  information<-list()
  tab<-list()
  invisible(lapply(1:length(text2),function(x){
    print(x)
    
    Y<-trimws(text2[[x]])
    #information
    if(x==1){ 
      info<-Y
      
      acc<-unlist(strsplit(info[grep(x = info,pattern = "ACCOUNT SUMMARY")+3],split = "\\s\\s+"))
      #stm<-unlist(strsplit(info[grep(x = info,pattern = "Statement Period")],split = "\\s\\s+"))
     # stm<-trimws(gsub(pattern = "To",replacement = "",x = stm))
      stm1<-c("Statment start Date",NA)
      stm2<-c("Statment end Date",NA)
     # depT<-unlist(strsplit(info[grep(x = info,pattern = "Total Deposits")],split = "\\s\\s+"))
      depT<-c("Total Deposits",depT[2])
      
     # withT<-trimws(gsub(pattern = ".*(Total Withdrawals)","",info[grep(pattern = "Total Withdrawals",x = info)]))
      withT<-c("Total withdrawals",NA)
      
      #Balance<-trimws(gsub(pattern = ".*(Closing Balance)","",info[grep(pattern = "Closing Balance ",x = info)]))
      Balance<-c("Closeing Balance",acc[5])
      
      #OBalance<-trimws(gsub(pattern = ".*(Opening Balance)","",info[grep(pattern = "Opening Balance",x = info)]))
      OBalance<-c("Opening Balance",acc[4])
      
      nam<-c("Client",info[3])
      bank<-c("Bank","POLARIS BANK")
      acc<-c("Account No",acc[1])
      dt<-as.data.frame(rbind(bank,nam,acc,OBalance,Balance,stm1,stm2,depT,withT),row.names = F)
      colnames(dt)<-c("Key" ,"Value")
      information[[1]]<<-dt
    }
    
    # table
    dt_reg<-"([0-9]{1,2}[/\\.-][0-9]{1,2}[/\\.-][0-9]{2,4})"
    tb<-Y[grep(pattern = paste0("^",dt_reg),Y)]
    if(length(tb)!=0){
    date_<-unlist(lapply(tb,function(x){str_extract_all(x,pattern=dt_reg)[[1]][2]}))
    
    amt<-strsplit(x = tb,"\\s+")
    amt<-lapply(amt,function(x){x<-unlist(x);
    return(tail(x,3))
    })
    
    
    
    amt<-as.data.frame(cbind(date_,ldply(amt,rbind)))
    if(nrow(amt)!=0){
      colnames(amt)<-c("Date","Debit","credit","Balance")
      tab[[x]]<<-amt}
  }
    
    }))
  
  information<-information[[1]]
  tab<- ldply(tab,data.frame)
}






information$Value<-unlist(lapply(information$Value,clean_num))
information$Value[information$Key=="Closeing Balance"]<-as.character(tab$Balance[nrow(tab)])
information$Value[information$Key=="Opening Balance"]<-as.character(tab$Balance[1])

information$Value[information$Key=="Statment start Date"]<-as.character(tab$Date[1])
information$Value[information$Key=="Statment end Date"]<-as.character(tab$Date[nrow(tab)])

information$Value[information$Key=="Total Deposits"]<-sum(as.numeric(unlist(lapply(as.character(tab$credit),clean_num))),na.rm = T)
information$Value[information$Key=="Total withdrawals"]<-sum(as.numeric(unlist(lapply(as.character(tab$Debit),clean_num))),na.rm=T)


tab$Debit<-as.numeric(unlist(lapply(tab$Debit,clean_num)))
tab$credit<-as.numeric(unlist(lapply(tab$credit,clean_num)))
tab$Balance<-as.numeric(unlist(lapply(tab$Balance,clean_num)))
tab$Date<-as.Date(as.character(tab$Date),format="%d-%m-%Y")
tab$Month<-as.yearmon(tab$Date)

debit<-aggregate(Debit~Month,tab,sum)
credit<-aggregate(credit~Month,tab,sum)

mab<-aggregate(Balance~Date,tab,mean)
  
ind<-count(tab, 'Date')

mab<-tab[cumsum(ind$freq),]
mab_<-list()
itr<-1
for(grp in unique((mab$Month))){
  tmp<-mab[mab$Month==grp,c("Date","Balance","Month")]
  tmp_<-data.frame('Date'=seq(zoo::as.Date(tmp$Month[1]),by="day",length=numberOfDays(as.Date(tmp$Month[1]))))
 mab_[[itr]]<-merge(tmp,tmp_,by="Date",all=T)
 itr<-itr+1
}
mab<-ldply(mab_,data.frame)
mab$Month<-as.yearmon(mab$Date)
if(is.na(mab$Balance[1])){
  mab$Balance[1]<-tab$Balance[1]
}
mab$Balance<-na.locf(mab$Balance)
mabV<-aggregate(Balance~Month,mab,sum)

mabV$Balance<-mabV$Balance/unlist(lapply(mabV$Month, function(x){numberOfDays(as.Date(x))}))

information2<-merge(debit,merge(credit,mabV))
return(list(information,information2))
}




