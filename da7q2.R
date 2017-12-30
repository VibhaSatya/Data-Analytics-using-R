data<- read.csv("D:/DA/handwriting_recognition.csv",header=TRUE,sep=",")
data<-as.data.frame(data)
transactionCount <- sum(data["Freq"])

#support count of the association rules
scount1<-0
scount2<-0
scount3<-0
scount4<-0

#association rule A->B 
#to find the number of transactions in which A appears
A1<-0
A2<-0
A3<-0
A4<-0

#to find the number of transactions in which B appears
B1<-0
B2<-0
B3<-0
B4<-0

i<-1
while(i<=nrow(data))
{
#(i)
if((data[i,"Profession"]=="Artist") & (data[i,"Gender"]=="Female") & (data[i,"Recognition"]=="Recognized"))
scount1<-scount1+data[i,"Freq"]
if((data[i,"Profession"]=="Artist") & (data[i,"Gender"]=="Female"))
A1<-A1+data[i,"Freq"]
if(data[i,"Recognition"]=="Recognized")
B1<-B1+data[i,"Freq"]

#(ii)
if((data[i,"Profession"]=="Engineer") & (data[i,"Gender"]=="Male"))
scount2<-scount2+data[i,"Freq"]
if((data[i,"Profession"]=="Engineer"))
A2<-A2+data[i,"Freq"]
if(data[i,"Gender"]=="Male")
B2<-B2+data[i,"Freq"]

#(iii)
if((data[i,"Profession"]=="Actor") & (data[i,"Gender"]=="Female") & (data[i,"Recognition"]=="Recognized"))
scount3<-scount3+data[i,"Freq"]
if((data[i,"Profession"]=="Actor") & (data[i,"Recognition"]=="Recognized"))
A3<-A3+data[i,"Freq"]
if(data[i,"Gender"]=="Female")
B3<-B3+data[i,"Freq"]

#(iv)
if((data[i,"Profession"]=="Doctor") & (data[i,"Gender"]=="Male") & (data[i,"Recognition"]=="Unrecognized"))
scount4<-scount4+data[i,"Freq"]
if((data[i,"Profession"]=="Doctor") & (data[i,"Gender"]=="Male"))
A4<-A4+data[i,"Freq"]
if(data[i,"Recognition"]=="Unrecognized")
B4<-B4+data[i,"Freq"]

i<-i+1
}

s1<-scount1/transactionCount
c1<-scount1/A1
l1<-scount1/(A1*B1)

s2<-scount2/transactionCount
c2<-scount2/A2
l2<-scount2/(A2*B2)

s3<-scount3/transactionCount
c3<-scount3/A3
l3<-scount3/(A3*B3)

s4<-scount4/transactionCount
c4<-scount4/A4
l4<-scount4/(A4*B4)

print("i")
print("Support")
s1
print("Confidence")
c1
print("Lift")
l1



