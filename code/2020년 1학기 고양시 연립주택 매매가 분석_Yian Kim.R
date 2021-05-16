#회귀프로젝트 전처리1 : 유의미한 데이터만 추출
setwd('C:/Users/st4ndup/Desktop/Rproject/2020_RegressionAnalysisProject-/data')

library(stringr)

#공동주택실거래가현황
D0=read.csv('★경기도_고양시_공동주택실거래가현황_20191112.csv')
Goyang=D0[,c(1,6,7,9,10,11)]
Goyang$시군구=str_sub(Goyang$시군구,-3,-1)
names(Goyang)[1]="동"
Goyang$동=as.factor(Goyang$동)
Goyang$동=gsub(" 풍동","풍동",Goyang$동)
Goyang$동=gsub("리현동","사리현동",Goyang$동)
Goyang$동=as.factor(Goyang$동)
Goyang$면적당가격=Goyang$거래금액.만원./Goyang$전용면적...
Goyang$계약년월=substr(Goyang$계약년월,1,4)
Goyang$계약년월=as.numeric(Goyang$계약년월)
Goyang$노후정도=Goyang$계약년월-Goyang$건축년도
Goyang=Goyang[,c(7,1,2,5,8)]
str(Goyang)


#동 정보 기록
Goyang_city=data.frame(levels(Goyang$동))
pop=read.csv("행정구역_읍면동_별_5세별_주민등록인구_2011년__20200429100335.csv")
pop$항목=substr(pop$항목,1,1)
#편의상 5세 단위로 구분된 인구수를 유소년,청소년,청년,중년,장년 5개로 나눔
pop=cbind(pop,apply(pop[,5:6],1,sum))
pop=cbind(pop,apply(pop[,7:8],1,sum))
pop=cbind(pop,apply(pop[,9:12],1,sum))
pop=cbind(pop,apply(pop[,13:16],1,sum))
pop=cbind(pop,apply(pop[,17:25],1,sum))
pop=pop[,c(1,2,26:30)]
names(pop)=c("동","항목","유소년","청소년","청년","중년","장년")

#차후 처리할 행정동->법정동변환에서 
#누락된 동과 면적 차이를 고려하여 인구밀도와 면적으로 대체
면적=rep(c(5.62,12.74,11.32,2.19,0.92,25.35,6.76,11.57,25.04,15.05,13.78
                ,2.31,1.94,6.01,0.69,4.28,1.94,7.3,10.79,6.82,2.84,1.53,5.67,1.77
                ,0.8,2.21,0.63,10.28,2.41,24.98,0.65,0.82,1.12,2.19,0.97,0.96,3.52
                ,13.17,19.16)
              ,each=3)
pop=cbind(pop,면적)
pop[,3]=pop[,3]/pop[,8]
pop[,4]=pop[,4]/pop[,8]
pop[,5]=pop[,5]/pop[,8]
pop[,6]=pop[,6]/pop[,8]
pop[,7]=pop[,7]/pop[,8]
#인구밀도는 km2당 명수를 단위로 함.
#행정동 단위의 pop파일을 법정동 단위의 Goyang_city로 옮겨줌
Goyang_city[,2:11]=0
names(Goyang_city)[2:11]=c("유소년남","유소년여","청소년남","청소년여","청년남","청년여","중년남","중년여","장년남","장년여")
str(pop)
#해당 법정동은 포함하는 행정동들의 인구밀도 평균을 가짐
Goyang_city[Goyang_city=="가좌동",][c(2,4,6,8,10)]=
  pop[pop$동=="송산동"&pop$항목=="남",][3:7]
Goyang_city[Goyang_city=="가좌동",][c(3,5,7,9,11)]=
  pop[pop$동=="송산동"&pop$항목=="여",][3:7]
Goyang_city[Goyang_city=="고양동",][c(2,4,6,8,10)]=
  pop[pop$동=="고양동"&pop$항목=="남",][3:7]
Goyang_city[Goyang_city=="고양동",][c(3,5,7,9,11)]=
  pop[pop$동=="고양동"&pop$항목=="여",][3:7]
Goyang_city[Goyang_city=="관산동",][c(2,4,6,8,10)]=
  pop[pop$동=="관산동"&pop$항목=="남",][3:7]
Goyang_city[Goyang_city=="관산동",][c(3,5,7,9,11)]=
  pop[pop$동=="관산동"&pop$항목=="여",][3:7]
Goyang_city[Goyang_city=="대화동",][c(2,4,6,8,10)]=
  (pop[pop$동=="대화동"&pop$항목=="남",][3:7]+
  pop[pop$동=="송포동"&pop$항목=="남",][3:7])/2
Goyang_city[Goyang_city=="대화동",][c(3,5,7,9,11)]=
  (pop[pop$동=="대화동"&pop$항목=="여",][3:7]+
  pop[pop$동=="송포동"&pop$항목=="여",][3:7])/2
Goyang_city[Goyang_city=="덕이동",][c(2,4,6,8,10)]=
  pop[pop$동=="송포동"&pop$항목=="남",][3:7]
Goyang_city[Goyang_city=="덕이동",][c(3,5,7,9,11)]=
  pop[pop$동=="송포동"&pop$항목=="여",][3:7]
Goyang_city[Goyang_city=="도내동",][c(2,4,6,8,10)]=
  pop[pop$동=="흥도동"&pop$항목=="남",][3:7]
Goyang_city[Goyang_city=="도내동",][c(3,5,7,9,11)]=
  pop[pop$동=="흥도동"&pop$항목=="여",][3:7]
Goyang_city[Goyang_city=="동산동",][c(2,4,6,8,10)]=
  pop[pop$동=="창릉동"&pop$항목=="남",][3:7]
Goyang_city[Goyang_city=="동산동",][c(3,5,7,9,11)]=
  pop[pop$동=="창릉동"&pop$항목=="여",][3:7]
Goyang_city[Goyang_city=="마두동",][c(2,4,6,8,10)]=
  (pop[pop$동=="마두1동"&pop$항목=="남",][3:7]+
     pop[pop$동=="마두2동"&pop$항목=="남",][3:7])/2
Goyang_city[Goyang_city=="마두동",][c(3,5,7,9,11)]=
  (pop[pop$동=="마두1동"&pop$항목=="여",][3:7]+
     pop[pop$동=="마두2동"&pop$항목=="여",][3:7])/2
Goyang_city[Goyang_city=="백석동",][c(2,4,6,8,10)]=
  (pop[pop$동=="백석1동"&pop$항목=="남",][3:7]+
     pop[pop$동=="백석2동"&pop$항목=="남",][3:7]+
     pop[pop$동=="장항1동"&pop$항목=="남",][3:7])/3
Goyang_city[Goyang_city=="백석동",][c(3,5,7,9,11)]=
  (pop[pop$동=="백석1동"&pop$항목=="여",][3:7]+
     pop[pop$동=="백석2동"&pop$항목=="여",][3:7]+
     pop[pop$동=="장항1동"&pop$항목=="여",][3:7])/3
Goyang_city[Goyang_city=="사리현동",][c(2,4,6,8,10)]=
  pop[pop$동=="고봉동"&pop$항목=="남",][3:7]
Goyang_city[Goyang_city=="사리현동",][c(3,5,7,9,11)]=
  pop[pop$동=="고봉동"&pop$항목=="여",][3:7]
Goyang_city[Goyang_city=="삼송동",][c(2,4,6,8,10)]=
  (pop[pop$동=="삼송동"&pop$항목=="남",][3:7]+
     pop[pop$동=="창릉동"&pop$항목=="남",][3:7])/2
Goyang_city[Goyang_city=="삼송동",][c(3,5,7,9,11)]=
  (pop[pop$동=="삼송동"&pop$항목=="여",][3:7]+
     pop[pop$동=="창릉동"&pop$항목=="여",][3:7])/2
Goyang_city[Goyang_city=="성사동",][c(2,4,6,8,10)]=
  (pop[pop$동=="주교동"&pop$항목=="남",][3:7]+
     pop[pop$동=="흥도동"&pop$항목=="남",][3:7]+
     pop[pop$동=="성사1동"&pop$항목=="남",][3:7]+
     pop[pop$동=="성사2동"&pop$항목=="남",][3:7])/4
Goyang_city[Goyang_city=="성사동",][c(3,5,7,9,11)]=
  (pop[pop$동=="주교동"&pop$항목=="여",][3:7]+
     pop[pop$동=="흥도동"&pop$항목=="여",][3:7]+
     pop[pop$동=="성사1동"&pop$항목=="여",][3:7]+
     pop[pop$동=="성사2동"&pop$항목=="여",][3:7])/4
Goyang_city[Goyang_city=="성석동",][c(2,4,6,8,10)]=
  pop[pop$동=="고봉동"&pop$항목=="남",][3:7]
Goyang_city[Goyang_city=="성석동",][c(3,5,7,9,11)]=
  pop[pop$동=="고봉동"&pop$항목=="여",][3:7]
Goyang_city[Goyang_city=="식사동",][c(2,4,6,8,10)]=
  pop[pop$동=="식사동"&pop$항목=="남",][3:7]
Goyang_city[Goyang_city=="식사동",][c(3,5,7,9,11)]=
  pop[pop$동=="식사동"&pop$항목=="여",][3:7]
Goyang_city[Goyang_city=="신원동",][c(2,4,6,8,10)]=
  pop[pop$동=="원신동"&pop$항목=="남",][3:7]
Goyang_city[Goyang_city=="신원동",][c(3,5,7,9,11)]=
  pop[pop$동=="원신동"&pop$항목=="여",][3:7]
Goyang_city[Goyang_city=="원흥동",][c(2,4,6,8,10)]=
  pop[pop$동=="흥도동"&pop$항목=="남",][3:7]
Goyang_city[Goyang_city=="원흥동",][c(3,5,7,9,11)]=
  pop[pop$동=="흥도동"&pop$항목=="여",][3:7]
Goyang_city[Goyang_city=="일산동",][c(2,4,6,8,10)]=
  (pop[pop$동=="일산1동"&pop$항목=="남",][3:7]+
     pop[pop$동=="일산2동"&pop$항목=="남",][3:7]+
     pop[pop$동=="일산3동"&pop$항목=="남",][3:7])/3
Goyang_city[Goyang_city=="일산동",][c(3,5,7,9,11)]=
  (pop[pop$동=="일산1동"&pop$항목=="여",][3:7]+
     pop[pop$동=="일산2동"&pop$항목=="여",][3:7]+
     pop[pop$동=="일산3동"&pop$항목=="여",][3:7])/3
Goyang_city[Goyang_city=="장항동",][c(2,4,6,8,10)]=
  (pop[pop$동=="장항1동"&pop$항목=="남",][3:7]+
     pop[pop$동=="장항2동"&pop$항목=="남",][3:7]+
     pop[pop$동=="정발산동"&pop$항목=="남",][3:7])/3
Goyang_city[Goyang_city=="장항동",][c(3,5,7,9,11)]=
  (pop[pop$동=="장항1동"&pop$항목=="여",][3:7]+
     pop[pop$동=="장항2동"&pop$항목=="여",][3:7]+
     pop[pop$동=="정발산동"&pop$항목=="여",][3:7])/3
Goyang_city[Goyang_city=="주교동",][c(2,4,6,8,10)]=
  (pop[pop$동=="성사1동"&pop$항목=="남",][3:7]+
     pop[pop$동=="성사2동"&pop$항목=="남",][3:7]+
     pop[pop$동=="주교동"&pop$항목=="남",][3:7])/3
Goyang_city[Goyang_city=="주교동",][c(3,5,7,9,11)]=
  (pop[pop$동=="성사1동"&pop$항목=="여",][3:7]+
     pop[pop$동=="성사2동"&pop$항목=="여",][3:7]+
     pop[pop$동=="주교동"&pop$항목=="여",][3:7])/3
Goyang_city[Goyang_city=="주엽동",][c(2,4,6,8,10)]=
  (pop[pop$동=="주엽1동"&pop$항목=="남",][3:7]+
     pop[pop$동=="주엽2동"&pop$항목=="남",][3:7])/2
Goyang_city[Goyang_city=="주엽동",][c(3,5,7,9,11)]=
  (pop[pop$동=="주엽1동"&pop$항목=="여",][3:7]+
     pop[pop$동=="주엽2동"&pop$항목=="여",][3:7])/2
Goyang_city[Goyang_city=="중산동",][c(2,4,6,8,10)]=
  pop[pop$동=="중산동"&pop$항목=="남",][3:7]
Goyang_city[Goyang_city=="중산동",][c(3,5,7,9,11)]=
  pop[pop$동=="중산동"&pop$항목=="여",][3:7]
Goyang_city[Goyang_city=="탄현동",][c(2,4,6,8,10)]=
  (pop[pop$동=="일산1동"&pop$항목=="남",][3:7]+
     pop[pop$동=="일산2동"&pop$항목=="남",][3:7]+
     pop[pop$동=="탄현동"&pop$항목=="남",][3:7])/3
Goyang_city[Goyang_city=="탄현동",][c(3,5,7,9,11)]=
  (pop[pop$동=="일산1동"&pop$항목=="여",][3:7]+
     pop[pop$동=="일산2동"&pop$항목=="여",][3:7]+
     pop[pop$동=="탄현동"&pop$항목=="여",][3:7])/3
Goyang_city[Goyang_city=="토당동",][c(2,4,6,8,10)]=
  (pop[pop$동=="능곡동"&pop$항목=="남",][3:7]+
     pop[pop$동=="행주동"&pop$항목=="남",][3:7])/2
Goyang_city[Goyang_city=="토당동",][c(3,5,7,9,11)]=
  (pop[pop$동=="능곡동"&pop$항목=="여",][3:7]+
     pop[pop$동=="행주동"&pop$항목=="여",][3:7])/2
Goyang_city[Goyang_city=="풍동",][c(2,4,6,8,10)]=
  pop[pop$동=="풍산동"&pop$항목=="남",][3:7]
Goyang_city[Goyang_city=="풍동",][c(3,5,7,9,11)]=
  pop[pop$동=="풍산동"&pop$항목=="여",][3:7]
Goyang_city[Goyang_city=="행신동",][c(2,4,6,8,10)]=
  (pop[pop$동=="행신1동"&pop$항목=="남",][3:7]+
     pop[pop$동=="행신2동"&pop$항목=="남",][3:7]+
     pop[pop$동=="행신3동"&pop$항목=="남",][3:7])/3
Goyang_city[Goyang_city=="행신동",][c(3,5,7,9,11)]=
  (pop[pop$동=="행신1동"&pop$항목=="여",][3:7]+
     pop[pop$동=="행신2동"&pop$항목=="여",][3:7]+
     pop[pop$동=="행신3동"&pop$항목=="여",][3:7])/3
Goyang_city[Goyang_city=="향동동",][c(2,4,6,8,10)]=
  pop[pop$동=="화전동"&pop$항목=="남",][3:7]
Goyang_city[Goyang_city=="향동동",][c(3,5,7,9,11)]=
  pop[pop$동=="화전동"&pop$항목=="여",][3:7]
Goyang_city[Goyang_city=="화정동",][c(2,4,6,8,10)]=
  (pop[pop$동=="화정1동"&pop$항목=="남",][3:7]+
     pop[pop$동=="화정2동"&pop$항목=="남",][3:7])/2
Goyang_city[Goyang_city=="화정동",][c(3,5,7,9,11)]=
  (pop[pop$동=="화정1동"&pop$항목=="여",][3:7]+
     pop[pop$동=="화정2동"&pop$항목=="여",][3:7])/2
#연립주택거래가에 포함되는 동만 조사하여 인구 기록 완료.
names(Goyang_city)[1]='동'

#총인구밀도
총인구밀도=c()
for (i in 1:27){
  총인구밀도[i]=sum(Goyang_city[i,2:11])
}

#성비(여자 1명당 남자 명수)
성비=c()
for (i in 1:27){
  성비[i]=sum(Goyang_city[i,c(2,4,6,8,10)])/sum(Goyang_city[i,c(3,5,7,9,11)])
}

#고령화정도
고령화정도=c()
for (i in 1:27){
  고령화정도[i]=sum(Goyang_city[i,10:11])/총인구밀도[i]
}


#인구지표 pop 생성
pop=data.frame(cbind(as.vector(Goyang_city$동),총인구밀도,성비,고령화정도))
pop[,2]=as.numeric(as.character(pop[,2]))
pop[,3]=as.numeric(as.character(pop[,3]))
pop[,4]=as.numeric(as.character(pop[,4]))
names(pop)[1]="동"
str(pop)


#공중위생업현황
D1=read.csv('경기도 고양시 공중위생업현황 20191104.csv')
D1=D1[,c(1,4)]
for(i in (1:length(D1$업소소재지.지번.))){
  D1[i,3]=str_split(D1$업소소재지.지번.[i]," ")[[1]][4]
}
D1=D1[,c(1,3)]
D1$업종명=substr(D1$업종명,1,1)
T1=t(table(D1))
D1=data.frame(T1)
names(D1)[1]="동"
#법정동과 업종별 빈도수로 변환한 D1을 이용해서 Goyang_city에 입력
#건물위생관리업 목욕장업 미용업 세탁업 숙박업 이용업
str(D1)
Goyang_city[,12:17]=""
names(Goyang_city)[12:17]=c("건물위생관리업","목욕장업","미용업","세탁업","숙박업","이용업")
for(i in 1:length(Goyang_city[,1])){
  Goyang_city[i,12]=D1[(D1$동==factor(Goyang_city[i,1],levels(D1$동))&D1$업종명=="건"),3]
}
for(i in 1:length(Goyang_city[,1])){
  Goyang_city[i,13]=D1[(D1$동==factor(Goyang_city[i,1],levels(D1$동))&D1$업종명=="목"),3]
}
for(i in 1:length(Goyang_city[,1])){
  Goyang_city[i,14]=D1[(D1$동==factor(Goyang_city[i,1],levels(D1$동))&D1$업종명=="미"),3]
}
for(i in 1:length(Goyang_city[,1])){
  Goyang_city[i,15]=D1[(D1$동==factor(Goyang_city[i,1],levels(D1$동))&D1$업종명=="세"),3]
}
for(i in 1:length(Goyang_city[,1])){
  Goyang_city[i,16]=D1[(D1$동==factor(Goyang_city[i,1],levels(D1$동))&D1$업종명=="숙"),3]
}
for(i in 1:length(Goyang_city[,1])){
  Goyang_city[i,17]=D1[(D1$동==factor(Goyang_city[i,1],levels(D1$동))&D1$업종명=="이"),3]
}

#대부중개업현황
D2=read.csv('경기도 고양시 대부(중개)업 현황)_2019년10월.csv')
#본 시트에는 대부(중개)업만을 포함하므로 지역에 따른 빈도만 추출
D2=D2[,6]
tempD2=c()
for(i in 1:201){
  tempD2[i]=substr(str_split(D2, pattern="\\(")[[i]][2],1,3)
}
tempD2=gsub("근린생","삼송동",tempD2)
tempD2=gsub("정발산","정발산동",tempD2)
D2=data.frame(t(table(tempD2)))
D2=D2[,2:3]
names(D2)=c("동","대부업")
Goyang_city=merge(Goyang_city,D2,by='동',all.x=T)
Goyang_city$대부업[is.na(Goyang_city$대부업)]=0

#주점현황
D3=read.csv('경기도 고양시 유흥단란주점현황 20191104 .csv')
D3=D3[,c(1,4)]
tempD3=c()
for(i in 1:183){
  tempD3[i]=str_split(D3[,2]," ")[[i]][4]
}
D3[,2]=tempD3
D3=data.frame(t(table(D3)))
names(D3)[1]="동"
Goyang_city=merge(Goyang_city,D3[D3$업종명=="단란주점",][,c(1,3)]
                  ,by='동',all.x=T)
Goyang_city=merge(Goyang_city,D3[D3$업종명=="유흥주점",][,c(1,3)]
                  ,by='동',all.x=T)
Goyang_city[is.na(Goyang_city[,19]),19]=0
Goyang_city[is.na(Goyang_city[,20]),20]=0
names(Goyang_city)[c(19,20)]=c("단란주점","유흥주점")

#지식산업센터현황
D4=read.csv('경기도 고양시_지식산업센터현황_20200228.csv')
#동,시설면적,유치가능업체
D4=D4[,c(3,7,8)]
tempD4=c()
for(i in 1:12){
  tempD4[i]=str_split(D4[,1]," ")[[i]][4]
}
D4[,1]=tempD4
tempD4=data.frame(t(table(D4[,1])))[,2:3]
names(tempD4)[1:2]=c("동","지식산업센터")
총센터면적=c()
for(i in 1:5){
  총센터면적[i]=sum(D4[D4[,1]==tempD4[i,1],2])
}
총유치업체수=c()
for(i in 1:5){
  총유치업체수[i]=sum(D4[D4[,1]==tempD4[i,1],3])
}
tempD4[,3]=총센터면적
tempD4[,4]=총유치업체수
D4=tempD4
names(D4)[3:4]=c("총센터면적","총유치업체수")
Goyang_city=merge(Goyang_city,D4,by='동',all.x=T)
Goyang_city[is.na(Goyang_city[,21]),21]=0
Goyang_city[is.na(Goyang_city[,22]),22]=0
Goyang_city[is.na(Goyang_city[,23]),23]=0

#공동주택현황
D5=read.csv('경기도_고양시_공동주택현황_20191120.csv')
#동, 동수,세대수
D5=D5[,c(3,9,10)]
D5$지번=substr(D5$지번,1,3)
D5$지번=gsub("풍동1","풍동",D5$지번)
D5$지번=gsub("풍동 ","풍동",D5$지번)
D5$지번=gsub("정발산","정발산동",D5$지번)
D5$지번=gsub("사리현","사리현동",D5$지번)
D5$지번=gsub("고양시","고양동",D5$지번)
D5$지번=gsub("고양향","고양동",D5$지번)
tempD5=data.frame(t(table(D5[,1])))[,-1]
for(i in 1:32){
  tempD5[i,2]=sum(D5[D5[,1]==tempD5[i,1],2])
}
for(i in 1:32){
  tempD5[i,3]=sum(D5[D5[,1]==tempD5[i,1],3])
}
D5=tempD5
names(D5)=c("동","연립주택동수","연립주택가구수")
Goyang_city=merge(Goyang_city,D5,by='동',all.x=T)

#공연장현황
D6=read.csv('경기도_고양시_공연장현황_2019.csv')
#영업중인 공연장만 인정
D6=D6[D6[,5]=="영업중",]
D6=D6[,9]
tempD6=c()
for(i in (1:16)){
  tempD6[i]=str_split(D6," ")[[i]][4]
}
D6=data.frame(t(table(tempD6)))[,-1]
names(D6)=c("동","공연장")
Goyang_city=merge(Goyang_city,D6,by='동',all.x=T)
Goyang_city[is.na(Goyang_city[,26]),26]=0

#대규모점포등록현황
D7=read.csv("경기도_고양시_대규모점포등록현황_20190701.csv")
D7=D7[,c(3,4)]
tempD7=c("행신동","화정동","주엽동","화정동","중산동"
         ,"대화동","백석동","장항동","대화동","백석동"
         ,"도내동","장항동","주엽동","대화동","도내동"
         ,"장항동","백석동","동산동","백석동","대화동"
         ,"화정동","화정동","백석동","장항동","마두동"
         ,"중산동","마두동","장항동","마두동","주엽동"
         ,"주엽동","주엽동","주엽동","주엽동","대화동"
         ,"대화동","대화동","동산동","장항동","대화동","삼송동")
D7=data.frame(t(table(tempD7)))[,-1]
names(D7)=c("동","대규모점포")
Goyang_city=merge(Goyang_city,D7,by='동',all.x=T)
Goyang_city[is.na(Goyang_city[,27]),27]=0

#도서관현황
D8=read.csv("경기도_고양시_도서관_20191105.csv")
#도서, 비도서, 간행물의 합을 도서관총자료 변수로 합치기
D8=D8[,c(2,13:15)]
tempD8=data.frame(t(table(D8[,1])))[,-1]
tempD8[,3]=0
names(tempD8)=c("동","도서관","도서관총자료")
for(i in 1:25){
  tempD8[i,3]=sum(D8[D8[,1]==tempD8[i,1],2:4])
}
D8=tempD8
Goyang_city=merge(Goyang_city,D8,by='동',all.x=T)
Goyang_city[is.na(Goyang_city[,28]),28]=0
Goyang_city[is.na(Goyang_city[,29]),29]=0

#도시공원현황
D9=read.csv("경기도_고양시_도시공원정보_20190427.csv")
D9=D9[,c(5,8)]
tempD9=c()
for(i in 1:248){
  tempD9[i]=str_split(D9[,1]," ")[[i]][4]
}
D9[,1]=tempD9
tempD9=data.frame(t(table(tempD9)))[,-1]
for(i in 1:27){
  tempD9[i,3]=sum(D9[D9[,1]==tempD9[i,1],2])
}
names(tempD9)=c("동","도시공원","도시공원총면적")
D9=tempD9
Goyang_city=merge(Goyang_city,D9,by='동',all.x=T)
Goyang_city[is.na(Goyang_city[,30]),30]=0
Goyang_city[is.na(Goyang_city[,31]),31]=0

#의료기관현황
D10=read.csv("경기도_고양시_의료기관현황_20191105.csv")
D10=D10[,2]
tempD10=c()
for(i in 1:1185){
  tempD10[i]=substr(str_split(D10, pattern="\\(")[[i]][2],1,3)
}
tempD10=gsub("풍동)","풍동",tempD10)
tempD10=gsub("풍동,","풍동",tempD10)
tempD10=gsub("재)한","백석동",tempD10)
tempD10=gsub("607","화정동",tempD10)
tempD10=gsub("벽산블","가좌동",tempD10)
tempD10=gsub("사리현","사리현동",tempD10)
tempD10=gsub("신성프","덕이동",tempD10)
tempD10=gsub("일부호","백석동",tempD10)
tempD10=gsub("정발산","정발산동",tempD10)
tempD10=gsub("주) ","주엽동",tempD10)
D10=data.frame(t(table(tempD10)))[,-1]
names(D10)=c("동","의료기관")
Goyang_city=merge(Goyang_city,D10,by='동',all.x=T)
Goyang_city[is.na(Goyang_city[,32]),32]=0

#의료유사업소현황
D11=read.csv("경기도_고양시_의료유사업소현황_20191105.csv")
D11=D11[,2]
tempD11=c()
for(i in 1:18){
  tempD11[i]=substr(str_split(D11, pattern="\\(")[[i]][2],1,3)
}
D11=data.frame(t(table(tempD11)))[,-1]
names(D11)=c("동","의료유사업소")
Goyang_city=merge(Goyang_city,D11,by='동',all.x=T)
Goyang_city[is.na(Goyang_city[,33]),33]=0

#자동차대여업현황
D12=read.csv("경기도_고양시_자동차대여업 현황_20191107.csv")
D12=D12[,4]
tempD12=c()
for(i in 1:62){
  tempD12[i]=substr(str_split(D12, pattern="\\(")[[i]][2],1,3)
}
tempD12=gsub("풍동)","풍동",tempD12)
tempD12=gsub("르노삼","백석동",tempD12)
tempD12=gsub("바로주","행신동",tempD12)
D12=data.frame(t(table(tempD12)))[,-1]
names(D12)=c("동","자동차대여업")
Goyang_city=merge(Goyang_city,D12,by='동',all.x=T)
Goyang_city[is.na(Goyang_city[,34]),34]=0

#전통시장현황
D13=read.csv("경기도_고양시_전통시장_20190710.csv")
D13=D13[,4]
#전통시장은 지역별로 있고 없고의 여부 = 0,1 만 기록 (factor)
tempD13=c()
for(i in 1:8){
  tempD13[i]=substr(str_split(D13," ")[[i]][4],1,3)
}
D13=data.frame(t(table(tempD13)))[,-1]
D13[,2]="Y"
D13[,2]=as.factor(D13[,2])
names(D13)=c("동","전통시장여부")
Goyang_city=merge(Goyang_city,D13,by='동',all.x=T)
levels(Goyang_city$전통시장여부)=c("Y","N")
Goyang_city$전통시장여부[is.na(Goyang_city$전통시장여부)]="N"


#영화상영관현황
D14=read.csv("경기도_고양시_영화상영관현황_20191120.csv")
D14=D14[,4]
tempD14=c()
for(i in 1:71){
  tempD14[i]=substr(str_split(D14,"\\(")[[i]][2],1,3)
}
D14=data.frame(t(table(tempD14)))[,-1]
names(D14)=c("동","영화상영관수")
Goyang_city=merge(Goyang_city,D14,by='동',all.x=T)
Goyang_city[is.na(Goyang_city[,36]),36]=0

#여행업현황
D15=read.csv("경기도_고양시_여행업 현황(201911).csv")
D15=D15[,4]
tempD15=c()
for(i in 1:323){
  tempD15[i]=substr(str_split(D15,"\\(")[[i]][2],1,3)
}
tempD15=gsub("풍동)","풍동",tempD15)
tempD15=gsub("풍동,","풍동",tempD15)
tempD15=gsub("B03","장항동",tempD15)
D15=data.frame(t(table(tempD15)))[,-1]
names(D15)=c("동","여행업")
Goyang_city=merge(Goyang_city,D15,by='동',all.x=T)
Goyang_city[is.na(Goyang_city[,37]),37]=0

#어린이보호구역
D16=read.csv("경기도_고양시_어린이보호구역_20191223.csv")
D16=D16[,4]
tempD16=c()
for(i in 1:167){
  tempD16[i]=substr(str_split(D16," ")[[i]][4],1,3)
}
D16=data.frame(t(table(tempD16)))[,-1]
names(D16)=c("동","어린이보호구역")
Goyang_city=merge(Goyang_city,D16,by='동',all.x=T)
Goyang_city[is.na(Goyang_city[,38]),38]=0

#약국현황
D17=read.csv("경기도_고양시_약국현황_20191105.csv")
D17=D17[,3]
tempD17=c()
for(i in 1:419){
  tempD17[i]=substr(str_split(D17,"\\(")[[i]][2],1,3)
}
tempD17=gsub("풍동)","풍동",tempD17)
tempD17=gsub("풍동,","풍동",tempD17)
tempD17=gsub("전체)","대화동",tempD17)
tempD17=gsub("사리현","사리현동",tempD17)
tempD17=gsub("정발산","정발산동",tempD17)
D17=data.frame(t(table(tempD17)))[,-1]
names(D17)=c("동","약국")
Goyang_city=merge(Goyang_city,D17,by='동',all.x=T)
Goyang_city[is.na(Goyang_city[,39]),39]=0

#문화유통업(pc, 노래방)
D18=read.csv("경기도_고양시_문화유통업현황_20191120.csv")
D18=D18[,3]
tempD18=c()
for(i in 1:1169){
  tempD18[i]=substr(str_split(D18,"\\(")[[i]][2],1,3)
}
tempD18=gsub("풍동)","풍동",tempD18)
tempD18=gsub("풍동,","풍동",tempD18)
tempD18=gsub("정발산","정발산동",tempD18)
tempD18=gsub("주)태","주교동",tempD18)
D18=data.frame(t(table(tempD18)))[,-1]
names(D18)=c("동","문화유통업")
Goyang_city=merge(Goyang_city,D18,by='동',all.x=T)
Goyang_city[is.na(Goyang_city[,40]),40]=0

#전기차충전소
D19=read.csv("경기도_고양시_전기차충전소_20190619.csv")
D19=D19[,c(10,11,14)]
tempD19=c()
for(i in 1:26){
  tempD19[i]=substr(str_split(D19[,3]," ")[[i]][4],1,3)
  D19[i,4]=substr(str_split(D19[,3]," ")[[i]][4],1,3)
}
D19[,-3]
tempD19=data.frame(t(table(tempD19)))[,-1]
names(tempD19)=c("동","전기차충전소")
for(i in 1:12){
  tempD19[i,3]=sum(D19[D19[,4]==tempD19[i,1],1:2])
}
D19=tempD19[,-2]
names(D19)=c("동","전기차충전기")
Goyang_city=merge(Goyang_city,D19,by='동',all.x=T)
Goyang_city[is.na(Goyang_city[,41]),41]=0

#Goyang에 행정동별정보 Goyang_city merge
Goyang_city=Goyang_city[,-(2:11)]
Goyang=merge(Goyang,pop,by='동',all.x=T)
Goyang=merge(Goyang,Goyang_city,by='동',all.x=T)
Goyang=Goyang[,-1]
Goyang$건물위생관리업=as.numeric(Goyang$건물위생관리업)
Goyang$목욕장업=as.numeric(Goyang$목욕장업)
Goyang$미용업=as.numeric(Goyang$미용업)
Goyang$세탁업=as.numeric(Goyang$세탁업)
Goyang$숙박업=as.numeric(Goyang$숙박업)
Goyang$이용업=as.numeric(Goyang$이용업)
set.seed(111)
S=sample(7467,7467)
Goyang=Goyang[S,]
rownames(Goyang)=NULL

write.csv(Goyang,"Goyang.csv")


########################################

#다중공선성문제로 공중위생업통합
Goyang[,8]=rowSums(Goyang[,8:13])
names(Goyang)[8]="공중위생업"
Goyang=Goyang[,-(9:13)]

#다중공선성문제로 연립주택동수 삭제
Goyang=Goyang[,-15]

#다중공선성문제로 도서관 삭제
Goyang=Goyang[,-18]

#다중공선성문제로 도시공원 삭제
Goyang=Goyang[,-19]

#다중공선성문제로 지식산업센터는 유치업체로 통일
Goyang=Goyang[,-(12:13)]

#다중공선성문제로 주점류 통합
Goyang[,10]=rowSums(Goyang[,10:11])
names(Goyang)[10]="주점류"
Goyang=Goyang[,-11]

#나머지 직관적으로 다중공선성이 있음을 예측하기 힘든 변수들은 플롯으로 해결
library(corrplot)
M=cor(Goyang[,-c(1:7,20)])
corrplot(M, method="number")

#의료기관이 중요하다고 판단하여 공중위생업, 연립주택가구수, 도서관총자료, 여행업 지우고 약국은 합치기
Goyang[,17]=rowSums(Goyang[,c(17,24)])
names(Goyang)[17]="의료및약국"
Goyang=Goyang[, -c(8,12,15,22,24)]

M=cor(Goyang[,-c(1:7,17)])
#corrplot(M, method="number")

#다중공선성, 의료유사업소 삭제
Goyang=Goyang[,-15]

#영화관은 문화유통업에 합치기
Goyang[,19]=rowSums(Goyang[,c(19,17)])
Goyang=Goyang[,-17]

M=cor(Goyang[,-c(1:7,16)])
#corrplot(M, method="number")

#공연장은 범주형으로 변경
Goyang$공연장=as.factor(Goyang$공연장)
Goyang$공연장[Goyang$공연장!=0]="1"
levels(Goyang$전통시장여부)=c("1","0")
levels(Goyang$공연장)

M=cor(Goyang[,-c(1:7,11,16)])
#corrplot(M, method="number")

#다중공선성, 어린이보호구역 삭제
Goyang=Goyang[,-17]

M=cor(Goyang[,-c(11,16)])
corrplot(M, method="number")

Goyang$층=as.numeric(Goyang$층)

str(Goyang)
summary(Goyang)

plot(y=Goyang$면적당가격,Goyang[,2])
plot(y=Goyang$면적당가격,Goyang[,3])
plot(y=Goyang$면적당가격,Goyang[,4]) # 노후정도
plot(y=Goyang$면적당가격,Goyang[,5])
plot(y=Goyang$면적당가격,Goyang[,6])
plot(y=Goyang$면적당가격,Goyang[,7]) # 고령화정도
plot(y=Goyang$면적당가격,Goyang[,8])
plot(y=Goyang$면적당가격,Goyang[,9]) # 주점류
plot(y=Goyang$면적당가격,Goyang[,10])
plot(y=Goyang$면적당가격,Goyang[,11])
plot(y=Goyang$면적당가격,Goyang[,12])
plot(y=Goyang$면적당가격,Goyang[,13])
plot(y=Goyang$면적당가격,Goyang[,14]) # 의료및약국
plot(y=Goyang$면적당가격,Goyang[,15])
plot(y=Goyang$면적당가격,Goyang[,16]) # 전통시장여부
plot(y=Goyang$면적당가격,Goyang[,17])
plot(y=Goyang$면적당가격,Goyang[,18])

#Y
plot(y=Goyang$면적당가격,Goyang$노후정도)
plot(y=Goyang$면적당가격,Goyang$전통시장여부)

#N
plot(y=Goyang$면적당가격,Goyang$주점류)
plot(y=Goyang$면적당가격,Goyang$대규모점포)

library(car)

model=lm(면적당가격~.,Goyang)
summary(model)
par(mfrow=c(2,2))
plot(model)
#crPlots(model)

model1=step(model)
summary(model1)
par(mfrow=c(2,2))
plot(model1)
#crPlots(model1)

Goyang2=Goyang
Goyang2$면적당가격=log(Goyang2$면적당가격)

model2=lm(면적당가격~.,Goyang2)
summary(model2)
par(mfrow=c(2,2))
plot(model2)
#crPlots(model2)

model2=step(model2)
summary(model2)
par(mfrow=c(2,2))
plot(model2)
#crPlots(model2)

#표준화변수
beta=c()
for(i in c(2:10,12:15,17:18)){
  beta[i]=
    model2$coefficients[i]*sd(Goyang2[,i])/sd(Goyang2$면적당가격)
}
beta
model2$coefficients[2]*sd(Goyang2[,2])/sd(Goyang2$면적당가격)

sd(Goyang2[,1])
sd(Goyang2[,2])
sd(Goyang2[,3])

plot(Goyang2$총인구밀도,Goyang2$면적당가격)

