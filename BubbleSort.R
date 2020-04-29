### 7주차 강의 1

### 2020/04/29 KeonwooPark

## Bubble Sort

Bubble_Sort<-function(d,col_num=1 , decreasing=FALSE){  #정렬 알고리즘에 열별로 정렬 기능 넣기
  Flag=F
  size_d <-length(d[,1])      # 데이터프레임 임의의 열의 모든행의 수가 사이즈로됨.
  for (i1 in 1:(size_d-1)){
    for (i2 in 1:(size_d-i1)){
      if(decreasing==F){
        if(d[i2, col_num]>d[i2+1, col_num]){
          Flag=T
          ### swap
          temp_value=d[i2,]
          d[i2,] = d[i2+1,]
          d[i2+1,] = temp_value
        }
      }else{
        if(d[i2, col_num]<d[i2+1, col_num]){
          Flag=T
          ### swap
          temp_value=d[i2,]
          d[i2,] = d[i2+1,]
          d[i2+1,] = temp_value
        }
      }
    }
    if(Flag==F){
      break
    }
  }
  return(d)
}

a=sample(1:10)
a
Bubble_Sort(a)






# 알고리즘 속도 측정
set.seed(1234)
test_data<-data.frame(d1=sample(1:1000),
                      d2=1:1000,
                      d3=1000:1,
                      d4=c(1:500,sample(501:1000)),
                      d5=c(sample(1:500),c(501:1000))
)
head(test_data)



n=5
Simulation_Results <- data.frame(d1=rep(0,n),
                                 d2=rep(0,n),
                                 d3=rep(0,n),
                                 d4=rep(0,n),
                                 d5=rep(0,n))
for (i1 in 1:n){
  for (i2 in 1:5){
    T1<-Sys.time()
    Bubble_Sort(test_data[,i2])
    T2<-Sys.time()
    T3=T2-T1
    Simulation_Results[i1,i2]=as.numeric(difftime(T2,T1,units="secs"))
    
  }
}
Simulation_Results


res <- apply(Simulation_Results,2,mean) ## 열로 더하려면 2 중요한기능
res
### 여러 행이 있을 때
set.seed(1234)
x1=rep(1:10,each=2,time=5)
x2=rep(1:5,each=20)
y=x1+x2+rnorm(100,0,1)
index = sample(1:100)

data_1 <- data.frame(index,y,x1,x2)

head(data_1)
summary(data_1)
data_1

Bubble_Sort(data_1,3)
