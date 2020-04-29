### 2020/04/29 Keonwoo Park

### 데이터 구조론
### Selection sort DataFrame

set.seed(1234)
x1=rep(1:10,each=2,time=5)
x2=rep(1:5,each=20)
y=x1+x2+rnorm(100,0,1)
index = sample(1:100)

data_1 <- data.frame(index,y,x1,x2)

head(data_1)

summary(data_1)
Selection_Sort(data_1,2,T)





set.seed(1234)

a <- sample(1:10000,10000)
a
print(a)
Selection_Sort<- function(d,col_num=1,decreasing=FALSE){
  size_d <- length(d[,1])
  if(decreasing == FALSE){
    for(i1 in 1:(size_d-1)){
      min_value = d[i1, col_num]
      min_index=i1
      for(i2 in (i1+1):size_d){
        if(d[i2,col_num]<min_value){
          min_value=d[i2,col_num]
          min_index=i2
        }
      }
      ### Swap
      tem_value = d[i1,]
      d[i1,] = d[min_index,]
      d[min_index,] = tem_value
    }
    
  }else{
    for(i1 in 1:(size_d-1)){
      max_value = d[i1,col_num]
      max_index=i1
      for(i2 in (i1+1):size_d){
        if(d[i2,col_num]>max_value){
          max_value=d[i2,col_num]
          max_index=i2
        }
      }
      ### Swap
      tem_value = d[i1,]
      d[i1,] = d[max_index,]
      d[max_index,] = tem_value
    }
  
  }
  return(d)
}

a = sample(1:10)
a
Selection_Sort(a,T)


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
    Selection_Sort(test_data[,i2])
    T2<-Sys.time()
    T3=T2-T1
    Simulation_Results[i1,i2]=as.numeric(difftime(T2,T1,units="secs"))
    
  }
}


Simulation_Results

res <- apply(Simulation_Results,2,mean) ## 열로 더하려면 2 중요한기능
res







#### 내가 짜는 Selection_sort()

selcection_sort<- function(d){
  size_d <- length(d)
  for(i1 in 1:(size_d-1)){
    for(i2 in (i1+1):size_d){
      if(d[i1]>d[i2]){
        temp_value = d[i1]
        d[i1] = d[i2]
        d[i2] = temp_value
      }
    }
  }
  return(d)
}
a <- sample(1:1000,1000)
selcection_sort(a)
a
