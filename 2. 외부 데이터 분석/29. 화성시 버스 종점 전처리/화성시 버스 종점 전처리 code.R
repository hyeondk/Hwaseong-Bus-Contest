### Code: 종점 csv file 작업 전처리 ###

## 1. csv file 호출
data1 <- read.csv(file.choose(), header = FALSE) # 종점
data2 <- read.csv(file.choose()) # 종점 (1)
data3 <- read.csv(file.choose()) # 6~8501 버스 분류
data4 <- read.csv(file.choose()) # 1 ~ 19-3 종점


## 2. file마다 전처리 작업
## (1) data1
data1.processing <- function(x = data1) {
  data1$V1 <- as.character(data1$V1)
  
  mydata1 <<- data.frame(matrix(data = NA, nrow = nrow(x), ncol = 2))
  colnames(mydata1) <<- c("버스고유ID", "종점번호")
  
  for(i in 1:nrow(x)) {
    mydata1[i, 1] <<- strsplit(x[i, ], split = "/")[[1]][1]
    mydata1[i, 2] <<- strsplit(x[i, ], split = "/")[[1]][2]
  }
}

data1.processing()

# 결과 확인
mydata1


## (2) data2
data2.processing <- function(x = data2) {
  data2[, 2] <- as.character(data2[, 2])
  
  mydata2 <<- data.frame(matrix(data = NA, nrow = nrow(x), ncol = 2))
  colnames(mydata2) <<- c("버스고유ID", "종점번호")
  
  for(i in 1:nrow(x)) {
    mydata2[i, 1] <<- strsplit(x[i, 2], split = "/")[[1]][1]
    mydata2[i, 2] <<- strsplit(x[i, 2], split = "/")[[1]][2]
  }
  
  mydata2 <<- mydata2[!is.na(mydata2[, 2]), ]
}

data2.processing()

# 결과 확인
mydata2


## (3) data3
data3.processing <- function(x = data3) {
  mydata3 <<- data.frame(matrix(data = NA, nrow = nrow(x), ncol = 2))
  colnames(mydata3) <<- c("버스고유ID", "종점번호")
  
  for(i in 1:nrow(x)) {
    mydata3[i, 1] <<- as.character(x[i, 1])
    mydata3[i, 2] <<- as.character(x[i, 2])
  }
}

data3.processing()

# 결과 확인
mydata3


## (4) data4
data4.processing <- function(x = data4) {
  mydata4 <<- data.frame(matrix(data = NA, nrow = nrow(x), ncol = 2))
  colnames(mydata4) <<- c("버스고유ID", "종점번호")
  
  for(i in 1:nrow(x)) {
    mydata4[i, 1] <<- as.character(x[i, 2])
    mydata4[i, 2] <<- as.character(x[i, 3])
  }
  
  mydata4[20, 1] <<- "233000051"
}

data4.processing()

# 결과 확인
mydata4


## 3. 데이터 합치기
merge_data <- rbind(mydata1, mydata2, mydata3, mydata4)

mergedata.processing <- function(x = merge_data) {
  final_data <<- data.frame(matrix(data = NA, nrow = nrow(x), ncol = 2))
  colnames(final_data) <<- c("버스고유ID", "종점번호")
  
  for(i in 1:nrow(merge_data)) {
    final_data[i, 1] <<- as.character(x[i, 1])
    final_data[i, 2] <<- as.character(x[i, 2])
  }
}

mergedata.processing()


## 4. csv file로 제작
write.csv(final_data, file = "버스고유ID 및 종점.csv")
