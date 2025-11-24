library(tidyverse)
# 多重填补法（pmm）进行缺失值插补 -------------------------------------------------------------------
library(lattice) #调入函数包
# library(nnet)
library(mice) #前三个包是mice的基础
## here is only consider topsoil and subsoil

col_types <- c("guess","guess","guess","text", rep("guess",185))
data <- readxl::read_xlsx("data_filter_1.xlsx", col_types = col_types, na=c("","NA")) #when you input data, you have change missing values to NA (this is necessary to mice necessary)

worksheet <- data %>%
  dplyr::select(Paper.ID,depth_class,Ecosystem_type,AI,MAP,MAT,PS_Bio15,TS_Bio4,TAR_Bio7,MaxTWM_Bio5,MinTCM_Bio6,MMTD_Bio2,PWM_Bio13,
                PDM_Bio14,Elevation,Slope,Mean_pH,Moisture,TC,CEC,Bulk.Density,
         RootDepth,GPP,Sand.content,Clay.content,Silt.content,Bedrock,Shannon_EVI,LAI,AbovegroundBiomass,BelowgroundBiomass,depth_mid,length,degree,CUE)

na_columns <- names(worksheet)[colSums(is.na(worksheet)) > 0]
print(na_columns)

worksheet$Paper.ID <- as.factor(worksheet$Paper.ID)
worksheet$depth_class <- as.factor(worksheet$depth_class)
worksheet$Ecosystem_type <- as.factor(worksheet$Ecosystem_type)

numeric_cols <- c("AI","MAP","MAT","PS_Bio15","TS_Bio4","TAR_Bio7","MaxTWM_Bio5","MinTCM_Bio6","MMTD_Bio2","PWM_Bio13",
                  "PDM_Bio14","Elevation","Slope","Mean_pH","Moisture","TC","CEC","Bulk.Density",
                  "RootDepth","GPP","Sand.content","Clay.content","Silt.content","Bedrock","Shannon_EVI","LAI",
                  "AbovegroundBiomass","BelowgroundBiomass","length","degree","depth_mid","CUE")

worksheet[numeric_cols] <- lapply(worksheet[numeric_cols], as.numeric)

worksheet <-dplyr::select(worksheet, c(2:35))

mice::md.pattern(worksheet)

#多重填补法
#m多重填补法的填补矩阵数，默认为5次；maxit最大迭代次数，默认5次；method, 填补用的方法,查看：help(mice)

imputed_Data <- mice(worksheet, m=5, maxit = 10, method = 'cart', seed = 500) #here i use cart becasue in my dataset, i have both factor and numeric

imputed_Data$imp#填补结果#填补结果pred
imputed_Data$predictorMatrix #which matrix used 

#分面板观察 (acatually i d not really know what this step is doing)
stripplot(imputed_Data, col=c("grey", mdc(2)), pch=c(1,20))
#任意2组变量插补后结果展示 (acatually i d not really know what this step is doing)
xyplot(imputed_Data, Moisture ~ Bulk.Density | .imp, pch=20, cex=1.2)

#this part is to find the suitable model in the fit
library(MASS)
completed1 <- complete(imputed_Data,1) #first you need to select first data set
step_model <- step(lm(CUE ~ ., data = completed1)) #then use step to find the model
formula(step_model) #this is to get what variables used in the model, then using these variables to fit
AIC(step_model)


#分析结果优化
##使用生成的填补的结果，应用统计模型，对每个完整的数据集进行评价。这里我们以多元线性回归模型作为评价填充值好坏的模型
##使用with()函数，对5组插补数据集进行多元线性回归分析模型，进行T检验，判断数据集中每个变量的有效性
# fit <- with(imputed_Data, lm(CUE ~ depth_class + Ecosystem_type + MAP + MAT + PS + Elevation + 
#                                Mean_pH + Moisture + TC + CEC + Bulk.Density + GPP + Sand.content + 
#                                Clay.content + LAI + AbovegroundBiomass + BelowgroundBiomass))
# summary(fit)
# 
# pooled_fit <- pool(fit) #this is to pool 5 different together
# summary(pooled_fit)
#对插补的结果，再进行R^2检验
pool.r.squared(fit)
#here is check whether is still has missing values or not
na_columns <- names(completed1)[colSums(is.na(completed1)) > 0]
print(na_columns)

write.csv(completed1, file = "Env_factors_cart.csv")
##end##



