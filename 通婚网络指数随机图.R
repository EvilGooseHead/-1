library(statnet)
library(readxl)
library(texreg)
library(corrplot)
library(Hmisc)
setwd("E:/省际通婚网络/省际通婚网络 excel")

'marriage2000b' <- read_excel('女性的省际婚嫁网络-edgelist1--2000年及以后-含关系权重-5‰.xlsx')
matrixmarriage2000b <- as.matrix(marriage2000b[,-1])
vectorlabels<-unlist(marriage2000b[,1])
colnames(matrixmarriage2000b) <- vectorlabels
rownames(matrixmarriage2000b) <- vectorlabels
matrixmarriage2000b <- ifelse(matrixmarriage2000b > 0, 1, 0)

'marriage1990b' <- read_excel('女性的省际婚嫁网络-edgelist1--1990-1999年-含关系权重-5‰.xlsx')
matrixmarriage1990b <- as.matrix(marriage1990b[,-1])
colnames(matrixmarriage1990b) <- vectorlabels
rownames(matrixmarriage1990b) <- vectorlabels
matrixmarriage1990b <- ifelse(matrixmarriage1990b > 0, 1, 0)

'distance' <- read_excel('地理位置矩阵_有序.xlsx')
matrixdistance <- as.matrix(distance[,-1])
colnames(matrixdistance) <- vectorlabels
rownames(matrixdistance) <- vectorlabels
matrixdistancem <- ifelse(matrixdistance >= 1302, 1, 0)
matrixdistance300 <- ifelse(matrixdistance >= 300, 1, 0)

matrixdistancem <- diag.remove(matrixdistancem)
matrixdistance300 <-  diag.remove(matrixdistance300)
matrixmarriage1990b <- diag.remove(matrixmarriage1990b)
matrixmarriage2000b <- diag.remove(matrixmarriage2000b)

distancem <- as.network(matrixdistancem, directed = TRUE)
distance300 <- as.network(matrixdistance300, directed = TRUE)
net1990b <- as.network(matrixmarriage1990b, directed = TRUE )
net2000b <- as.network(matrixmarriage2000b, directed = TRUE )

attributes1990 <- data.frame(read_excel('自变量1990-1999.xlsx',range ="A1:E32"),row.names = 1)
attributes2000 <- data.frame(read_excel('自变量2000年以后.xlsx',range ="A1:E32"),row.names = 1)

attributes2000$私营企业就业人数.百万. <- log(attributes2000$私营企业就业人数.百万. )
attributes1990$私营企业就业人数.百万. <- log(attributes1990$私营企业就业人数.百万. )

#数据探索：仿真与比较2000

set.seed( 567 )
randomdir <- rgraph( 31, 1, tprob = .22, mode = "digraph" ) 
randomdirnet <- as.network( randomdir, directed = TRUE ) 

par( mfrow = c( 2,2 ), mai = c( .75,.75,.2,.2 ) ) 
hist( degree(net2000b, cmode = "indegree" ), main = "", 
      breaks=max(degree(net2000b, cmode="indegree"))/2, xlab = "Indegree ( Net2000b )", xlim = range( 0:20 ), ylim = range( 0:8 ) ) 
hist( degree( randomdirnet,cmode = "indegree" ), main = "", 
      breaks=max(degree(randomdirnet, cmode="indegree"))/2, xlab = "Indegree 
( random network )", xlim = range( 0:20 ), ylim = range( 0:10 ) ) 
hist( degree(net2000b,cmode = "outdegree" ), main = "", 
      breaks=max(degree(net2000b, cmode="outdegree"))/2, xlab = "Outdegree ( Net2000b )", xlim = range( 0:20 ), ylim = range( 0:8 ) ) 
hist( degree( randomdirnet,cmode = "outdegree" ), main = "", 
      breaks=max(degree(randomdirnet, cmode="outdegree"))/2, xlab = 
        "Outdegree ( random network )", xlim = range( 0:20 ), ylim = range( 0:10 
        ) ) 

par( mfrow = c( 1,2 ), mai = c( .5,1,.5,.5 ) ) 
barplot( dyad.census( net2000b ), xlab = "Dyad types", names.arg = c( 
  "Mutual", "Asymmetrical", "Null" ), main = "Net2000b" ) 
barplot( dyad.census( randomdirnet ),xlab = "Dyad types", names.arg = 
           c( "Mutual", "Asymmetrical", "Null" ), main = "Random network" ) 

n2001tri <- triad.census( net2000b )
randtri <- triad.census( randomdirnet ) 
triads <- rbind( n2001tri,randtri ) 
triads
barplot( as.matrix( triads ),beside = TRUE,col = c( 225,200 ) ) 
legend( "topright", inset = .05, title = "Triad Census", c( "Net2000b","Random" ), horiz = FALSE, pch = 15, col = c( 225,200 ) ) 

#数据探索：仿真与比较1990

set.seed( 567 )
randomdir1990 <- rgraph( 31, 1, tprob = .106, mode = "digraph" ) 
randomdirnet1990 <- as.network( randomdir1990, directed = TRUE ) 

par( mfrow = c( 2,2 ), mai = c( .75,.75,.2,.2 ) ) 
hist( degree(net1990b, cmode = "indegree" ), main = "", 
      breaks=max(degree(net1990b, cmode="indegree"))/2, xlab = "Indegree ( Net1990b )", xlim = range( 0:20 ), ylim = range( 0:12 ) ) 
hist( degree( randomdirnet1990,cmode = "indegree" ), main = "", 
      breaks=max(degree(randomdirnet1990, cmode="indegree"))/2, xlab = "Indegree 
( random network )", xlim = range( 0:20 ), ylim = range( 0:12 ) ) 
hist( degree(net1990b,cmode = "outdegree" ), main = "", 
      breaks=max(degree(net1990b, cmode="outdegree"))/2, xlab = "Outdegree ( Net1990b )", xlim = range( 0:20 ), ylim = range( 0:12 ) ) 
hist( degree( randomdirnet1990,cmode = "outdegree" ), main = "", 
      breaks=max(degree(randomdirnet1990, cmode="outdegree"))/2, xlab = 
        "Outdegree ( random network )", xlim = range( 0:20 ), ylim = range( 0:12 
        ) ) 

par( mfrow = c( 1,2 ), mai = c( .5,1,.5,.5 ) ) 
barplot( dyad.census( net1990b ), xlab = "Dyad types", names.arg = c( 
  "Mutual", "Asymmetrical", "Null" ), main = "Net1990b" ) 
barplot( dyad.census( randomdirnet1990 ),xlab = "Dyad types", names.arg = 
           c( "Mutual", "Asymmetrical", "Null" ), main = "Random network" ) 

n1990tri <- triad.census( net1990b )
randtri1990 <- triad.census( randomdirnet1990 ) 
triads1990 <- rbind( n1990tri,randtri1990 ) 
triads1990
barplot( as.matrix( triads1990 ),beside = TRUE,col = c( 225,200 ) ) 
legend( "topright", inset = .05, title = "Triad Census", c( "Net1990b","Random" ), horiz = FALSE, pch = 15, col = c( 225,200 ) ) 

#自变量相关矩阵

a <- attributes1990 
b <- attributes2000

A1 <- cor(attributes1990)
A2 <- cor(attributes2000)

par( mfrow = c( 1,1 )) 

corrplot(A1,
         method ='number', 
         type = "upper",
         order = "alphabet",
         tl.col = "black", tl.srt = 45)

corrplot(A2,
         method ='number', 
         type = "upper",
         order = "alphabet",
         tl.col = "black", tl.srt = 45)

#添加网络属性



net2000b %v% "私营企业就业人数（对数）" <- attributes2000$'私营企业就业人数.百万.'
net1990b %v% "私营企业就业人数（对数）" <- attributes1990$'私营企业就业人数.百万.'

net2000b %v% "20-34岁总人口数（千万)" <- attributes2000$'X20.34岁总人口数.千万.'
net1990b %v% "20-34岁总人口数（千万)" <- attributes1990$'X20.34岁总人口数.千万.'

net2000b %v% "未婚男/女人数比" <- attributes2000$'未婚男.女人数比'
net1990b %v% "未婚男/女人数比" <- attributes1990$'未婚男.女人数比'

net2000b %v% "平均受教育年数" <- attributes2000$'平均受教育年数'
net1990b %v% "平均受教育年数" <- attributes1990$'平均受教育年数'

#2000年结构效应模型
m1_2000 <- ergm(net2000b ~ edges 
                ,control=control.ergm(MCMC.samplesize=10000, MCMC.burnin=100000,MCMC.interval=100, seed = 567,parallel = 8))
summary(m1_2000)


#2000年主效应模型1
m2_2000<- ergm(net2000b ~ edges +  mutual
               + nodecov('20-34岁总人口数（千万)')
               + nodeicov('20-34岁总人口数（千万)')
               + nodecov('未婚男/女人数比')
               + nodeicov('未婚男/女人数比'), 
               #以上是第一个模型
               control=control.ergm(MCMC.samplesize=10000, MCMC.burnin=100000,MCMC.interval=100, seed = 567,parallel = 8))
summary(m2_2000)

#拟合检验1
gofm2_2000 <- gof(m2_2000)
plot(gofm2_2000)

mcmc.diagnostics(m2_2000)

m3_2000<- ergm(net2000b ~ edges 
               + nodecov('20-34岁总人口数（千万)')
               + nodeicov('20-34岁总人口数（千万)')
               + nodecov('未婚男/女人数比')
               + nodeicov('未婚男/女人数比')
               #以上是第一个模型
               + absdiff('平均受教育年数')
               + diff('平均受教育年数',dir="h-t"),
               control=control.ergm(MCMC.samplesize=10000, MCMC.burnin=100000,MCMC.interval=100, seed = 567,parallel = 8))

m4_2000 <- ergm(net2000b ~ edges 
                + nodecov('20-34岁总人口数（千万)')
                + nodeicov('20-34岁总人口数（千万)')
                + nodecov('未婚男/女人数比')
                + nodeicov('未婚男/女人数比')  
                #以上是第一个模型
                + absdiff('平均受教育年数')
                + diff('平均受教育年数',dir="h-t")
                #以上是第二个模型，同质性
                + edgecov(distancem)
                + edgecov(distance300),
                #以上是第三个模型
                control=control.ergm(MCMC.samplesize=10000, MCMC.burnin=100000,MCMC.interval=100, seed = 567,parallel = 8))
summary(m4_2000)

#2000年主效应模型2

m5_2000 <- ergm(net2000b ~ edges 
                + nodecov('20-34岁总人口数（千万)')  
                + nodeicov('20-34岁总人口数（千万)') 
                + nodecov('未婚男/女人数比') 
                + nodeicov('未婚男/女人数比')  
                #以上是第一个模型
                + absdiff('私营企业就业人数（对数）')
                + diff('私营企业就业人数（对数）',dir='h-t'),
                control=control.ergm(MCMC.samplesize=10000, MCMC.burnin=100000,MCMC.interval=100, seed = 567,parallel = 8))
summary(m5_2000)

m6_2000 <- ergm(net2000b ~ edges 
                + nodecov('20-34岁总人口数（千万)')  
                + nodeicov('20-34岁总人口数（千万)') 
                + nodecov('未婚男/女人数比') 
                + nodeicov('未婚男/女人数比')  
                #以上是第一个模型
                + absdiff('私营企业就业人数（对数）')
                + diff('私营企业就业人数（对数）',dir='h-t')
                #以上是第二个模型，同质性
                + edgecov(distancem)
                + edgecov(distance300),
                #以上是第三个模型
                control=control.ergm(MCMC.samplesize=10000, MCMC.burnin=100000,MCMC.interval=100, seed = 567,parallel = 8))
summary(m6_2000)

#拟合检验2
gofm6_2000 <- gof(m6_2000)
plot(gofm6_2000)

mcmc.diagnostics(m6_2000)

#1990年结构效应模型
m1_1990 <- ergm(net1990b ~ edges 
                ,control=control.ergm(MCMC.samplesize=10000, MCMC.burnin=100000,MCMC.interval=10, seed = 567,parallel = 8))
summary(m1_1990)

#拟合检验3
gofm6_2000 <- gof(m6_2000)
plot(gofm6_2000)

mcmc.diagnostics(m6_2000)

#1990年结构效应+主效应模型1

m2_1990 <- ergm(net1990b ~ edges 
                + nodecov('20-34岁总人口数（千万)')
                + nodeicov('20-34岁总人口数（千万)')
                + nodecov('未婚男/女人数比')
                + nodeicov('未婚男/女人数比'), 
                #以上是第一个模型
                control=control.ergm(MCMC.samplesize=10000, MCMC.burnin=100000,MCMC.interval=100, seed = 567,parallel = 8))
summary(m2_1990)

m3_1990 <- ergm(net1990b ~ edges 
                + nodecov('20-34岁总人口数（千万)')
                + nodeicov('20-34岁总人口数（千万)')
                + nodecov('未婚男/女人数比')
                + nodeicov('未婚男/女人数比')
                #以上是第一个模型
                + absdiff('平均受教育年数')
                + diff('平均受教育年数',dir="h-t"),
                control=control.ergm(MCMC.samplesize=10000, MCMC.burnin=100000,MCMC.interval=100, seed = 567,parallel = 8))
summary(m3_1990)

m4_1990 <- ergm(net1990b ~ edges 
                + nodecov('20-34岁总人口数（千万)')
                + nodeicov('20-34岁总人口数（千万)')
                + nodecov('未婚男/女人数比')
                + nodeicov('未婚男/女人数比')  
                #以上是第一个模型
                + absdiff('平均受教育年数')
                + diff('平均受教育年数',dir="h-t")
                #以上是第二个模型，同质性
                + edgecov(distancem)
                + edgecov(distance300),
                #以上是第三个模型
                control=control.ergm(MCMC.samplesize=10000, MCMC.burnin=100000,MCMC.interval=100, seed = 567,parallel = 8))
summary(m4_1990)

#1990年结构效应+主效应模型2

m5_1990 <- ergm(net1990b ~ edges 
                + nodecov('20-34岁总人口数（千万)')  
                + nodeicov('20-34岁总人口数（千万)') 
                + nodecov('未婚男/女人数比') 
                + nodeicov('未婚男/女人数比')  
                #以上是第一个模型
                + absdiff('私营企业就业人数（对数）')
                + diff('私营企业就业人数（对数）',dir='h-t'),
                control=control.ergm(MCMC.samplesize=10000, MCMC.burnin=100000,MCMC.interval=100, seed = 567,parallel = 8))
summary(m5_1990)

m6_1990 <- ergm(net1990b ~ edges 
                + nodecov('20-34岁总人口数（千万)')  
                + nodeicov('20-34岁总人口数（千万)') 
                + nodecov('未婚男/女人数比') 
                + nodeicov('未婚男/女人数比')  
                #以上是第一个模型
                + absdiff('私营企业就业人数（对数）')
                + diff('私营企业就业人数（对数）',dir='h-t')
                #以上是第二个模型，同质性
                + edgecov(distancem)
                + edgecov(distance300),
                #以上是第三个模型
                control=control.ergm(MCMC.samplesize=10000, MCMC.burnin=100000,MCMC.interval=100, seed = 567,parallel = 8))
summary(m6_1990)

#拟合检验4
gofm6_2000 <- gof(m6_2000)
plot(gofm6_2000)



#输出结果到WORD
htmlreg(list(m1_2000,m2_2000,m3_2000,m4_2000,m5_2000,m6_2000),
        file = '模型汇总2000.doc',
        custom.header = list('2000-2010年嫁入/出网络' = 1:6) ,
        stars = c(0.001, 0.01, 0.05, 0.1),
        single.row = T,
        custom.model.names = c('0模型','控制变量','教育水平','地理距离b','私企就业','地理距离b'),
        digits = 2)

htmlreg(list(m1_1990,m2_1990,m3_1990,m4_1990,m5_1990,m6_1990),
        file = '模型汇总1990.doc',
        custom.header = list('1989-1999年嫁入/出网络' = 1:6) ,
        stars = c(0.001, 0.01, 0.05, 0.1),
        single.row = T,
        custom.model.names = c('0模型','控制变量','教育水平','地理距离b','私企就业','地理距离b'),
        digits = 2)