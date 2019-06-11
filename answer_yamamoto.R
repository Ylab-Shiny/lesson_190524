#パッケージの読み込み
library(tidyverse)

library(nycflights13)

#RstudioのGlobal Enviromentにflightsオブジェクトを作成
flights <- flights
#上から５行目までのデータ表示
head(flights)

#tailnumのデータ数
data_tailnum <- flights %>%
  filter(!is.na(tailnum)) %>% 
  group_by(tailnum)%>%
  summarise(N = n()) %>%
  arrange(desc(N))

#飛行機の最大・最小数
max(data_tailnum$N)

min(data_tailnum$N)

#遅れに関するデータオブジェクト data_delay
data_delay <- flights %>%
  filter(!is.na(dep_delay),!is.na(arr_delay)) %>%
  group_by(tailnum) %>%
  mutate(delay_score = abs(dep_delay) + abs(arr_delay)) %>%
  select(tailnum,delay_score) %>%
  summarise(score = sum(delay_score),N = n()) %>%
  mutate(mean_score = score / N) %>%
  arrange(desc(mean_score))
print("---- 問２の答え　----")
print(data_delay$tailnum[1])

# deta_delay の確認
head(data_delay)

tail(data_delay)

#dep_delay またはarr_delay が負のものをリストワイズ
data_avoid_delay <- flights %>% 
  filter(!is.na(dep_delay),!is.na(arr_delay)) %>% 
  mutate(delay_value = dep_delay + arr_delay) %>% 
  filter(delay_value >= 0) %>% 
  group_by(hour) %>% 
  select(hour,delay_value) %>% 
  summarise(sum_delay = sum(delay_value)) %>% 
  arrange(sum_delay)

#最適な時間
print("---- 問３の答え　----")
BestWorst_hours <- c(data_avoid_delay$hour[1],data_avoid_delay$hour[nrow(data_avoid_delay)])
print(paste0("遅延をできるだけ避けるには、",BestWorst_hours[1],"時発の便を選択するといいでしょう")) 
print(paste0("一方、遅延を避けにくいのは",BestWorst_hours[2],"時発の便です"))