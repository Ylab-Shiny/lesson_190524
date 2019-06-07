
#パッケージの読み込み
library(tidyverse)

library(nycflights13)

＃RstudioのGlobalEnvironmentにflights オブジェクトを作成
flights <- flights
＃上から5行までのデータの表示
head(flights)


2.どの飛行機が定時離着記録に関して最悪か。

# tailumのデータ数
data_tailum <- flights %>%
  filter(!is.na(tailum)) %>%
  group_by(tailum) %>%
  summarise(N = n()) %>%
  arrange(desc(N))

# 飛行機の最大・最小数
max(data_tailum$N)

min(data_tailum$N)

#遅れに関するデータオブジェクトdata_delay
data_delay <-flights %>%
  filter(!is.na(dep_delay), !is.na(arr_delay)) %>%
  group_by(tailum) %>%
  select(tailum, delay_score) %>%
  summarise(score = sum(delay_score), N= n()) %>%
  mutate(mean_score = score / N) %>%
  arrange(desc(mean_score)

# data_delayの確認
head(data_delay)

tail(data_delay)


３．遅延を出来るだけ避けたいとすれば、どの時間に飛行するとよいか。

# dep_delay またはarr_delayが負のものをリストワイズ
data_avoid_delay <- flights %>%
  filter(!is.na(dep_delay), !is.na(arr_delay)) %>%
  mutate(delay_value = dep_delay + arr_delay) %>%
  filter(delay_value >= 0) %>%
  group_by(hour) %>%
  summarise(sum_delay = sum(delay_value)) %>%
  arrange(sum_delay)

#最適な時間
BestWorst_hours <- c(data_avoid_delay$hur[1], data_avoid_delay$hour[nrow(data_avoid_delay)])
pribt(paste0("遅延をできるだけ避けるには、", BestWorst_hours[1], "時発の便を選択するとよいでしょう"))
print(paste0("一方、遅延を最も避けにくいのは、", BestWorst_hours[2], "時発の便です"))


