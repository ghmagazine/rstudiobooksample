## 3-2 tidyrによるtidy dataへの変形

scores_messy <- data.frame(
  名前 = c("生徒A", "生徒B"),
  算数 = c(   100,    100),
  国語 = c(    80,    100),
  理科 = c(    60,    100),
  社会 = c(    40,     20),
  stringsAsFactors = FALSE   # 文字列が因子型に変換されないようにする
)


# tidyverse のパッケージ群を読み込み
library(tidyverse)

gather(scores_messy,
       key = "教科", value = "点数",   # 新しくできる列の名前を指定
       算数, 国語, 理科, 社会)          # 変形する対象の列を指定


# gather()でtidy dataに変形
scores_tidy <- gather(scores_messy,
                      key = "教科", value = "点数",
                      算数, 国語, 理科, 社会)

# spread()で横長に戻す
spread(scores_tidy, key = 教科, value = 点数)


## 3-3 dplyrによる基本的なデータ操作 

mpg


d <- data.frame(x = 1:3)
d[, "x"]


# as_tibble()でtibbleに変換
d_tibble <- as_tibble(d)
d_tibble[, "x"]


mpg %>%
  # 列の絞り込み
  select(model, displ, year, cyl)


mpg %>%
  # 列の絞り込み
  select(manufacturer, model, displ, year, cyl) %>%
  # 行の絞り込み
  filter(manufacturer == "audi") %>%
  # 新しい列を作成
  mutate(century = ceiling(year / 100))


mpg %>%
  filter(manufacturer == "audi")


mpg %>%
  filter(manufacturer == "audi", cyl >= 6)


mpg %>%
  filter(manufacturer == "audi" & cyl >= 6)


mpg %>%
  filter(manufacturer == "audi" | cyl >= 6)


mpg %>%
  filter(!(manufacturer == "audi" | cyl >= 6))


## COLUMN: dplyrの関数内でのコード実行 ------------------------------------------

mpg %>%
  filter(manufacturer == "audi")


manufacturer == "audi"


mpg$manufacturer == "audi"


mpg[mpg$manufacturer == "audi", ]


# 3つの条件を&で結合
mpg[mpg$manufacturer == "audi" &
    mpg$cyl >=  6 &
    mpg$cyl <  10, ]


mpg %>%
  filter(
    manufacturer == "audi",
    cyl >=  6,
    cyl <  10
  )

## ------------------------------------------------------------------------


mpg %>%
  arrange(cty)


mpg %>%
  arrange(cty, hwy)


mpg %>%
  arrange(-cty)


mpg %>%
  arrange(-manufacturer)


mpg %>%
  arrange(desc(manufacturer))


mpg %>%
  arrange(desc(cty, hwy))


mpg %>%
  select(model)


mpg %>%
  select(model, trans)


mpg %>%
  select(manufacturer:year)


mpg %>%
  select(-manufacturer)


mpg %>%
  select(MODEL = model, TRANS = trans)


mpg %>%
  rename(MODEL = model, TRANS = trans)


mpg %>%
  select(starts_with("c"))


starts_with("c")


mpg %>%
  arrange(starts_with("c"))


mpg %>%
  # cylそれぞれの値が6以上なら"6以上"、それ以外なら"6未満"、という列cty_6を追加
  mutate(cty_6 = if_else(cyl >= 6, "6以上", "6未満"))


mpg %>%
  mutate(cty = if_else(cyl >= 6, "6以上", "6未満"))


mpg %>%
  transmute(cty_6 = if_else(cyl >= 6, "6以上", "6未満"), year)


mpg %>%
  mutate(
    century = ceiling(year/100),        # ceiling()は値を切り上げる関数
    century_int = as.integer(century)   # ceiling()はの結果は数値型なので、整数型に変換
  )


mpg %>%
  summarise(displ_max = max(displ))


mpg %>%
  summarise(displ_range = range(displ))


## 3-4 dplyrによる応用的なデータ操作

mpg_grouped <- mpg %>%
  group_by(manufacturer, year)


mpg_grouped %>%
  transmute(displ_rank = rank(displ, ties.method = "max"))


mpg_grouped %>%
  filter(n() >= 20)


mpg_grouped %>%
  summarise(displ_max = max(displ))


ungroup(mpg_grouped)


## COLUMN: ウィンドウ関数 ------------------------------------------------------

c(1, 2, 3) + c(1, 2, 3)

c(1 + 1,
  2 + 2,
  3 + 3)


# 長さ3のベクトルなので順位は1～3まである
rank(c(1, 10, 100))

# それぞれ長さ1のベクトルなので順位は1しかない
c(rank(1),
  rank(10),
  rank(100))


# デフォルトだと1つ前の値を返す
lag(1:10)

# nを指定するとその数だけ前の値を返す
lag(1:10, n = 3)


# tibble()はtibbleを作成するための関数、data.frame()と違ってstringsAsFactors = FALSEは必要ない
uriage <- tibble(
  day   = c(   1,   1,   2,   2,   3,   3,   4,   4),  # 日付
  store = c( "a", "b", "a", "b", "a", "b", "a", "b"),  # 店舗ID
  sales = c( 100, 500, 200, 500, 400, 500, 800, 500)   # 売上額
)


uriage %>%
  # 店舗IDでグループ化
  group_by(store) %>%
  # 各日について前日の売上との差を計算
  mutate(sales_diff = sales - lag(sales))


uriage %>%
  group_by(store) %>%
  mutate(sales_mean = mean(sales),         # 各店舗の平均売上額
         sales_err  = sales - sales_mean)  # 各日の売上と平均売上額との差


uriage %>%
  group_by(store) %>%
  mutate(sales_err = sales - mean(sales))

## ------------------------------------------------------------------------


## COLUMN: selectのセマンティクスとmutateのセマンティクス、tidyeval ---------------------

mpg %>%
  mutate(new_col = cyl)

mpg %>%
  select(new_col = cyl)


mpg %>%
  select(new_col = 5)


mpg %>%
  select(manufacturer:year)


mpg %>%
  select(1:4)


mpg %>%
  select(manufacturer + 1)


mpg %>%
  select(c("model", "year"))


mpg %>%
  mutate(cyl2 = sqrt("cyl"))


col <- as.name("cyl")


mpg %>%
  mutate(cyl2 = sqrt(!! col))


rlang::expr(
  mpg %>%
    mutate(cyl2 = sqrt(!! col))
)


cyl <- c("manufacturer", "year")

# cyl列が選択されてしまう
mpg %>%
  select(cyl)


# manufacturer列、year列が選択される
mpg %>%
  select(!! cyl)


# runif()の結果を再現性あるものにするため、乱数のシードを固定
set.seed(1)

# runif()で0～100の範囲の乱数を10個ずつ生成
d <- tibble(
  id   = 1:10,
  test1 = runif(10, max = 100),
  test2 = runif(10, max = 100),
  test3 = runif(10, max = 100),
  test4 = runif(10, max = 100)
)


d %>%
  mutate(
    test1 = round(test1),
    test2 = round(test2),
    test3 = round(test3),
    test4 = round(test4)
  )


d_tidy <- d %>%
  # gather()でもselectのセマンティクス（コラム参照）が使える
  gather(key = "test", value = "value", test1:test4)
  
d_tidy


d_tidy %>%
  mutate(value = round(value))


d_tidy %>%
  group_by(test) %>%
  summarise(value_avg = mean(value))


d_tidy %>%
  mutate(value = round(value)) %>%
  spread(key = test, value = value)


d %>%
  mutate_all(round)


mpg %>%
  mutate_all(round)


mpg %>%
  mutate_if(is.numeric, round)


d %>%
  mutate_at(c("test1", "test2", "test3", "test4"), round)


d %>%
  mutate_at(vars(-id), round)

## ------------------------------------------------------------------------


## 3-5 dplyrによる2つのデータセットの結合と絞り込み


uriage


tenko <- tibble(
  day    = c(     1,     2,    3,     4),
  rained = c( FALSE, FALSE, TRUE, FALSE)
)


uriage %>%
  inner_join(tenko, by = "day")


tenko2 <- tibble(
  DAY    = c(     1,     2,    3,     4),
  rained = c( FALSE, FALSE, TRUE, FALSE)
)


uriage %>%
  inner_join(tenko2, by = c("day" = "DAY"))


tenko3 <- tibble(
  DAY    = c(     1,     1,    2,    2,     3),
  store  = c(    "a",  "b",  "a",  "b",   "b"),
  rained = c( FALSE, FALSE, TRUE, FALSE, TRUE)
)


uriage %>%
  inner_join(tenko3, by = c("day" = "DAY", "store"))


uriage %>%
  left_join(tenko3, by = c("day" = "DAY", "store"))


res <- uriage %>%
  left_join(tenko3, by = c("day" = "DAY", "store"))

# ベクトルなら、第2引数にはNAの代わりに入れる値を直接指定する
res %>%
  mutate(rained = replace_na(rained, FALSE))


# データフレームなら、第2引数には「列名 = NAの代わりの値」という形式のリストを指定する
res %>%
  replace_na(list(rained = FALSE))


res %>%
  group_by(store) %>%
  arrange(day) %>%
  fill(rained) %>%
  ungroup()


tenko4 <- tibble(
  day    = c(    2,    3,    3),
  store  = c(  "a",  "a",  "b"),
  rained = c( TRUE, TRUE, TRUE)
)


combinations <- tenko4 %>%
  # 絞り込みに使う列のみを選択
  select(day, store) %>%
  # 各行を1つのリストに変換
  transpose()

uriage %>%
  # 各行を1つのリストに変換
  mutate(x = map2(day, store, ~ list(day = .x, store = .y))) %>%
  # 一致する組み合わせがあるもののみに絞り込み
  filter(x %in% !! combinations)


uriage %>%
  semi_join(tenko4, by = c("day", "store"))


uriage %>%
  inner_join(tenko4, by = c("day", "store"))

