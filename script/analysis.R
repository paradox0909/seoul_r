# 필요한 패키지 설치 및 로드
if (!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)

# 데이터 불러오기 (index.csv: skip=1, apartment_price.csv: 기본)
index_data <- read.csv("/Users/paradoxmyung/Desktop/Paradox/project/seoul_r/data/index.csv", fileEncoding = "UTF-8", skip = 1)
apartment_data <- read.csv("/Users/paradoxmyung/Desktop/Paradox/project/seoul_r/data/apartment_price.csv", fileEncoding = "UTF-8")

# 디버깅: No 컬럼 타입 및 값 확인
cat("\n=== index.csv의 No 컬럼 타입 ===\n")
print(class(index_data$No))
cat("\n=== index.csv의 No 컬럼 값(앞 20개) ===\n")
print(head(index_data$No, 20))

# No 컬럼이 문자형이면 숫자로 변환
if (is.character(index_data$No)) {
  index_data$No <- as.numeric(index_data$No)
  cat("\nNo 컬럼을 숫자로 변환함\n")
}

# 구 이름 공백 제거 및 서울시 구만 필터링 (강남구, 서초구 반드시 포함)
index_data <- index_data %>%
  filter((No >= 8 & No <= 40) | 분류.3 %in% c("강남구", "서초구")) %>%
  mutate(구 = trimws(분류.3))

apartment_data <- apartment_data %>%
  mutate(구 = trimws(지역명))

# 공통 구 추출
common_gus <- intersect(index_data$구, apartment_data$구)
cat("공통 구:", common_gus, "\n")

# 서울시 모든 구(강남구, 서초구 포함) 사용
index_seoul <- index_data %>% 
  filter(구 %in% common_gus) %>%
  distinct(구, .keep_all = TRUE)  # 중복 제거
apartment_seoul <- apartment_data %>% 
  filter(구 %in% common_gus) %>%
  distinct(구, .keep_all = TRUE)  # 중복 제거

cat("index_seoul 구:", unique(index_seoul$구), "\n")
cat("apartment_seoul 구:", unique(apartment_seoul$구), "\n")

# 디버깅: 필터링 후 index_data 확인
cat("\n=== 필터링 후 index_data (앞 5행) ===\n")
print(head(index_data, 5))

# 구 이름 확인
cat("\n=== index.csv의 구 목록 ===\n")
print(sort(unique(index_data$구)))
cat("\n=== apartment_price.csv의 구 목록 ===\n")
print(sort(unique(apartment_data$구)))

# 분석에 사용할 시점 지정
price_col <- "X2025.04"   # apartment_price.csv의 실제 평당가격 컬럼명

# 2025년 3월 매매지수 컬럼명 부분 문자열로 자동 탐색
target_col_candidates <- grep("2025.*03|2025.*3", colnames(index_data), value = TRUE)
cat("자동 탐색된 2025년 3월 매매지수 컬럼명:", target_col_candidates, "\n")
index_col <- "지수.9"  # 2025년 3월 매매지수

# 실제 컬럼명 확인 및 자동 매칭
cat("\n=== index.csv 컬럼명 ===\n")
print(colnames(index_data))
cat("\n=== apartment_price.csv 컬럼명 ===\n")
print(colnames(apartment_data))

# 평당가격(x축) 추출
x_data <- apartment_seoul %>%
  mutate(평당가격 = .data[[price_col]]) %>%
  select(구, 평당가격)
x_data$평당가격 <- as.numeric(x_data$평당가격)

# 디버깅: x_data 확인
cat("\n=== x_data 확인 ===\n")
print(x_data)

# 매매지수(y축) 추출
y_data <- index_seoul %>%
  select(구, 매매지수 = all_of(index_col))
y_data$매매지수 <- as.numeric(y_data$매매지수)

# 디버깅: y_data 확인
cat("\n=== y_data 확인 ===\n")
print(y_data)

# 병합
merged_data <- inner_join(x_data, y_data, by = "구")

# 디버깅: merged_data 상세 확인
cat("\n=== merged_data 상세 확인 ===\n")
print(merged_data)

# 상관계수 계산
correlation <- cor(merged_data$평당가격, merged_data$매매지수)
cat("상관계수:", correlation, "\n")

# 산점도
p <- ggplot(merged_data, aes(x = 평당가격, y = 매매지수)) +
  geom_point(size = 3, color = "#0072B5") +
  geom_smooth(method = "lm", se = FALSE, color = "#E15759") +
  geom_text(aes(label = 구), vjust = -0.5, size = 3) +
  labs(title = "서울시 구별 아파트 매매지수와 평당가격의 상관관계",
       subtitle = "매매지수: 2025년 3월 기준\n평당가격: 2025년 4월 기준",
       x = "2025년 4월 구별 아파트 평당가격 (만원)",
       y = "2025년 3월 구별 아파트 매매지수") +
  theme_bw() +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        plot.subtitle = element_text(hjust = 0.5))

print(p)

# output 폴더가 없으면 생성
if (!dir.exists("/Users/paradoxmyung/Desktop/Paradox/project/seoul_r/output")) dir.create("/Users/paradoxmyung/Desktop/Paradox/project/seoul_r/output")
ggsave("/Users/paradoxmyung/Desktop/Paradox/project/seoul_r/output/apartment_correlation.png", p, width = 12, height = 8) 