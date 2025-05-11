# 필요한 패키지 설치 및 로드
if (!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)

# CSV 파일 읽기
# index.csv: 첫 행이 헤더가 아니므로 skip=1 옵션 사용
# apartment_price.csv: 첫 행이 헤더이므로 기본 옵션 사용
index_data <- read.csv("/Users/paradoxmyung/Desktop/Paradox/project/seoul_r/data/index.csv", fileEncoding = "UTF-8", skip = 1)
apartment_data <- read.csv("/Users/paradoxmyung/Desktop/Paradox/project/seoul_r/data/apartment_price.csv", fileEncoding = "UTF-8")

# index.csv의 No 컬럼 타입 확인 및 디버깅
# No 컬럼이 문자형일 경우 분석을 위해 숫자형으로 변환 필요
cat("\n=== index.csv의 No 컬럼 타입 ===\n")
print(class(index_data$No))
cat("\n=== index.csv의 No 컬럼 값(앞 20개) ===\n")
print(head(index_data$No, 20))

# No 컬럼 타입 변환 (문자형인 경우 숫자형으로)
if (is.character(index_data$No)) {
  index_data$No <- as.numeric(index_data$No)
  cat("\nNo 컬럼을 숫자로 변환함\n")
}

# 데이터 전처리: 구 이름 정제 및 필터링
# 1. 구 이름의 앞뒤 공백 제거
# 2. 서울시 구만 선택 (No가 8-40 사이)
# 3. 강남구와 서초구는 반드시 포함되도록 설정
index_data <- index_data %>%
  filter((No >= 8 & No <= 40) | 분류.3 %in% c("강남구", "서초구")) %>%
  mutate(구 = trimws(분류.3))

apartment_data <- apartment_data %>%
  mutate(구 = trimws(지역명))

# 두 데이터셋에 공통으로 존재하는 구 추출
# 이후 분석에서는 이 공통 구들만 사용
common_gus <- intersect(index_data$구, apartment_data$구)
cat("공통 구:", common_gus, "\n")

# 서울시 구 데이터 추출
# distinct() 함수로 중복 데이터 제거하여 각 구가 한 번만 포함되도록 함
index_seoul <- index_data %>% 
  filter(구 %in% common_gus) %>%
  distinct(구, .keep_all = TRUE)
apartment_seoul <- apartment_data %>% 
  filter(구 %in% common_gus) %>%
  distinct(구, .keep_all = TRUE)

# 추출된 구 목록 확인
cat("index_seoul 구:", unique(index_seoul$구), "\n")
cat("apartment_seoul 구:", unique(apartment_seoul$구), "\n")

# 필터링 결과 확인
cat("\n=== 필터링 후 index_data (앞 5행) ===\n")
print(head(index_data, 5))

# 두 데이터셋의 구 목록 비교
cat("\n=== index.csv의 구 목록 ===\n")
print(sort(unique(index_data$구)))
cat("\n=== apartment_price.csv의 구 목록 ===\n")
print(sort(unique(apartment_data$구)))

# 분석 시점 설정
price_col <- "X2025.04"   # apartment_price.csv의 2025년 4월 평당가격 컬럼
index_col <- "지수.9"     # index.csv의 2025년 3월 매매지수 컬럼

# 컬럼명 확인
cat("\n=== index.csv 컬럼명 ===\n")
print(colnames(index_data))
cat("\n=== apartment_price.csv 컬럼명 ===\n")
print(colnames(apartment_data))

# x축 데이터 준비: 2025년 4월 평당가격
x_data <- apartment_seoul %>%
  mutate(평당가격 = .data[[price_col]]) %>%
  select(구, 평당가격)
x_data$평당가격 <- as.numeric(x_data$평당가격)

cat("\n=== x축 데이터(평당가격) 확인 ===\n")
print(x_data)

# y축 데이터 준비: 2025년 3월 매매지수
y_data <- index_seoul %>%
  select(구, 매매지수 = all_of(index_col))
y_data$매매지수 <- as.numeric(y_data$매매지수)

cat("\n=== y축 데이터(매매지수) 확인 ===\n")
print(y_data)

# x축, y축 데이터 병합
merged_data <- inner_join(x_data, y_data, by = "구")

cat("\n=== 최종 분석 데이터 확인 ===\n")
print(merged_data)

# 상관분석: 평당가격과 매매지수 간의 상관계수 계산
correlation <- cor(merged_data$평당가격, merged_data$매매지수)
cat("평당가격과 매매지수의 상관계수:", correlation, "\n")

# 산점도 그래프 생성
# 1. 기본 산점도 (파란색 점)
# 2. 선형회귀선 추가 (빨간색 선)
# 3. 구 이름 라벨 추가
# 4. 그래프 제목과 축 이름 설정
# 5. 흰색 배경으로 테마 설정
p <- ggplot(merged_data, aes(x = 평당가격, y = 매매지수)) +
  geom_point(size = 3, color = "#0072B5") +  # 산점도 점
  geom_smooth(method = "lm", se = FALSE, color = "#E15759") +  # 선형회귀선
  geom_text(aes(label = 구), vjust = -0.5, size = 3) +  # 구 이름 라벨
  labs(title = "서울시 구별 아파트 매매지수와 평당가격의 상관관계",
       subtitle = "매매지수: 2021년 6월의 아파트 가격을 100이라고 정하고 현재 가격을 상대적으로 나타낸 값",
       x = "2025년 4월 구별 아파트 제곱미터 당 가격 (만원)",
       y = "2025년 3월 구별 아파트 매매지수") +
  theme_bw() +  # 기본 테마 설정
  theme(panel.background = element_rect(fill = "white"),  # 패널 배경 흰색
        plot.background = element_rect(fill = "white"),   # 플롯 배경 흰색
        plot.subtitle = element_text(hjust = 0.5))        # 부제목 가운데 정렬

# 그래프 출력
print(p)

# 그래프 저장
# 1. output 폴더가 없으면 생성
# 2. 그래프를 PNG 파일로 저장 (크기: 12x8 인치)
if (!dir.exists("/Users/paradoxmyung/Desktop/Paradox/project/seoul_r/output")) dir.create("/Users/paradoxmyung/Desktop/Paradox/project/seoul_r/output")
ggsave("/Users/paradoxmyung/Desktop/Paradox/project/seoul_r/output/apartment_correlation.png", p, width = 12, height = 8) 