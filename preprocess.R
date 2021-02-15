library(tidyverse)
library(caret)
library(rlang)
library(lubridate)

preprocess_shop_base <- function(shop_base){
  
  reference_date <- '2020-02-20'
  
  # 店鋪形態
  shop_base <- shop_base %>% 
    mutate(
      is_shop_form_SSS    = if_else(grepl("SSS", shop_form), 1L, 0L, missing = 0L),
      is_shop_form_reuse  = if_else(grepl("リユース", shop_form), 1L, 0L, missing = 0L),
      is_shop_form_normal = if_else(grepl("通常", shop_form), 1L, 0L, missing = 0L)
    )
  
  # オープンとリニューアルからの年数
  shop_base <- shop_base %>% 
    mutate(
      # 運用時はこっち
      # months_since_opening = 15
      months_since_opening = as.integer(time_length(interval(as.Date(opening_date), as.Date(reference_date)), 'month')) # ,
      # months_since_renewal = as.integer(time_length(interval(as.Date(renewal_date), as.Date(reference_date)), 'month')),
      #is_renewal_flg = if_else(months_since_renewal >= 0 & months_since_renewal <= 15, 1L, 0L , missing = 0L)
   )
  
  # logを追加
  shop_base <- shop_base %>% 
    mutate(
      shop_code = as.integer(shop_code),
      area_of_site = as.numeric(area_of_site),
      area_of_building = as.numeric(area_of_building),
      area_of_site = if_else(area_of_site == 0, NA_real_, area_of_site, missing = NA_real_),
      log_area_of_site = log(area_of_site),
      log_area_of_building = log(area_of_building)
    )
  
  return(shop_base)
}


preprocess_shop_facilty <- function(shop_facilty){
  colsNewName <- c('shop_code', "店名", "building_floor", "sales_floor", "building_pos", "get_in_num", "is_bali", "parking_num", "boundary_walk",
                   "is_second_parking", "is_bicycle_parking", "is_parking_comfort", "is_pole_signboard", "is_New_logo", "is_ten_years")
  
  names(shop_facilty) <- colsNewName
  
  shop_facilty <- shop_facilty %>% mutate(shop_code = as.integer(shop_code))
  
  # 経年10以上
  # 10年以上のフラグがついているものは1。それ以外は0とする
  shop_facilty <- shop_facilty %>% mutate(is_ten_years = if_else(!is.na(is_ten_years), 1L, 0L, missing = 0L))
  
  # 新旧ロゴ
  shop_facilty <- shop_facilty %>% mutate(is_New_logo = if_else(is_New_logo == "新ロゴ", 1L, 0L, missing = 0L))
  
  # ポール看板
  shop_facilty <- shop_facilty %>% mutate(is_pole_signboard = if_else(is_pole_signboard == "有り", 1L, 0L, missing = 0L))
  
  # 駐車場の快適性
  shop_facilty <- shop_facilty %>% mutate(is_parking_comfort = if_else(is_parking_comfort == "○", 1L, 0L, missing = 0L))
  
  # 駐輪場の有無
  shop_facilty <- shop_facilty %>% mutate(is_bicycle_parking = if_else(is_bicycle_parking == "有", 1L, 0L, missing = 0L))
  
  # 第二駐車場
  shop_facilty <- shop_facilty %>% mutate(is_second_parking = if_else(is_second_parking == "有り", 1L, 0L, missing = 0L))
  
  # 歩道との境界
  shop_facilty <- shop_facilty %>% 
    mutate(
      is_boundary_fence = if_else(grepl("フェンス", boundary_walk), 1L, 0L, missing = 0L),
      is_boundary_curb  = if_else(grepl("縁石", boundary_walk), 1L, 0L, missing = 0L),
      is_boundary_step  = if_else(grepl("段差", boundary_walk), 1L, 0L, missing = 0L),
      is_boundary_flat  = if_else(grepl("フラット", boundary_walk), 1L, 0L, missing = 0L)) %>% 
    dplyr::select(-boundary_walk)
 
  # 駐車場台数
  shop_facilty <- shop_facilty %>% 
    mutate(
      is_coin_or_share = if_else(grepl("共有|コイン", parking_num), 1L, 0L, missing = 0L),
      parking_num = str_replace(parking_num, pattern = "共有", replacement = ""),
      parking_num = str_replace(parking_num, pattern = "コイン", replacement = ""),
      parking_num = str_replace(parking_num, pattern = "台", replacement = ""),
      parking_num = as.integer(parking_num),
      parking_num = if_else(is.na(parking_num), 0L, parking_num, missing = parking_num))
  
  # バリカー
  shop_facilty <- shop_facilty %>% 
    mutate(is_bali = if_else(is_bali == "有り", 1L, 0L, missing = 0L))
  
  # 乗り入れ数
  table(shop_facilty$get_in_num)
  # 乗り入れ数が - の店鋪はSCもしくは入口がオープンになっている
  # View(shop_facilty %>% filter(get_in_num == '-'))
  # その他店鋪は裏や離れたところに駐車場がある
  # View(shop_facilty %>% filter(get_in_num == 'その他'))
  # 欠損値の場合
  # View(shop_facilty %>% filter(is.na(get_in_num)))
  shop_facilty <- shop_facilty %>% 
    mutate(get_in_num = if_else(get_in_num == "その他" | get_in_num == "-" | is.na(get_in_num), "4", get_in_num, missing = get_in_num),
           get_in_num = as.integer(get_in_num))
  
  # 建物位置
  shop_facilty <- shop_facilty %>% 
    mutate(
      # building_pos = if_else(building_pos == "SC内", "SC", building_pos), 
      # building_pos = if_else(building_pos == "セットバック", "setback", building_pos), 
      # building_pos = if_else(building_pos == "接道", "connecting_road", building_pos)
      building_pos_SC = if_else(building_pos == 'SC内', 1L, 0L, missing = 0L),
      building_pos_setback = if_else(building_pos == 'セットバック', 1L, 0L, missing = 0L),
      building_pos_connecting_road = if_else(building_pos == '接道', 1L, 0L, missing = 0L)
    )
  
  
  # 建物階層、売場階層
  # 階数の数字のみにする建物階層がNAの場合、売場階層で埋める
  # table(shop_facilty$building_floor)
  # table(shop_facilty$sales_floor)
  # shop_facilty <- shop_facilty %>% 
  #  mutate(building_floor = if_else(is.na(building_floor), sales_floor, building_floor))
  
  shop_facilty <- 
    shop_facilty %>% mutate(
      sales_floor = if_else(sales_floor == "3F", "2F", sales_floor),
      building_floor = if_else(building_floor != "1F" & building_floor != "2F", 
                               "other", building_floor, missing = "other")
    )
  
  shop_facilty <- shop_facilty %>% 
    mutate(
      building_floor_1F = if_else(building_floor == "1F", 1L, 0L, missing = 0L),
      building_floor_2F = if_else(building_floor == "2F", 1L, 0L, missing = 0L),
      building_floor_other = if_else(building_floor == "other", 1L, 0L, missing = 0L),
      sales_floor_1F = if_else(sales_floor == '1F', 1L, 0L, missing = 0L),
      sales_floor_2F = if_else(sales_floor == '2F', 1L, 0L, missing = 0L)
    )
  
  # colsSelect <- c("shop_code", "店名", "building_floor_1F", "building_floor_2F", "building_floor_other", "sales_floor_1F", 
  #                "sales_floor_2F", "building_pos_connecting_road", "building_pos_SC", "building_pos_setback", "get_in_num", 
  #                "is_bali", "parking_num", "is_second_parking", "is_bicycle_parking", "is_parking_comfort", "is_pole_signboard",  
  #                "is_New_logo", "is_ten_years", "is_boundary_fence", "is_boundary_curb", "is_boundary_step", 
  #                "is_boundary_flat", "is_coin_or_share")
  
  # return(shop_facilty <- shop_facilty %>% dplyr::select(one_of(colsSelect)))
  return(shop_facilty)
}


preprocess_market <- function(market){
  # 不要列削除
  colsToRemove <- c("TA名称", "ID", "sheet_line", "sheet", "address", 
                    "経度(日本)", "緯度(日本)", "詳細", "集計単位", "エリア数")
  
  market <- market %>% dplyr::select(-one_of(colsToRemove))
  
  
  # 自店舗を含むため-1する
  market <- market %>% mutate(自社他店舗数 = if_else(自社他店舗数 > 0, 自社他店舗数 - 1, 自社他店舗数))
  
  # 名前をxgboostで処理可能な様に変更
  change_to_english <- function(data){
    colnames(data) = str_replace(colnames(data), '人口', 'population_')
    colnames(data) = str_replace(colnames(data), '総数', 'total_')
    colnames(data) = str_replace(colnames(data), '男性', 'male_')
    colnames(data) = str_replace(colnames(data), '女性', 'female_')
    colnames(data) = str_replace(colnames(data), '0-4歳', 'age_0_4_')
    colnames(data) = str_replace(colnames(data), '5-9歳', 'age_5_9_')
    colnames(data) = str_replace(colnames(data), '10-14歳', 'age_10_14_')
    colnames(data) = str_replace(colnames(data), '15-19歳', 'age_15_19_')
    colnames(data) = str_replace(colnames(data), '20-24歳', 'age_20_24_')
    colnames(data) = str_replace(colnames(data), '25-29歳', 'age_25_29_')
    colnames(data) = str_replace(colnames(data), '30-34歳', 'age_30_34_')
    colnames(data) = str_replace(colnames(data), '35-39歳', 'age_35_39_')
    colnames(data) = str_replace(colnames(data), '40-44歳', 'age_40_44_')
    colnames(data) = str_replace(colnames(data), '45-49歳', 'age_45_49_')
    colnames(data) = str_replace(colnames(data), '50-54歳', 'age_50_54_')
    colnames(data) = str_replace(colnames(data), '55-59歳', 'age_55_59_')
    colnames(data) = str_replace(colnames(data), '60-64歳', 'age_60_64_')
    colnames(data) = str_replace(colnames(data), '65歳以上', 'age_65_over_')
    
    colnames(data) = str_replace(colnames(data), '0-2歳', 'age_0_2_')
    colnames(data) = str_replace(colnames(data), '0-5歳', 'age_0_5_')
    colnames(data) = str_replace(colnames(data), '3-5歳', 'age_3_5_')
    colnames(data) = str_replace(colnames(data), '6-11歳', 'age_6_11_')
    colnames(data) = str_replace(colnames(data), '12-14歳', 'age_12_14_')
    colnames(data) = str_replace(colnames(data), '15-17歳', 'age_15_17_')
    colnames(data) = str_replace(colnames(data), '18歳以上', 'age_18_over_')
    colnames(data) = str_replace(colnames(data), '病院数', 'hospitals_')
    colnames(data) = str_replace(colnames(data), '一般診療所数', 'general_clinic_')
    colnames(data) = str_replace(colnames(data), '歯科診療所数', 'dental_clinics_')
    colnames(data) = str_replace(colnames(data), '生徒･学生', 'students_')
    
    # ()を認識しないためここのみ
    colnames(data)[which(colnames(data) == "核家族以外(親族のみ)の一般世帯数15m4w")] <- "non_nuclear_family_general_household_15m4w"
    
    colnames(data) = str_replace(colnames(data), '一般世帯数', 'general_household_')
    colnames(data) = str_replace(colnames(data), '親族のみ', 'relatives_only_')
    colnames(data) = str_replace(colnames(data), '一般世帯人員が1人', 'general_household_one_')
    colnames(data) = str_replace(colnames(data), '一般世帯人員が2人', 'general_household_two_')
    colnames(data) = str_replace(colnames(data), '一般世帯人員が3人', 'general_household_three_')
    colnames(data) = str_replace(colnames(data), '一般世帯人員が4人', 'general_household_four_')
    colnames(data) = str_replace(colnames(data), '一般世帯人員が5人', 'general_household_five_')
    colnames(data) = str_replace(colnames(data), '一般世帯人員が6人', 'general_household_six_')
    colnames(data) = str_replace(colnames(data), '一般世帯人員が7人以上', 'general_household_seven_over_')
    colnames(data) = str_replace(colnames(data), '核家族の', 'nuclear_family_')
    colnames(data) = str_replace(colnames(data), '6歳未満のいる', 'age_under6_in_')
    colnames(data) = str_replace(colnames(data), '20-29歳1人の', 'age_20_29_only_')
    colnames(data) = str_replace(colnames(data), 'のいる', 'in_')
    colnames(data) = str_replace(colnames(data), '高齢単身世帯数', 'elderly_single_person_households_')
    colnames(data) = str_replace(colnames(data), '高齢夫婦世帯数', 'elderly_couple_households_')
    colnames(data) = str_replace(colnames(data), '持ち家主世帯数', 'owned_households_')
    colnames(data) = str_replace(colnames(data), '公営・都市機構・公社の借家主世帯数', 'rented_households_of_public_')
    colnames(data) = str_replace(colnames(data), '民営の借家主世帯数', 'rented_households_of_privately_')
    colnames(data) = str_replace(colnames(data), '給与住宅主世帯数', 'salary_housing_households_')
    colnames(data) = str_replace(colnames(data), '間借り世帯数', 'rented_households_of_borrowing_')
    colnames(data) = str_replace(colnames(data), '一戸建世帯数', 'detached_households_')
    colnames(data) = str_replace(colnames(data), '長屋建世帯数', 'Nagaya_households_')
    colnames(data) = str_replace(colnames(data), '共同住宅世帯数', 'apartment_houses_')
    colnames(data) = str_replace(colnames(data), '1･2階建', 'stories_1_or_2_')
    colnames(data) = str_replace(colnames(data), '3-5階建', 'stories_3_5_')
    colnames(data) = str_replace(colnames(data), '6-10階建', 'stories_6_10_')
    colnames(data) = str_replace(colnames(data), '11-14階建', 'stories_11_14_')
    colnames(data) = str_replace(colnames(data), '15階建以上', 'stories_15_over_')
    colnames(data) = str_replace(colnames(data), '1･2階に住む', 'living_on_1_or_2_floor_')
    colnames(data) = str_replace(colnames(data), '3-5階に住む', 'living_on_3_5_floor_')
    colnames(data) = str_replace(colnames(data), '6-10階に住む', 'living_on_6_10_floor_')
    colnames(data) = str_replace(colnames(data), '11-14階に住む', 'living_on_11_14_floor_')
    colnames(data) = str_replace(colnames(data), '15階以上に住む', 'living_on_15_floor_over_')
    colnames(data) = str_replace(colnames(data), '昼間', 'Daytime_')
    colnames(data) = str_replace(colnames(data), '在住在学者', 'Residents_')
    colnames(data) = str_replace(colnames(data), '在住第2･3次産業', 'second_and_tertiary_industries_')
    colnames(data) = str_replace(colnames(data), '全産業事業所数', 'all_industrial_establishments_')
    colnames(data) = str_replace(colnames(data), '飲食料品小売業事業所数', 'food_and_beverage_retailers_')
    colnames(data) = str_replace(colnames(data), '小売業事業所数', 'retail_establishments_')
    colnames(data) = str_replace(colnames(data), '飲食店事業所数', 'restaurant_establishments_')
    colnames(data) = str_replace(colnames(data), '自社他店舗数', 'other_stores_own_company')
    colnames(data) = str_replace(colnames(data), '他社競合店舗数', 'other_stores_competition_company')
    colnames(data) = str_replace(colnames(data), '住宅に住む一般世帯人員', 'the_public_household_')
    colnames(data) = str_replace(colnames(data), '第2･3次産業', 'second_and_tertiary_industries_')
    return(data)
  }
  market <- change_to_english(market)
  
  # -----------------------------------------------------------------------
  # 追加データ
  # market2 <- readxl::read_excel(filesToRead[[2]])
  
  # 不要列削除
  # market2 <- market2 %>% dplyr::select(-one_of(colsToRemove))
  
  # market2 <- market2 %>% change_to_english()
  
  # market <- market %>% inner_join(market2, by = c('shop', '種別'))
  # rm(market2)
  # -----------------------------------------------------------------------
  
  # log化
  target_cols <- colnames(market)[-which(colnames(market) %in% c('shop', '種別'))]
  market <- market %>% mutate_at(vars(target_cols), ~ as.numeric(.))
  # infが発生しない様に + 1する
  market_log <- market %>% 
    mutate_at(vars(target_cols), ~ log(. + 1))
  
  # カラム名が重複しない様にlogをつける
  colsNewName <- c('shop', '種別')
  for (i in 1:(ncol(market_log) - 2)) {
    # print(i)
    tmp <- paste0('log_', target_cols[[i]])
    colsNewName <- c(colsNewName, tmp)
    rm(tmp)
  }
  
  names(market_log) <- colsNewName
  rm(colsNewName, target_cols)
  
  # ratio変数作成
  # 関数定義
  create_ratio <- function(data, denominator, numerators, prefix='ratio', suffix=''){
    # 分母をシンボル化
    denominator <- rlang::sym(denominator)
    for (i in 1:length(numerators)) {
      # 分子になる列を順に取り出していく
      extract_numerator <- numerators[[i]]
      # 新しい列名を設定
      new_col_name <- paste0(prefix, '_', extract_numerator, suffix)
      # 分子と新しい列名をシンボル化
      extract_numerator <- rlang::sym(extract_numerator)
      new_col_name <- rlang::sym(new_col_name)
      
      data <- data %>% 
        mutate(!!new_col_name := !!extract_numerator/!!denominator)
    }
    return(data)
  }
  
  # 総人口系
  colsToNumerators <- colnames(market %>% dplyr::select(starts_with('male'), starts_with('female'), 
                                                        starts_with('age_'), starts_with('Daytime'), 
                                                        starts_with('all_industrial'), starts_with('retail_establishments'),
                                                        starts_with('Residents'), starts_with('second_and_tertiary_industries_population'),
                                                        starts_with('hospitals'), starts_with('general_clinic'),
                                                        starts_with('dental_clinics'), starts_with('food_and_beverage_retailers'),
                                                        starts_with('restaurant_establishments')) %>% dplyr::select(-contains('household')))
  market <- create_ratio(market, 'population_total_15m4w', colsToNumerators)
  
  
  # 男性系
  colsToNumerators <- colnames(market %>% dplyr::select(contains('male')) %>% dplyr::select(-contains('female'), -male_population_15m4w))
  market <- create_ratio(market, 'male_population_15m4w', colsToNumerators, suffix = '_male')
  
  
  # 女性系
  colsToNumerators <- colnames(market %>% dplyr::select(contains('female')) %>% dplyr::select(-female_population_15m4w))
  market <- create_ratio(market, 'female_population_15m4w', colsToNumerators, suffix = '_female')
  
  
  # 世帯系
  colsToNumerators <- colnames(market %>% dplyr::select(contains('household'), starts_with('apartment'), 
                                                        starts_with('stories'), starts_with('living_on')) %>% dplyr::select(-general_household_15m4w))
  market <- create_ratio(market, 'general_household_15m4w', colsToNumerators)
  
  # 昼間人口系
  market <- create_ratio(market, 'Daytime_population_total_15m4w', 
                         c('Daytime_second_and_tertiary_industries_population_15m4w',
                           "Daytime_students_total_15m4w"), 
                         suffix = '_daytime')
  
  # diff変数
  market <- market %>% mutate(
    diff_population_all_minus_daytime = population_total_15m4w - Daytime_population_total_15m4w
  )
  
  # logと結合
  market <- market %>% inner_join(market_log, by = c('shop', '種別'))
  rm(market_log)
  
  # 展開時の名称用に名前を変更
  market$種別 <- market$種別 %>% str_replace_all(
    c('円\\[0.5km\\]' = 'circle_0.5km', 
      '円\\[1km\\]'   = 'circle_1km', 
      '円\\[2km\\]'   = 'circle_2km', 
      '円\\[3km\\]'   = 'circle_3km', 
      '円\\[4km\\]'   = 'circle_4km',
      '円\\[5km\\]'   = 'circle_5km',
      '円\\[10km\\]'  = 'circle_10km',
      '時間圏\\[5分\\]\\(自動車\\)'   = 'time_5min',
      '時間圏\\[10分\\]\\(自動車\\)'  = 'time_10min',
      '時間圏\\[15分\\]\\(自動車\\)'  = 'time_15min',
      '時間圏\\[20分\\]\\(自動車\\)'  = 'time_20min'))
  
  
  # 横展開
  colsSpred <- colnames(market)[-which(colnames(market) %in% c('shop', '種別'))]
  market <- market %>% group_by(shop) %>% 
    tidyr::pivot_wider(names_from = "種別", values_from = colsSpred) %>% 
    ungroup()
  return(market)
}


preprocess_final <- function(shop_base, shop_facilty, market){
  predict.kmeans <- function(object, newdata){
    centers <- object$centers
    n_centers <- nrow(centers)
    dist_mat <- as.matrix(dist(rbind(centers, newdata)))
    dist_mat <- dist_mat[-seq(n_centers), seq(n_centers)]
    max.col(-dist_mat)
  }
  
  # ---------------------------------------------------------------------------------
  # 店舗コードの修正
  # 運用では不要
  shop_facilty <- shop_facilty %>% 
    mutate(
      shop_code = if_else(shop_code == 801L, 802L, shop_code, missing = shop_code),
      shop_code = if_else(shop_code == 800L, 801L, shop_code, missing = shop_code)
    )
  # ---------------------------------------------------------------------------------
  
  t_cols <- colnames(market)[-which(colnames(market) %in% c('shop'))]
  market <- market %>% mutate_at(vars(t_cols), ~ as.numeric(.))
  
  
  df <- shop_base %>% dplyr::select(-floor_num) %>% 
    inner_join(shop_facilty %>% dplyr::select(-店名), by = 'shop_code') %>% 
    inner_join(market, by = c("店名" = "shop"))
  
  # ---------------------------------------------------------------------------------
  # 運用では不要
  # 2階部分を使用していない店舗
  shop_area_div_half <- c(71, 89, 154, 173, 174)
  df_shop_area_half <- df %>% filter(shop_code %in% shop_area_div_half)
  
  df_shop_area_half <- df_shop_area_half %>% mutate(area_of_building = area_of_building / 2)
  
  # 2, 3階部分を使用していない店舗
  shop_area_div_one_third <- c(811)
  df_shop_area_one_third <- df %>% filter(shop_code %in% shop_area_div_one_third)
  
  # df_shop_area_one_third <- df_shop_area_one_third %>% mutate(area_of_site = area_of_site / 3)
  df_shop_area_one_third <- df_shop_area_one_third %>% mutate(area_of_building = area_of_building / 3)
  
  # 正確な売り場面積がわかっている店舗
  shop_area_correct <- c(126, 809, 810)
  df_shop_area_correct <- df %>% filter(shop_code %in% shop_area_correct)
  
  
  
  df_shop_area_correct <- df_shop_area_correct %>% 
    mutate(
      area_of_building = if_else(shop_code == 126, 402.96, area_of_building),
      area_of_building = if_else(shop_code == 809, 127 * 3.30579, area_of_building), # 127坪
      area_of_building = if_else(shop_code == 810, 529.54, area_of_building)
    )
  
  df <- df %>% filter(!shop_code %in% c(shop_area_div_half, shop_area_div_one_third, shop_area_correct))
  
  df <- bind_rows(df, df_shop_area_half, df_shop_area_one_third, df_shop_area_correct)
  rm(df_shop_area_correct, df_shop_area_half, df_shop_area_one_third)
  
  colnames(df)[which(colnames(df) == "店名")] <- "shop_name"
  
  # ---------------------------------------------------------------------------------
  # logの取り直しと比率の作成
  df <- df %>% 
    mutate(
      log_area_of_site = log(area_of_site),
      log_area_of_building = log(area_of_building),
      area_ratio = area_of_building / area_of_site
    )
  
  df <- df %>% 
    mutate(
      months_since_opening_divid_population_total_15m4w_circle_10km = months_since_opening / population_total_15m4w_circle_10km,
      months_since_opening_divid_general_household_15m4w_circle_10km = months_since_opening / general_household_15m4w_circle_10km
    )
  
  # Target Encording用, 前処理
  colsToUse <- c('shop_code', 'area_of_site', 'area_of_building', 'building_floor_2F', 'building_floor_other', 'sales_floor_1F',
                 'sales_floor_2F','building_pos_connecting_road', 'building_pos_SC', 'building_pos_setback', 'get_in_num')
  
  df <- df %>% 
    mutate(
      shop_form_for_TE = if_else(is_shop_form_normal == 1L, 3L, 0L),
      shop_form_for_TE = if_else(is_shop_form_reuse == 1L, 2L, shop_form_for_TE),
      shop_form_for_TE = if_else(is_shop_form_SSS == 1L, 1L, shop_form_for_TE),
    ) %>% dplyr::select(-starts_with('is_shop_form_'))
  
  dataFolder <- "sub_file"
  filePaths <- list.files(dataFolder, recursive = TRUE, full.names = TRUE)
  
  dataSource <- 'shop_form'
  filesToRead <- filePaths[which(grepl(dataSource, filePaths))]
  
  TE_table <- readRDS(filesToRead)
  
  df <- df %>% left_join(TE_table, by = 'shop_form_for_TE') %>% dplyr::select(-shop_form_for_TE)
  
  df_cluster <- df %>% dplyr::select(one_of(colsToUse)) %>% 
    mutate_all( ~ as.numeric(.)) %>% 
    mutate(area_of_site = if_else(is.na(area_of_site), area_of_building, area_of_site))
  
  dataFolder <- "models"
  filePaths2 <- list.files(dataFolder, recursive = TRUE, full.names = TRUE, pattern = 'rds')
  
  dataSource <- "preProcess"
  filesToRead <- filePaths2[which(grepl(dataSource, filePaths2))]
  
  preProc <- readRDS(filesToRead)
  
  df_cluster <- predict(preProc, df_cluster)
  dfForPred <- df_cluster %>% dplyr::select(-shop_code)
  
  for (dataSource in 7:9){
    filesToRead <- filePaths2[which(grepl(dataSource, filePaths2))]
    kmodel　<- readRDS(filesToRead)
    filesToRead <- filePaths[which(grepl(dataSource, filePaths))]
    sub_table <- readRDS(filesToRead)
    new_col_name <- rlang::sym(paste0('cluster_', dataSource))
    
    df_cluster <- df_cluster %>% 
      mutate(!!new_col_name := predict.kmeans(kmodel, dfForPred)) %>% 
      left_join(sub_table, by = paste0('cluster_', dataSource))
      
  }
  
  df_cluster <- df_cluster %>% dplyr::select(shop_code, starts_with('TS_'))
  
  df <- df %>% left_join(df_cluster, by = 'shop_code')
  
  return(df)
  
}
