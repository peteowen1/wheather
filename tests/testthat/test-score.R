test_that("score_temp_single returns 100 within ideal range", {
  # Exactly at boundaries and inside the range
  expect_equal(score_temp_single(23, 21, 25, 4, 1.5), 100)
  expect_equal(score_temp_single(21, 21, 25, 4, 1.5), 100)
  expect_equal(score_temp_single(25, 21, 25, 4, 1.5), 100)
})

test_that("score_temp_single decays outside ideal range", {
  score <- score_temp_single(30, 21, 25, 4, 1.5)
  expect_true(score < 100)
  expect_true(score >= 0)
  # Further away = lower score
  expect_true(score_temp_single(35, 21, 25, 4, 1.5) < score)
})

test_that("score_temp_single handles NA", {
  expect_true(is.na(score_temp_single(NA, 21, 25, 4, 1.5)))
})

test_that("score_rain returns 100 for zero precipitation", {
  params <- default_params()
  expect_equal(score_rain(0, NA, NA, params), 100)
})

test_that("score_rain penalises heavy rainfall", {
  params <- default_params()
  light <- score_rain(2, 0, 0, params)
  heavy <- score_rain(15, 0, 0, params)
  expect_true(light > heavy)
  expect_true(heavy < 30)
})

test_that("score_rain handles snow penalty", {
  params <- default_params()
  no_snow <- score_rain(0, 0, 0, params)
  with_snow <- score_rain(0, 0, 3, params)
  expect_true(with_snow < no_snow)
})

test_that("score_humidity returns 100 in sweet spot", {
  params <- default_params()
  expect_equal(score_humidity(50, params), 100)
  expect_equal(score_humidity(40, params), 100)
  expect_equal(score_humidity(60, params), 100)
})

test_that("score_humidity penalises muggy harder than dry", {
  params <- default_params()
  dry <- score_humidity(20, params)    # 20% below ideal_min of 40
  muggy <- score_humidity(80, params)  # 20% above ideal_max of 60
  expect_true(muggy < dry)  # asymmetric: humid_decay > dry_decay
})

test_that("score_wind returns 100 for calm conditions", {
  params <- default_params()
  expect_equal(score_wind(5, NA, params), 100)
  expect_equal(score_wind(15, NA, params), 100)  # at pleasant_max
})

test_that("score_wind returns 0 for extreme wind", {
  params <- default_params()
  expect_equal(score_wind(50, NA, params), 0)
  expect_equal(score_wind(60, NA, params), 0)
})

test_that("score_wind applies gust penalty", {
  params <- default_params()
  no_gust <- score_wind(20, NA, params)
  with_gust <- score_wind(20, 50, params)
  expect_true(with_gust < no_gust)
})

test_that("score_sky handles all-NA gracefully", {
  params <- default_params()
  expect_true(is.na(score_sky(NA, NA, NA, NA, params)))
})

test_that("weighted_total renormalises when components are NA", {
  weights <- default_weights()
  scores <- list(temp = 80, rain = 60, sky = NA, humidity = 70, wind = 90)
  result <- weighted_total(scores, weights)
  # Should only use non-NA weights (temp=0.25, rain=0.25, humidity=0.10, wind=0.20)
  expected <- (80 * 0.25 + 60 * 0.25 + 70 * 0.10 + 90 * 0.20) / (0.25 + 0.25 + 0.10 + 0.20)
  expect_equal(result, expected)
})

test_that("weighted_total returns NA when all components are NA", {
  weights <- default_weights()
  scores <- list(temp = NA, rain = NA, sky = NA, humidity = NA, wind = NA)
  expect_true(is.na(weighted_total(scores, weights)))
})

test_that("vectorized scores match scalar scores", {
  params <- default_params()
  # Test data: mix of normal, edge case, and NA values
  tm   <- c(23, 10, NA, 35)
  tmin <- c(18, 5, 12, 28)
  tmax <- c(27, 15, NA, 40)

  scalar <- vapply(seq_along(tm), \(i)
    score_temp(tm[i], tmin[i], tmax[i], params), numeric(1))
  vectorized <- score_temp_vec(tm, tmin, tmax, params)
  expect_equal(vectorized, scalar, tolerance = 1e-10)
})

test_that("vectorized rain scores match scalar", {
  params <- default_params()
  prcp <- c(0, 5, 15, NA)
  hrs  <- c(0, 3, 8, NA)
  snow <- c(0, 0, 2, NA)

  scalar <- vapply(seq_along(prcp), \(i)
    score_rain(prcp[i], hrs[i], snow[i], params), numeric(1))
  vectorized <- score_rain_vec(prcp, hrs, snow, params)
  expect_equal(vectorized, scalar, tolerance = 1e-10)
})

test_that("vectorized wind scores match scalar", {
  params <- default_params()
  wnd <- c(5, 20, 50, NA)
  gst <- c(NA, 30, 60, NA)

  scalar <- vapply(seq_along(wnd), \(i)
    score_wind(wnd[i], gst[i], params), numeric(1))
  vectorized <- score_wind_vec(wnd, gst, params)
  expect_equal(vectorized, scalar, tolerance = 1e-10)
})
