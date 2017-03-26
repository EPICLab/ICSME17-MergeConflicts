df = data.frame(
  complex_lines = c(5, 58, 114, 224, 170),
  expertise = c(5, 46, 150, 216, 150),
  complex_files = c(8, 68, 147, 204, 90),
  num_lines = c(2, 80, 195, 180, 55),
  time_resolve = c(14, 112, 153, 100, 75),
  atomicity = c(20, 96, 153, 116, 65),
  dependencies = c(20, 112, 117, 132, 70),
  num_files = c(10, 138, 150, 104, 30),
  non_func = c(47, 126, 93, 60, 20)
)

wilcox.test(complex_lines ~ expertise, data=df)
