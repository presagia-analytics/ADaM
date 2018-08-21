library(haven)

var_names <- c("addefirr", "adef", "adefirr", "ador", "adorirr", "adsl")

dir.create(file.path("..", "data"))
for (var_name in var_names) {
  read_cmd <- sprintf('%s <- read_sas(file.path("ruca-data", "%s.sas7bdat"))', 
                      var_name, var_name)
  write_cmd <- sprintf('save(%s, file = file.path("..", "data", "%s.rda"))', 
                       var_name, var_name)
  eval(parse(text = read_cmd))
  eval(parse(text = write_cmd))
}

