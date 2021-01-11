require(foreign)
require(readstata13)
require(openxlsx)
require(tidyverse)
require(DT)

map(list.files('UI'), function(file) source(file.path("UI", file)))
