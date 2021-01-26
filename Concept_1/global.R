require(foreign)
require(readstata13)
require(openxlsx)
require(tidyverse)
require(DT)

map(list.files('UI/tabs'), function(file) source(file.path("UI/tabs", file)))
map(list.files('UI/nodes'), function(file) source(file.path("UI/nodes", file)))
map(list.files('UI/dropdowns'), function(file) source(file.path("UI/dropdowns", file)))


