library(shinylogs)

untar("8a36736625a2437bb084ba65b32a8c14.tar")

logs <- read_json_logs("logs")

logs$inputs
