now = format(Sys.time(), "%y%m%d_%H%M%S")
filename = paste0("new_file_", now)
dir = "/home/biomeris/test/results"     #1) specify dir path
if(!dir.exists(dir)) dir.create(dir)    #2) create dir if not exists
file.create(file.path(dir,paste(filename)))  #3) create empty file in dir path