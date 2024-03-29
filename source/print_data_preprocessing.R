# # Absolute path
# my_path <- r"{D:\Κωνσταντίνος Data\Σχολής\Διπλωματική Εργασία\Main\source}"
# setwd(my_path)
# # Relative path
# clean_data_folder <- r"{..\dataset\preprocessed_results}"
# data_file <- r"{\M6_2.RData}"
# data_path <- paste(clean_data_folder, data_file, sep="")

setwd(r"{D:\Κωνσταντίνος Data\Σχολής\Διπλωματική Εργασία\Main\dataset\preprocessed_results}")
data_file <- r"{M12_1.RData}"

load(data_file)

# Print the preprocessing comments
cat(paste(names(config_list), config_list, sep = ": ", collapse = "\n"))
cat(paste(preprocessing_comments, collapse="\n\n"))

setwd(r"{D:\Κωνσταντίνος Data\Σχολής\Διπλωματική Εργασία\Main\source}")
