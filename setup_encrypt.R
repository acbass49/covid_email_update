# Title : setup_encrypt
# Author : Alex Bass
# Desc : Setup of encrypted files

source("encrypt_functions.R")

(pw_name <- gargle:::secret_pw_name("covid_email_update"))
(pw <- gargle:::secret_pw_gen())

# Save password in .renvironment

# obtain service account token

secret_write("encrypted_file.json", "weeklyemail-777afdd0a87c.json")

# removed secret file from directory or add to .gitignore
