# Title : encrypt_functions
# Author : Alex Bass
# Desc : Modified encrypting functions from sodium package

# (pw_name <- gargle:::secret_pw_name("covid_email_update"))
# (pw <- gargle:::secret_pw_gen())

# Save in .renvironment

secret_write <- function(name, input) {
  if (is.character(input)) {
    input <- readBin(input, "raw", file.size(input))
  } else if (!is.raw(input)) {
    stop('Need raw or character')
  }
  
  enc <- sodium::data_encrypt(
    msg = input,
    key = sodium::hash(charToRaw(Sys.getenv('COVID_EMAIL_UPDATE_PASSWORD'))),
    nonce = sodium::hex2bin("cb36bab652dec6ae9b1827c684a7b6d21d2ea31cd9f766ac")
  )
  attr(enc, "nonce") <- NULL
  writeBin(enc, name)
  
  invisible(name)
}

secret_read <- function(name) {
  
  path <- name
  raw <- readBin(path, "raw", file.size(path))
  
  sodium::data_decrypt(
    bin = raw,
    key = sodium::hash(charToRaw(Sys.getenv('COVID_EMAIL_UPDATE_PASSWORD'))),
    nonce = sodium::hex2bin("cb36bab652dec6ae9b1827c684a7b6d21d2ea31cd9f766ac")
  )
}
