source_plus = function(file_path) {
  # Set the file path as a global variable
  assign("SOURCE_SCRIPT_PATH", file_path, envir = .GlobalEnv)
  
  # Source the file at the given path
  source(file_path)
}