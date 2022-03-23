# # Set Environment Variables
# 
# Sys.unsetenv(c("USER", "AWS_PROFILE", "AWS_DEFAULT_REGION", "AWS_REGION", "AWS_ACCESS_KEY_ID", "AWS_ACCESS_KEY", "AWS_SECRET_ACCESS_KEY"))
# 
# Sys.setenv(AWS_PROFILE        = "038640439615",
#            AWS_DEFAULT_REGION = "us-east-2",
#            AWS_REGION         = "us-east-2")
# 
# svc <- paws::secretsmanager()
# 
# see <- svc$get_secret_value(SecretId = "system_narratives_1AOdf2WRb7")
# 
# see <- jsonlite::fromJSON(see$SecretString)
# 
# Sys.setenv(AWS_ACCESS_KEY_ID     = see$aws_access_key,
#            AWS_SECRET_ACCESS_KEY = see$aws_secret_access_key)