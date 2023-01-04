.libPaths("C:/Users/jcur01/AppData/Local/R/win-library/4.2/")
library(devtools)

use_r("usefulFunctions")

load_all()


dataWarehouse_function_specific("Change Requests")


exists("dataWarehouse_function_specific", where = globalenv(), inherits = FALSE)

check()

install()
