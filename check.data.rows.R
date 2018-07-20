if (!"mylib" %in% ls()) source("../auxiliary/mylib.R")
mylib(c("magrittr", "dplyr", "data.table", "plyr"))
# dir.ls <- list.dirs("data/raw_input")[list.dirs("data/raw_input") %>% grep("to_91APP", .)]
shop.ids <- c(1194, 38115, 38356)
df <- NULL
# shop.id <- 1194
for (shop.id in shop.ids) {
    fls <- list.files(sprintf("data/raw_input/%s/to_91APP", shop.id), 
                      pattern = "CrmOrderSlave.*.txt", full.names = T)
    df1 <- lapply(fls, function(fnm) {
        date.id <- strsplit(basename(fnm), "_") %>% "[["(1) %>% "["(4) %>% substr(., 1, 8)
        nrow <- fread(fnm) %>% nrow()
        data.frame(ShopId = shop.id, DateId = date.id, NumberOfRows = nrow)
    }) %>% ldply()
    df <- rbind(df, df1)
}
dt <- data.table(df)
dt <- dt[, .(NumberOfRows=sum(NumberOfRows)), by = .(ShopId, DateId)]
write.csv(dt, "data/processed/nrows.csv", row.names = F)
