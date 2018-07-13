if (!"mylib" %in% ls()) source("../auxiliary/mylib.R")
mylib(c("data.table", "stringr", "magrittr", "plyr", "DiagrammeR", "DiagrammeRsvg", "rsvg"))
options(stringsAsFactors = F)

# parameters
# gen.type <- "csp_as_edge" # "csp_as_node" or "csp_as_edge"
# viz.label <- "label_as_csp_name" # "label_as_id" or "label_as_csp_name"
remove.pattern1 <- "\\[|\\]|\\)|,|;|\\'" # remove for csp name
remove.pattern2 <- "\\(|#|tmpl|@"  # remove for temp table name
remove.pattern3 <- "^'+$" # convert data source to Python

# source for parsing
sql.ls <- c("DsDWSP_script.sql", "DsDSSP_script.sql", "CRMTempDBSP_script.sql",
            "CRMDBSP_script.sql", "CRMDWSP_script.sql", "CRMDSSP_script.sql")
# target.sql <- sql.ls[1]

for (target.sql in sql.ls) {
    # source for parsing
    sql.texts <- readLines(file.path("data/raw_input/", target.sql))
    
    sql.texts %<>% trimws() %>% "["(-grep("^-", .)) #%>% head()
    
    sp.line.id <- grep("StoredProcedure", sql.texts)
    # egdes = csp_name
    # ex. "/****** Object:  StoredProcedure [dbo].[******]    Script Date: 2018/7/4 下午 03:22:36 ******/" 
    # into: dbo.trans_ShopIdFbMapping
    edges <- sql.texts[sp.line.id] %>% strsplit(., " +") %>% lapply(., "[", 4) %>% 
        unlist() %>% gsub(remove.pattern1, "", .)   # remove '[ ] ,'
    
    # gen.type <- "csp_as_node"
    # viz.label <- "label_as_id"
    # generate the inputs / outputs of the stored procefures
    csp.list <- lapply(1:length(sp.line.id), function(i) {
        print(sprintf("%s csp processing", i))
        if (i==length(sp.line.id)) { # last line id then grab the last line
            i.end <- length(sql.texts)
            print(paste(target.sql, sprintf("end (%s)", i.end), collapse=" "))
        } else { # befor next line id
            i.end <- (sp.line.id[(i + 1)] - 1)
        }
        code.snippet <- sql.texts[sp.line.id[i]:i.end]
        code.snippet.connected <- paste(code.snippet, collapse = " ")
        for (keyword in c("INTO |[Ii]nto ", "SELECT |[Ss]elect ", "FROM |[Ff]rom ",
                          "JOIN |[Jj]oin ", "WHERE |[Ww]here ", "ORDER BY |[Oo]rder [Bb]y ",
                          "GROUP BY |[Gg]roup by ", "MERGE |[Mm]erge ")) { # DELETE
            code.snippet.connected <- lapply(code.snippet.connected, function(input) {
                l1 <- length(input)
                output <- strsplit(input, keyword) %>% unlist()
                l2 <- length(output)
                # get formatted key word. ex. "INTO |[Ii]nto " --> keep only "INTO"
                keyword.rep <- strsplit(keyword, " \\|") %>% unlist() %>% "["(1)
                # if need to be cut
                if (l2 > l1) output <- c(output[1], paste(keyword.rep, output[-1]))
                output
            }) %>% unlist()
            # stop()
        }
        # nth csp processed
        # "edge": name of store procedure
        target <- strsplit(code.snippet.connected[1], " +") %>% unlist()
        edge <- target[grep("StoredProcedure", target) + 1] %>% 
            gsub(remove.pattern1, "", .)    # remove '[ ] ,'
        # get "from" table
        from <- code.snippet.connected[grep("FROM |JOIN ", code.snippet.connected)] %>% 
            strsplit(., " +") %>% lapply(., "[", 2) %>% unlist() %>% 
            gsub(remove.pattern3, 'Python', .) %>% 
            gsub(remove.pattern1, "", .)
        if(is.null(from)) from <- NA
        if(sum(grepl(remove.pattern2, from)>0)) from <- from[-grep(remove.pattern2, from)]
        # get "to" table
        to <- code.snippet.connected[grep("INTO |MERGE ", code.snippet.connected)] %>% 
            strsplit(., " +") %>% lapply(., "[", 2) %>% unlist() %>% 
            gsub(remove.pattern3, 'Python', .) %>% 
            gsub(remove.pattern1, "", .)
        if(is.null(to)) to <- NA
        if(sum(grepl(remove.pattern2, to)>0)) to <- to[-grep(remove.pattern2, to)]
        expand.grid(i=i, from = from , to = to, rel = edge, stringsAsFactors = F)
        # if (gen.type=="csp_as_node") {
        #     rbind(expand.grid(i = i, from = from, to = edge, stringsAsFactors = F), 
        #           expand.grid(i = i, from = edge, to = to, stringsAsFactors = F))
        # } else { # csp_as_edge
        #     expand.grid(i=i, from = from , to = to, rel = edge, stringsAsFactors = F)
        # }
    })
    # structured csp, input, output (nodes, edges)
    df <- ldply(csp.list)
    df <- unique(df)
    
    dt <- data.table(node=c(df$rel, df$from, df$to), 
                     type=c(rep(c("csp", "tb", "tb"), each=nrow(df))))
    dt <- dt[, .(n=.N), by = .(node, type)]
    setnames(dt, c("node", "n"), c("label", "value"))
    
    node.list <- dt
    setorder(node.list, -type, -value, label)
    
    for (viz.label in c("label_as_id", "label_as_name")) {
        # node data frame
        if(viz.label=="label_as_id") {
            node.label <- TRUE
        } else {
            node.label <- node.list$label
        }
        ndf <- create_node_df(
            n = nrow(node.list),
            type = node.list$type,
            label = node.label,
            value = node.list$value,
            name = node.list$label
        )
        for (gen.type in c( "csp_as_edge", "csp_as_node")) { # must in this order, because df is not reusable.
            print(sprintf("%s %s", viz.label, gen.type))
            if (gen.type=="csp_as_node") {
                df1 <- df[, c("i", "from", "rel")]
                setnames(df1, "rel", "to")
                df1$rel <- "input"
                df2 <- df[, c("i", "rel", "to")]
                setnames(df2, "rel", "from")
                df2$rel <- "output"
                df1 <- rbind(df1, df2) 
                df1 <- unique(df1)
            }
            # # below if only for DsDW currently
            # if (sum(df$from %in% c("DataServiceDW.dbo.DimDate", "DataServiceDW.dbo.DimShop")) >0) 
            #     df <- df[!df$from %in% c("DataServiceDW.dbo.DimDate", "DataServiceDW.dbo.DimShop"),]
            
            source("gen.graph.of.sql.R")
        }
        
        if (viz.label=="label_as_id") {
            write.csv(ndf, file.path("data/output", 
                                     sprintf("%s_%s_%s_node.csv", target.sql.name, 
                                             gen.type.name, viz.label.name)))
            write.csv(df, file.path("data/output", 
                                     sprintf("%s_%s_%s_edge.csv", target.sql.name, 
                                             gen.type.name, viz.label.name)))
        }
        # stop()
    }
    
}
file.remove("temp.gv")
