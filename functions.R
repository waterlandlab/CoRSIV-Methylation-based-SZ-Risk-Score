marginal_plot = function(x, y, group = NULL, data = NULL, lm_show = FALSE, lm_formula = y ~ x, bw = "nrd0", adjust = 1, alpha = 1, plot_legend = T, ...){
  require(scales)
  ###############
  # Plots a scatterplot with marginal probability density functions for x and y. 
  # Data may be grouped or ungrouped. 
  # For each group, a linear fit can be plotted. It is hidden by default, but can be shown by providing lm_show = TRUE.
  # The model can be modified using the 'lm_formula' argument.
  # The 'bw' and 'adjust' argument specify the granularity used for estimating probability density functions. See ?density for more information.
  # For large datasets, opacity may be decreased by setting alpha to a value between 0 and 1.
  # Additional graphical parameters are passed to the main plot, so you can customize axis labels, titles etc.
  ###############
  moreargs = eval(substitute(list(...)))
  
  # prepare consistent df
  if(missing(group)){
    if(missing(data)){
      if(length(x) != length(y)){stop("Length of arguments not equal")}
      data = data.frame(x = as.numeric(x), y = as.numeric(y))
    } else {
      data = data.frame(x = as.numeric(data[,deparse(substitute(x))]), 
                        y = as.numeric(data[,deparse(substitute(y))]))
    }
    if(sum(!complete.cases(data)) > 0){
      warning(sprintf("Removed %i rows with missing data", sum(!complete.cases(data))))
      data = data[complete.cases(data),]
    }
    group_colors = "black"
  } else {
    if(missing(data)){
      if(length(x) != length(y) | length(x) != length(group)){stop("Length of arguments not equal")}
      data = data.frame(x = as.numeric(x), y = as.numeric(y), group = as.factor(group))
    } else {
      data = data.frame(x = as.numeric(data[,deparse(substitute(x))]), 
                        y = as.numeric(data[,deparse(substitute(y))]),
                        group = as.factor(data[,deparse(substitute(group))]))
    }
    if(sum(!complete.cases(data)) > 0){
      warning(sprintf("Removed %i rows with missing data", sum(!complete.cases(data))))
      data = data[complete.cases(data),]
    }
    data = subset(data, group %in% names(which(table(data$group) > 5)))
    data$group = droplevels(data$group)
    group_colors = c('#0C4B8E','#BF382A')#rainbow(length(unique(data$group)))
  } 
  
  # log-transform data (this is need for correct plotting of density functions)
  if(!is.null(moreargs$log)){
    if(!moreargs$log %in% c("y", "x", "yx", "xy")){
      warning("Ignoring invalid 'log' argument. Use 'y', 'x', 'yx' or 'xy.")
    } else {
      data = data[apply(data[unlist(strsplit(moreargs$log, ""))], 1, function(x) !any(x <= 0)), ]
      data[,unlist(strsplit(moreargs$log, ""))] = log10(data[,unlist(strsplit(moreargs$log, ""))])
    }
    moreargs$log = NULL # remove to prevent double logarithm when plotting
  }
  
  # Catch unwanted user inputs
  if(!is.null(moreargs$col)){moreargs$col = NULL}
  if(!is.null(moreargs$type)){moreargs$type = "p"}
  
  # get some default plotting arguments
  if(is.null(moreargs$xlim)){moreargs$xlim = range(data$x)} 
  if(is.null(moreargs$ylim)){moreargs$ylim = range(data$y)}
  if(is.null(moreargs$xlab)){moreargs$xlab = deparse(substitute(x))}
  if(is.null(moreargs$ylab)){moreargs$ylab = deparse(substitute(y))}
  if(is.null(moreargs$las)){moreargs$las = 1} 
  
  # plotting
  tryCatch(expr = {
    ifelse(!is.null(data$group), data_split <- split(data, data$group), data_split <- list(data))
    orig_par = par(no.readonly = T)
    par(mar = c(0.25,5,1,0))
    layout(matrix(1:4, nrow = 2, byrow = T), widths = c(10,3), heights = c(3,10))
    
    # upper density plot
    plot(NULL, type = "n", xlim = moreargs$xlim, ylab = "density",
         ylim = c(0, max(sapply(data_split, function(group_set) max(density(group_set$x, bw = bw)$y)))), main = NA, axes = F)
    axis(2, las = 1)
    mapply(function(group_set, group_color){lines(density(group_set$x, bw = bw, adjust = adjust), col = group_color, lwd = 2)}, data_split, group_colors)
    
    # legend
    par(mar = c(0.25,0.25,0,0))
    plot.new()
    if(!missing(group) & plot_legend){
      legend("center", levels(data$group), fill = group_colors, border = group_colors, bty = "n", title = deparse(substitute(group)), title.adj = 0.1)
    }
    
    # main plot
    par(mar = c(4,5,0,0))
    if(missing(group)){
      do.call(plot, c(list(x = quote(data$x), y = quote(data$y), col = quote(scales::alpha("black", alpha))), moreargs))
    } else {
      do.call(plot, c(list(x = quote(data$x), y = quote(data$y), col = quote(scales::alpha(group_colors[data$group], alpha))), moreargs))
    }
    axis(3, labels = F, tck = 0.01)
    axis(4, labels = F, tck = 0.01)
    box()
    
    if(lm_show == TRUE & !is.null(lm_formula)){
      mapply(function(group_set, group_color){
        lm_tmp = lm(lm_formula, data = group_set)
        x_coords = seq(min(group_set$x), max(group_set$x), length.out = 100)
        y_coords = predict(lm_tmp, newdata = data.frame(x = x_coords))
        lines(x = x_coords, y = y_coords, col = group_color, lwd = 2.5)
      }, data_split, rgb(t(ceiling(col2rgb(group_colors)*0.8)), maxColorValue = 255))
    }
    
    # right density plot
    par(mar = c(4,0.25,0,1))
    plot(NULL, type = "n", ylim = moreargs$ylim, xlim = c(0, max(sapply(data_split, function(group_set) max(density(group_set$y, bw = bw)$y)))), main = NA, axes = F, xlab = "density")
    mapply(function(group_set, group_color){lines(x = density(group_set$y, bw = bw, adjust = adjust)$y, y = density(group_set$y, bw = bw)$x, col = group_color, lwd = 2)}, data_split, group_colors)
    axis(1)
  }, finally = {
    par(orig_par)
  })
}
    

get_corr_df <- function(df_chr){
  df_chr_trans <- as.data.frame(t(df_chr))
  df_chr_trans$CG <- rownames(df_chr_trans)
  
  df_chr_trans <- merge(df_chr_trans,probe_loc,by="CG")
  
  df_chr_trans$pos <- as.numeric(df_chr_trans$pos)
  df_chr_trans <- df_chr_trans[with(df_chr_trans, order(pos)),]
  probs_in_chr <- subset(df_chr_trans,select = -c(status,chr,pos,cluster_id))
  probs <- probs_in_chr$CG
  probs_in_chr$CG <- NULL
  probs_in_chr_df <- data.frame(t(probs_in_chr))
  colnames(probs_in_chr_df) <- probs
  corr_matrix<-cor(probs_in_chr_df,use="complete.obs")
  correlation_df <- data.frame(row=rownames(corr_matrix)[row(corr_matrix)[upper.tri(corr_matrix)]],
                               col=colnames(corr_matrix)[col(corr_matrix)[upper.tri(corr_matrix)]],
                               corr_group=corr_matrix[upper.tri(corr_matrix)])
  return(correlation_df)
}


keep_only_numeric_columns <- function(dataframe){
  nums <- unlist(lapply(dataframe, is.numeric))
  df_numeric <- dataframe[,nums]
  df_numeric <- na.aggregate(df_numeric)
  df_numeric <- df_numeric[ , colSums(is.na(df_numeric)) == 0]
  return(df_numeric)
}

########### get distance from (0,0) along y=x ######


###########
randomize <- function(dat){
  
  for(col in colnames(dat)){
    
    dat[,paste(col)] <- sample(dat[,paste(col)])
  }
  return(dat)
}

############
compute_residual <- function(methy_data_case_control){
  require(data.table)
  
  mccs_data <- fread("./complete_mccs_data.csv",stringsAsFactors=FALSE,header = T,data.table=FALSE)
  #high_bcell_pairs <- mccs_data[mccs_data$bcell > 0.25 &mccs_data$CaseControl==1 & mccs_data$CancerCellType == "MBCN", ]$ID
  #mccs_data <- subset(mccs_data, !(ID %in% high_bcell_pairs))
  
  clinical_var <- mccs_data[,1:15]
  m2beta <- function(m){return (2^m/(2^m+1))}
  probe_data <- m2beta(mccs_data[,16:5272])
  
  probe_loc <- read.csv("./CoRSIV_ESS_SIV_CG_sites_clusters_hg38.csv",header = T,stringsAsFactors = F)
  probe_loc <- probe_loc[order(probe_loc$chr, probe_loc$pos),]
  probe_loc <- probe_loc[!duplicated(probe_loc$CG),]
  
  avg_prob_df <- data.frame(matrix(ncol = 0, nrow = dim(probe_data)[1]))
  for(probe_cluster_id in unique(probe_loc$cluster_id)){
    #print(probe_cluster_id)
    #probe_cluster_id <- "single14"
    probs_temp <- probe_loc[probe_loc$cluster_id==probe_cluster_id,]$CG
    if(length(intersect(colnames(probe_data),probs_temp)) > 0){
      data_prob_cluster <- probe_data[intersect(colnames(probe_data),probs_temp)]
      avg_prob_df[[probs_temp[1]]] <- apply(data_prob_cluster, 1,mean)
    }
  }
  
  mccs_data <- cbind(clinical_var,avg_prob_df)
  #mccs_data <- mccs_data[mccs_data$CancerCellType!="BC",]
  
  #########################
  cancer = "BC"
  base_model_control_data <- mccs_data[mccs_data$CancerCellType!=cancer & mccs_data$CaseControl==0,]
  mccs_data_cancer <- mccs_data[mccs_data$CancerCellType==cancer,]
  #input two dataframes with columns are CpG probes and rows are samples
  df1_orig <- base_model_control_data
  df2_orig <- mccs_data_cancer[mccs_data_cancer$CaseControl==1,]#BC Cases
  df1 <- keep_only_numeric_columns(df1_orig)
  df2 <- keep_only_numeric_columns(df2_orig)
  
  all_genome_df <- data.frame(matrix(ncol = 9, nrow = 0))
  number_of_comb <- 0
  for (i in 1:22){
    #i=1
    probs <- as.vector(as.character(probe_loc[probe_loc$chr==i,]$CG))
    selected_probs <- intersect(intersect(colnames(df1),probs),colnames(df2))
    number_of_comb <- number_of_comb + dim(combn(length(selected_probs),2))[2]
    df1_chr <- df1[selected_probs]
    df2_chr <- df2[selected_probs]
    correlaiton_df1 <- get_corr_df(df1_chr)
    correlaiton_df2 <- get_corr_df(df2_chr)
    merged_df <- merge(correlaiton_df1, correlaiton_df2, by.x=c("row", "col"), by.y=c("row", "col"))
    temp1 <- merge(merged_df,probe_loc,by.x = "row",by.y="CG")
    temp2 <- merge(temp1,probe_loc,by.x = "col",by.y="CG")
    merged_df_pos <- temp2[
      with(temp2, order(pos.x, pos.y)),
      ]
    
    merged_df_pos$diff <- merged_df_pos$corr_group.y-merged_df_pos$corr_group.x
    merged_df_pos$distance <- merged_df_pos$pos.y-merged_df_pos$pos.x
    merged_df_pos <- merged_df_pos[c('col','row','corr_group.x','corr_group.y','diff','pos.x','pos.y','distance','chr.x')]
    colnames(merged_df_pos) <- c("CG1","CG2","group1_r","group2_r",'Diff_r','position2','position1','distance','chr')
    all_genome_df <- rbind(all_genome_df,merged_df_pos)
  }
  
  cg_pairs <- all_genome_df
  colnames(cg_pairs) <-c("CG1","CG2","Control_r","Case_r","Diff_r","position2","position1","distance","chr")
  head(cg_pairs)
  dim(cg_pairs)
  
  cg_pairs <- cg_pairs[ 0.3 < abs(cg_pairs$Control_r) & abs(cg_pairs$Control_r) < 1 &
                          1000000 < cg_pairs$distance,]
  
  
  cancer_control_data <- methy_data_case_control[methy_data_case_control$CaseControl==0 ,]
  cancer_case_data <- methy_data_case_control[methy_data_case_control$CaseControl==1 ,]
  cancer_cases_controls <-  methy_data_case_control
  df_controls <- data.frame(control_individuals=as.character(cancer_cases_controls[cancer_cases_controls$CaseControl==0,]$Sample.y))
  dim(df_controls)
  df_cases <- data.frame(case_individuals=as.character(cancer_cases_controls[cancer_cases_controls$CaseControl==1,]$Sample.y))
  dim(df_cases)
  colname_vector_control <-c("individuals")
  colname_vector_case <-c("individuals")
  
  for(pair_number in 1:dim(cg_pairs)[1]){
    
    #print(pair_number)
    if(sum(c(as.character(cg_pairs[pair_number,]$CG1),as.character(cg_pairs[pair_number,]$CG2)) %in% colnames(methy_data_case_control)) < 2){
      next
    }
    
    compute_base_df <- base_model_control_data[c(as.character(cg_pairs[pair_number,]$CG1),as.character(cg_pairs[pair_number,]$CG2),"CaseControl")]
    compute_base_df$CaseControl <- rep(-1,dim(compute_base_df)[1])
    
    compute_case_df <- cancer_case_data[c(as.character(cg_pairs[pair_number,]$CG1),as.character(cg_pairs[pair_number,]$CG2),"CaseControl")]
    compute_case_df$CaseControl <- rep(1,dim(compute_case_df)[1])
    
    compute_control_df <- cancer_control_data[c(as.character(cg_pairs[pair_number,]$CG1),as.character(cg_pairs[pair_number,]$CG2),"CaseControl")]
    compute_control_df$CaseControl <- rep(0,dim(compute_control_df)[1])
    
    pair_data_df <- rbind(compute_base_df,compute_case_df,compute_control_df)
    colnames(pair_data_df) <- c("X","Y","CaseControl")
    pair_data_df$CaseControl <- as.factor(pair_data_df$CaseControl)
    
    test <- pair_data_df[pair_data_df$CaseControl==-1,]
    if(dim(na.omit(test))[1]<3){
      next
    }
    # 
    
    colname_vector_control <- c(colname_vector_control,paste(as.character(cg_pairs[pair_number,]$CG1),
                                                             as.character(cg_pairs[pair_number,]$CG2),sep = "_"))
    colname_vector_case <- c(colname_vector_case,paste(as.character(cg_pairs[pair_number,]$CG1),
                                                       as.character(cg_pairs[pair_number,]$CG2),sep = "_"))
    
    train <- pair_data_df[pair_data_df$CaseControl==-1,]
    test_controls <- pair_data_df[pair_data_df$CaseControl==0,]
    test_cases <- pair_data_df[pair_data_df$CaseControl==1,]
    lmodel <- lm(Y~X,data =train)
    
    
    pred_controls <- predict(lmodel,newdata = test_controls)
    pred_cases <- predict(lmodel,newdata = test_cases)
    
    
    resid_control <- test_controls$Y - pred_controls
    resid_cases <- test_cases$Y - pred_cases
    
    var(as.numeric(resid_control))
    var(as.numeric(resid_cases))
    
    df_controls$residual <- resid_control
    colnames(df_controls) <-colname_vector_control
    df_cases$residual <- resid_cases
    colnames(df_cases) <-colname_vector_case
    
  }
  
  #df_cases$case_individuals <- NULL
  #df_controls$control_individuals <- NULL
  df_cases$CaseControl <- rep(1,dim(df_cases)[1])
  df_controls$CaseControl <- rep(0,dim(df_controls)[1])
  df <- rbind(df_controls,df_cases)
  df$CaseControl <- as.factor(df$CaseControl)
  return(df)
}

get_top_genes <- function(probe_pairs_vector){
  
  intersect_probe_pairs <- data.frame(probe_pairs_vector)
  intersect_probe_pairs$pairs <- row.names(intersect_probe_pairs)
  
  colnames(intersect_probe_pairs) <- c("probe_pair","id")
  
  probe_pairs<- intersect_probe_pairs %>%
    separate(probe_pair, c("CG1", "CG2"), "_")
  hm450k_prob_gene <- read.csv("./annotations/hm450k_probe_ref_gene.csv")
  hm450k_prob_gene <- separate(data = hm450k_prob_gene, col = info.UCSC_RefGene_Name, into = c("left", "right"), sep = ";")
  
  
  temp1 <- merge(probe_pairs,hm450k_prob_gene,by.x ="CG1", by.y = "row.names.info.")
  temp2 <- merge(temp1,hm450k_prob_gene,by.x ="CG2", by.y = "row.names.info.")
  
  probe_gene_df <- temp2[c("CG1","CG2","left.x","left.y")]
  
  
  gene_freq <- data.frame(table(c(as.character(probe_gene_df$left.x),
                                  as.character(probe_gene_df$left.y)
  )))
  
  gene_freq <- gene_freq[order(gene_freq$Freq,decreasing = T),]
  return(list(gene_freq,probe_gene_df))
  
  
}



average_corsiv_methylation <- function(DF){

    temp_clinical <- DF[ , !grepl( "cg[0-9]" , names( DF ) ) ]
    probe_data <-DF[ , grepl( "cg[0-9]" , names( DF ) )]

    avg_prob_df <- data.frame(matrix(ncol = 0, nrow = dim(probe_data)[1]))

    for(probe_cluster_id in unique(CoRSIV_Probes$cluster_id)){
        #print(probe_cluster_id)
        #probe_cluster_id <- "single14"
        probs_temp <- CoRSIV_Probes[CoRSIV_Probes$cluster_id==probe_cluster_id,]$CG
        if(length(intersect(colnames(probe_data),probs_temp)) > 0){
            data_prob_cluster <- probe_data[intersect(colnames(probe_data),probs_temp)]
            avg_prob_df[[probs_temp[1]]] <- apply(data_prob_cluster, 1,mean)
        }
    }
    return(cbind(temp_clinical,avg_prob_df))
}
