#Sys.setenv(RSTUDIO_PANDOC="/usr/lib/rstudio-server/bin/pandoc")

setwd("/home/madhu/workspace/Demandprediction/workingversion")
rmarkdown::render("DP_final_report_ops_Berlin.Rmd")
file.copy("DP_final_report_ops_Berlin.html","/home/madhu/workspace/shinyserver/DemandPred_html/Berlin",overwrite=TRUE)
rmarkdown::render("DP_final_report_ops_London.Rmd")
file.copy("DP_final_report_ops_London.html","/home/madhu/workspace/shinyserver/DemandPred_html/London",overwrite=TRUE)
rmarkdown::render("DP_final_report_ops_Paris.Rmd")
file.copy("DP_final_report_ops_Paris.html","/home/madhu/workspace/shinyserver/DemandPred_html/Paris",overwrite=TRUE)


 rmarkdown::render("Detailanalysisreport.Rmd")
 fname_b<-paste0("/home/madhu/workspace/shinyserver/dp_detailanalysis/Detailanalysisreport-",Sys.Date(),".html")
 file.rename("Detailanalysisreport.html",fname_b)
 file.rename(paste("df_prediction_",Sys.Date(),".dat",sep=""),paste("data_output/df_prediction_",Sys.Date(),".dat",sep=""))
 
 rmarkdown::render("backtesting.Rmd")
 file.rename("backtesting.html","/home/madhu/workspace/shinyserver/dp_detailanalysis/backtesting.html")
