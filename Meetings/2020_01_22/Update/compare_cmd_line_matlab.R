

library(tibble)
library(mdiHelpR)

col_pal <- c("#FFFFFF", grDevices::colorRampPalette(RColorBrewer::brewer.pal(n = 7, name = "Blues"))(100))
breaks <- defineBreaks(col_pal, lb = 0, ub = 1)

mason <- readRDS("~/Desktop/test_mdi_priors/num_4/compare_tibble.rds")
matlab <- readRDS("./Desktop/MATLAB_output/MATLAB_MDI/compare_tibble.rds")
matlab_ci <- readRDS("./Desktop/MATLAB_output/MATLAB_consensus_1000_seeds_500_iter/compare_tibble.rds")

mason$dataset
matlab$dataset

ppi_mason <- mason$similarity_matrix[mason$dataset == "PPI"][[1]]
ppi_matlab <- matlab$similarity_matrix[matlab$dataset == "PPI"][[1]]

ph1 <- compareMatrices(ppi_matlab,
                ppi_mason,
                curr_title = paste0("PPI: MATLAB vs CMD Line"),
                col_pal = col_pal,
                col_names = F,
                save_name = "matlab_v_cmd_consesnsus_ppi.png"
)

ph1


timecourse_mason <- mason$similarity_matrix[mason$dataset == "Timecourse"][[1]]
timecourse_matlab <- matlab$similarity_matrix[matlab$dataset == "Timecourse"][[1]]

ph2 <- compareMatrices(timecourse_matlab,
                      timecourse_mason,
                      curr_title = paste0("Timecourse: MATLAB vs CMD Line"),
                      col_pal = col_pal,
                      col_names = F,
                      save_name = "matlab_v_cmd_consesnsus_timecourse.png"
)

ph2

harbison_mason <- mason$similarity_matrix[mason$dataset == "Harbison"][[1]]
harbison_matlab <- matlab$similarity_matrix[matlab$dataset == "Harbison"][[1]]

ph3 <- compareMatrices(harbison_mason,
                       harbison_matlab,
                       curr_title = paste0("Harbison: MATLAB vs CMD Line"),
                       col_pal = col_pal,
                       col_names = F,
                       save_name = "matlab_v_cmd_consesnsus_harbison.png"
)

ph3





ppi_mason <- mason$similarity_matrix[mason$dataset == "PPI"][[1]]
ppi_matlab_ci <- matlab_ci$similarity_matrix[matlab_ci$dataset == "PPI"][[1]]

ph1 <- compareMatrices(ppi_matlab_ci,
                       ppi_mason,
                       curr_title = paste0("PPI: MATLAB CI vs CMD Line"),
                       col_pal = col_pal,
                       col_names = F,
                       save_name = "matlab_ci_v_cmd_consesnsus_ppi.png"
)

ph1


timecourse_mason <- mason$similarity_matrix[mason$dataset == "Timecourse"][[1]]
timecourse_matlab_ci <- matlab_ci$similarity_matrix[matlab_ci$dataset == "Timecourse"][[1]]

ph2 <- compareMatrices(timecourse_matlab_ci,
                       timecourse_mason,
                       curr_title = paste0("Timecourse: MATLAB CI vs CMD Line"),
                       col_pal = col_pal,
                       col_names = F,
                       save_name = "matlab_ci_v_cmd_consesnsus_timecourse.png"
)

ph2

harbison_mason <- mason$similarity_matrix[mason$dataset == "Harbison"][[1]]
harbison_matlab_ci <- matlab_ci$similarity_matrix[matlab_ci$dataset == "Harbison"][[1]]

ph3 <- compareMatrices(harbison_mason,
                       harbison_matlab_ci,
                       curr_title = paste0("Harbison: MATLAB CI vs CMD Line"),
                       col_pal = col_pal,
                       col_names = F,
                       save_name = "matlab_ci_v_cmd_consesnsus_harbison.png"
)

ph3



