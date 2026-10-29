###############################
# Directories
###############################

# Data
OUT = data/output/
PROC = data/processed/
RAW = data/raw/

# MPA specific scripts
PROC_PIPA = scripts/01_processing/pipa/
PROC_GMR = scripts/01_processing/gmr/
PROC_RNP = scripts/01_processing/revilla/
PROC_PNMS = scripts/01_processing/palau/

# MPA Specific results
FIG_PIPA = results/figures/pipa_original/
FIG_GMR = results/figures/gmr/
FIG_RNP = results/figures/revilla/
FIG_PNMS = results/figures/palau/

###############################
# Define functions
###############################

all_models: \
	$(OUT)gmr/m_region_log.rds \
	$(OUT)palau/m_region_log.rds \
	$(OUT)pipa/m_region_log.rds \
	$(OUT)revilla/m_region_log.rds

all_figures: \
	$(FIG_PIPA).results_complete \
	$(FIG_GMR).results_complete \
	$(FIG_RNP).results_complete \
	$(FIG_PNMS).results_complete

# Data building ################################################################

$(PROC)pipa_rasterized.rds: \
	$(PROC_PIPA)pipa_analysis_data.R \
	$(PROC_PIPA)get_mpa_and_eez_pipa.R \
	$(RAW)rfmo/allrfmo_month_1deg_purseseine.rds \
	$(RAW)mpa/pipa.gpkg \
	$(RAW)eez/kiribati_eez.gpkg \
	$(RAW)eez/kiribati_eez_line.gpkg \
	$(RAW)eez/kiribati_eez_phoenix.gpkg \
	$(RAW)eez/kiribati_eez_gilbert.gpkg
	Rscript $(PROC_PIPA)pipa_analysis_data.R

$(PROC)gmr_rasterized.rds: \
	$(PROC_GMR)gmr_analysis_data.R \
	$(PROC_GMR)get_mpa_and_eez_gmr.R \
	$(RAW)mpa/galapagos_world_heritage.gpkg \
	$(RAW)eez/ecuador_eez_main.gpkg \
	$(RAW)eez/ecuador_eez_galapagos.gpkg \
	$(RAW)rfmo/allrfmo_month_1deg_purseseine.rds
	Rscript $(PROC_GMR)gmr_analysis_data.R

$(PROC)revilla_rasterized.rds: \
	$(PROC_RNP)revilla_analysis_data.R \
	$(PROC_RNP)get_mpa_and_eez_rev.R \
	$(RAW)mpa/revillagigedo_park.gpkg \
	$(RAW)eez/mexico_eez.gpkg \
	$(RAW)rfmo/allrfmo_month_1deg_purseseine.rds
	Rscript $(PROC_RNP)revilla_analysis_data.R

$(PROC)palau_rasterized.rds: \
	$(PROC_PNMS)palau_analysis_data.R \
	$(PROC_PNMS)get_mpa_and_eez_palau.R \
	$(RAW)mpa/pnms_2015.gpkg \
	$(RAW)eez/palau_eez.gpkg \
	$(RAW)eez/fsm_eez.gpkg \
	$(RAW)rfmo/allrfmo_month_1deg_purseseine.rds
	Rscript $(PROC_PNMS)palau_analysis_data.R

# Model outputs ################################################################

$(OUT)pipa/m_region_log.rds: \
	$(PROC)pipa_rasterized.rds \
	scripts/02_analysis/pipa_model.R
	Rscript scripts/02_analysis/pipa_model.R

$(OUT)gmr/m_region_log.rds: \
	$(PROC)gmr_rasterized.rds \
	scripts/02_analysis/gmr_model.R
	Rscript scripts/02_analysis/gmr_model.R

$(OUT)revilla/m_region_log.rds: \
	$(PROC)revilla_rasterized.rds \
	scripts/02_analysis/revilla_model.R
	Rscript scripts/02_analysis/revilla_model.R

$(OUT)palau/m_region_log.rds: \
	$(PROC)palau_rasterized.rds \
	scripts/02_analysis/palau_model.R
	Rscript scripts/02_analysis/palau_model.R

# Figures ######################################################################

$(FIG_PIPA).results_complete: \
	$(PROC)pipa_rasterized.rds \
	scripts/03_content/produce_pipa_figures.R
	Rscript scripts/03_content/produce_pipa_figures.R
	touch $(FIG_PIPA).results_complete

$(FIG_GMR).results_complete: \
	$(PROC)gmr_rasterized.rds \
	scripts/03_content/produce_gmr_figures.R
	Rscript scripts/03_content/produce_gmr_figures.R
	touch $(FIG_GMR).results_complete

$(FIG_RNP).results_complete: \
	$(PROC)revilla_rasterized.rds \
	scripts/03_content/produce_revilla_figures.R
	Rscript scripts/03_content/produce_revilla_figures.R
	touch $(FIG_RNP).results_complete

$(FIG_PNMS).results_complete: \
	$(PROC)palau_rasterized.rds \
	scripts/03_content/produce_palau_figures.R
	Rscript scripts/03_content/produce_palau_figures.R
	touch $(FIG_PNMS).results_complete
