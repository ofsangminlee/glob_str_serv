# Define variables

# Function to run R script with renv activation
# Usage: $(call runr,input_script,output_file)
define runr
	(echo "renv::activate()"; cat $(1)) | R --vanilla > $(2) 2>&1
endef
# Function to run Julia with project activation
runj = julia --project=.

# Define file paths
sd = ./src/data
sdl = ./src/data/log
sp = ./src/param
spl = ./src/param/log
sm = ./src/model
sml = ./src/model/log

all: clean_all_output run_data run_param run_model check_logs

clean_all_output: clean_output_files clean_figures_tables clean_log_data clean_log_param clean_log_model
	@echo "---Deleted all outputs of the code---"

clean_output_files:
	@echo "---Deleting outputs (cleaned data, estimation/inversion result, model simulation result)---"
	@cd ./output && find ./ -type f -delete

clean_figures_tables:
	@echo "---Deleting figures and tables---"
	@cd ./doc/figures && find ./ -type f -delete
	@cd ./doc/tables && find ./ -type f -delete	
	@cd ./doc/numbers && find ./ -type f -delete	

clean_log_data:
	@echo "---Deleting printed outputs of the data-part codes---"
	@rm -f $(sdl)/*.out

clean_log_param:
	@echo "---Deleting printed outputs of the estimation and model inversion codes---"
	@rm -f $(spl)/*.out

clean_log_model:
	@echo "---Deleting printed outputs of the model simulation codes---"
	@rm -f $(sml)/*.out

check_logs:
	@echo "---Checking logs for errors and warnings---"
	@cd ./src/ && bash ./check_logs.sh

run_data:
	@echo "*** For each code, if it takes more than 5 minutes on the author's local machine (M2 Max Processor with 12-core CPU and 64GB RAM), then I denote the runtime of the code. ***"
	@echo "---Data part---"

	@echo "* Running 'get_country_industry_names.R'"
	@$(call runr,$(sd)/get_country_industry_names.R,$(sdl)/get_country_industry_names.out )

	@echo "* Running 'motivation_two_services.R'"
	@$(call runr,$(sd)/motivation_two_services.R,$(sdl)/motivation_two_services.out)

	@echo "* Running 'condense_icio_remove_tax.R' (This takes approximately 12 minutes on the author's local machine.)"
	@$(call runr,$(sd)/condense_icio_remove_tax.R,$(sdl)/condense_icio_remove_tax.out)

	@echo "* Running 'extract_pwt.R'"
	@$(call runr,$(sd)/extract_pwt.R,$(sdl)/extract_pwt.out)

	@echo "* Running 'get_price_deflators.R' (This takes approximately 13 minutes on the author's local machine.)"
	@$(call runr,$(sd)/get_price_deflators.R,$(sdl)/get_price_deflators.out)

	@echo "* Running 'get_price_level.R'"	
	@$(call runr,$(sd)/get_price_level.R,$(sdl)/get_price_level.out)

	@echo "* Running 'get_data_cons_prod_trade.R'"	
	@$(call runr,$(sd)/get_data_cons_prod_trade.R,$(sdl)/get_data_cons_prod_trade.out)

	@echo "* Running 'motivation_trade.R'"	
	@$(call runr,$(sd)/motivation_trade.R,$(sdl)/motivation_trade.out)

	@echo "* Running 'motivation_str_tran.R'"	
	@$(call runr,$(sd)/motivation_str_tran.R,$(sdl)/motivation_str_tran.out)

	@echo "* Running 'get_bea_prod.R'"	
	@$(call runr,$(sd)/get_bea_prod.R,$(sdl)/get_bea_prod.out)

run_param:
	@echo "---Model parameterization (estimation and inversion) part---"
	@echo "* Running 'preference.R' (This takes approximately 9 minutes on the author's local machine.)"
	@$(call runr,$(sp)/preference.R,$(spl)/preference.out)

	@echo "* Running 'production.R'"
	@$(call runr,$(sp)/production.R,$(spl)/production.out)

	@echo "* Running 'cal_prd_tau.R' (This takes approximately 8 minutes on the author's local machine.)"
	@$(call runr,$(sp)/cal_prd_tau.R,$(spl)/cal_prd_tau.out)

	@echo "* Running 'cal_tau_head_ries.R'"
	@$(call runr,$(sp)/cal_tau_head_ries.R,$(spl)/cal_tau_head_ries.out)

	@echo "* Running 'analyze_tau.R'"
	@$(call runr,$(sp)/analyze_tau.R,$(spl)/analyze_tau.out)

	@echo "* Running 'get_two_country.R'"
	@$(call runr,$(sp)/get_two_country.R,$(spl)/get_two_country.out)

	@echo "* Running 'prd_tau_interaction.R'"
	@$(call runr,$(sp)/prd_tau_interaction.R,$(spl)/prd_tau_interaction.out)

run_model:
	@echo "---Model simulations---"

	@echo "* Running 'inputs.jl'"
	@$(runj) $(sm)/inputs.jl > $(sml)/inputs.out 2>&1

	@echo "* Running 'run_models.jl' (This takes approximately 10 minutes on the author's local machine. This code runs baseline and most of the other model specifications.)"
	@$(runj) $(sm)/run_models.jl > $(sml)/run_models.out 2>&1

	@echo "* Running 'shapley.jl'"
	@$(runj) $(sm)/shapley.jl > $(sml)/shapley.out 2>&1

	@echo "* Running 'clean_result.jl'"
	@$(runj) $(sm)/clean_result.jl > $(sml)/clean_result.out 2>&1

	@echo "* Running 'plot_fit.R'"
	@$(call runr,$(sm)/plot_fit.R,$(sml)/plot_fit.out)

	@echo "* Running 'plot_one.R'"
	@$(call runr,$(sm)/plot_one.R,$(sml)/plot_one.out)

	@echo "* Running 'plot_67.R'"
	@$(call runr,$(sm)/plot_67.R,$(sml)/plot_67.out)

	@echo "* Running 'analyze_prd.R'"
	@$(call runr,$(sm)/analyze_prd.R,$(sml)/analyze_prd.out)

	@echo "* Running 'run_usa_row.jl'"
	@$(runj) $(sm)/run_usa_row.jl > $(sml)/run_usa_row.out 2>&1

	@echo "* Running 'plot_usa_row.R'"
	@$(call runr,$(sm)/plot_usa_row.R,$(sml)/plot_usa_row.out)
