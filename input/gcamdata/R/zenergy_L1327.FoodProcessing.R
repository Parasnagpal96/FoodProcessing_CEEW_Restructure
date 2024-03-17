# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_L1327.FoodProcessing
#'
#' Sets up input, output, and IO coefficients for FoodProcessing and subtracts input energy from industry energy use
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L1327.in_EJ_R_FoodProcessing_Yh}, \code{L1327.IO_GJkg_R_FoodProcessing_F_Yh},\code{L1327.out_Mt_R_FoodProcessing_Yh}, \code{L1327.in_EJ_R_indenergy_F_Yh}. The corresponding file in the
#' @details This chunk generates input, Energy inputs are then subtracted from industrial energy use, feedstock and any resulting negative values
#' are dealt with by moving their accounting to the FoodProcessing sector.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows filter group_by left_join mutate select semi_join summarise summarise_all
#' @importFrom tidyr gather spread
#' @author Paras Nagpal Feb,2023
module_energy_L1327.FoodProcessing <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "energy/A_regions",
             FILE = "energy/mappings/enduse_fuel_aggregation",
             "L1326.in_EJ_R_indenergy_F_Yh",
             FILE = "energy/FoodProcessing/FoodProcessing_prod_USGS",
             FILE = "energy/FoodProcessing/FoodProcessing_prod_region_IAA",
             FILE = "energy/FoodProcessing/FoodProcessing_energy_region_IAA",
             FILE = "energy/FoodProcessing/IAA_ctry_region",
             FILE = "common/iso_GCAM_regID"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L1327.in_EJ_R_FoodProcessing_Yh",
             "L1327.IO_GJkg_R_FoodProcessing_F_Yh",
             "L1327.in_EJ_R_indenergy_F_Yh",
             "L1327.out_Mt_R_FoodProcessing_Yh"))
  } else if(command == driver.MAKE) {

    # Silence global variable package check
    raw <- subsector <- minicam.energy.input <- Country <- sector <-
    share <- value <- FoodProcessing <- year <- value.y <- value.x <- iso <- data_type <-
      flow <- var <- region <- IAA_region <- region_total <- value_region <- GCAM_region_ID <-
      fuel <- en <- industry <- output <- input <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    A_regions <- get_data(all_data, "energy/A_regions")
	  enduse_fuel_aggregation <- get_data(all_data, "energy/mappings/enduse_fuel_aggregation")
    L1326.in_EJ_R_indenergy_F_Yh <- get_data(all_data, "L1326.in_EJ_R_indenergy_F_Yh", strip_attributes = TRUE)
    FoodProcessing_prod_country <- get_data(all_data, "energy/FoodProcessing/FoodProcessing_prod_USGS")
    FoodProcessing_prod_region <- get_data(all_data, "energy/FoodProcessing/FoodProcessing_prod_region_IAA",)
    FoodProcessing_energy_region <- get_data(all_data, "energy/FoodProcessing/FoodProcessing_energy_region_IAA")
    IAA_ctry_region <- get_data(all_data, "energy/FoodProcessing/IAA_ctry_region")
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")


    # ===================================================
    # 2. Perform computations

    # Set constants used for this chunk
    # ---------------------------------
    # Determine historical years not available in data set (additional years) to copy values from final available year (final_CO2_year)

    # ===================================================

    # First downscale IAA production and energy data to countries, using country-level USGS data

    # country to IAA region mapping varies by Foodprocessing data category. Create full mapping tables
    IAA_ctry_region_full <- IAA_ctry_region %>%
      gather(-iso, key = "data_type", value = "region") %>%
      filter(grepl("FoodProcessing", data_type)) %>%
      mutate(flow = "FoodProcessing",
             var = if_else(grepl("_prod", data_type), "production", "energy"),
             IAA_region = if_else(grepl("FoodProcessing", data_type), region, "FoodProcessing")) %>%
      select(iso, flow, var, IAA_region)

    # Calculate country shares within each IAA region for each data type
    ctry_shares <- FoodProcessing_prod_country %>%
      left_join(IAA_ctry_region_full, by = "iso") %>%
      group_by(year, flow, var, IAA_region) %>%
      mutate(region_total = sum(value),
             share = value / region_total) %>%
      ungroup %>%
      select(iso, year, flow, var, IAA_region, share)


    # Country level data only goes back to 1990
    # for earlier years, use 1990 country shares within IAA regions to downscale
    ctry_shares %>% filter(year == min(year)) %>%
      select(-year) %>%
      repeat_add_columns(tibble(year = c(min(HISTORICAL_YEARS):(min(ctry_shares$year)-1)))) %>%
      bind_rows(ctry_shares) ->
      ctry_shares

    # Downscale regional FoodProcessing and FoodProcessing_enuse production
    L1327.out_Mt_ctry_FoodProcessing_Yh <- FoodProcessing_prod_region %>%
      mutate(var = "production",
             # Convert to Mt
             value = value  * CONV_KT_MT,
             unit = "Mt") %>%
      rename(value_region = value) %>%
      left_join(ctry_shares, by = c("year", "flow", "var", "IAA_region")) %>%
      mutate(value = value_region * share) %>%
      na.omit %>%
      select(iso, year, flow, value)

    # Aggregate production to GCAM regions
    L1327.out_Mt_R_FoodProcessing_Yh <- L1327.out_Mt_ctry_FoodProcessing_Yh %>%
      left_join_error_no_match(iso_GCAM_regID, by = "iso") %>%
      group_by(GCAM_region_ID, year, flow) %>%
      summarise(value = sum(value)) %>%
      ungroup %>%
      select(GCAM_region_ID, year, sector=flow, value)

    L1327.in_EJ_ctry_FoodProcessing_Yh <- FoodProcessing_energy_region %>%
      mutate(var = "energy") %>%
      rename(value_region = value) %>%
      left_join(ctry_shares, by = c("year", "flow", "var", "IAA_region")) %>%
      mutate(value = value_region * share) %>%
      na.omit %>%
      select(iso, year, flow, fuel, value)
#####################################################################
    # Aggregate to GCAM regions and combine FoodProcessing fuel use
    L1327.in_EJ_R_FoodProcessing_Yh <- L1327.in_EJ_ctry_FoodProcessing_Yh %>%
      left_join_error_no_match(iso_GCAM_regID, by = "iso") %>%
     group_by(GCAM_region_ID, year, fuel) %>%
      summarise(value = sum(value)) %>%
      mutate(sector = "FoodProcessing") %>%
      ungroup %>%
      select(GCAM_region_ID, year, sector, fuel, value)

    # check whether there are regions/years with energy use but no production
    L1327.in_EJ_R_FoodProcessing_Yh %>%
      group_by(GCAM_region_ID, year) %>%
      summarise(en = sum(value)) %>%
      filter(en > 0) %>%
      ungroup ->
      L1327.in_EJ_R_FoodProcessing_Yh_total

    L1327.out_Mt_R_FoodProcessing_Yh %>%
      group_by(GCAM_region_ID, year) %>%
      summarise(prod = sum(value)) %>%
      filter(prod > 0) %>%
      ungroup ->
      L1327.out_Mt_R_FoodProcessing_Yh_total

    L1327.in_EJ_R_FoodProcessing_Yh_total %>%
      left_join(L1327.out_Mt_R_FoodProcessing_Yh_total, by = c("GCAM_region_ID", "year")) %>%
      filter(is.na(prod)) %>%
      select(GCAM_region_ID, year) ->
      rm_prod

    # Remove energy use from regions with no FoodProcessing production
    L1327.in_EJ_R_FoodProcessing_Yh %>%
      anti_join(rm_prod, by = c("GCAM_region_ID", "year")) ->
      L1327.in_EJ_R_FoodProcessing_Yh


    #Mapping the fuel used in FoodProcessing sector
    L1327.in_EJ_R_FoodProcessing_Yh %>%
      left_join(select(enduse_fuel_aggregation, fuel, industry), by = "fuel") %>%
      select(-fuel, fuel = industry) %>%
      na.omit() %>%
      group_by(GCAM_region_ID, year, sector, fuel) %>%
      summarise(value = sum(value)) %>%
      ungroup()->
      L1327.in_EJ_R_FoodProcessing_Yh

	  #Calculate the remaining industrial energy use and feedstock
    L1326.in_EJ_R_indenergy_F_Yh %>%
      complete(GCAM_region_ID, nesting(sector, fuel, year), fill = list(value = 0)) %>%
      rename(raw = value) %>%
      left_join(L1327.in_EJ_R_FoodProcessing_Yh %>%
                  group_by(GCAM_region_ID, year, fuel) %>%
                  summarise(value = sum(value)), by = c("GCAM_region_ID", "year", "fuel")) %>%
      ungroup() %>%
      mutate(value = replace_na(value, 0),
             value = raw - value , raw = NULL) ->
      L1327.in_EJ_R_indenergy_F_Yh_tmp


    L1327.in_EJ_R_indenergy_F_Yh_tmp %>%
      filter(value < 0) %>%
      mutate(value = if_else(value > 0 , value, 0)) ->
      indeergy_tmp

    #Adjust negative energy use
    L1327.in_EJ_R_FoodProcessing_Yh %>%
      left_join(indeergy_tmp %>% select(-sector), by = c("GCAM_region_ID", "fuel", "year"))  %>%
      mutate(value.y = replace_na(value.y, -1) ,value = if_else(value.y < 0 , value.x, 0)) %>%
      select(GCAM_region_ID, fuel, year, sector, value) ->
      L1327.in_EJ_R_FoodProcessing_Yh_recal


    #Recalculate
    L1326.in_EJ_R_indenergy_F_Yh %>%
      rename(raw = value) %>%
      left_join(L1327.in_EJ_R_FoodProcessing_Yh_recal %>%
                  group_by(GCAM_region_ID, year, fuel) %>%
                  summarise(value = sum(value)), by = c("GCAM_region_ID", "year", "fuel")) %>%
      ungroup() %>%
      mutate(value = replace_na(value, 0),
             value = raw - value, raw = NULL) ->
      L1327.in_EJ_R_indenergy_F_Yh


    L1327.in_EJ_R_FoodProcessing_Yh_recal %>%
      mutate(sector = if_else(fuel == "electricity", "FoodProcessing", "FoodProcessing_enuse")) ->
      L1327.in_EJ_R_FoodProcessing_Yh

    #Calculate coefficients
    L1327.in_EJ_R_FoodProcessing_Yh %>%
      rename(input = value) %>%
      left_join(L1327.out_Mt_R_FoodProcessing_Yh %>% rename(output = value), by = c("GCAM_region_ID", "year", "sector")) %>%
      mutate(value = if_else(output > 0, input / output, 0)) ->
      L1327.IO_GJkg_R_FoodProcessing_F_Yh_tmp
    #It's not the final coefficients.Need scale


    L1327.IO_GJkg_R_FoodProcessing_F_Yh_tmp %>%
      filter(fuel != "electricity") %>%
      left_join(L1327.IO_GJkg_R_FoodProcessing_F_Yh_tmp %>% filter(fuel != "electricity", value > 0) %>%
                  group_by(GCAM_region_ID, sector, year) %>% summarise(sum = sum(output)), by = c("GCAM_region_ID", "year", "sector")) %>%
      mutate(value = sum / output * value, sum = NULL) %>%
      bind_rows(L1327.IO_GJkg_R_FoodProcessing_F_Yh_tmp %>% filter(fuel == "electricity")) %>%
      mutate(value = replace_na(value, 0), input = NULL, output =NULL) ->
      L1327.IO_GJkg_R_FoodProcessing_F_Yh


    # =======================================================
    # Produce outputs
    L1327.in_EJ_R_FoodProcessing_Yh %>%
      add_title("Historical input energy use for the FoodProcessing sector") %>%
      add_units("EJ") %>%
      add_comments("Obtained from World FoodProcessing association and then aggregated to GCAM regions") %>%
      add_legacy_name("L1327.in_EJ_R_FoodProcessing_Yh") %>%
      add_precursors("energy/A_regions", "energy/FoodProcessing/FoodProcessing_prod_USGS", "energy/FoodProcessing/FoodProcessing_energy_region_IAA",
                     "energy/FoodProcessing/IAA_ctry_region", "common/iso_GCAM_regID", "energy/mappings/enduse_fuel_aggregation") ->
      L1327.in_EJ_R_FoodProcessing_Yh

    L1327.out_Mt_R_FoodProcessing_Yh %>%
      add_title("Historical FoodProcessing production by region, fuel, and year") %>%
      add_units("Mt") %>%
      add_comments("Regional outputs are from World FoodProcessing association, downscaled using data from USGS Mineral Yearbooks, and then aggregated to GCAM regions") %>%
      add_legacy_name("L1327.out_Mt_R_FoodProcessing_Yh") %>%
      add_precursors( "energy/A_regions", "energy/FoodProcessing/FoodProcessing_energy_region_IAA", "energy/FoodProcessing/FoodProcessing_prod_USGS",
                      "energy/FoodProcessing/FoodProcessing_prod_region_IAA", "energy/FoodProcessing/IAA_ctry_region", "common/iso_GCAM_regID") ->
      L1327.out_Mt_R_FoodProcessing_Yh


    L1327.IO_GJkg_R_FoodProcessing_F_Yh%>%
      add_title("Input-output coefficients for FoodProcessing production") %>%
      add_units("EJ/Mt") %>%
      add_comments("Calculated by input/output") %>%
      add_legacy_name("L1327.IO_GJkg_R_FoodProcessing_F_Yh") %>%
      add_precursors("energy/A_regions", "energy/FoodProcessing/FoodProcessing_prod_region_IAA", "energy/FoodProcessing/FoodProcessing_prod_USGS",
                     "energy/FoodProcessing/FoodProcessing_energy_region_IAA", "energy/FoodProcessing/IAA_ctry_region", "common/iso_GCAM_regID",
                     "energy/mappings/enduse_fuel_aggregation") ->
      L1327.IO_GJkg_R_FoodProcessing_F_Yh

    L1327.in_EJ_R_indenergy_F_Yh %>%
      add_title("Adjusted historical input energy balances for industrial energy use") %>%
      add_units("Exajoules") %>%
      add_comments("Subtracted FoodProcessing energy use from industrial energy use values in L1326.in_EJ_R_indenergy_F_Yh") %>%
      add_comments("To determine adjusted input energy for industrial energy use") %>%
      add_legacy_name("L1327.in_EJ_R_indenergy_F_Yh") %>%
      add_precursors("L1326.in_EJ_R_indenergy_F_Yh", "energy/FoodProcessing/FoodProcessing_prod_region_IAA", "energy/FoodProcessing/FoodProcessing_energy_region_IAA",
                     "energy/FoodProcessing/IAA_ctry_region", "common/iso_GCAM_regID","energy/mappings/enduse_fuel_aggregation") ->
      L1327.in_EJ_R_indenergy_F_Yh


    return_data(L1327.in_EJ_R_FoodProcessing_Yh, L1327.IO_GJkg_R_FoodProcessing_F_Yh, L1327.out_Mt_R_FoodProcessing_Yh, L1327.in_EJ_R_indenergy_F_Yh)

  } else {
    stop("Unknown command")
  }
}

