# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_FoodProcessing_xml
#'
#' Construct XML data structure for \code{FoodProcessing.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{FoodProcessing.xml}. The corresponding file in the
#' original data system was \code{batch_FoodProcessing_xml.R} (energy XML).
module_energy_FoodProcessing_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2327.Supplysector_FoodProcessing",
             "L2327.FinalEnergyKeyword_FoodProcessing",
             "L2327.SubsectorLogit_FoodProcessing",
             "L2327.SubsectorShrwtFllt_FoodProcessing",
             "L2327.SubsectorInterp_FoodProcessing",
             "L2327.StubTech_FoodProcessing",
             "L2327.GlobalTechShrwt_FoodProcessing",
             "L2327.GlobalTechCoef_FoodProcessing",
             "L2327.GlobalTechCost_FoodProcessing",
             "L2327.GlobalTechTrackCapital_FoodProcessing",
			       "L2327.GlobalTechSCurve_FoodProcessing",
             "L2327.GlobalTechProfitShutdown_FoodProcessing",
             "L2327.StubTechProd_FoodProcessing",
             "L2327.StubTechCalInput_FoodProcessing",
             "L2327.StubTechCoef_FoodProcessing",
             "L2327.PerCapitaBased_FoodProcessing",
             "L2327.BaseService_FoodProcessing",
             "L2327.PriceElasticity_FoodProcessing",
             "L2327.GlobalTechCapture_FoodProcessing"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "FoodProcessing.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L2327.Supplysector_FoodProcessing <- get_data(all_data, "L2327.Supplysector_FoodProcessing")
    L2327.FinalEnergyKeyword_FoodProcessing <- get_data(all_data, "L2327.FinalEnergyKeyword_FoodProcessing")
    L2327.SubsectorLogit_FoodProcessing <- get_data(all_data, "L2327.SubsectorLogit_FoodProcessing")
    L2327.SubsectorShrwtFllt_FoodProcessing <- get_data(all_data, "L2327.SubsectorShrwtFllt_FoodProcessing")
    L2327.SubsectorInterp_FoodProcessing <- get_data(all_data, "L2327.SubsectorInterp_FoodProcessing")
    L2327.StubTech_FoodProcessing <- get_data(all_data, "L2327.StubTech_FoodProcessing")
    L2327.GlobalTechShrwt_FoodProcessing <- get_data(all_data, "L2327.GlobalTechShrwt_FoodProcessing")
    L2327.GlobalTechCoef_FoodProcessing <- get_data(all_data, "L2327.GlobalTechCoef_FoodProcessing")
    L2327.GlobalTechCost_FoodProcessing <- get_data(all_data, "L2327.GlobalTechCost_FoodProcessing")
    L2327.GlobalTechTrackCapital_FoodProcessing <- get_data(all_data, "L2327.GlobalTechTrackCapital_FoodProcessing")
	  L2327.GlobalTechSCurve_FoodProcessing <- get_data(all_data, "L2327.GlobalTechSCurve_FoodProcessing")
    L2327.GlobalTechProfitShutdown_FoodProcessing <- get_data(all_data, "L2327.GlobalTechProfitShutdown_FoodProcessing")
    L2327.GlobalTechCapture_FoodProcessing <- get_data(all_data, "L2327.GlobalTechCapture_FoodProcessing")
    L2327.StubTechProd_FoodProcessing <- get_data(all_data, "L2327.StubTechProd_FoodProcessing")
    L2327.StubTechCalInput_FoodProcessing <- get_data(all_data, "L2327.StubTechCalInput_FoodProcessing")
    L2327.StubTechCoef_FoodProcessing <- get_data(all_data, "L2327.StubTechCoef_FoodProcessing")
    L2327.PerCapitaBased_FoodProcessing <- get_data(all_data, "L2327.PerCapitaBased_FoodProcessing")
    L2327.BaseService_FoodProcessing <- get_data(all_data, "L2327.BaseService_FoodProcessing")
    L2327.PriceElasticity_FoodProcessing <- get_data(all_data, "L2327.PriceElasticity_FoodProcessing")
    # ===================================================

    # Produce outputs
    create_xml("FoodProcessing.xml") %>%
      add_logit_tables_xml(L2327.Supplysector_FoodProcessing, "Supplysector") %>%
      add_xml_data(L2327.FinalEnergyKeyword_FoodProcessing, "FinalEnergyKeyword") %>%
      add_logit_tables_xml(L2327.SubsectorLogit_FoodProcessing, "SubsectorLogit") %>%
      add_xml_data(L2327.SubsectorShrwtFllt_FoodProcessing, "SubsectorShrwtFllt") %>%
      add_xml_data(L2327.SubsectorInterp_FoodProcessing, "SubsectorInterp") %>%
      add_xml_data(L2327.StubTech_FoodProcessing, "StubTech") %>%
      add_xml_data(L2327.GlobalTechShrwt_FoodProcessing, "GlobalTechShrwt") %>%
      add_node_equiv_xml("input") %>%
      add_xml_data(L2327.GlobalTechCoef_FoodProcessing, "GlobalTechCoef") %>%
      add_xml_data(L2327.GlobalTechTrackCapital_FoodProcessing, "GlobalTechTrackCapital") %>%
      add_xml_data(L2327.GlobalTechCost_FoodProcessing, "GlobalTechCost") %>%
	    add_xml_data(L2327.GlobalTechSCurve_FoodProcessing, "GlobalTechSCurve") %>%
      add_xml_data(L2327.GlobalTechProfitShutdown_FoodProcessing, "GlobalTechProfitShutdown") %>%
      add_xml_data(L2327.GlobalTechCapture_FoodProcessing, "GlobalTechCapture") %>%
      add_xml_data(L2327.StubTechProd_FoodProcessing, "StubTechProd") %>%
      add_xml_data(L2327.StubTechCalInput_FoodProcessing, "StubTechCalInput") %>%
      add_xml_data(L2327.StubTechCoef_FoodProcessing, "StubTechCoef") %>%
      add_xml_data(L2327.PerCapitaBased_FoodProcessing, "PerCapitaBased") %>%
      add_xml_data(L2327.BaseService_FoodProcessing, "BaseService") %>%
      add_xml_data(L2327.PriceElasticity_FoodProcessing, "PriceElasticity") %>%
      add_precursors("L2327.Supplysector_FoodProcessing", "L2327.FinalEnergyKeyword_FoodProcessing", "L2327.SubsectorLogit_FoodProcessing",
                     "L2327.SubsectorShrwtFllt_FoodProcessing",
                     "L2327.SubsectorInterp_FoodProcessing","L2327.StubTechProd_FoodProcessing",
                     "L2327.StubTech_FoodProcessing","L2327.StubTechCoef_FoodProcessing",
                     "L2327.GlobalTechShrwt_FoodProcessing", "L2327.GlobalTechCoef_FoodProcessing", "L2327.GlobalTechCost_FoodProcessing",
                     "L2327.GlobalTechProfitShutdown_FoodProcessing", "L2327.GlobalTechSCurve_FoodProcessing",
                     "L2327.StubTechCalInput_FoodProcessing","L2327.GlobalTechCapture_FoodProcessing",
                     "L2327.PerCapitaBased_FoodProcessing", "L2327.BaseService_FoodProcessing",
                     "L2327.PriceElasticity_FoodProcessing", "L2327.GlobalTechTrackCapital_FoodProcessing") ->
      FoodProcessing.xml

    return_data(FoodProcessing.xml)
  } else {
    stop("Unknown command")
  }
}

