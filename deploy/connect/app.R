# Posit Connect entry point. Restore/install the tested package before launching.
options(shiny.maxRequestSize = 100 * 1024^2)
MetLipAutomation::load_MetLip_FrontEnd()
