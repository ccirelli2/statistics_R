install.packages('logger')
library(logger)
log_threshold(DEBUG)
log_layout(layout_glue_colors)
log_threshold(TRACE)


log_info('Info')
log_error('Error')
log_warn(('Warn'))
