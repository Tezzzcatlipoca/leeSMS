
#
# Programa para extraer el ultimo periodo abierto para un indice especifico
#

ult.periodo<-function(index_id) {
     smsh<-odbcConnect('smsh',uid='nretail',pwd='nretail')
     quer.per<-paste0("SELECT period_id, index_id FROM index_period_source WHERE status_id = 2 AND index_id = ",index_id)
     ind.periodos<-sqlQuery(smsh,quer.per)
     periodos<-as.integer(as.character(unique(ind.periodos$period_id)))
     period_id<-max(periodos)
     odbcClose(smsh)
     period_id

} # End of function

scan.ult.periodo<-function() {
     smsh<-odbcConnect('scan',uid='scanning',pwd='scanning')
     quer.per<-paste0("SELECT period_id, index_id FROM index_period_source WHERE index_id = 1")
     ind.periodos<-sqlQuery(smsh,quer.per)
     periodos<-as.integer(as.character(unique(ind.periodos$period_id)))
     period_id<-max(periodos)
     odbcClose(smsh)
     period_id

} # End of function

