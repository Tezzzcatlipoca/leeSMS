

ipm<-function(index_id,variables,args=NA){
     smsh<-odbcConnect('smsh','nretail','nretail')
     varos<-paste0(variables,collapse = ", ")
     per<-ult.periodo(index_id)
     if (!is.na(args)) {
          linea<-paste0("SELECT ",varos," FROM index_period_market WHERE status_id IN (2) AND index_id = ",index_id, " AND period_id = ",per," AND ",args)
     } else {
          linea<-paste0("SELECT ",varos," FROM index_period_market wHERE status_id IN (2) AND index_id = ",index_id, " AND period_id = ",per)
     }
     out<-sqlQuery(smsh,linea)
     close(smsh)
     out
}

ipm_cur<-function(index_id,variables,args=NA){
     sms<-odbcConnect('sms','nretail','nretail')
     varos<-paste0(variables,collapse = ", ")
     if (!is.na(args)) {
          linea<-paste0("SELECT ",varos," FROM index_period_market WHERE status_id IN (2) AND index_id = ",index_id," AND ",args)
     } else {
          linea<-paste0("SELECT ",varos," FROM index_period_market wHERE status_id IN (2) AND index_id = ",index_id)
     }
     out<-sqlQuery(sms,linea)
     close(sms)
     out
}

ipm_raw<-function(index_id,variables,args=NA){
     smsh<-odbcConnect('smsh','nretail','nretail')
     varos<-paste0(variables,collapse = ", ")
     #per<-ult.periodo(index_id)
     if (!is.na(args)) {
          linea<-paste0("SELECT ",varos," FROM index_period_market WHERE status_id IN (2) AND index_id = ",index_id, " AND ",args)
     } else {
          linea<-paste0("SELECT ",varos," FROM index_period_market wHERE status_id IN (2) AND index_id = ",index_id)
     }
     out<-sqlQuery(smsh,linea)
     close(smsh)
     out
}

ipc_cur<-function(index_id,variables,args=NA){
     sms<-odbcConnect('sms','nretail','nretail')
     varos<-paste0(variables,collapse = ", ")
     if (!is.na(args)) {
          linea<-paste0("SELECT ",varos," FROM index_period_cell WHERE status_id IN (2) AND index_id = ",index_id," AND ",args)
     } else {
          linea<-paste0("SELECT ",varos," FROM index_period_cell wHERE status_id IN (2) AND index_id = ",index_id)
     }
     out<-sqlQuery(sms,linea)
     close(sms)
     out
}

ips_cur<-function(index_id,variables,args=NA){
     sms<-odbcConnect('sms','nretail','nretail')
     varos<-paste0(variables,collapse = ", ")
     if (!is.na(args)) {
          linea<-paste0("SELECT ",varos," FROM index_period_source WHERE status_id IN (6,7,8,9) AND index_id = ",index_id," AND ",args)
     } else {
          linea<-paste0("SELECT ",varos," FROM index_period_source wHERE status_id IN (6,7,8,9) AND index_id = ",index_id)
     }
     out<-sqlQuery(sms,linea)
     close(sms)
     out
}

ipc<-function(index_id,variables,args=NA){
     smsh<-odbcConnect('smsh','nretail','nretail')
     varos<-paste0(variables,collapse = ", ")
     per<-ult.periodo(index_id)
     if (!is.na(args)) {
          linea<-paste0("SELECT ",varos," FROM index_period_cell WHERE status_id IN (2) AND index_id = ",index_id, " AND period_id = ",per," AND ",args)
     } else {
          linea<-paste0("SELECT ",varos," FROM index_period_cell wHERE status_id IN (2) AND index_id = ",index_id, " AND period_id = ",per)
     }
     out<-sqlQuery(smsh,linea)
     close(smsh)
     out
}

ipc_raw<-function(index_id,variables,args=NA){
     smsh<-odbcConnect('smsh','nretail','nretail')
     varos<-paste0(variables,collapse = ", ")
     #per<-ult.periodo(index_id)
     if (!is.na(args)) {
          linea<-paste0("SELECT ",varos," FROM index_period_cell WHERE status_id IN (2) AND index_id = ",index_id, " AND ",args)
     } else {
          linea<-paste0("SELECT ",varos," FROM index_period_cell wHERE status_id IN (2) AND index_id = ",index_id)
     }
     out<-sqlQuery(smsh,linea)
     close(smsh)
     out
}

ips<-function(index_id,variables,args=NA){
     smsh<-odbcConnect('smsh','nretail','nretail')
     per<-ult.periodo(index_id)
     varos<-paste0(variables,collapse = ", ")
     if (!is.na(args)) {
          linea<-paste0("SELECT ",varos," FROM index_period_source WHERE status_id IN (6,7,8,9) AND index_id = ",index_id," AND period_id = ",per," AND ",args)
     } else {
          linea<-paste0("SELECT ",varos," FROM index_period_source wHERE status_id IN (6,7,8,9) AND index_id = ",index_id," AND period_id = ",per)
     }
     out<-sqlQuery(smsh,linea)
     close(smsh)
     out
}

ips_raw<-function(index_id,variables,args=NA){
     smsh<-odbcConnect('smsh','nretail','nretail')
     #per<-ult.periodo(index_id)
     varos<-paste0(variables,collapse = ", ")
     if (!is.na(args)) {
          linea<-paste0("SELECT ",varos," FROM index_period_source WHERE status_id IN (6,7,8,9) AND index_id = ",index_id," AND ",args)
     } else {
          linea<-paste0("SELECT ",varos," FROM index_period_source wHERE status_id IN (6,7,8,9) AND index_id = ",index_id)
     }
     out<-sqlQuery(smsh,linea)
     close(smsh)
     out
}

scan_ips<-function(variables,args=NA){
     smsh<-odbcConnect('scan','scanning','scanning')
     index_id<-1
     ind.periodos<-sqlQuery(smsh,"SELECT DISTINCT period_id FROM index_period_source WHERE index_id=1")
     periodos<-as.integer(as.character(unique(ind.periodos$period_id)))
     per<-max(periodos)
     varos<-paste0(variables,collapse = ", ")
     if (!is.na(args)) {
          linea<-paste0("SELECT ",varos," FROM index_period_source WHERE status_id IN (6,7,8,9) AND index_id = ",index_id," AND period_id = ",per," AND ",args)
     } else {
          linea<-paste0("SELECT ",varos," FROM index_period_source wHERE status_id IN (6,7,8,9) AND index_id = ",index_id," AND period_id = ",per)
     }
     out<-sqlQuery(smsh,linea)
     close(smsh)
     out
}

scan_sm<-function(variables,args=NA){
     smsh<-odbcConnect('scan','scanning','scanning')
     index_id<-1
     #ind.periodos<-sqlQuery(smsh,"SELECT DISTINCT period_id FROM index_period_source WHERE index_id=1")
     #periodos<-as.integer(as.character(unique(ind.periodos$period_id)))
     #per<-max(periodos)
     varos<-paste0(variables,collapse = ", ")
     if (!is.na(args)) {
          linea<-paste0("SELECT ",varos," FROM source_master WHERE ",args)
     } else {
          linea<-paste0("SELECT ",varos," FROM source_master")
     }
     out<-sqlQuery(smsh,linea)
     close(smsh)
     out
}

sm<-function(variables,args=NA){
     sms<-odbcConnect('sms','nretail','nretail')
     varos<-paste0(variables,collapse = ", ")
     if (!is.na(args)) {
          linea<-paste0("SELECT ",varos," FROM source_master WHERE ",args)
     } else {
          linea<-paste0("SELECT ",varos," FROM source_master")
     }
     out<-sqlQuery(sms,linea)
     close(sms)
     out
}
