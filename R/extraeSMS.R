

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

sm_ips<-function(index_id,variables,args=NA){
     # Extrae de IPS
     smsh<-odbcConnect('smsh','nretail','nretail')
     per<-ult.periodo(index_id)
     varos<-paste0(variables,collapse = ", ")
     if (!is.na(args)) {
          linea<-paste0("SELECT * FROM index_period_source WHERE status_id IN (6,7,8,9) AND index_id = ",index_id," AND period_id = ",per," AND ",args)
     } else {
          linea<-paste0("SELECT * FROM index_period_source wHERE status_id IN (6,7,8,9) AND index_id = ",index_id," AND period_id = ",per)
     }
     out_ips<-sqlQuery(smsh,linea)
     close(smsh)

     # Extrae de SM
     sms<-odbcConnect('sms','nretail','nretail')
     tiendas<-paste0(out_ips$source_id,collapse = ",")
     linea2<-paste0("SELECT * FROM source_master WHERE source_id IN (",tiendas,")")
     out_sm<-sqlQuery(sms,linea2)
     close(sms)

     # Une ambas tablas
     out_total<-merge(out_ips,out_sm,by="source_id",all.x=T)
     variables<-gsub(" ","",variables) # Elimina espacios en blanco
     vars<-as.character(strsplit(gsub(" ","",variables),",")[[1]]) # Extrae cada variable del input del usuario

     # Extraer variables buscadas
     found<-names(out_total) %in% vars
     corrected1<-paste0(vars,".x")
     corrected2<-paste0(vars,".y")
     found1<-which(names(out_total) %in% corrected1)
     found2<-which(names(out_total) %in% corrected2)
     found_tot<-c(found1,found2)
     found[found_tot]<-TRUE
     if(variables=="*"){
          out<-out_total
     } else {
          out<-out_total[,which(found)]
     }
     out
}

