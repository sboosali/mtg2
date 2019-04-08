##################################################
{ systemPackages
, postgresqlPackages
}:

##################################################
let
#------------------------------------------------#

systemPrograms = with systemPackages; [

  postgresql

];

#------------------------------------------------#

sqlPackages = with postgresqlPackages; [

  pg_similarity

];

#------------------------------------------------#
in
##################################################

systemPrograms ++ sqlPackages

##################################################
## Notes #########################################
##################################################

# the « postgresql » package provides these programs:
#
# .initdb-wrapped
# clusterdb
# createdb
# createlang
# createuser
# dropdb
# droplang
# dropuser
# ecpg
# initdb
# oid2name
# pg_archivecleanup
# pg_basebackup
# pg_config
# pg_controldata
# pg_ctl
# pg_dump
# pg_dumpall
# pg_isready
# pg_receivexlog
# pg_recvlogical
# pg_resetxlog
# pg_restore
# pg_rewind
# pg_standby
# pg_test_fsync
# pg_test_timing
# pg_upgrade
# pg_xlogdump
# pgbench
# postgres
# postmaster
# psql
# reindexdb
# vacuumdb
# vacuumlo

##################################################