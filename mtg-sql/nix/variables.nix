##################################################
{ 
}:

##################################################
# Environment Variables ##########################
##################################################
{

 PGDATA = (builtins.toString ~/.cache/postgresql/pgdata);

 LC_ALL      = "C.UTF-8";
 LC_MESSAGES = "C.UTF-8";

#PGDATA = ../pg;
#PGDATA = ~/.cache/postgresql/pgdata;

}
##################################################