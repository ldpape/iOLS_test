program define iOLS_test, eclass 
	syntax [anything] [if] [in] [aweight pweight fweight iweight] [, DELta(real 1) Reps(real 1)]
	marksample touse
	local list_var `anything'
	* Remarque : la fct gettoken utilise directement des local variables 
	* en 2e et 3e argument, donc pas besoin de prÃ©ciser que ce sont des
	* local variable en ajoutant les guillemets stata : `'
	* get depvar and indepvar
	gettoken depvar list_var : list_var
	gettoken indepvar list_var : list_var, p("(")
    * get endogenous variables and instruments
	gettoken endog list_var : list_var, bind
	gettoken endog endog : endog, p("(")
    gettoken endog instr_temp : endog , p("=")
    gettoken equalsign instr_temp : instr_temp , p("=")
	gettoken instr instr_temp : instr_temp, p(")")
	cap drop dep_pos 
	gen dep_pos = `depvar'>0
         cap program drop BOOTSTRAP_PROCEDURE_IOLS
         program BOOTSTRAP_PROCEDURE_IOLS, rclass
		 iOLS_OLS `depvar'  `indepvar' , delta(`delta') robust
         *lhs of test
         predict xb_temp, xb
         gen u_hat_temp = `depvar'*exp(-xb_temp)
         gen lhs_temp = log(delta+u_hat_temp) - log(delta)
         * rhs of test
         gen temp = log(`depvar' + delta*exp(xb_temp)) - xb_temp
         egen c_hat_temp = mean(temp)
         logit dep_pos `depvar'  `indepvar'
         predict p_hat_temp, pr
         gen rhs_temp = (c_hat_temp-log(delta))/p_hat_temp
         * run the test
         reg lhs_temp rhs_temp if dep_pos, nocons
         matrix b = e(b)
         ereturn post b
         * drop created variables
         cap drop *temp
         end 
		 bootstrap lambda = _b[rhs_temp] , reps(`reps'): BOOTSTRAP_PROCEDURE_IOLS
         test lambda==1
		cap drop dep_pos
******************************************************************************
* Return the information to STATA output
******************************************************************************

end
