#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]

List ssfcpp(List x)

{

//Parameter and variable declaration-------------------------------------------------------------------------------------------------------
  double imax = x["imax"];                   //Iteration times
  double tmax = x["tmax"];                   //Projection period
  double recfun = x["recfun"];               //Recruitment function 1:normal, 2:Autocorrelation
  double SB0 = x["SB0"];                     //SB0
  double R0 = x["R0"];                       //R0
  double h = x["h"];                         //Steepness
  double sigmar = x["sigmar"];               //Sigma R
  double age = x["age"];                     //Maximum age (start age 0)
  double gender = x["gender"];               //Number of gender
  double rho = x["rho"];                     //Autocorrelation parameter
  double manage = x["manage"];               //Management scenario 1:Constant F, 2:Constant catch
  double catb = x["catb"];                   //Average catch
  double Fmult = x["Fmult"];                 //F multiplier
  double upto = 0.0001;                      //Difference between estimated catch and target catch
  double thresh = 0;                         //Threshold of the target catch
  double limit = x["limit"];                 //Biological limit reference point
  double target = x["target"];               //Biological target reference point
  int assess = x["assess"];                  //Assessment period
  double alpha = x["alpha"];                 //Coefficient of environmental index

  NumericVector spawn_seas = x["spawn_seas"];//Spawning season (0:no spawning, 1:spawning)
  NumericVector int_n = x["int_n"];          //Initial population number
  NumericVector maa = x["maa"];              //Natural Mortality at Age
  NumericVector vec_sel = x["sel"];          //F at age by quarterly
  NumericVector vec_waa2 = x["waa_mid"];     //Weight at age middle in quarter
  NumericVector fec = x["fec"];              //Maturity X Fecundity
  NumericVector env = x["env"];              //Environmental index

  NumericMatrix fmulti (imax+1, tmax+1);
  vec_sel.attr("dim") = Dimension(gender*(age+1), 4);
  NumericMatrix sel = as<NumericMatrix>(vec_sel);
  NumericMatrix faa ((age+1)*gender, 4);
  NumericMatrix faa_tmp ((age+1)*gender, 4);
  NumericMatrix zaa ((age+1)*gender, 4);
  NumericMatrix zaa_tmp ((age+1)*gender, 4);
  double sb_tmp = 0;
  double R_tmp = 0;

  vec_waa2.attr("dim") =  Dimension(gender*(age+1), 4);
  NumericMatrix waa2 = as<NumericMatrix>(vec_waa2);

  NumericVector tmp_N ((age+1)*gender);      //Temporal population number matrix
  NumericMatrix sb (imax+1, tmax);           //Spawning biomass
  NumericMatrix R (imax+1, tmax);            //Recruitment
  NumericMatrix Sigma_R (imax+1, tmax);      //Uncertainty of recruitment
  NumericMatrix n(imax+1, tmax+1);           //Uncertainty of recruitment with enviromental effect

  NumericVector c (4);                       //Catch weight (mt)
  NumericVector c_est (4);
  NumericMatrix C (imax+1, tmax);            //Catch weight (mt)
  NumericMatrix C_est (imax+1, tmax);        //Estimated catch weigt under constant catch scenario

  NumericMatrix N_qt1 ((age+1)*gender, tmax);//Generate quarterly population number to check the program
  NumericMatrix N_qt2 ((age+1)*gender, tmax);
  NumericMatrix N_qt3 ((age+1)*gender, tmax);
  NumericMatrix N_qt4 ((age+1)*gender, tmax);

  NumericMatrix N ((age+1)*gender, 5);
  NumericMatrix N_est ((age+1)*gender, 4);

//Set the recruitment deviation----------------------------------------------------------------------------------------------------------

  for (int i = 0; i < imax; i++) {
    Sigma_R(i, _) = rnorm(tmax+1, 0, sigmar);
  }

//Start future projection----------------------------------------------------------------------------------------------------------------

  for (int i = 0; i < imax+1; i++) {

    N(_,0) = clone(int_n); //Set initial population number.

    for (int t = 0; t < tmax; t++) {

//Calculate total mortality which depends on management scenario.
      if (manage == 1) {

        for (int q = 0; q < 4; q++) {
          faa(_,q) = Fmult*sel(_,q);
          zaa(_,q) = faa(_,q)+maa/4.0;
        }

      } else if (manage == 2) {

        for (int x = 0; x < 10000; x++) {

          fmulti(i,t) = x*0.001;
          N_est = clone(N);

          for (int q = 0; q < 4; q++) {
            faa(_,q) = fmulti(i,t)*Fmult*sel(_,q);
            zaa(_,q) = faa(_,q)+maa/4.0;
            c_est[q] = sum(waa2(_,q)*(faa(_,q)/zaa(_,q))*(1-exp(-zaa(_,q)))*N_est(_,q));
            N_est(_,q+1) = N_est(_,q)*exp(-zaa(_,q));
          }

          C_est(i,t) = sum(c_est);
          thresh = catb - C_est(i,t);

          if (thresh < upto) break;

        }

      } else if (manage == 3) {

        fmulti(i,0) = 1.0;
        N_est = clone(N);

        for (int q = 0; q < 4; q++) {

          faa_tmp(_,q) = fmulti(i,t)*Fmult*sel(_,q);
          zaa_tmp(_,q) = faa_tmp(_,q) + maa/4.0;
          N_est(_,q+1) = N_est(_,q)*exp(-zaa_tmp(_,q));

          if (spawn_seas[q] == 1) {
            sb_tmp = sum(fec*N_est(_,q));
          }
        }

        if (t % assess == 0 && t>0) {

          if(sb_tmp >= target) {
            fmulti(i,t+1) = 1.0;
          } else if (sb_tmp  >= limit && sb_tmp  < target){
            fmulti(i,t+1) = sb_tmp/(target-limit)-limit/(target-limit);
          } else {
            fmulti(i,t+1) = 0.0;
          }

        } else {
          fmulti(i,t+1) = fmulti(i,t);
        }

        for (int q = 0; q < 4; q++) {
          faa(_,q) = fmulti(i,t)*Fmult*sel(_,q);
          zaa(_,q) = faa(_,q) + maa/4.0;
        }

      } else {

        fmulti(i,0) = 1.0;
        N_est = clone(N);

        for (int q = 0; q < 4; q++) {

          faa_tmp(_,q) = fmulti(i,t)*Fmult*sel(_,q);
          zaa_tmp(_,q) = faa_tmp(_,q) + maa/4.0;
          N_est(_,q+1) = N_est(_,q)*exp(-zaa_tmp(_,q));

          if (spawn_seas[q] == 1) {

            sb_tmp = sum(fec*N_est(_,q));
            R_tmp = ((4*h*R0*sb(i,t))/(SB0*(1-h)+sb(i,t)*(5*h-1)))*exp(Sigma_R(i,t)-(pow(sigmar,2))/2);

          }
        }

        if (t % assess == 0 && t>0) {

          if(R_tmp >= target) {
            fmulti(i,t+1) = 1.0;
          } else if (R_tmp  >= limit && R_tmp  < target){

            fmulti(i,t+1) = R_tmp/(target-limit)-limit/(target-limit);

          } else {
            fmulti(i,t+1) = 0.0;
          }

        } else {
          fmulti(i,t+1) = fmulti(i,t);
        }

        for (int q = 0; q < 4; q++) {

          faa(_,q) = fmulti(i,t)*Fmult*sel(_,q);
          zaa(_,q) = faa(_,q) + maa/4.0;

        }

      }


//First of all, recruitment need to calculate
  for (int q = 0; q < 4; q++) {

    if (spawn_seas[q] == 1) {

      sb(i,t) = sum(fec*N(_,q));                                                                              //Calculate spawning biomass

      if (i < imax) {

        if (recfun == 1) {

          R(i,t) = ((4*h*R0*sb(i,t))/(SB0*(1-h)+sb(i,t)*(5*h-1)))*exp(Sigma_R(i,t)-(pow(sigmar,2))/2);        //Recruitment in time t+1

        } else if (recfun == 2) {

          n(i,0) = Sigma_R(i,0);                                                                              //Coefficient representing first-order autocorrelation in time 0
          n(i,t+1) = rho*n(i,t) + pow((1-pow(rho,2)),0.5)*Sigma_R(i,t+1);                                     //Coefficient representing first-order autocorrelation in time t+1
          R(i,t) = ((4*h*R0*sb(i,t))/(SB0*(1-h)+sb(i,t)*(5*h-1)))*exp(n(i,t)-(pow(sigmar,2))/2);              //Recruitment with enviroment effect in time t+1

        } else {

          R(i,t) = ((4*h*R0*sb(i,t))/(SB0*(1-h)+sb(i,t)*(5*h-1)))*exp(alpha*env(t))*exp(Sigma_R(i,t)-(pow(sigmar,2))/2);
        }

      } else {

        R(i,t) = ((4*h*R0*sb(i,t))/(SB0*(1-h)+sb(i,t)*(5*h-1)));
      }

      if (gender == 1) {
        N(0,q) = N(0,q)+R(i,t);                                                                               //Number of recruitment
      } else {

        N(0,q) = N(0,q)+0.5*R(i,t);                                                                           //Number of Female recruitment
        N(age+1,q) = N(age+1,q)+0.5*R(i,t);                                                                   //Number of Male recruitment

      }
    }

//Next, population number at next quarter and quarterly catch will calculate
  N(_,q+1) = N(_,q)*exp(-zaa(_,q));
  c[q] = sum(waa2(_,q)*(faa(_,q)/zaa(_,q))*(1-exp(-zaa(_,q)))*N(_,q));                                      //Catch weight at quarter

  }

  N_qt1(_,t) = N(_,0);                                                                                         //Save quarterly population number in qt1
  N_qt2(_,t) = N(_,1);                                                                                         //Save quarterly population number in qt2
  N_qt3(_,t) = N(_,2);                                                                                         //Save quarterly population number in qt3
  N_qt4(_,t) = N(_,3);                                                                                         //Save quarterly population number in qt4
  C(i,t) = sum(c);                                                                                             //Save catch weight

//Calculate plus group
  if (gender == 1) {

    for (int j = 0; j < age-1; j++) {

      tmp_N[j+1] = N(j,4);

    }

    tmp_N[age] = N(age-1,4)+N(age,4);

  } else {
    for (int j = 0; j < age-1; j++) {
      tmp_N[j+1] = N(j,4);
    }

    for (int j = age+1; j < age*2; j++) {
      tmp_N[j+1] = N(j,4);
    }

    tmp_N[age] = N(age-1,4)+N(age,4);
    tmp_N[age*2+1] = N(age*2,4)+N(age*2+1,4);

  }

  N(_,0) = tmp_N;

  }
  }

return List::create(_["SB"]=sb, _["C"]=C, _["N_qt1"]=N_qt1, _["N_qt2"]=N_qt2, _["N_qt3"]=N_qt3, _["N_qt4"]=N_qt4,
_["R"]=R, _["fmulti"]=fmulti);

}
