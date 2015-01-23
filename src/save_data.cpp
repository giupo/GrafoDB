#include <Rcpp.h>
#include <json/json.h>
#include <string.h>
#include <iostream>
#include <stdlib.h>
#include <vector>
#include <string>
#include <pqxx/pqxx>
#include <sstream>
#include <math.h>
#include "utils.hpp"

using namespace pqxx;
using namespace Rcpp;
using namespace std;


class Series {
public:
  unsigned int anno;
  unsigned int periodo;
  unsigned int freq;
  std::vector<double> dati;
  
  
  Series(NumericVector raw) {
    dati = as<vector<double> >(raw);
    NumericVector tsp = raw.attr("tsp");
    double start = tsp[0];
    freq = (unsigned int) tsp[2];
    anno = (unsigned int) floor(start);
    periodo = (start - anno) * freq + 1;
    if(periodo == 0) {
      periodo = freq;
      --anno;
    }    
  }
};

// Enable C++11 via this plugin (Rcpp 0.10.3 or later)
// [[Rcpp::plugins(cpp11)]]

//' shit full
//'
//' @name save_data
//' @usage save_data(series, tag)
// @param username username per la connessione
// @param password password per la connessione
// @param hostname hostname del database
// @param port porta di ascolto del server
// @param dbname nome del db
//' @param series lista di serie da salvare
//' @param tag tag del database da cui caricare le serie
//' @export
//' @import Rcpp
//' @useDynLib GrafoDB
//
// [[Rcpp::export]]

void save_data(List series, CharacterVector tag) {
  
  //CharacterVector x = CharacterVector::create( "foo", "bar" )  ;
  //NumericVector y   = NumericVector::create( 0.0, 1.0 ) ;
  //  List z            = List::create( _["x"]=x,_["y"]= y ) ;
  
  //string username0 = as<string>(username); 
  //string password0 = as<string>(password); 
  //string hostname0 = as<string>(hostname); 
  //string port0 = as<string>(port); 
  //string dbname0 = as<string>(dbname);
  
  string tag0 = as<string>(tag);
  CharacterVector nomi = series.names();
  for(CharacterVector::iterator it = nomi.begin(); it != nomi.end(); ++it) {
    std::string nome = as<string>(*it);
    NumericVector serie = series[nome];
    Series s(serie);
    Rprintf("anno: %u\n", s.anno);
    Rprintf("periodo: %u\n", s.periodo);
    Rprintf("freq: %u\n", s.freq);

    
  }
}
