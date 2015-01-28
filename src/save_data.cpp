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

#include "grafodb.hpp"
#include "save_graph.hpp"

using namespace pqxx;
using namespace Rcpp;
using namespace std;


// Enable C++11 via this plugin (Rcpp 0.10.3 or later)
// [[Rcpp::plugins(cpp11)]]

//' Save Nativo (non implementato)
//'
//' @name save_data
//' @usage save_data(series, tag)
//' @param series lista di serie da salvare
//' @param tag tag del database da cui caricare le serie
//' @export
//' @import Rcpp
//' @useDynLib GrafoDB
//
// [[Rcpp::export]]
void save_data(List dati, List functions, CharacterMatrix archi,  
               CharacterVector tag, CharacterVector newtag ) {
  
  //CharacterVector x = CharacterVector::create( "foo", "bar" )  ;
  //NumericVector y   = NumericVector::create( 0.0, 1.0 ) ;
  //  List z            = List::create( _["x"]=x,_["y"]= y ) ;
  
  //string username0 = as<string>(username); 
  //string password0 = as<string>(password); 
  //string hostname0 = as<string>(hostname); 
  //string port0 = as<string>(port); 
  //string dbname0 = as<string>(dbname);

  string conninfo = "user=m24000 password=dic14dic dbname=grafo host=osiride-lv-016 port=5432";
  
  pqxx::connection conn(conninfo);
  pqxx::work* T = new pqxx::work(conn, "Save Transaction");
  
  string tag0 = as<string>(tag);
  string newtag0 = as<string>(newtag);
  
  GrafoDB g(dati, functions, archi, tag0);
  save_graph(g, newtag0, T);
  
  T->commit();
  delete T;
  conn.disconnect();  
}
