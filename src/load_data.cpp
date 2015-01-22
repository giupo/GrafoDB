// #include <libpq-fe.h>
#include <Rcpp.h>
#include <json/json.h>
#include <string.h>
#include <iostream>
#include <stdlib.h>
#include <pthread.h>
#include <vector>
#include <string>
#include <pqxx/pqxx>

using namespace pqxx;
using namespace Rcpp;
using namespace std;

// Enable C++11 via this plugin (Rcpp 0.10.3 or later)
// [[Rcpp::plugins(cpp11)]]

//' shit full
//'
//' @name load_data
//' @usage load_data(username, password, hostname, port, dbname, names, tag)
//' @param username username per la connessione
//' @param password password per la connessione
//' @param hostname hostname del database
//' @param port porta di ascolto del server
//' @param dbname nome del db
//' @param names nomi di serie da caricare
//' @param tag tag del database da cui caricare le serie
//' @return a list
//' @export
//' @useDynLib testpq
//
// [[Rcpp::export]]

List load_data(SEXP username,
               SEXP password, 
               SEXP hostname,
               SEXP port,
               SEXP dbname,
               SEXP names,
               SEXP tag) {
  Json::Value root; // will contains the root value after parsing.
  Json::Value value; // this will contain each value while parsing
  Json::Reader reader;
  Json::StyledWriter styledWriter;

  vector<string> names0 = as<vector<string> >(names);  
  string username0 = as<string>(username); 
  string password0 = as<string>(password); 
  string hostname0 = as<string>(hostname); 
  string port0 = as<string>(port); 
  string dbname0 = as<string>(dbname);
  string conninfo = "user="+username0+" password="+password0 +
    " dbname="+dbname0 + " host="+hostname0+" port="+port0; 
  string tag0 = as<string>(tag);

  unsigned int i, j;
  unsigned int totalSize;
  
  pqxx::connection conn(conninfo);
  const char* sql = "select name, anno, periodo, freq, dati "
                    "from dati where name = $1 and tag = $2";
  
  conn.prepare("getdata", sql);  
  pqxx::work T(conn, "DemoTransaction");
  
  List z = List::create();
  for(vector<string>::iterator name = names0.begin(); 
      name != names0.end(); ++name) {
    
    pqxx::result res = T.prepared("getdata")(name->c_str())(tag0.c_str()).exec();
    totalSize = res.size();
    
    
    for (i = 0; i < totalSize; ++i) {    
      string name;
      double anno;
      double periodo;
      double freq;
      string sDati;
      
      res[i]["name"].to(name);
      res[i]["anno"].to(anno);
      res[i]["periodo"].to(periodo);
      res[i]["freq"].to(freq);
      res[i]["dati"].to(sDati);
      
      reader.parse(sDati, root);    
      
      // for tsp
      double start = anno + periodo/freq - 1/freq;
      double end = start + root.size()/freq - 1/freq; 
      
      vector<double> buffer(root.size());
      NumericVector dati(root.size()); 
      for(j = 0; j < buffer.size(); ++j) {
        value = root[j];
        if(value.isNull()) {
          dati[j] = NA_REAL;
        } else {
          dati[j] = value.asDouble();
        }
      }
      
      dati.attr("tsp") = NumericVector::create(start, end, freq);
      dati.attr("class") = "ts";
      z[name] = dati;
    } 
  }
  
  T.commit();
  conn.disconnect();
  //CharacterVector x = CharacterVector::create( "foo", "bar" )  ;
  //NumericVector y   = NumericVector::create( 0.0, 1.0 ) ;
  //  List z            = List::create( _["x"]=x,_["y"]= y ) ;

  return z ;
}
