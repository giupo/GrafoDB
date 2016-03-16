#ifndef __SERIES_HPP__
#define __SERIES_HPP__

#include <vector>
#include <Rcpp.h>

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
  
  Series(unsigned anno_, unsigned periodo_, 
         unsigned freq_, std::vector<double> dati_):
    anno(anno_), periodo(periodo_), freq(freq_), dati(dati_) {}
};

#endif
