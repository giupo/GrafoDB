#include "base_adapter.hpp"

#include <regex.h>

BaseAdapter::BaseAdapter(const string username,
                         const string password, 
                         const string host, 
                         const string port, 
                         const string dbname,
                         const string tag) {
  this->conninfo = "user=" + username + " password=" + password + 
    " dbname=" + dbname + " host=" + host + " port=" + port;
  this->tag = tag;
  this->matchOrdinal();
}

BaseAdapter::BaseAdapter(const string host,
                         const string port, 
                         const string dbname,
                         const string tag) {
  this->conninfo = "dbname=" + dbname + " host=" + host + " port=" + port;
  this->tag = tag;
  this->matchOrdinal();
}

void BaseAdapter::matchOrdinal() {
  regex_t regex;
  int reti;
  regmatch_t pmatch[1];
  char errmsg[100];

  reti = regcomp(&regex, "p[[:digit:]]+$", REG_EXTENDED);
  if (reti) {
    regerror(reti, &regex, errmsg,  sizeof(errmsg));  
    regfree(&regex);
    stop(string(errmsg));
  }
  reti = regexec(&regex, this->tag.c_str(), 1, pmatch, 0);
  if (0 == reti) {
    unsigned int start = pmatch[0].rm_so;
    unsigned int finish =  pmatch[0].rm_eo;
    this->ordinal = (unsigned int) atoi(this->tag.substr(start+1, finish).c_str());
    this->tag = this->tag.substr(0, start);
  } else if(REG_NOMATCH == reti) {
    this->ordinal = 0;
  } else {
    regerror(reti, &regex, errmsg,  sizeof(errmsg));  
    regfree(&regex);
    stop(string(errmsg));
  }
  regfree(&regex);
}
