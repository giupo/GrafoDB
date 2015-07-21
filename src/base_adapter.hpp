#ifndef __BASE_ADAPTER_HPP__
#define __BASE_ADAPTER_HPP__

#include <string>
#include "abstract_adapter.hpp"

using namespace std;

class BaseAdapter : public AbstractAdapter {
public:
  BaseAdapter(const string username, const string password, 
              const string host, const string port, 
              const string dbname, const string tag);
  BaseAdapter(const string host, const string port,
              const string dbname, const string tag);
  
  BaseAdapter(const BaseAdapter& other) {
    this->conninfo = other.conninfo;
    this->tag = other.tag;
    this->ordinal = other.ordinal;
  }
  
  BaseAdapter& operator=(const BaseAdapter& other) { 
    this->conninfo = other.conninfo;
    this->tag = other.tag;
    this->ordinal = other.ordinal;
    return *this;
  }
  
  virtual void matchOrdinal();

protected:
  std::string conninfo;
  std::string tag;
  unsigned int ordinal;
};

#endif
