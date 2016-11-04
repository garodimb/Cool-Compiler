#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include <iostream>
#include <map>
#include "cool-tree.h"
#include "stringtab.h"
#include "symtab.h"
#include "list.h"

#define TRUE 1
#define FALSE 0

class ClassTable;
typedef std::map<Symbol,Class_> ClassSymTable;
typedef ClassTable *ClassTableP;

// This is a structure that may be used to contain the semantic
// information such as the inheritance graph.  You may use it or not as
// you like: it is only here to provide a container for the supplied
// methods.

class ClassTable {
private:
  int semant_errors;
  ClassSymTable *class_symtable;
  void install_basic_classes();
  ostream& error_stream;

public:
  ClassTable(Classes);
  Class_ lookup_class(Symbol class_name);
  Feature lookup_method(Symbol method_name,Symbol class_name);
  Symbol lookup_attr(Symbol attr_name, Symbol class_name);
  void install_classes(Classes classes);
  void install_symbols(Classes classes);
  bool is_main_present();
  int errors() { return semant_errors; }
  ostream& semant_error();
  ostream& semant_error(Class_ c);
  ostream& semant_error(Symbol filename, tree_node *t);
};


#endif

