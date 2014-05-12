#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include <iostream>
#include "cool-tree.h"
#include "stringtab.h"
#include "symtab.h"
#include "list.h"
#include <map>
#include <set>
#include <vector>

#define TRUE 1
#define FALSE 0

class ClassTable;
typedef ClassTable *ClassTableP;

// This is a structure that may be used to contain the semantic
// information such as the inheritance graph.  You may use it or not as
// you like: it is only here to provide a container for the supplied
// methods.

class ClassTable {
private:
  int semant_errors;
  void install_basic_classes();
  ostream& error_stream;

  Class_ curr_class;

  void dump_inheritance();
  void check_for_cycles(Class_ parent, std::set<Class_> mark_set, int depth);
  void check_methods_recur(Class_ c, Class_ p);
  bool check_method_type_sig(Class_ c, Feature f);
  bool check_multiple_method(Class_ c, Feature f, std::set<Feature> curr_method_set); 

public:
  ClassTable(Classes);

  SymbolTable<Symbol, Symbol> *sym_tab;
  std::map<Symbol, Class_> class_lookup;
  std::map<Class_, std::set<Class_> > inheritance_set;
  std::map<Class_, std::set<Feature> > method_set;

  void   check_methods();
  void   check_types_and_scopes();
  Class_ get_curr_class();
  void   set_curr_class(Class_ c);
  bool   is_subtype(Symbol s1, Symbol s2);
  Symbol get_LUB(Symbol s1, Symbol s2);

  int errors() { return semant_errors; }
  ostream& semant_error();
  ostream& semant_error(Class_ c);
  ostream& semant_error(Symbol filename, tree_node *t);
};


#endif
