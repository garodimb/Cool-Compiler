#include <assert.h>
#include <stdio.h>
#include <list>
#include "emit.h"
#include "cool-tree.h"
#include "symtab.h"

enum Basicness     {Basic, NotBasic};
#define TRUE 1
#define FALSE 0

class CgenClassTable;
typedef CgenClassTable *CgenClassTableP;

class CgenNode;
typedef CgenNode *CgenNodeP;

typedef std::list<Feature> FeatureList;
typedef FeatureList *FeatureListP;

class CgenClassTable : public SymbolTable<Symbol,CgenNode> {
private:
   List<CgenNode> *nds;
   ostream& str;

// The following methods emit code for
// constants and global declarations.

   void code_global_data();
   void code_protObj();
   void code_class_nameTab();
   void code_class_ObjTab();
   void code_dispTab();
   void code_global_text();
   void code_class_methods();
   void code_object_init();
   void code_bools(int);
   void code_select_gc();
   void code_constants();

// The following creates an inheritance graph from
// a list of classes.  The graph is implemented as
// a tree of `CgenNode', and class names are placed
// in the base class symbol table.

   void install_basic_classes();
   void install_class(CgenNodeP nd);
   void install_classes(Classes cs);
   void build_inheritance_tree();
   void set_relations(CgenNodeP nd);
   void first_pass();
public:
   CgenClassTable(Classes, ostream& str);
   CgenNodeP lookup_class_by_tag(int tag);
   CgenNodeP lookup_class_by_name(Symbol name);
   void code();
   CgenNodeP root();
};

class SymbolInfo {
  private:
    char *base_reg;
    int offset;

  public:
    SymbolInfo(char *base_reg, int offset);
    char *get_base_reg() { return base_reg; }
    int get_offset() { return offset; }
};

typedef SymbolTable<Symbol, SymbolInfo> CgenSymTable;

class CgenNode : public class__class {
private: 
   CgenNodeP parentnd;                        // Parent of class
   List<CgenNode> *children;                  // Children of class
   Basicness basic_status;                    // `Basic' if class is basic
                                              // `NotBasic' otherwise
   FeatureListP methods;
   FeatureListP attrs;
   CgenSymTable *symtable;
   int tag, max_class_tag;
   void set_tag();

public:
   CgenNode(Class_ c,
            Basicness bstatus,
            CgenClassTableP class_table);

   void add_child(CgenNodeP child);
   List<CgenNode> *get_children() { return children; }
   void set_parentnd(CgenNodeP p);
   CgenNodeP get_parentnd() { return parentnd; }
   int basic() { return (basic_status == Basic); }
   FeatureListP get_methods() { return methods; }
   FeatureListP get_attrs() { return attrs; }
   int get_tag() { return tag; }
   CgenSymTable *get_symtable() { return symtable; }
   void first_pass();
   void code_protObj(ostream &str);
   void code_class_nameTab(ostream &str);
   void code_class_ObjTab(ostream &str);
   void code_dispTab(ostream &str);
   void code_class_methods(ostream &str);
   void code_object_init(ostream &str);
};

class BoolConst 
{
 private: 
  int val;
 public:
  BoolConst(int);
  void code_def(ostream&, int boolclasstag);
  void code_ref(ostream&) const;
};

