
//**************************************************************
//
// Code generator SKELETON
//
// Read the comments carefully. Make sure to
//    initialize the base class tags in
//       `CgenClassTable::CgenClassTable'
//
//    Add the label for the dispatch tables to
//       `IntEntry::code_def'
//       `StringEntry::code_def'
//       `BoolConst::code_def'
//
//    Add code to emit everyting else that is needed
//       in `CgenClassTable::code'
//
//
// The files as provided will produce code to begin the code
// segments, declare globals, and emit constants.  You must
// fill in the rest.
//
//**************************************************************

#include "cgen.h"
#include "cgen_gc.h"

extern void emit_string_constant(ostream& str, char *s);
extern int cgen_debug;
static int max_tag = 0;
static int intclasstag;
static int stringclasstag;
static int boolclasstag;
static int label_postfix = 0;
static int local_attr_offset = 1; // Keep track of number of local attrs visible
static CgenClassTableP curr_classtable;
static CgenNodeP curr_class;


//
// Three symbols from the semantic analyzer (semant.cc) are used.
// If e : No_type, then no code is generated for e.
// Special code is generated for new SELF_TYPE.
// The name "self" also generates code different from other references.
//
//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
Symbol 
       arg,
       arg2,
       Bool,
       concat,
       cool_abort,
       copy,
       Int,
       in_int,
       in_string,
       IO,
       length,
       Main,
       main_meth,
       No_class,
       No_type,
       Object,
       out_int,
       out_string,
       prim_slot,
       self,
       SELF_TYPE,
       Str,
       str_field,
       substr,
       type_name,
       val;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
  arg         = idtable.add_string("arg");
  arg2        = idtable.add_string("arg2");
  Bool        = idtable.add_string("Bool");
  concat      = idtable.add_string("concat");
  cool_abort  = idtable.add_string("abort");
  copy        = idtable.add_string("copy");
  Int         = idtable.add_string("Int");
  in_int      = idtable.add_string("in_int");
  in_string   = idtable.add_string("in_string");
  IO          = idtable.add_string("IO");
  length      = idtable.add_string("length");
  Main        = idtable.add_string("Main");
  main_meth   = idtable.add_string("main");
//   _no_class is a symbol that can't be the name of any 
//   user-defined class.
  No_class    = idtable.add_string("_no_class");
  No_type     = idtable.add_string("_no_type");
  Object      = idtable.add_string("Object");
  out_int     = idtable.add_string("out_int");
  out_string  = idtable.add_string("out_string");
  prim_slot   = idtable.add_string("_prim_slot");
  self        = idtable.add_string("self");
  SELF_TYPE   = idtable.add_string("SELF_TYPE");
  Str         = idtable.add_string("String");
  str_field   = idtable.add_string("_str_field");
  substr      = idtable.add_string("substr");
  type_name   = idtable.add_string("type_name");
  val         = idtable.add_string("_val");
}

static char *gc_init_names[] =
  { "_NoGC_Init", "_GenGC_Init", "_ScnGC_Init" };
static char *gc_collect_names[] =
  { "_NoGC_Collect", "_GenGC_Collect", "_ScnGC_Collect" };


//  BoolConst is a class that implements code generation for operations
//  on the two booleans, which are given global names here.
BoolConst falsebool(FALSE);
BoolConst truebool(TRUE);

//*********************************************************
//
// Define method for code generation
//
// This is the method called by the compiler driver
// `cgtest.cc'. cgen takes an `ostream' to which the assembly will be
// emmitted, and it passes this and the class list of the
// code generator tree to the constructor for `CgenClassTable'.
// That constructor performs all of the work of the code
// generator.
//
//*********************************************************

void program_class::cgen(ostream &os) 
{
  // spim wants comments to start with '#'
  os << "# start of generated code\n";

  initialize_constants();
  CgenClassTable *codegen_classtable = new CgenClassTable(classes,os);

  os << "\n# end of generated code\n";
}


//////////////////////////////////////////////////////////////////////////////
//
//  emit_* procedures
//
//  emit_X  writes code for operation "X" to the output stream.
//  There is an emit_X for each opcode X, as well as emit_ functions
//  for generating names according to the naming conventions (see emit.h)
//  and calls to support functions defined in the trap handler.
//
//  Register names and addresses are passed as strings.  See `emit.h'
//  for symbolic names you can use to refer to the strings.
//
//////////////////////////////////////////////////////////////////////////////

static void emit_load(char *dest_reg, int offset, char *source_reg, ostream& s)
{
  s << LW << dest_reg << " " << offset * WORD_SIZE << "(" << source_reg << ")" 
    << endl;
}

static void emit_store(char *source_reg, int offset, char *dest_reg, ostream& s)
{
  s << SW << source_reg << " " << offset * WORD_SIZE << "(" << dest_reg << ")"
      << endl;
}

static void emit_load_imm(char *dest_reg, int val, ostream& s)
{ s << LI << dest_reg << " " << val << endl; }

static void emit_load_address(char *dest_reg, char *address, ostream& s)
{ s << LA << dest_reg << " " << address << endl; }

static void emit_partial_load_address(char *dest_reg, ostream& s)
{ s << LA << dest_reg << " "; }

static void emit_load_bool(char *dest, const BoolConst& b, ostream& s)
{
  emit_partial_load_address(dest,s);
  b.code_ref(s);
  s << endl;
}

static void emit_load_string(char *dest, StringEntry *str, ostream& s)
{
  emit_partial_load_address(dest,s);
  str->code_ref(s);
  s << endl;
}

static void emit_load_int(char *dest, IntEntry *i, ostream& s)
{
  emit_partial_load_address(dest,s);
  i->code_ref(s);
  s << endl;
}

static void emit_move(char *dest_reg, char *source_reg, ostream& s)
{ s << MOVE << dest_reg << " " << source_reg << endl; }

static void emit_neg(char *dest, char *src1, ostream& s)
{ s << NEG << dest << " " << src1 << endl; }

static void emit_add(char *dest, char *src1, char *src2, ostream& s)
{ s << ADD << dest << " " << src1 << " " << src2 << endl; }

static void emit_addu(char *dest, char *src1, char *src2, ostream& s)
{ s << ADDU << dest << " " << src1 << " " << src2 << endl; }

static void emit_addiu(char *dest, char *src1, int imm, ostream& s)
{ s << ADDIU << dest << " " << src1 << " " << imm << endl; }

static void emit_div(char *dest, char *src1, char *src2, ostream& s)
{ s << DIV << dest << " " << src1 << " " << src2 << endl; }

static void emit_mul(char *dest, char *src1, char *src2, ostream& s)
{ s << MUL << dest << " " << src1 << " " << src2 << endl; }

static void emit_sub(char *dest, char *src1, char *src2, ostream& s)
{ s << SUB << dest << " " << src1 << " " << src2 << endl; }

static void emit_sll(char *dest, char *src1, int num, ostream& s)
{ s << SLL << dest << " " << src1 << " " << num << endl; }

static void emit_jalr(char *dest, ostream& s)
{ s << JALR << "\t" << dest << endl; }

static void emit_jal(char *address,ostream &s)
{ s << JAL << address << endl; }

static void emit_return(ostream& s)
{ s << RET << endl; }

static void emit_gc_assign(ostream& s)
{ s << JAL << "_GenGC_Assign" << endl; }

static void emit_disptable_ref(Symbol sym, ostream& s)
{  s << sym << DISPTAB_SUFFIX; }

static void emit_init_ref(Symbol sym, ostream& s)
{ s << sym << CLASSINIT_SUFFIX; }

static void emit_label_ref(int l, ostream &s)
{ s << "label" << l; }

static void emit_protobj_ref(Symbol sym, ostream& s)
{ s << sym << PROTOBJ_SUFFIX; }

static void emit_method_ref(Symbol classname, Symbol methodname, ostream& s)
{ s << classname << METHOD_SEP << methodname; }

static void emit_label_def(int l, ostream &s)
{
  emit_label_ref(l,s);
  s << ":" << endl;
}

static void emit_beqz(char *source, int label, ostream &s)
{
  s << BEQZ << source << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_beq(char *src1, char *src2, int label, ostream &s)
{
  s << BEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bne(char *src1, char *src2, int label, ostream &s)
{
  s << BNE << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bleq(char *src1, char *src2, int label, ostream &s)
{
  s << BLEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blt(char *src1, char *src2, int label, ostream &s)
{
  s << BLT << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blti(char *src1, int imm, int label, ostream &s)
{
  s << BLT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bgti(char *src1, int imm, int label, ostream &s)
{
  s << BGT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_branch(int l, ostream& s)
{
  s << BRANCH;
  emit_label_ref(l,s);
  s << endl;
}

//
// Push a register on the stack. The stack grows towards smaller addresses.
//
static void emit_push(char *reg, ostream& str)
{
  emit_store(reg,0,SP,str);
  emit_addiu(SP,SP,-4,str);
}

//
// Fetch the integer value in an Int object.
// Emits code to fetch the integer value of the Integer object pointed
// to by register source into the register dest
//
static void emit_fetch_int(char *dest, char *source, ostream& s)
{ emit_load(dest, DEFAULT_OBJFIELDS, source, s); }

//
// Emits code to store the integer value contained in register source
// into the Integer object pointed to by dest.
//
static void emit_store_int(char *source, char *dest, ostream& s)
{ emit_store(source, DEFAULT_OBJFIELDS, dest, s); }


static void emit_test_collector(ostream &s)
{
  emit_push(ACC, s);
  emit_move(ACC, SP, s); // stack end
  emit_move(A1, ZERO, s); // allocate nothing
  s << JAL << gc_collect_names[cgen_Memmgr] << endl;
  emit_addiu(SP,SP,4,s);
  emit_load(ACC,0,SP,s);
}

static void emit_gc_check(char *source, ostream &s)
{
  if (source != (char*)A1) emit_move(A1, source, s);
  s << JAL << "_gc_check" << endl;
}

static void emit_copy(ostream &s)
{
	s << JAL; emit_method_ref(Object, copy, s); s << endl;
}


////////////////////////////////////////////////////////////////////////////////
//
//  Utility functions to support code generation
//
////////////////////////////////////////////////////////////////////////////////


static FeatureList::iterator lookup_feature_by_name(Symbol name, FeatureListP feature_list)
{
  FeatureList::iterator it;
  assert(feature_list!=NULL);
  for(it = feature_list->begin(); it != feature_list->end(); ++it){
    if((*it)->get_name()==name){
      return it;
    }
  }
  return it;
}

static int get_attr_offset(Symbol name, FeatureListP attrs)
{
  FeatureList::iterator it = lookup_feature_by_name(name, attrs);
  assert(it != attrs->end());
  return std::distance(attrs->begin(), it) + DEFAULT_OBJFIELDS;
}

static int get_method_offset(Symbol type, Symbol name)
{
  CgenNodeP node = curr_classtable->lookup_class_by_name(type);
  assert(node);
  FeatureListP methods = node->get_methods();
  FeatureList::iterator it = lookup_feature_by_name(name, methods);
  assert(it != methods->end());
  return std::distance(methods->begin(), it);
}

///////////////////////////////////////////////////////////////////////////////
// Activation record structure
// DEFAULT_FRAME = 3 (Old FP + Old SELF + RA)
// Actual Parameter starts at : CURR_FP + DEFAULT_FRAME - 1(Xn is closet to FP)
// +----------+------------+
// |     x1   |            |
// +----------+            +
// |    x2    |            |
// +----------+   Actual   +
// |  ......  | Parameters |
// +----------+            +
// |     xn   |            |
// +----------+------------+
// |  Old FP  |            |
// +----------+------------+
// | Old SELF |            |
// +----------+------------+
// |    RA    |  New FP    |
// +----------+------------+
//
///////////////////////////////////////////////////////////////////////////////
/* callee callsequence on method call */
static void callee_callseq(ostream &str)
{
  emit_addiu(SP, SP, -3 * WORD_SIZE, str);
  emit_store(FP, 3, SP, str); // Store Old frame pointer
  emit_store(SELF, 2, SP, str); // Store Old self object address
  emit_store(RA, 1, SP, str); // Store return address
  // Load frame pointer with new value.
  // Frame pointer points to top of frame(At return address)
  emit_addiu(FP, SP, WORD_SIZE, str);
  //Move SELF object address passed in ACC to SELF
  emit_move(SELF, ACC, str);
}

/* Callee call sequence on return from method */
static void callee_callseq_ret(int num_args, ostream &str)
{
  emit_load(FP, 3, SP, str); // Restore FP
  emit_load(SELF, 2, SP, str); // Restore SELF
  emit_load(RA, 1, SP, str); // Load return address to return
  emit_addiu(SP , SP, (3 + num_args) * WORD_SIZE, str); // Restore statck
  emit_return(str); // Return from method
}

///////////////////////////////////////////////////////////////////////////////
//
// coding strings, ints, and booleans
//
// Cool has three kinds of constants: strings, ints, and booleans.
// This section defines code generation for each type.
//
// All string constants are listed in the global "stringtable" and have
// type StringEntry.  StringEntry methods are defined both for String
// constant definitions and references.
//
// All integer constants are listed in the global "inttable" and have
// type IntEntry.  IntEntry methods are defined for Int
// constant definitions and references.
//
// Since there are only two Bool values, there is no need for a table.
// The two booleans are represented by instances of the class BoolConst,
// which defines the definition and reference methods for Bools.
//
///////////////////////////////////////////////////////////////////////////////

//
// Strings
//
void StringEntry::code_ref(ostream& s)
{
  s << STRCONST_PREFIX << index;
}

//
// Emit code for a constant String.
// You should fill in the code naming the dispatch table.
//

void StringEntry::code_def(ostream& s, int stringclasstag)
{
  IntEntryP lensym = inttable.add_int(len);

  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s  << LABEL                                             // label
      << WORD << stringclasstag << endl                                 // tag
      << WORD << (DEFAULT_OBJFIELDS + STRING_SLOTS + (len+4)/4) << endl // size
      << WORD; emit_disptable_ref(Str,s);
      s << endl;                                              // dispatch table
      s << WORD;  lensym->code_ref(s);  s << endl;            // string length
  emit_string_constant(s,str);                                // ascii string
  s << ALIGN;                                                 // align to word
}

//
// StrTable::code_string
// Generate a string object definition for every string constant in the 
// stringtable.
//
void StrTable::code_string_table(ostream& s, int stringclasstag)
{  
  for (List<StringEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,stringclasstag);
}

//
// Ints
//
void IntEntry::code_ref(ostream &s)
{
  s << INTCONST_PREFIX << index;
}

//
// Emit code for a constant Integer.
// You should fill in the code naming the dispatch table.
//

void IntEntry::code_def(ostream &s, int intclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s << LABEL                                // label
      << WORD << intclasstag << endl                      // class tag
      << WORD << (DEFAULT_OBJFIELDS + INT_SLOTS) << endl  // object size
      << WORD; emit_disptable_ref(Int, s);
      s << endl;                                          // dispatch table
      s << WORD << str << endl;                           // integer value
}


//
// IntTable::code_string_table
// Generate an Int object definition for every Int constant in the
// inttable.
//
void IntTable::code_string_table(ostream &s, int intclasstag)
{
  for (List<IntEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,intclasstag);
}


//
// Bools
//
BoolConst::BoolConst(int i) : val(i) { assert(i == 0 || i == 1); }

void BoolConst::code_ref(ostream& s) const
{
  s << BOOLCONST_PREFIX << val;
}
  
//
// Emit code for a constant Bool.
// You should fill in the code naming the dispatch table.
//

void BoolConst::code_def(ostream& s, int boolclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s << LABEL                                  // label
      << WORD << boolclasstag << endl                       // class tag
      << WORD << (DEFAULT_OBJFIELDS + BOOL_SLOTS) << endl   // object size
      << WORD; emit_disptable_ref(Bool, s);
      s << endl;                                            // dispatch table
      s << WORD << val << endl;                             // value (0 or 1)
}

//////////////////////////////////////////////////////////////////////////////
//
//  CgenClassTable methods
//
//////////////////////////////////////////////////////////////////////////////

//***************************************************
//
//  Emit code to start the .data segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_data()
{
  Symbol main    = idtable.lookup_string(MAINNAME);
  Symbol string  = idtable.lookup_string(STRINGNAME);
  Symbol integer = idtable.lookup_string(INTNAME);
  Symbol boolc   = idtable.lookup_string(BOOLNAME);

  str << "\t.data\n" << ALIGN;
  //
  // The following global names must be defined first.
  //
  str << GLOBAL << CLASSNAMETAB << endl;
  str << GLOBAL; emit_protobj_ref(main,str);    str << endl;
  str << GLOBAL; emit_protobj_ref(integer,str); str << endl;
  str << GLOBAL; emit_protobj_ref(string,str);  str << endl;
  str << GLOBAL; falsebool.code_ref(str);  str << endl;
  str << GLOBAL; truebool.code_ref(str);   str << endl;
  str << GLOBAL << INTTAG << endl;
  str << GLOBAL << BOOLTAG << endl;
  str << GLOBAL << STRINGTAG << endl;

  //
  // We also need to know the tag of the Int, String, and Bool classes
  // during code generation.
  //
  str << INTTAG << LABEL
      << WORD << intclasstag << endl;
  str << BOOLTAG << LABEL 
      << WORD << boolclasstag << endl;
  str << STRINGTAG << LABEL 
      << WORD << stringclasstag << endl;    
}


//***************************************************
//
//  Emit code to start the .text segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_text()
{
  str << GLOBAL << HEAP_START << endl
      << HEAP_START << LABEL 
      << WORD << 0 << endl
      << "\t.text" << endl
      << GLOBAL;
  emit_init_ref(idtable.add_string("Main"), str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Int"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("String"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Bool"),str);
  str << endl << GLOBAL;
  emit_method_ref(idtable.add_string("Main"), idtable.add_string("main"), str);
  str << endl;
}

void CgenClassTable::code_bools(int boolclasstag)
{
  falsebool.code_def(str,boolclasstag);
  truebool.code_def(str,boolclasstag);
}

void CgenClassTable::code_select_gc()
{
  //
  // Generate GC choice constants (pointers to GC functions)
  //
  str << GLOBAL << "_MemMgr_INITIALIZER" << endl;
  str << "_MemMgr_INITIALIZER:" << endl;
  str << WORD << gc_init_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_COLLECTOR" << endl;
  str << "_MemMgr_COLLECTOR:" << endl;
  str << WORD << gc_collect_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_TEST" << endl;
  str << "_MemMgr_TEST:" << endl;
  str << WORD << (cgen_Memmgr_Test == GC_TEST) << endl;
}


//********************************************************
//
// Emit code to reserve space for and initialize all of
// the constants.  Class names should have been added to
// the string table (in the supplied code, is is done
// during the construction of the inheritance graph), and
// code for emitting string constants as a side effect adds
// the string's length to the integer table.  The constants
// are emmitted by running through the stringtable and inttable
// and producing code for each entry.
//
//********************************************************

void CgenClassTable::code_constants()
{
  //
  // Add constants that are required by the code generator.
  //
  stringtable.add_string("");
  inttable.add_string("0");

  stringtable.code_string_table(str,stringclasstag);
  inttable.code_string_table(str,intclasstag);
  code_bools(boolclasstag);
}


CgenClassTable::CgenClassTable(Classes classes, ostream& s) : nds(NULL) , str(s)
{
   enterscope();
   if (cgen_debug) cout << "Building CgenClassTable" << endl;
   install_basic_classes();
   install_classes(classes);
   build_inheritance_tree();
   first_pass();
   code();
   exitscope();
}

void CgenClassTable::install_basic_classes()
{

// The tree package uses these globals to annotate the classes built below.
  //curr_lineno  = 0;
  Symbol filename = stringtable.add_string("<basic class>");

//
// A few special class names are installed in the lookup table but not
// the class list.  Thus, these classes exist, but are not part of the
// inheritance hierarchy.
// No_class serves as the parent of Object and the other special classes.
// SELF_TYPE is the self class; it cannot be redefined or inherited.
// prim_slot is a class known to the code generator.
//
  addid(No_class,
	new CgenNode(class_(No_class,No_class,nil_Features(),filename),
			    Basic,this));
  addid(SELF_TYPE,
	new CgenNode(class_(SELF_TYPE,No_class,nil_Features(),filename),
			    Basic,this));
  addid(prim_slot,
	new CgenNode(class_(prim_slot,No_class,nil_Features(),filename),
			    Basic,this));

// 
// The Object class has no parent class. Its methods are
//        cool_abort() : Object    aborts the program
//        type_name() : Str        returns a string representation of class name
//        copy() : SELF_TYPE       returns a copy of the object
//
// There is no need for method bodies in the basic classes---these
// are already built in to the runtime system.
//
  install_class(
   new CgenNode(
    class_(Object, 
	   No_class,
	   append_Features(
           append_Features(
           single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
           single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
           single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	   filename),
    Basic,this));

// 
// The IO class inherits from Object. Its methods are
//        out_string(Str) : SELF_TYPE          writes a string to the output
//        out_int(Int) : SELF_TYPE               "    an int    "  "     "
//        in_string() : Str                    reads a string from the input
//        in_int() : Int                         "   an int     "  "     "
//
   install_class(
    new CgenNode(
     class_(IO, 
            Object,
            append_Features(
            append_Features(
            append_Features(
            single_Features(method(out_string, single_Formals(formal(arg, Str)),
                        SELF_TYPE, no_expr())),
            single_Features(method(out_int, single_Formals(formal(arg, Int)),
                        SELF_TYPE, no_expr()))),
            single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
            single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
	   filename),	    
    Basic,this));

//
// The Int class has no methods and only a single attribute, the
// "val" for the integer. 
//
   install_class(
    new CgenNode(
     class_(Int, 
	    Object,
            single_Features(attr(val, prim_slot, no_expr())),
	    filename),
     Basic,this));

//
// Bool also has only the "val" slot.
//
    install_class(
     new CgenNode(
      class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename),
      Basic,this));

//
// The class Str has a number of slots and operations:
//       val                                  ???
//       str_field                            the string itself
//       length() : Int                       length of the string
//       concat(arg: Str) : Str               string concatenation
//       substr(arg: Int, arg2: Int): Str     substring
//       
   install_class(
    new CgenNode(
      class_(Str, 
	     Object,
             append_Features(
             append_Features(
             append_Features(
             append_Features(
             single_Features(attr(val, Int, no_expr())),
            single_Features(attr(str_field, prim_slot, no_expr()))),
            single_Features(method(length, nil_Formals(), Int, no_expr()))),
            single_Features(method(concat, 
				   single_Formals(formal(arg, Str)),
				   Str, 
				   no_expr()))),
	    single_Features(method(substr, 
				   append_Formals(single_Formals(formal(arg, Int)), 
						  single_Formals(formal(arg2, Int))),
				   Str, 
				   no_expr()))),
	     filename),
        Basic,this));

}

// CgenClassTable::install_class
// CgenClassTable::install_classes
//
// install_classes enters a list of classes in the symbol table.
//
void CgenClassTable::install_class(CgenNodeP nd)
{
  Symbol name = nd->get_name();

  if (probe(name))
    {
      return;
    }

  // The class name is legal, so add it to the list of classes
  // and the symbol table.
  nds = new List<CgenNode>(nd,nds);
  addid(name,nd);
}

void CgenClassTable::install_classes(Classes cs)
{
  for(int i = cs->first(); cs->more(i); i = cs->next(i))
    install_class(new CgenNode(cs->nth(i),NotBasic,this));
}

//
// CgenClassTable::build_inheritance_tree
//
void CgenClassTable::build_inheritance_tree()
{
  for(List<CgenNode> *l = nds; l; l = l->tl())
      set_relations(l->hd());
}

//
// CgenClassTable::set_relations
//
// Takes a CgenNode and locates its, and its parent's, inheritance nodes
// via the class table.  Parent and child pointers are added as appropriate.
//
void CgenClassTable::set_relations(CgenNodeP nd)
{
  CgenNode *parent_node = probe(nd->get_parent());
  nd->set_parentnd(parent_node);
  parent_node->add_child(nd);
}

void CgenNode::add_child(CgenNodeP n)
{
  children = new List<CgenNode>(n,children);
}

void CgenNode::set_parentnd(CgenNodeP p)
{
  assert(parentnd == NULL);
  assert(p != NULL);
  parentnd = p;
}


void CgenClassTable::first_pass()
{
	root()->first_pass();
}

void CgenNode::first_pass()
{
	set_tag();
	if(parentnd && parentnd->get_methods()){
		methods = new FeatureList(parentnd->get_methods()->begin(),parentnd->get_methods()->end());
	}
	else{
		methods = new FeatureList();
	}
	if(parentnd && parentnd->get_attrs()){
		attrs = new FeatureList(parentnd->get_attrs()->begin(),parentnd->get_attrs()->end());
	}
	else{
		attrs = new FeatureList();
	}

	/* Process all the features of class(attrs,methods). */
  for(int i = features->first(); features->more(i); i = features->next(i)) {
		features->nth(i)->first_pass(methods, attrs);
		features->nth(i)->set_parent(name);
	}

  /* Add all symbols from parent as well as this class to
     symbol table */
  symtable = new CgenSymTable();
  symtable->enterscope();

  int offset = 0;
  if(cgen_debug)
    cout << "[INFO] Creating Symbol table for " << name << " class" << endl;
  for(FeatureList::iterator it = attrs->begin(); it != attrs->end(); ++it){
    if(cgen_debug)
      cout << "[INFO] Symbol: " << name << "." << (*it)->get_name() << ", Address: "
      << SELF << "+" << offset + DEFAULT_OBJFIELDS << endl;
    symtable->addid((*it)->get_name(), new SymbolInfo(SELF,offset+DEFAULT_OBJFIELDS));
    ++offset;
  }

	for(List<CgenNode> *l = children; l; l = l->tl()) {
		l->hd()->first_pass();
	}
}

void method_class::first_pass(FeatureListP methods, FeatureListP attrs)
{
	FeatureList::iterator it = lookup_feature_by_name(name,methods);
	if(it == methods->end()){
		methods->push_back(this);
	}
}

void attr_class::first_pass(FeatureListP methods, FeatureListP attrs)
{
	attrs->push_back(this);
}

void CgenNode::set_tag()
{
	tag = max_tag;
	++max_tag;
	if(name==Int){
		intclasstag = tag;
	}
	else if(name==Str){
		stringclasstag = tag;
	}
	else if(name==Bool){
		boolclasstag = tag;
	}
}

void CgenClassTable::code_protObj()
{
	root()->code_protObj(str);
}

void CgenNode::code_protObj(ostream &str)
{
	// Add -1 eye catcher
	str << WORD << "-1" << endl;
	emit_protobj_ref(name, str);
	str << LABEL;
	str << WORD << tag << endl
		<< WORD << DEFAULT_OBJFIELDS + attrs->size() << endl
		<< WORD;
	emit_disptable_ref(name,str);
	str << endl;
	for(FeatureList::iterator it = attrs->begin(); it != attrs->end(); ++it){
		(*it)->code_protObj(str);
	}
	for(List<CgenNode> *l = children; l; l = l->tl()) {
		l->hd()->code_protObj(str);
	}

}

/* Code for attributes in object prototype */
void attr_class::code_protObj(ostream &str)
{
	str << WORD;
  if (type_decl == Str) {
	/* For String attribute initialize it to empty string */
    StringEntry *string_entry = stringtable.lookup_string("");
    assert(string_entry);
    string_entry->code_ref(str);
  } else if (type_decl == Int) {
	/* For Int attribute initialize it to 0 */
    IntEntry *int_entry = inttable.lookup_string("0");
    assert(int_entry);
    int_entry->code_ref(str);
  } else if (type_decl == Bool) {
	/* For Bool attribute initialize it false */
    falsebool.code_ref(str);
  } else {
    str << 0;
  }
  str << endl;
}

CgenNodeP CgenClassTable::lookup_class_by_name(Symbol name)
{
  if (name == SELF_TYPE) {
    return curr_class;
  }

  for(List<CgenNode> *l = nds; l; l = l->tl()) {
    if (l->hd()->get_name() == name) {
      return l->hd();
    }
  }
  return NULL;
}

CgenNodeP CgenClassTable::lookup_class_by_tag(int tag)
{
	for(List<CgenNode> *l = nds; l; l = l->tl()) {
		if (l->hd()->get_tag() == tag) {
		return l->hd();
		}
	}
	return NULL;
}

/* A table, which at index (class tag) ∗ 4 contains a pointer
   to a String object containing the name of the class associated
*/

void CgenClassTable::code_class_nameTab()
{
	CgenNodeP cgen_node;
	str << CLASSNAMETAB << LABEL;
	/* Add entry in table by increasing tag number */
	for(int i = 0 ; i < max_tag; ++i){
		cgen_node = lookup_class_by_tag(i);
		assert(cgen_node);
		cgen_node->code_class_nameTab(str);
	}
}

/* Entry in class name table, pointing to a String object containing
   the name of the class associated
*/
void CgenNode::code_class_nameTab(ostream &str)
{
	StringEntry *string_entry = stringtable.lookup_string(name->get_string());
	assert(string_entry);
	str << WORD; string_entry->code_ref(str); str << endl;
}

/* A table, which at index (class tag) ∗ 8 contains a pointer to
   the prototype object and at index (class tag) ∗ 8 + 4 contains
   a pointer to the initialization method for that class.
*/
void CgenClassTable::code_class_ObjTab()
{
	CgenNodeP cgen_node;
	str << CLASSOBJTAB << LABEL;
	for(int i = 0 ; i < max_tag; ++i){
		cgen_node = lookup_class_by_tag(i);
		assert(cgen_node);
		cgen_node->code_class_ObjTab(str);
	}
}

/* Entry in class_ObjTab, pointes to prototype object and
   initialization block
*/
void CgenNode::code_class_ObjTab(ostream &str)
{
	str << WORD; emit_protobj_ref(name, str); str << endl;
	str << WORD; emit_init_ref(name, str); str << endl;
}

/* Dispatch tables */
void CgenClassTable::code_dispTab()
{
	root()->code_dispTab(str);
}

/* Dispatch tables of class */
void CgenNode::code_dispTab(ostream &str)
{
	emit_disptable_ref(name,str);
	str << LABEL;
	for(FeatureList::iterator it = methods->begin(); it != methods->end(); ++it){
		(*it)->code_dispTab(str);
	}

	for(List<CgenNode> *l = children; l; l = l->tl()) {
		l->hd()->code_dispTab(str);
	}
}

/* Dispatch table entry, reference to method */
void method_class::code_dispTab(ostream &str)
{
	str << WORD; emit_method_ref(parent,name,str);
	str << endl;
}

void CgenClassTable::code_class_methods()
{
  curr_classtable = this;
  for(List<CgenNode> *l = nds; l; l = l->tl()) {
    l->hd()->code_class_methods(str);
  }
}

void CgenNode::code_class_methods(ostream &str)
{
  curr_class = this;
  if(basic()){
    return ;
  }
  for(int i = features->first();
    features->more(i); i = features->next(i)){
    features->nth(i)->code_class_method(str);
  }
}

void method_class::code_class_method(ostream &str)
{
  // Emit label for method
  emit_method_ref(parent, name, str); str << LABEL;
  callee_callseq(str);

  /* Get symbol table and place formal parameters,
     Order of processing formal parameter should be
     same as pushing actual parameter
  */
  CgenSymTable *symtable = curr_class->get_symtable();
  symtable->enterscope();

  int offset = 0;
  int num_formals = formals->len();
  if(cgen_debug)
    cout << "[INFO] Processing formals of " << curr_class->get_name()
    << "." << name << " method" << endl;
  for(int i = formals->first();
    formals->more(i); i = formals->next(i)){
    /* Compute offset from current fp where actual arguments
       will get pushed. Arguments are push x1, x2, ... ,xn in
       this order
    */
    offset = FRAME_DEFAULT + num_formals - i -1;
    if(cgen_debug)
      cout << "[INFO] Symbol: " << formals->nth(i)->get_name()
           << "." << formals->nth(i)->get_name() << ", Address: "
           << "FP + " << offset << endl;
    symtable->addid(formals->nth(i)->get_name(),
      new SymbolInfo(FP, offset));
  }
  // Generate code for method body
  expr->code(str);
  symtable->exitscope();
  callee_callseq_ret(formals->len(),str);
}

void CgenClassTable::code_object_init()
{
  curr_classtable = this;
  for(List<CgenNode> *l = nds; l; l = l->tl()) {
    l->hd()->code_object_init(str);
  }
}

void CgenNode::code_object_init(ostream &str)
{
  curr_class = this;
  // Label for init method
  emit_init_ref(name, str); str << LABEL;
  callee_callseq(str);

  /* Initialize parent object, checking get_attrs to make sure parent is
     not No_Class
  */
  if(parentnd && parentnd->get_attrs()){
    str << JAL; emit_init_ref(parentnd->get_name(),str);
    str << endl;
  }

  // Process each attribute of class
  for(int i = features->first();
    features->more(i); i = features->next(i)){
    features->nth(i)->code_object_init(attrs, str);
  }

  // Move self to accumalator as a result(Optional)
  emit_move(ACC, SELF, str);
  callee_callseq_ret(0,str);
}

void attr_class::code_object_init(FeatureListP attrs, ostream &str)
{
  if (init->type){
    // Evaluate init expression
    init->code(str);

    // Get offset and store result from ACC to offset
    // This will overwride defalut value of attr
    int offset = get_attr_offset(name, attrs);
    emit_store(ACC, offset, SELF, str);
  }
}

void CgenClassTable::code()
{
  if (cgen_debug) cout << "[INFO] coding global data" << endl;
  code_global_data();

  if (cgen_debug) cout << "[INFO] choosing gc" << endl;
  code_select_gc();

  if (cgen_debug) cout << "[INFO] coding constants" << endl;
  code_constants();

  if (cgen_debug) cout << "[INFO] coding class_nameTab" << endl;
  code_class_nameTab();

  if(cgen_debug) cout << "[INFO] coding class_ObjTab" << endl;
  code_class_ObjTab();

   if (cgen_debug) cout << "[INFO] coding dispatch tables" << endl;
  code_dispTab();

  if (cgen_debug) cout << "[INFO] coding prototype objects" << endl;
  code_protObj();

  if (cgen_debug) cout << "[INFO] coding global text" << endl;
  code_global_text();

  if(cgen_debug) cout << "[INFO] coding object initializer" << endl;
  code_object_init();

  if (cgen_debug) cout << "[INFO] coding class methods" << endl;
  code_class_methods();

}


CgenNodeP CgenClassTable::root()
{
   return probe(Object);
}


///////////////////////////////////////////////////////////////////////
//
// CgenNode methods
//
///////////////////////////////////////////////////////////////////////

CgenNode::CgenNode(Class_ nd, Basicness bstatus, CgenClassTableP ct) :
   class__class((const class__class &) *nd),
   parentnd(NULL),
   children(NULL),
   basic_status(bstatus)
{ 
   stringtable.add_string(name->get_string());          // Add class name to string table
   methods = NULL;
   attrs = NULL;
}

////////////////////////////////////////////////////////////////////
//
// SymbolInfo methods
//
////////////////////////////////////////////////////////////////////

SymbolInfo::SymbolInfo(char *base_reg, int offset) :
    base_reg(base_reg),
    offset(offset){}

//******************************************************************
//
//   Fill in the following methods to produce code for the
//   appropriate expression.  You may add or remove parameters
//   as you wish, but if you do, remember to change the parameters
//   of the declarations in `cool-tree.h'  Sample code for
//   constant integers, strings, and booleans are provided.
//
//*****************************************************************

void assign_class::code(ostream &s) {
	expr->code(s);
	SymbolInfo *symbol_info = curr_class->get_symtable()->lookup(name);
	assert(symbol_info);
	emit_store(ACC, symbol_info->get_offset(), symbol_info->get_base_reg(),s);
}

/* Dispatch of typpe expr@type_name.name(actual) */
void static dispatch_common(Expression expr, Symbol type_name, Symbol name,
  Expressions actual, ostream &str)
{
  /* Evaluate all actual parameters and push them on stack
     Evaluation Order Semantics: x1, x2, xn
     Push Order Semantics: Same as formal parameter
  */
  for(int i = actual->first(); actual->more(i); i = actual->next(i)){
    actual->nth(i)->code(str);
    emit_push(ACC, str);
  }

  /* Evaluate exression */
  expr->code(str);

  // Branch to function definition
  emit_bne(ACC, ZERO, label_postfix, str);
  StringEntry *entry = stringtable.lookup_string(curr_class->get_filename()->get_string());
  assert(entry);
  // Load filename in ACC and line number in T1
  emit_load_string(ACC, entry, str);
  emit_load_imm(T1, curr_class->get_line_number(), str);
  emit_jal("_dispatch_abort", str); // Call will not be returned here

  emit_label_def(label_postfix, str); // Location to jump

  /* type_name is No_type in case of dynamic dispatch
	 ACC contains type of expr evaluated
  */

  /* Load proper dispatch table */
  if (type_name == No_type) {
    type_name = expr->get_type();
    emit_load(T1, DISPTABLE_OFFSET, ACC, str);
  }
  else{
    emit_partial_load_address(T1, str); emit_disptable_ref(type_name, str); str << endl;
  }

  /* Load method def address in T1 */
  int offset = get_method_offset(type_name, name);
  emit_load(T1, offset, T1, str);
  emit_jalr(T1, str); // Jump to method def
  label_postfix++;
}

void static_dispatch_class::code(ostream &s) {
  dispatch_common(expr, type_name, name, actual, s);
}

void dispatch_class::code(ostream &s) {
  dispatch_common(expr, No_type, name, actual, s);
}

/*
	Evaluate expression e1 and e2 and load it's int values in
	T1 and T2. Also it copies result of e2(Int Object). This can
	be used as temporary storage to store result of arith expr
*/
void static arith_common(Expression e1, Expression e2, ostream &s)
{
	e1->code(s); // Evaluate e1
	emit_fetch_int(ACC, ACC, s); // Load int result in ACC
	emit_push(ACC, s); // Push result on stack
	local_attr_offset++;
	e2->code(s); // EValuate e2
	emit_copy(s); // Make copy of e2 result object(Int)
	emit_load(T1, 1, SP, s); // Load int value of e1
	emit_addiu(SP, SP, WORD_SIZE, s); // Pop from stack
	local_attr_offset--;
	emit_fetch_int(T2, ACC, s); // Load int result of e2
}

void cond_class::code(ostream &s) {
}

void loop_class::code(ostream &s) {
}

void typcase_class::code(ostream &s) {
}

void block_class::code(ostream &s) {
	for(int i = body->first(); body->more(i);
		i = body->next(i)){
		body->nth(i)->code(s);
	}
}

void let_class::code(ostream &s) {
}

void plus_class::code(ostream &s) {
	arith_common(e1,e2,s); // Evaluate e1 and e2

	// add will cause trap on overflow, so use addu
	emit_addu(T1, T1, T2, s); // Add e1 and e2
	// store result in temporary created by arith_common
	emit_store_int(T1, ACC, s);
}

void sub_class::code(ostream &s) {
	arith_common(e1,e2,s); // Evaluate e1 and e2
	emit_sub(T1, T1, T2, s); // Sub e1 and e2
	// store result in temporary created by arith_common
	emit_store_int(T1, ACC, s);
}

void mul_class::code(ostream &s) {
	arith_common(e1,e2,s); // Evaluate e1 and e2
	emit_mul(T1, T1, T2, s); // Mul e1 and e2
	// store result in temporary created by arith_common
	emit_store_int(T1, ACC, s);
}

void divide_class::code(ostream &s) {
	arith_common(e1,e2,s); // Evaluate e1 and e2
	emit_div(T1, T1, T2, s); // div e1 and e2
	// store result in temporary created by arith_common
	emit_store_int(T1, ACC, s);
}

void neg_class::code(ostream &s) {
	e1->code(s); // Evaluate e1
	emit_copy(s); // Copy e1(Int Obj) as temp
	emit_fetch_int(T1, ACC, s); // Load int value
	emit_neg(T1, T1, s); // Negate value
	emit_store_int(T1, ACC, s); // Store value
}

void lt_class::code(ostream &s) {
}

void eq_class::code(ostream &s) {
}

void leq_class::code(ostream &s) {
}

void comp_class::code(ostream &s) {
}

void int_const_class::code(ostream& s)  
{
  //
  // Need to be sure we have an IntEntry *, not an arbitrary Symbol
  //
  emit_load_int(ACC,inttable.lookup_string(token->get_string()),s);
}

void string_const_class::code(ostream& s)
{
  emit_load_string(ACC,stringtable.lookup_string(token->get_string()),s);
}

void bool_const_class::code(ostream& s)
{
  emit_load_bool(ACC, BoolConst(val), s);
}

void new__class::code(ostream &s) {
	if(type_name == SELF_TYPE){
    // Load class table address
    emit_load_address(T1, CLASSOBJTAB, s);

    // Load class tag
    emit_load(T2, TAG_OFFSET, SELF, s);

    // Obj_Prot = CLASSOBJTAB + 2 * WORD_SIZE(4)
    emit_sll(T2, T2, 3, s); // Multiply Tag by 8(shift left logical)

    /* Not used S1 before this, so no need to preserve */
    emit_addu(S1, T1, T2, s); // Obj_Prot reference in S1
    emit_load(ACC, 0, S1, s); // Obj_Prot in ACC
    emit_copy(s); // Copy Obj_prot
    emit_load(T1, 1, S1, s); // Obj_init in T1
    emit_jalr(T1, s); // Initialize object
	}
	else if( type_name == Bool){
		// Load false Bool object address in ACC
		emit_load_bool( ACC, falsebool, s);
	}
	else{
		// Copy prototype object
		emit_partial_load_address(ACC, s);
		emit_protobj_ref(type_name,s);  s << endl;
		emit_copy(s);
		// Initialize prototype object
		s << JAL; emit_init_ref(type_name, s);
		s << endl;
	}
}

void isvoid_class::code(ostream &s) {
  // Evaluate e1
  e1->code(s);

  // If void(i.e. result zero) branch to label
  emit_beq(ACC, ZERO, label_postfix, s);

  // False branch(non-void expr)
  emit_load_bool(ACC, falsebool, s);
  emit_branch(label_postfix + 1, s); // Jump to end of label

  // True branch(void expr)
  emit_label_def(label_postfix, s);
  emit_load_bool(ACC, truebool, s);

  // End of branching
  emit_label_def(label_postfix + 1, s);
  label_postfix += 2;
}

void no_expr_class::code(ostream &s) {
	emit_load_imm(ACC, 0, s);
}

void object_class::code(ostream &s) {
  if (name == self) {
    emit_move(ACC, SELF, s);
  } else {
    SymbolInfo *symbol_info = curr_class->get_symtable()->lookup(name);
    assert(symbol_info);
    emit_load(ACC, symbol_info->get_offset(), symbol_info->get_base_reg(), s);
  }
}


