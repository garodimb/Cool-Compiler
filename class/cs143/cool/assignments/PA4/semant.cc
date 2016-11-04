#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "semant.h"
#include "utilities.h"


extern int semant_debug;
extern char *curr_filename;

//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
static Symbol
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

/* Keep pointer to curr_class and classTable */
static ClassTable* classtable;
static Class_ curr_class;
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

ClassTable::ClassTable(Classes classes) : semant_errors(0) , error_stream(cerr) {
	class_symtable = new ClassSymTable;
}

/* lookup in class_symtable for class_name and return it */
Class_ ClassTable::lookup_class(Symbol class_name){
	ClassSymTable::iterator it = class_symtable->find(class_name);
	Class_ class_ = NULL;
	if(it!=class_symtable->end()){
		class_ = it->second;
	}
	return class_;
}

void ClassTable::install_classes(Classes classes){

	/* Install perdefined classes in class_symbol table */
	install_basic_classes();

	/* Install user defined classes in class_symtable */
    for(int i = classes->first(); classes->more(i); i = classes->next(i)){
		Class_ class_ = classes->nth(i);
		Symbol class_name = class_->getName();

		/* Check whether class_name is valid */
		if(class_name == SELF_TYPE){
			ostream &err_stream = semant_error(class_);
			err_stream << "Class name " << class_name << " is not allowed" << endl;
		}
		/* Check whether class_name class is already defined */
		else if(lookup_class(class_name)!=NULL){
			ostream &err_stream = semant_error(class_);
			err_stream << "Class " << class_name << " is already defined" << endl;
		}
		else{
			class_symtable->insert(std::pair<Symbol,Class_>(class_->getName(),class_));
		}
	}

	/* Check whether parent of each class is already defined */
	for(int i = classes->first(); classes->more(i); i = classes->next(i)){
		Class_ class_ = classes->nth(i);
		Symbol parent_name = class_->getParent();
		Symbol class_name = class_->getName();
		if(lookup_class(parent_name)==NULL){
			ostream &err_stream = semant_error(class_);
			err_stream << "Undefined class " << parent_name << endl;
		}
		else if(parent_name == Bool || parent_name == Int || parent_name == Str || parent_name == SELF_TYPE){
			ostream &err_stream = semant_error(class_);
			err_stream << "Class " << class_name << " cannot inherit class " << parent_name << "." << endl;
		}
	}
	}

/* Install symbols from different classes to symbol_table and method_table */
void ClassTable::install_symbols(Classes classes){
	//install_basic_symbols();
	Class_ class_;
	for(ClassSymTable::iterator it = class_symtable->begin(); it != class_symtable->end(); ++it){
		class_ = it->second;
		class_->install_symbols();
	}
}

/* Traverse each class and then install every available features */
void class__class::install_symbols(){
	object_table->enterscope();
	curr_class = this;
	for(int i = features->first(); features->more(i); i = features->next(i)){
		features->nth(i)->install_symbols();
	}
}

/* Install method in method_table */
void method_class::install_symbols(){
	MethodTable *method_table = curr_class->getMethodTable();
	if(name==self){
		ostream& err_stream = classtable->semant_error(curr_class);
		err_stream << "Method " << name << " is not allowed." << endl;
	}
	else if(method_table->find(name)!=method_table->end()){
		ostream& err_stream = classtable->semant_error(curr_class);
		err_stream << "Method " << name << " is multiply defined in class." << endl;
	}
	else{
		method_table->insert(std::pair<Symbol,Feature>(name,this));
	}
}

/* Install attr in object_table */
void attr_class::install_symbols(){
	ObjectTable *object_table = curr_class->getObjectTable();
	if(name==self){
		ostream& err_stream = classtable->semant_error(curr_class);
		err_stream << "Varible " << name << " is not allowed" << endl;
	}
	else if(object_table->probe(name)!=NULL){
		ostream& err_stream = classtable->semant_error(curr_class);
		err_stream << "Attribute " << name << " is multiply defined in class." << endl;
	}
	else{
		object_table->addid(name,new Symbol(type_decl));
	}
}

/* Check whether Main class exists and has main method defined in it */
bool ClassTable::is_main_present(){
	Class_ main_class = lookup_class(Main);
	if(main_class==NULL){
		ostream& err_stream = semant_error();
		err_stream << "Class Main is not defined." << endl;
		return false;
	}
	else{
		MethodTable *method_table = main_class->getMethodTable();
		if(method_table->find(main_meth) == method_table->end()){
			ostream& err_stream = semant_error();
			err_stream << "No 'main' method in class Main." << endl;
			return false;
		}
	}
	return true;
}

/* Lookup method method_name in class class_name */
Feature ClassTable::lookup_method(Symbol method_name, Symbol class_name){
	Class_ class_ = lookup_class(class_name);
	Feature feature = NULL;
	if(class_!= NULL){
		feature = class_->getMethod(method_name);
	}
	return feature;
}

/* Lookup attribute attr_name in class class_name */
Symbol ClassTable::lookup_attr(Symbol attr_name, Symbol class_name){
	Class_ class_ = lookup_class(class_name);
	Symbol attr = NULL;
	if(class_!=NULL){
		attr = class_->getAttr(attr_name);
	}
	return attr;
}

/* Get method from this class or it's parent class */
Feature class__class::getMethod(Symbol method_name){
	MethodTable::iterator imeth = method_table->find(method_name);
	Feature method = NULL;
	if(imeth != method_table->end()){
		method = imeth->second;
	}
	else if(parent != No_class){
		Class_ class_parent = classtable->lookup_class(parent);
		method = class_parent->getMethod(method_name);
	}
	return method;
}

/* Get attribute from this class or it's parent class */
Symbol class__class::getAttr(Symbol attr_name){
	Symbol type_decl = *(object_table->probe(attr_name));
	if(type_decl==NULL){
		if(parent != No_class){
			Class_ class_parent = classtable->lookup_class(parent);
			type_decl = class_parent->getAttr(attr_name);
		}
	}
	return type_decl;
}

void ClassTable::install_basic_classes() {

    // The tree package uses these globals to annotate the classes built below.
   // curr_lineno  = 0;
    Symbol filename = stringtable.add_string("<basic class>");

    // The following demonstrates how to create dummy parse trees to
    // refer to basic Cool classes.  There's no need for method
    // bodies -- these are already built into the runtime system.

    // IMPORTANT: The results of the following expressions are
    // stored in local variables.  You will want to do something
    // with those variables at the end of this method to make this
    // code meaningful.

    //
    // The Object class has no parent class. Its methods are
    //        abort() : Object    aborts the program
    //        type_name() : Str   returns a string representation of class name
    //        copy() : SELF_TYPE  returns a copy of the object
    //
    // There is no need for method bodies in the basic classes---these
    // are already built in to the runtime system.

    Class_ Object_class =
	class_(Object,
	       No_class,
	       append_Features(
			       append_Features(
					       single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
					       single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
			       single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	       filename);
	class_symtable->insert(std::pair<Symbol,Class_>(Object,Object_class));    //
    // The IO class inherits from Object. Its methods are
    //        out_string(Str) : SELF_TYPE       writes a string to the output
    //        out_int(Int) : SELF_TYPE            "    an int    "  "     "
    //        in_string() : Str                 reads a string from the input
    //        in_int() : Int                      "   an int     "  "     "
    //
    Class_ IO_class =
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
	       filename);
	class_symtable->insert(std::pair<Symbol,Class_>(IO,IO_class));
    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer.
    //
    Class_ Int_class =
	class_(Int,
	       Object,
	       single_Features(attr(val, prim_slot, no_expr())),
	       filename);
	class_symtable->insert(std::pair<Symbol,Class_>(Int,Int_class));
    //
    // Bool also has only the "val" slot.
    //
    Class_ Bool_class =
	class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename);
	class_symtable->insert(std::pair<Symbol,Class_>(Bool,Bool_class));
    //
    // The class Str has a number of slots and operations:
    //       val                                  the length of the string
    //       str_field                            the string itself
    //       length() : Int                       returns length of the string
    //       concat(arg: Str) : Str               performs string concatenation
    //       substr(arg: Int, arg2: Int): Str     substring selection
    //
    Class_ Str_class =
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
	       filename);
	class_symtable->insert(std::pair<Symbol,Class_>(Str,Str_class));
}

////////////////////////////////////////////////////////////////////
//
// semant_error is an overloaded function for reporting errors
// during semantic analysis.  There are three versions:
//
//    ostream& ClassTable::semant_error()
//
//    ostream& ClassTable::semant_error(Class_ c)
//       print line number and filename for `c'
//
//    ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
//       print a line number and filename
//
///////////////////////////////////////////////////////////////////

ostream& ClassTable::semant_error(Class_ c)
{
    return semant_error(c->get_filename(),c);
}

ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
{
    error_stream << filename << ":" << t->get_line_number() << ": ";
    return semant_error();
}

ostream& ClassTable::semant_error()
{
    semant_errors++;
    return error_stream;
}



/*   This is the entry point to the semantic checker.

     Your checker should do the following two things:

     1) Check that the program is semantically correct
     2) Decorate the abstract syntax tree with type information
        by setting the `type' field in each Expression node.
        (see `tree.h')

     You are free to first do 1), make sure you catch all semantic
     errors. Part 2) can be done in a second stage, when you want
     to build mycoolc.
 */
void program_class::semant()
{
    initialize_constants();

    /* ClassTable constructor may do some semantic analysis */
    ClassTable *classtable = new ClassTable(classes);
    ::classtable = classtable;
    /* some semantic analysis code may go here */
    classtable->install_classes(classes);
    classtable->install_symbols(classes);
    classtable->is_main_present();
    if (classtable->errors()) {
	cerr << "Compilation halted due to static semantic errors." << endl;
	exit(1);
    }
}
