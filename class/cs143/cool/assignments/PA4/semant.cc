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
	else if(object_table->lookup(name)!=NULL){
		ostream& err_stream = classtable->semant_error(curr_class);
		err_stream << "Attribute " << name << " is multiply defined in class." << endl;
	}
	else{
		object_table->addid(name,&(type_decl));
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

/* Get method from this class or it's parent(ancestor) class */
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

/* Get attribute from this class or it's parent(ancestor) class */
Symbol class__class::getAttr(Symbol attr_name){
	Symbol *type_decl = object_table->lookup(attr_name);
	if(type_decl==NULL){
		if(parent != No_class){
			Class_ class_parent = classtable->lookup_class(parent);
			return class_parent->getAttr(attr_name);
		}
		else{
			return NULL;
		}
	}
	return *type_decl;
}

/* Compare two method by using singatures */
bool method_class::is_equal_feature(Feature method){
	Formals method_formals = method->getFormals();

	/* Check whether both methods have same number of
		formal arguments */
	if(formals->len()!=method_formals->len()){
		return false;
	}

	/* Check whether return type of both methods are same */
	else if(getReturnType()!=method->getReturnType()){
		return false;
	}

	/* Check whether all formal arguments have same type for both methods */
	else{
		int i, j;
		i = formals->first();
		j = method_formals->first();

		/* Get declare type of each formal argument and check whether they
			are same */
		while(formals->more(i) && method_formals->more(j)){
			if(formals->nth(i)->getTypedecl()!=method_formals->nth(j)->getTypedecl()){
				return false;
			}
			i = formals->next(i);
			j = method_formals->next(j);
		}
	}
	return true;
}

/* Compare two attributes using declare type*/
bool attr_class::is_equal_feature(Feature attr){
	if(type_decl == attr->getTypedecl()){
		return true;
	}
	return false;
}

/* Check if attribute is redfined in base class */
bool attr_class:: check_features()
{
	bool err_flag = true;
	Symbol class_parent_name = curr_class->getParent();
	if(class_parent_name != No_class){

		/* Check whether attribute with same name and type is defined
			in ancestor class */
		Class_ class_parent = classtable->lookup_class(class_parent_name);
		if(class_parent->getAttr(name)){
			ostream& err_stream = classtable->semant_error(curr_class->getFileName(),this);
			err_stream << "Attribute " << name << " of class "
			<< curr_class->getName() << " is already defined in parent class "
		    << class_parent_name << "." << endl;
			err_flag = false;
		}
	}
	return err_flag;
}

/*  Check whether method if redfined in child class has same
	signature as that of parent */
bool method_class:: check_features(){
	bool err_flag = true;
	/* Get parent class */
	Symbol class_parent_name = curr_class->getParent();
	if(class_parent_name != No_class){
		Class_ class_parent = classtable->lookup_class(class_parent_name);

		/* Check if method with same name of this is present in ancestor
		   of class in which this method is defined. If defined check whether
		   siganture of both the methods are same */
		Feature parent_method = class_parent->getMethod(name);
		if(parent_method && !(is_equal_feature(parent_method))){
				ostream& err_stream = classtable->semant_error(curr_class);
				err_stream <<"Method " << name << " of parent class "
						   << class_parent->getName() << " is redefined with different signature in child class "
						   << curr_class->getName() << "." << endl;
				err_flag = true;
			}
	}
	return err_flag;
}

/* Check all features follows semantic rules of inheritance */
bool class__class::check_features(){
	bool err_flag = true;
	curr_class = this;

	/*Iterate over all features(attributes and methods and check) */
	for(int i = features->first(); features->more(i); i = features->next(i)){
		if(!features->nth(i)->check_features()){
			err_flag = false;
		}
	}
	return err_flag;
}

/* 	Check whether features(attributes and methods) are properly
	declared in child and parent class of all classes*/
bool ClassTable::check_features(){
	bool err_flag = true;
	/* Iterate over all classes and check its features */
	for(ClassSymTable::iterator cl_it = class_symtable->begin();
		cl_it != class_symtable->end(); ++cl_it){
		if(!cl_it->second->check_features()){
			err_flag = false;
		}
	}
	return err_flag;
}

Symbol ClassTable::lub(Symbol type1, Symbol type2){
	std::map<Symbol,bool> hash_table;
	Class_ class_ = lookup_class(type1);
	Class_ class_parent_name = NULL;

	if(type1 == No_type){
		return type2;
	}
	else if(type2 == No_type){
		return type1;
	}
	while(class_->getParent() != No_class){
		hash_table[class_->getName()] = true;
		class_ = lookup_class(class_->getParent());
	}
	class_ = lookup_class(type2);

	while(class_->getParent() != No_class){
		if(hash_table.find(class_->getName())!=hash_table.end()){
			return class_->getName();
		}
		class_ = lookup_class(class_->getParent());
	}
	return Object;
}

bool ClassTable::is_sub_type(Symbol parent, Symbol child){
	if(child == No_type || child == parent){
		return true;
	}
	Class_ class_ = classtable->lookup_class(child);
	Symbol class_name = class_->getName();
	Symbol class_parent_name = class_->getParent();
	while(class_parent_name != No_class){
		if(class_name == parent){
			return true;
		}
		class_ = classtable->lookup_class(class_parent_name);
		class_name = class_->getName();
		class_parent_name = class_->getParent();
	}
	return false;
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
    classtable->check_features();
    if (classtable->errors()) {
		cerr << "Compilation halted due to static semantic errors." << endl;
		exit(1);
    }
    classtable->semant();
    if (classtable->errors()) {
		cerr << "Compilation halted due to type errors." << endl;
		exit(1);
    }
}

void ClassTable::semant(){
	for(ClassSymTable::iterator cl_it = class_symtable->begin();
		cl_it != class_symtable->end(); ++cl_it){
		if(cl_it->first == Object || cl_it->first == Int || cl_it->first == Bool
			|| cl_it->first == IO || cl_it->first == Str){
			continue ;
		}
		cl_it->second->semant();
	}
}

void class__class::semant(){
	curr_class = this;
	// cout << " Class: " << name << endl;
	for(int i = features->first(); features->more(i); i = features->next(i)){
		features->nth(i)->semant();
	}
}

void method_class::semant(){
	// // cout << " Method: " << name << endl;
	/* Are we entering in scope? */
	ObjectTable *object_table = curr_class->getObjectTable();
	object_table->enterscope();
	for(int i = formals->first();
		formals->more(i); i = formals->next(i)){
		formals->nth(i)->semant();
	}
	expr->semant();
	if(return_type != expr->get_type()){
		ostream& err_stream = classtable->semant_error(curr_class->get_filename(),this);
		err_stream << "Inferred return type of method " << expr->get_type()
		<< " does not match with declared return type " << return_type  << "." << endl;
	}
	object_table->exitscope();
}

void attr_class::semant(){
	// // cout << " Attr: " << name << endl;
	init->semant();
	if(init->get_type()!=No_type && !classtable->is_sub_type(type_decl,init->get_type())){
		ostream& err_stream = classtable->semant_error(curr_class->get_filename(),this);
		err_stream << "Cannot assign " << init->get_type() << " to "
		<< type_decl << " for identifier " << name << "." << endl;
	}
}

void formal_class::semant(){
	// // cout << " Formal: " << name << endl;
	if(name == self){
		ostream& err_stream = classtable->semant_error(curr_class->get_filename(),this);
		err_stream << "'self' cannot be name of formal parameter" << endl;
	}
	if(type_decl == SELF_TYPE){
		ostream& err_stream = classtable->semant_error(curr_class->get_filename(),this);
		err_stream << " Formal parameter " << name << " cannot have type SELF_TYPE." << endl;
	}
	else if(classtable->lookup_class(type_decl)==NULL){
		ostream& err_stream = classtable->semant_error(curr_class->get_filename(),this);
		err_stream << " Undefined class " << type_decl << " in formal parameter." << endl;
	}
	ObjectTable *object_table = curr_class->getObjectTable();
	if(object_table->probe(name) != NULL){
		ostream& err_stream = classtable->semant_error(curr_class->get_filename(),this);
		err_stream << " Formal parameter " << name << "redfined." << endl;
	}
	object_table->addid(name,&(type_decl));
}

void branch_class::semant(){
	// // cout << " Branch " << name << endl;
	expr->semant();
}

void assign_class::semant(){
	// cout << " Assign: " << name << endl;
	expr->semant();
	Symbol type_assigned = expr->get_type();
	Symbol type_decl = classtable->lookup_attr(name,curr_class->getName());
	type = No_type;
	if(type_decl==NULL){
		ostream& err_stream = classtable->semant_error(curr_class->get_filename(),this);
		err_stream << "Assignment to undeclared identifier " << name << "." << endl;
	}
	else if(!classtable->is_sub_type(type_decl,type_assigned)){
		ostream& err_stream = classtable->semant_error(curr_class->get_filename(),this);
		err_stream << "Cannot assign " << type_assigned << " to "
		<< type_decl << " for identifier " << name << "." << endl;
	}
	else{
		type = type_decl;
	}
}

void static_dispatch_class::semant(){
	expr->semant();
	Symbol class_name = expr->get_type();
	type = No_type;
	if(!classtable->is_sub_type(type_name,class_name)){
		ostream &err_stream = classtable->semant_error(curr_class->get_filename(),this);
		err_stream << "Type " << class_name << " is not subtype of "
		<< type_name << "." << endl;
		return;
	}
	Feature method = classtable->lookup_method(name,type_name);
	if(method == NULL){
		ostream &err_stream = classtable->semant_error(curr_class->get_filename(),this);
		err_stream << "Method " << name << " is not defined in class "
		<< type_name << "." << endl;
		return ;
	}
	for(int i = actual->first();
		actual->more(i); i = actual->next(i)){
		actual->nth(i)->semant();
	}

	Formals formals = method->getFormals();
	if(formals->len()!=actual->len()){
		ostream &err_stream = classtable->semant_error(curr_class->get_filename(),this);
		err_stream << "Method " << name << " is called with wrong number"
		<< " of arguments." << endl;
		return ;
	}
	/* Check whether all formal arguments have same type for both methods */
	int i, j;
	Symbol type_decl, type_passed;
	i = formals->first();
	j = actual->first();

	/* Get declare type of each formal argument and check whether they
		are same */
	while(formals->more(i) && actual->more(j)){
		type_decl = formals->nth(i)->getTypedecl();
		type_passed = actual->nth(i)->get_type();
		if(!classtable->is_sub_type(type_decl,type_passed)){
			ostream &err_stream = classtable->semant_error(curr_class->get_filename(),this);
			err_stream << "Passed wrong arugment, expecting "
			<< type_decl << ", passed " << type_passed << "." << endl;
		}
		i = formals->next(i);
		j = actual->next(j);
	}
	type = method->getTypedecl();
}

void dispatch_class::semant(){
	// cout << " Dispatch: " << name << endl;
	expr->semant();
	Symbol class_name = expr->get_type();
	type = No_type;
	Feature method = classtable->lookup_method(name,class_name);
	if(method == NULL){
		ostream &err_stream = classtable->semant_error(curr_class->get_filename(),this);
		err_stream << "Method " << name << " is not defined in class "
		<< class_name << "." << endl;
		return ;
	}
	for(int i = actual->first();
		actual->more(i); i = actual->next(i)){
		actual->nth(i)->semant();
	}

	Formals formals = method->getFormals();
	if(formals->len()!=actual->len()){
		ostream &err_stream = classtable->semant_error(curr_class->get_filename(),this);
		err_stream << "Method " << name << " is called with wrong number"
		<< " of arguments." << endl;
		return ;
	}
	/* Check whether all formal arguments have same type for both methods */
	int i, j;
	Symbol type_decl, type_passed;
	i = formals->first();
	j = actual->first();

	/* Get declare type of each formal argument and check whether they
		are same */
	while(formals->more(i) && actual->more(j)){
		type_decl = formals->nth(i)->getTypedecl();
		type_passed = actual->nth(i)->get_type();
		if(!classtable->is_sub_type(type_decl,type_passed)){
			ostream &err_stream = classtable->semant_error(curr_class->get_filename(),this);
			err_stream << "Passed wrong arugment, expecting "
			<< type_decl << ", passed " << type_passed << "." << endl;
		}
		i = formals->next(i);
		j = actual->next(j);
	}
	type = method->getTypedecl();
}

void cond_class::semant(){
	// cout << " Condition: " << "No name" << endl;
	pred->semant();
	then_exp->semant();
	else_exp->semant();
	type = classtable->lub(then_exp->get_type(),else_exp->get_type());
	if(pred->get_type()!=Bool){
		ostream& err_stream = classtable->semant_error(curr_class->get_filename(),this);
		err_stream << "Predicate is of type " << pred->get_type()
		<<", required Bool." << endl;
	}
}

void loop_class::semant(){
	// cout << " Loop: " << "No name" << endl;
	pred->semant();
	body->semant();
	type = Object;
	if(pred->get_type()!=Bool){
		ostream& err_stream = classtable->semant_error(curr_class->get_filename(),this);
		err_stream << "Predicate is of type " << pred->get_type()
		<<", required Bool." << endl;
	}
}

void typcase_class::semant(){
	// cout << "Type Case: " << "No name" << endl;
	expr->semant();
	Case case_ = NULL;
	Expression expr;
	type = No_type;
	/* @TODO Check for duplicate cases */
	for(int i = cases->first(); cases->more(i); i = cases->next(i)){
		expr = cases->nth(i)->getExpr();
		expr->semant();
		if(i==cases->first()){
			type = expr->get_type();
		}
		else{
			type = classtable->lub(type,expr->get_type());
		}
	}
}

void block_class::semant(){
	// cout << " Block: " << "No name" << endl;
	type = No_type;
	for(int i = body->first();
		body->more(i); i = body->next(i)){

		body->nth(i)->semant();
		type = body->nth(i)->get_type();
	}
}

void let_class::semant(){
	// cout << "Let: " << identifier << endl;
	init->semant();
	type = No_type;
	ObjectTable *object_table = curr_class->getObjectTable();
	if(init->get_type()!=No_type && !classtable->is_sub_type(type_decl,init->get_type())){
		ostream& err_stream = classtable->semant_error(curr_class->get_filename(),this);
		err_stream << "Cannot assign " << init->get_type() << " to "
		<< type_decl << " for identifier " << identifier << "." << endl;
	}
	object_table->enterscope();
	object_table->addid(identifier,&type_decl);
	body->semant();
	object_table->exitscope();
	type = body->get_type();
}

void plus_class::semant(){
	// cout << " Plus: " << "No name" << endl;
	e1->semant();
	e2->semant();
	type = Int;
	if(e1->get_type()!=Int || e2->get_type()!=Int){
		ostream& err_stream = classtable->semant_error(curr_class->get_filename(),this);
		err_stream << "Non-int argument, " << e1->get_type() << " + "
		<< e2->get_type() <<" used with +." << endl;
	}
}

void sub_class::semant(){
	// cout << " Subtraction: " << "No name" << endl;
	e1->semant();
	e2->semant();
	type = Int;
	if(e1->get_type()!=Int || e2->get_type()!=Int){
		ostream& err_stream = classtable->semant_error(curr_class->get_filename(),this);
		err_stream << "Non-int argument, " << e1->get_type() << " - "
		<< e2->get_type() <<" used with -." << endl;
	}
}

void mul_class::semant(){
	// cout << " Multiplication: " << "No name" << endl;
	e1->semant();
	e2->semant();
	type = Int;
	if(e1->get_type()!=Int || e2->get_type()!=Int){
		ostream& err_stream = classtable->semant_error(curr_class->get_filename(),this);
		err_stream << "Non-int argument, " << e1->get_type() << " * "
		<< e2->get_type() <<" used with *." << endl;
	}
}

void divide_class::semant(){
	// cout << " Divide: " << "No name" << endl;
	e1->semant();
	e2->semant();
	type = Int;
	if(e1->get_type()!=Int || e2->get_type()!=Int){
		ostream& err_stream = classtable->semant_error(curr_class->get_filename(),this);
		err_stream << "Non-int argument, " << e1->get_type() << " / "
		<< e2->get_type() <<" used with /." << endl;
	}
}

void neg_class::semant(){
	// cout << " Negation: " << "No name" << endl;
	e1->semant();
	type = Int;
	if(e1->get_type() != Int){
		ostream& err_stream = classtable->semant_error(curr_class->get_filename(),this);
		err_stream << "Non-int arguments used with ~." << endl;
	}
}

void lt_class::semant(){
	// cout << " Less than: " << "No name" << endl;
	e1->semant();
	e2->semant();
	type = Bool;
	if(e1->get_type() != Int || e2->get_type() != Int){
		ostream& err_stream = classtable->semant_error(curr_class->get_filename(),this);
		err_stream << "Non-int arguments used with < ." << endl;
	}
}

void eq_class::semant(){
	// cout << " Equal to: " << "No name" << endl;
	e1->semant();
	e2->semant();
	Symbol e1_type = e1->get_type();
	Symbol e2_type = e2->get_type();
	type = Bool;
	/* Both e1_type and e2_type is Int or Bool or Str and e1_type == e2_type */
	if( e1_type != e2_type){
		ostream& err_stream = classtable->semant_error(curr_class->get_filename(),this);
		err_stream << "Comparing two different types: " << e1_type << " and "
		<< e2_type << "." << endl;
		return;
	}
	if(!(e1_type == Int || e1_type == Bool || e1_type == Str)){
		ostream& err_stream = classtable->semant_error(curr_class->get_filename(),this);
		err_stream << "Comparison allowed ony with basic types, used " << e1_type
		<< "." << endl;
		return ;
	}
}

void leq_class::semant(){
	// cout << " Less than equal to: " << "No name" << endl;
	e1->semant();
	e2->semant();
	type = Bool;
	if(e1->get_type() != Int || e2->get_type() != Int){
		ostream& err_stream = classtable->semant_error(curr_class->get_filename(),this);
		err_stream << "Non-int arguments used with <=." << endl;
	}
	/* Always assign type as bool, in case of error it's recovery technique */
}

void comp_class::semant(){
	// cout << " NOT: " << "No name" << endl;
	e1->semant();
	type = Bool;
	if(e1->get_type()!=Bool){
		ostream& err_stream = classtable->semant_error(curr_class->get_filename(),this);
		err_stream << "Not is used with " << e1->get_type()
		 << " instead of Bool" << endl;
		type = No_type;
	}
}

void int_const_class::semant(){
	// cout << " INT_CONST: " << token << endl;
	type = Int;
}

void bool_const_class::semant(){
	// cout << " BOOL_CONST: " << val << endl;
	type = Bool;
}

void string_const_class::semant(){
	// cout << " STR_CONST: " << token << endl;
	type = Str;
}

void new__class::semant(){
	/* @TODO: Check for SELF_TYPE rule */
	// cout << " NEW: " << type_name << endl;
	type = type_name;
	if(classtable->lookup_class(type_name)==NULL){
		ostream& err_stream = classtable->semant_error(curr_class->get_filename(),this);
		err_stream << "New is used with undefined class " << type_name << "." << endl;
		type = No_type;
	}
}

void isvoid_class::semant(){
	// cout << " is_void: " << "No name" << endl;
	e1->semant();
	type = Bool;
}

void no_expr_class::semant(){
	// cout << " No_expr: " << "No name" << endl;
	type = No_type;
}

void object_class::semant(){
	// cout << " OBJECT: " << name << endl;
	type = No_type;
	if(name == self){
		type = curr_class->getName();
		return;
	}
	type = classtable->lookup_attr(name,curr_class->getName());
	if(type==NULL){
		ostream& err_stream = classtable->semant_error(curr_class->get_filename(),this);
		err_stream << "Undeclared indentifier " << name << "." << endl;
		type = No_type;
	}
}
