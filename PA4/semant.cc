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

    install_basic_classes();

    bool seen_Main = false;

    // check for duplicate classes in class tree, and verify Main is a class in class tree
    for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
        Class_ n = classes->nth(i);

        Symbol filename = n->get_filename();
        Symbol name = n->get_name();
        Symbol parent = n->get_parent();

        if (semant_debug) {
            error_stream << "Filename: " << filename << endl;
            error_stream << "Sym Name: " << name << endl;
            error_stream << "Parent  : " << parent << endl;
        }

        if (name == SELF_TYPE)
            semant_error(filename, n) << "Can't name class SELF_TYPE" << endl;

        if (name == Main) seen_Main = true;

        if (parent == Bool || parent == Int || parent == SELF_TYPE || parent == Str)
            semant_error(filename, n) << "Can't inherit from " << parent << endl;

        if (class_lookup.count(name))
            // already in set, duplicate
            semant_error(filename, n) << "Duplicate class definition" << endl;
        else
            class_lookup[name] = n;
    }

    if (!seen_Main) semant_error() << "Class Main is not defined." << endl;

    for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
        Class_ n = classes->nth(i);
        Symbol parent = n->get_parent();
        Symbol filename = n->get_filename();

        if (class_lookup.count(parent) == 0)
            // inherits from undefined parent
            semant_error(filename, n) << "Undefined parent inheritance" << endl;
        else {
            // add decendant to parent's inheritanceSet
            inheritance_set[class_lookup[parent]].insert(n);
        }
    }

    if (semant_debug) dump_inheritance();

    for (std::map<Class_,std::set<Class_> >::iterator it_p = inheritance_set.begin(); it_p != inheritance_set.end() && !semant_errors; it_p++) {
        if (semant_debug)
            error_stream << (it_p->first)->get_name() << endl;

        std::set<Class_> mark_set;
        check_for_cycles(it_p->first, mark_set, 0);
    }

    //create symbol table
    sym_tab = new SymbolTable<Symbol, Symbol>();
}

void ClassTable::check_for_cycles(Class_ parent, std::set<Class_> mark_set, int depth) {

    if (semant_debug) {
        for (int i = 0; i < depth; i++) error_stream << "  ";
        error_stream << parent->get_name() << endl;
    }

    //attempt to mark class
    if (mark_set.count(parent)) {
        semant_error() << "Class already traversed in Class Cycle Checker. Not Acyclic!" << endl;
        return;
    } else {
        mark_set.insert(parent);
    }

    std::set<Class_> child_set = inheritance_set[parent];
    for (std::set<Class_>::iterator it_c = child_set.begin(); it_c != child_set.end(); it_c++) {
        check_for_cycles(*it_c, mark_set, depth + 1);
    }
}

void ClassTable::dump_inheritance() {
    error_stream << "Inheritance: " << endl;

    for (std::map<Class_,std::set<Class_> >::iterator it_p = inheritance_set.begin(); it_p != inheritance_set.end(); it_p++) {
        error_stream << "  Parent: " << (it_p->first)->get_name() << endl;
        for (std::set<Class_>::iterator it_c = (it_p->second).begin(); it_c != (it_p->second).end(); it_c++) {
            error_stream << "    Child: " << (*it_c)->get_name() << endl;
        }
    }
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


    //
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


    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer.
    //
    Class_ Int_class =
    class_(Int,
           Object,
           single_Features(attr(val, prim_slot, no_expr())),
           filename);

    //
    // Bool also has only the "val" slot.
    //
    Class_ Bool_class =
    class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename);

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

    //Add all the base classes to the class looker-upper
    class_lookup[Object_class->get_name()] = Object_class;
    class_lookup[IO_class->get_name()] = IO_class;
    class_lookup[Int_class->get_name()] = Int_class;
    class_lookup[Bool_class->get_name()] = Bool_class;
    class_lookup[Str_class->get_name()] = Str_class;

    //All base types inherit from Object
    inheritance_set[Object_class].insert(IO_class);
    inheritance_set[Object_class].insert(Int_class);
    inheritance_set[Object_class].insert(Bool_class);
    inheritance_set[Object_class].insert(Str_class);
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

Class_ ClassTable::get_curr_class() {
    return curr_class;
}

void ClassTable::set_curr_class(Class_ c) {
    curr_class = c;
}

bool ClassTable::check_method_type_sig(Class_ c, Feature f) {

    std::set<Feature> feature_set = method_set[c];

    for(std::set<Feature>::iterator it = feature_set.begin(); it != feature_set.end(); it++) {
        Feature it_f = *it;

        if (semant_debug) error_stream << "Function: " << f->get_name() << "Iterator: " << it_f->get_name() << endl;

        // if function overrides parent function...
        if (it_f->get_name() == f->get_name()) {
            if (semant_debug) error_stream << "Overridden method!" << endl;

            // check that return type matches parent
            if (it_f->get_type() != f->get_type()) {
                semant_error(c) << "Return type of overridden function doesn't match parent" << endl;
                return false;
            }

            Formals it_formals = it_f->get_formals();
            Formals f_formals  = f->get_formals();
            // check # of formal arguments matches parent
            if (it_formals->len() != f_formals->len()) {
                semant_error(c) << "Overridden function is not the same arity as parent" << endl;
                return false;
            }

            // check that types on arguments match parent
            for (int i = it_formals->first(); it_formals->more(i); i = it_formals->next(i)) {
                if (it_formals->nth(i)->get_type() != f_formals->nth(i)->get_type()) {
                    semant_error(c) << "In redefined method " << it_formals->nth(i)->get_name()
                                    << ", parameter type " << f_formals->nth(i)->get_type()
                                    <<    " is different from original type " << it_formals->nth(i)->get_type()
                                    << endl;
                    return false;
                }
            }
        }
    }
    return true;
}

bool ClassTable::check_multiple_method(Class_ c, Feature f, std::set<Feature> curr_method_set) {

    for (std::set<Feature>::iterator it_f = curr_method_set.begin(); it_f != curr_method_set.end(); it_f++) {
        if (f->get_name() == (*it_f)->get_name()) {
            semant_error(c) << "Method multiply defined in single class" << endl;
            return false;
        }
    }
    return true;
}

void ClassTable::check_methods_recur(Class_ c, Class_ p) {

    Features features = c->get_features();

    Symbol p_symbol;
    if (p != NULL) {
        p_symbol = p->get_name(); // debug printing
        method_set[c] = method_set[p]; // initialize with same methods as parent
    } else {
        p_symbol = No_class;
    }

    if (semant_debug)
        error_stream << "Class: " << c->get_name() << " inherits from " << p_symbol << endl;

    std::set<Feature> curr_method_set;
    for (int i = features->first(); features->more(i); i = features->next(i)) {
        Feature f = features->nth(i);

        if (semant_debug) {
            error_stream << "  Feature: " << endl;
            error_stream << "    Type : " << (f->is_method() ? "Method" : "Attr") << endl;
            error_stream << "    Name : " << f->get_name() << endl;
        }

        if (f->is_method())
        {
            bool typesig_ok = check_method_type_sig(c, f);
            if (semant_debug) error_stream << "Signatures " << (typesig_ok ? "pass" : "fail") << endl;

            bool defined_once = check_multiple_method(c,f,curr_method_set);
            if (semant_debug) error_stream << "Method defined " << (defined_once ? "" : "more than ") << "once" << endl;

            if (typesig_ok && defined_once) curr_method_set.insert(f);
        }
    }

    method_set[c].insert(curr_method_set.begin(),curr_method_set.end());

    if (c->get_name() == Main) {
        bool main_in_Main = false;

        for (std::set<Feature>::iterator it_f = curr_method_set.begin(); it_f != curr_method_set.end(); it_f++)
            if ((*it_f)->get_name() == main_meth)
                main_in_Main = true;

        if (!main_in_Main)
            semant_error(c) << "No main method in Main class" << endl;
    }

    // recurse through children of current class
    std::set<Class_> child_set = inheritance_set[c];
    for (std::set<Class_>::iterator it_c = child_set.begin(); it_c != child_set.end(); it_c++) {
        check_methods_recur(*it_c, c);
    }
}

void ClassTable::check_methods() {
    check_methods_recur(class_lookup[Object], NULL);
}

void ClassTable::check_types_and_scopes() {
    // start from base class of all other classes
    Class_ rootClass = class_lookup[Object];

    set_curr_class(rootClass);
    rootClass->traverse(this);
}

// determines if class s1 is a subtype of class s2
bool ClassTable::is_subtype(Symbol s1, Symbol s2) {

    // everything is a subclass of Object
    if (s2 == Object)
        return true;

    // blank expressions are subclasses of everything
    if (s1 == No_class)
        return true;

    if (s2 == SELF_TYPE && s1 != SELF_TYPE) return false;

    if (s1 == SELF_TYPE) s1 = curr_class->get_name();
    if (s2 == SELF_TYPE) s2 = curr_class->get_name();

    // the only class with parent No_class should be Object
    while(s1 != No_class) {
        if (s1 == s2)
            return true;

        s1 = class_lookup[s1]->get_parent();
    }

    return false;
}

// find least upper bound (closest common ancestor)
Symbol ClassTable::get_LUB(Symbol s1, Symbol s2) {

    // if s1 is subtype of s2, s2 is LUB
    if (is_subtype(s1, s2)) return s2;

    // if s2 is subtype of s1, s1 is LUB
    if (is_subtype(s2, s1)) return s1;

    // neither easy case, start searching for common ancestor
    while (s1 != Object) {
        s1 = class_lookup[s1]->get_parent();
        if (is_subtype(s2, s1))
            return s1;
    }

    return Object;
}

Symbol class__class::traverse(ClassTable* env) {

    env->sym_tab->enterscope();

    Features f_list = get_features();

    for (int i = f_list->first(); f_list->more(i); i = f_list->next(i)) {
        Feature f = f_list->nth(i);
        Symbol f_name = f->get_name();

        // check attributes. Methods basically already checked.
        if (!f->is_method()) {
            if (env->sym_tab->lookup(f_name) != NULL) {
                env->semant_error(this) << "Illegal redefinition of attribute" << endl;
                return Object;
            }
            else if (f_name == self) {
                env->semant_error(this) << "Attribute cannot be named self" << endl;
                return Object;
            }
            else {
                if (semant_debug) cout << "Adding attribute " << f_name << " with type " << f->get_type() << endl;

                Symbol *type = new Symbol;
                *type = f->get_type();
                env->sym_tab->addid(f_name, type);
            }
        }
    }

    // now that this class's attributes are added to scope, traverse all the featuers and annotate/type-check
    for (int i = f_list->first(); f_list->more(i); i = f_list->next(i)) f_list->nth(i)->traverse(env);

    // recurse through children of current class, entering a new scope on each
    std::set<Class_> child_set = env->inheritance_set[this];
    for (std::set<Class_>::iterator it_c = child_set.begin(); it_c != child_set.end(); it_c++) {
        env->set_curr_class(*it_c);
        (*it_c)->traverse(env);
    }

    env->sym_tab->exitscope();
    return Object;
}

Symbol method_class::traverse(ClassTable* env) {
    env->sym_tab->enterscope();

    if (semant_debug) cout << "In method " << name << endl;

    for (int i = formals->first(); formals->more(i); i = formals->next(i))
        formals->nth(i)->traverse(env);

    Symbol expr_type = expr->traverse(env);

    if (!env->is_subtype(expr_type, return_type)) {
        env->semant_error(env->get_curr_class()) << "Return type (" << expr_type << ") isn't subtype of declared method return type (" << return_type << ")" << endl;
    }

    env->sym_tab->exitscope();
    return return_type;
}

Symbol attr_class::traverse(ClassTable* env) {
    if (semant_debug) cout << "In attr " << name << endl;

    Symbol init_type;
    init_type = init->traverse(env);

    if (!env->is_subtype(init_type, type_decl)) {
        env->semant_error(env->get_curr_class()) << "Initialization type (" << init_type << ") isn't subtype of declared type (" << type_decl << ")" << endl;
        return Object;
    }

    return init_type;
}

Symbol formal_class::traverse(ClassTable* env) {
    if (env->sym_tab->probe(name)) {
        env->semant_error(env->get_curr_class()) << "Formal argument is multiply defined" << endl;
        return set_type(Object)->get_type();
    }

    if (name == self) {
        env->semant_error(env->get_curr_class()) << "Formal argument can't be named self" << endl;
        return set_type(Object)->get_type();
    }

    if (type_decl == SELF_TYPE) {
        env->semant_error(env->get_curr_class()) << "Formal argument can't be of type SELF_TYPE" << endl;
        return set_type(Object)->get_type();
    }

    Symbol *type = new Symbol;
    *type = type_decl;
    env->sym_tab->addid(name, type);
    return type_decl;
}

Symbol branch_class::traverse(ClassTable* env) {
    if (semant_debug) cout << "In branch expression" << endl;

    env->sym_tab->enterscope();

    Symbol *type = new Symbol;
    *type = type_decl;
    env->sym_tab->addid(name, type);

    expr->traverse(env);

    env->sym_tab->exitscope();

    return type_decl;
}

Symbol assign_class::traverse(ClassTable* env) {
    if (semant_debug) cout << "In assign expression" << endl;

    Symbol *attr_type = env->sym_tab->lookup(name);

    if (attr_type == NULL) {
        env->semant_error(env->get_curr_class()) << "Assigning to undefined attribute" << endl;
        return set_type(Object)->get_type();
    }

    Symbol expr_type = expr->traverse(env);

    if (!env->is_subtype(expr_type,*attr_type)) {
        env->semant_error(env->get_curr_class()) << "Attempting to assign type " << expr_type << " to attribute with type " << (*attr_type) << endl;
        return set_type(Object)->get_type();
    }

    return set_type(expr_type)->get_type();
}

Symbol static_dispatch_class::traverse(ClassTable* env) { 
    if (semant_debug) cout << "In dispatch (" << name << ")" << endl;

    Symbol expr_type = expr->traverse(env);

    Class_ dispatch_class;

    if (expr_type == SELF_TYPE)
        dispatch_class = env->get_curr_class();
    else
        dispatch_class = env->class_lookup[expr_type];

    if (semant_debug) cout << "Attempting to dispatch on class (" << dispatch_class->get_name() << ")" << endl;

    std::set<Feature> dispatch_features = env->method_set[dispatch_class];

    Formals dispatch_formals = NULL;
    Symbol  dispatch_ret_type = Object;

    for (std::set<Feature>::iterator it_f = dispatch_features.begin(); it_f != dispatch_features.end(); it_f++) {
        if ((*it_f)->get_name() == name) {
            if (semant_debug) cout << "Found method (" << name << ") in class (" << dispatch_class->get_name() << ")" << endl;
            dispatch_formals  = (*it_f)->get_formals();
            dispatch_ret_type = (*it_f)->get_type();
        }
    }
    if (dispatch_formals == NULL) {
        env->semant_error(env->get_curr_class()) << "Didn't find method (" << name << ") in class (" << dispatch_class->get_name() << ")" << endl;
        return set_type(Object)->get_type();
    }

    // collect the types from traversing the formal arguments to the dispatching function
    std::vector<Symbol> actual_types;
    for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
        actual_types.push_back(actual->nth(i)->traverse(env));
    }

    if (actual_types.size() != (unsigned int)dispatch_formals->len()) {
        env->semant_error(env->get_curr_class()) << "Invalid number of arguments to method" << endl;
        return set_type(Object)->get_type();
    }

    for (int i = dispatch_formals->first(); dispatch_formals->more(i); i = dispatch_formals->next(i)) {
        Symbol f_name = dispatch_formals->nth(i)->get_type();
        Symbol f_type = dispatch_formals->nth(i)->get_type();

        if (!env->is_subtype(actual_types.at(i),f_type)) {
            env->semant_error(env->get_curr_class()) << "Applied argument (" << actual_types.at(i) << ") is not a subtype of (" << f_type << ")" << endl;
            return set_type(Object)->get_type();
        }
    }

    if (set_type(dispatch_ret_type)->get_type() == SELF_TYPE) {
        set_type(expr_type);
    }

    return get_type();
}

Symbol dispatch_class::traverse(ClassTable* env) {
    if (semant_debug) cout << "In dispatch (" << name << ")" << endl;

    Symbol expr_type = expr->traverse(env);

    Class_ dispatch_class;

    if (expr_type == SELF_TYPE)
        dispatch_class = env->get_curr_class();
    else
        dispatch_class = env->class_lookup[expr_type];

    if (semant_debug) cout << "Attempting to dispatch on class (" << dispatch_class->get_name() << ")" << endl;

    std::set<Feature> dispatch_features = env->method_set[dispatch_class];

    Formals dispatch_formals = NULL;
    Symbol  dispatch_ret_type = Object;

    for (std::set<Feature>::iterator it_f = dispatch_features.begin(); it_f != dispatch_features.end(); it_f++) {
        if ((*it_f)->get_name() == name) {
            if (semant_debug) cout << "Found method (" << name << ") in class (" << dispatch_class->get_name() << ")" << endl;
            dispatch_formals  = (*it_f)->get_formals();
            dispatch_ret_type = (*it_f)->get_type();
        }
    }
    if (dispatch_formals == NULL) {
        env->semant_error(env->get_curr_class()) << "Didn't find method (" << name << ") in class (" << dispatch_class->get_name() << ")" << endl;
        return set_type(Object)->get_type();
    }

    // collect the types from traversing the formal arguments to the dispatching function
    std::vector<Symbol> actual_types;
    for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
        actual_types.push_back(actual->nth(i)->traverse(env));
    }

    if (actual_types.size() != (unsigned int)dispatch_formals->len()) {
        env->semant_error(env->get_curr_class()) << "Invalid number of arguments to method" << endl;
        return set_type(Object)->get_type();
    }

    for (int i = dispatch_formals->first(); dispatch_formals->more(i); i = dispatch_formals->next(i)) {
        Symbol f_name = dispatch_formals->nth(i)->get_type();
        Symbol f_type = dispatch_formals->nth(i)->get_type();

        if (!env->is_subtype(actual_types.at(i),f_type)) {
            env->semant_error(env->get_curr_class()) << "Applied argument (" << actual_types.at(i) << ") is not a subtype of (" << f_type << ")" << endl;
            return set_type(Object)->get_type();
        }
    }

    if (set_type(dispatch_ret_type)->get_type() == SELF_TYPE) {
        set_type(expr_type);
    }

    return get_type();
}

Symbol cond_class::traverse(ClassTable* env) {
    if (pred->traverse(env) != Bool) {
        env->semant_error(env->get_curr_class()) << "Predicate of IF is not Bool" << endl;
        return set_type(Object)->get_type();
    }

    Symbol then_type = then_exp->traverse(env);
    Symbol else_type = else_exp->traverse(env);

    return set_type(env->get_LUB(then_type, else_type))->get_type();
}

Symbol loop_class::traverse(ClassTable* env) {
    if (pred->traverse(env) != Bool) {
        env->semant_error(env->get_curr_class()) << "Predicate of LOOP is not Bool" << endl;
        return set_type(Object)->get_type();
    }

    body->traverse(env);

    // The static type of a loop expression is Object
    return set_type(Object)->get_type();
}

Symbol typcase_class::traverse(ClassTable* env) {
    env->sym_tab->enterscope();

    if (semant_debug) cout << "In case" << endl;

    Symbol type = expr->traverse(env);

    std::set<Symbol> case_types;

    for (int i = cases->first(); cases->more(i); i = cases->next(i)) {
        Symbol case_type = cases->nth(i)->traverse(env);
        if (case_types.count(case_type)) {
            env->semant_error(env->get_curr_class()) << "Duplicate branch in case" << endl;
            return set_type(Object)->get_type();
        }
        case_types.insert(case_type);
    }

    env->sym_tab->exitscope();
    return set_type(type)->get_type();
}

Symbol block_class::traverse(ClassTable* env) {
    if (semant_debug) cout << "In block expression" << endl;
    Symbol expr_type;
    for (int i = body->first(); body->more(i); i = body->next(i)) {
        expr_type = body->nth(i)->traverse(env);
    }
    return set_type(expr_type)->get_type();
}

Symbol let_class::traverse(ClassTable* env) {
   
    if (identifier == self) {
        env->semant_error(env->get_curr_class()) << "Let binding cannot be named 'self'" << endl;
        return set_type(Object)->get_type();
    }

    Symbol init_type = init->traverse(env);

    if (!env->is_subtype(init_type, type_decl)) {
        env->semant_error(env->get_curr_class()) << "Initialization type is not a subtype of declared type" << endl;
        return set_type(Object)->get_type();
    }

    env->sym_tab->enterscope();
    Symbol *let_type = new Symbol;
    *let_type = type_decl;

    env->sym_tab->addid(identifier, let_type);
    set_type(body->traverse(env));
    env->sym_tab->exitscope();

    return get_type();
} 

Symbol plus_class::traverse(ClassTable* env) {
    if (e1->traverse(env) != Int) {
        env->semant_error(env->get_curr_class()) << "Expr 1 of Add is not Int" << endl;
        return set_type(Object)->get_type();
    }
    if (e2->traverse(env) != Int) {
        env->semant_error(env->get_curr_class()) << "Expr 2 of Add is not Int" << endl;
        return set_type(Object)->get_type();
    }
    return set_type(Int)->get_type();
}

Symbol sub_class::traverse(ClassTable* env) {
    if (e1->traverse(env) != Int) {
        env->semant_error(env->get_curr_class()) << "Expr 1 of Sub is not Int" << endl;
        return set_type(Object)->get_type();
    }
    if (e2->traverse(env) != Int) {
        env->semant_error(env->get_curr_class()) << "Expr 2 of Sub is not Int" << endl;
        return set_type(Object)->get_type();
    }
    return set_type(Int)->get_type();
}

Symbol mul_class::traverse(ClassTable* env) {
    if (e1->traverse(env) != Int) {
        env->semant_error(env->get_curr_class()) << "Expr 1 of Mul is not Int" << endl;
        return set_type(Object)->get_type();
    }
    if (e2->traverse(env) != Int) {
        env->semant_error(env->get_curr_class()) << "Expr 2 of Mul is not Int" << endl;
        return set_type(Object)->get_type();
    }
    return set_type(Int)->get_type();
}

Symbol divide_class::traverse(ClassTable* env) {
    if (e1->traverse(env) != Int) {
        env->semant_error(env->get_curr_class()) << "Expr 1 of Div is not Int" << endl;
        return set_type(Object)->get_type();
    }
    if (e2->traverse(env) != Int) {
        env->semant_error(env->get_curr_class()) << "Expr 2 of Div is not Int" << endl;
        return set_type(Object)->get_type();
    }
    return set_type(Int)->get_type();
}

Symbol neg_class::traverse(ClassTable* env) {
    if (e1->traverse(env) != Int) {
        env->semant_error(env->get_curr_class()) << "Expr 1 of Neg is not Int" << endl;
        return set_type(Object)->get_type();
    }
    return set_type(Int)->get_type();
}

Symbol lt_class::traverse(ClassTable* env) {
    if (e1->traverse(env) != Int) {
        env->semant_error(env->get_curr_class()) << "Expr 1 of LT is not Int" << endl;
        return set_type(Object)->get_type();
    }
    if (e2->traverse(env) != Int) {
        env->semant_error(env->get_curr_class()) << "Expr 2 of LT is not Int" << endl;
        return set_type(Object)->get_type();
    }
    return set_type(Bool)->get_type();
}

Symbol eq_class::traverse(ClassTable* env) {
    Symbol e1_type = e1->traverse(env);
    Symbol e2_type = e2->traverse(env);

    bool type_ok = true;

    if (e1_type == Str || e2_type == Str)   type_ok = (e1_type == e2_type);
    if (e1_type == Int || e2_type == Int)   type_ok = (e1_type == e2_type);
    if (e1_type == Bool || e2_type == Bool) type_ok = (e1_type == e2_type);

    if (!type_ok) {
        env->semant_error(env->get_curr_class()) << "Expr 1 is type " << e1_type << " and Expr 2 is not in EQ" << endl;
        return set_type(Object)->get_type();
    }
    return set_type(Bool)->get_type();
}

Symbol leq_class::traverse(ClassTable* env) {
    if (e1->traverse(env) != Int) {
        env->semant_error(env->get_curr_class()) << "Expr 1 of EQ is not Int" << endl;
        return set_type(Object)->get_type();
    }
    if (e2->traverse(env) != Int) {
        env->semant_error(env->get_curr_class()) << "Expr 2 of EQ is not Int" << endl;
        return set_type(Object)->get_type();
    }
    return set_type(Bool)->get_type();
}

Symbol comp_class::traverse(ClassTable* env) {
    if (e1->traverse(env) != Bool) {
        env->semant_error(env->get_curr_class()) << "Expr 1 of COMP is not Bool" << endl;
        return set_type(Object)->get_type();
    }
    return set_type(Bool)->get_type();
}

Symbol int_const_class::traverse(ClassTable* env) {
    return set_type(Int)->get_type();
}

Symbol bool_const_class::traverse(ClassTable* env) {
    return set_type(Bool)->get_type();
}

Symbol string_const_class::traverse(ClassTable* env) {
    return set_type(Str)->get_type();
}

Symbol new__class::traverse(ClassTable* env) {

    if (type_name == SELF_TYPE) {
        return set_type(SELF_TYPE)->get_type();
    }

    if (env->class_lookup.find(type_name) == env->class_lookup.end()) {
        env->semant_error(env->get_curr_class()) << "Undefined type in NEW" << endl;
        return set_type(Object)->get_type();
    }
    return set_type(type_name)->get_type();
}

Symbol isvoid_class::traverse(ClassTable* env) {
    e1->traverse(env);
    return set_type(Bool)->get_type();
}

Symbol no_expr_class::traverse(ClassTable* env) { return No_class; }

Symbol object_class::traverse(ClassTable* env) {
    if (name == self) return set_type(SELF_TYPE)->get_type();

    Symbol *attr_type = env->sym_tab->lookup(name);

    if (attr_type == NULL) {
        env->semant_error(env->get_curr_class()) << "reference to undefined object " << endl;
        return set_type(Object)->get_type();
    }
    return set_type(*attr_type)->get_type();
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

    if (!classtable->errors()) {
        // inheritance graph is sound. Start checking features
        classtable->check_methods();
    }

    if (!classtable->errors()) {
        // passed basic sanity for methods. Check types and scopes, annotating along the way
        classtable->check_types_and_scopes();
    }

    if (classtable->errors()) {
        cerr << "Compilation halted due to static semantic errors." << endl;
        exit(1);
    }
}
