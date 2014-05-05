#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <iostream>
#include "semant.h"
#include "utilities.h"
#include <exception>
#include "list.h"

#include <set>
#include <vector>


extern int semant_debug;
extern char *curr_filename;

ErrorReporter *error_reporter;

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

// Construct for class table
ClassTable::ClassTable(Classes classes)
{
    // Before we go onto our own classes, first install basic classes
    install_basic_classes();

    for (int i = classes->first(); classes->more(i); i = classes->next(i))
    {
        Class_ class_ptr = classes->nth(i);
        Symbol name = class_ptr->get_name();
        Symbol parent = class_ptr->get_parent();
        if (name == SELF_TYPE || parent == SELF_TYPE)
        {
            error_reporter->semant_error(class_ptr)
                << "SELF_TYPE cannot be defined or inherited from" << endl;
        }
        else if (class_map.count(name) > 0)  // redefining user-defined class
        {
            error_reporter->semant_error(class_ptr) << "Class " << name
                << " was previously defined." << endl;
        }
        else if (name == Object || name == IO || name == Str || name == Int || name == Bool || name == SELF_TYPE)  // redefined basic class
        {
            error_reporter->semant_error(class_ptr)
                << "Redefinition of basic class " << name << "." << endl;
        }
        else
        {
            // cannot inherit from Bool, Int or String
            if (parent == Bool || parent == Int || parent == Str)
            {
                error_reporter->semant_error(class_ptr)
                    << "Class " << name
                    << " cannot inherit class " << parent << "." << endl;

                continue;
            }

            // class either inherits from Object or IO or some other user-defined class or from an undefined class (checked below)
            // add to map
            add_class(name, parent, class_ptr);
        }
    }

    // go through values and make sure they are all also present as keys in the map i.e. all classes inherit from defined classes
    for (std::map<Symbol, map_val>::iterator it=class_map.begin(); it != class_map.end(); ++it)
    {
        Symbol parent = (it->second).parent;
        if (it->first != Object && class_map.count(parent) == 0 && parent != Object && parent != IO)
        {
            // inheriting from a class that is not defined at all so report error
            error_reporter->semant_error((it->second).c)
                << "Class " << it->first
                << " inherits from an undefined class " << (it->second).parent
                << "." << endl;
        }
    }
}

bool ClassTable::is_child(Symbol child_name, Symbol class_name)
{
    if (child_name == SELF_TYPE)
    {
        // Both are SELF_TYPE, true.
        if (class_name == SELF_TYPE)
        {
            return true;
        }
        // Child is SELF_TYPE, parent isn't. Check if the current class is child of parent.
        else
        {
            return is_child(curr_class_ptr->get_name(), class_name);
        }
    }
    else // Child isn't SELF_TYPE
    {
        // If parent is SELF_TYPE, always false
        if (class_name == SELF_TYPE) return false;
        // Normal checking
        if (child_name == class_name) return true;
        while(child_name != Object)
        {
        if (class_map[child_name].parent == class_name) return true;
        child_name = class_map[child_name].parent;
        //Symbol temp = class_map[child_name].parent;
        }
        return false;
    }
}

bool ClassTable::check_for_cycles()
{
    bool cycle_exists = false;
    for (std::map<Symbol, map_val>::iterator it=class_map.begin(); it != class_map.end(); ++it)
    {
        std::set<Symbol> ancestors;
        ancestors.insert(it->first);
        Symbol curr_parent = (it->second).parent;
        while (class_map.count(curr_parent) > 0)  // max count will be 1 because if try to add second one, will already have been caught in main if check
        {
            map_val val = class_map[curr_parent];
            if (ancestors.count(val.parent) > 0)  // report error because will introduce cycle
            {
                error_reporter->semant_error((it->second).c) << "Class " << it->first << ", or an ancestor of " << it->first << ", is involved in an inheritance cycle." << endl;
                cycle_exists = true;
                break;
            }
            else
            {
                ancestors.insert(curr_parent);
                curr_parent = val.parent;
            }
        }
    }

    return cycle_exists;
}

void ClassTable::add_class(Symbol name, Symbol parent, Class_ c)
{
    map_val val;
    val.parent = parent;
    val.c = c;
    class_map[name] = val;
}

void ClassTable::install_basic_classes()
{

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

    // Adds our basic classes to our class table
    add_class(Object, No_class, Object_class);
    add_class(IO, Object, IO_class);
    add_class(Int, Object, Int_class);
    add_class(Bool, Object, Bool_class);
    add_class(Str, Object, Str_class);
}

////////////////////////////////////////////////////////////////////
//
// semant_error is an overloaded function for reporting errors
// during semantic analysis.  There are three versions:
//
//    ostream& ErrorReporter::semant_error()
//
//    ostream& ErrorReporter::semant_error(Class_ c)
//       print line number and filename for `c'
//
//    ostream& ErrorReporter::semant_error(Symbol filename, tree_node *t)
//       print a line number and filename
//
///////////////////////////////////////////////////////////////////
ErrorReporter::ErrorReporter() : semant_errors(0) , error_stream(cerr) { }

ostream& ErrorReporter::semant_error(Class_ c, tree_node *t)
{
    return semant_error(c->get_filename(),t);
}

ostream& ErrorReporter::semant_error(Class_ c)
{
    return semant_error(c->get_filename(),c);
}

ostream& ErrorReporter::semant_error(Symbol filename, tree_node *t)
{
    error_stream << filename << ":" << t->get_line_number() << ": ";
    return semant_error();
}

ostream& ErrorReporter::semant_error()
{
    semant_errors++;
    return error_stream;
}

bool ClassTable::class_exists(Symbol class_name)
{
    return class_map.count(class_name) > 0;
}

// Least upper bound// least common ancestor function
Symbol ClassTable::lca(Symbol class1, Symbol class2)
{
    // SELF_TYPE and SELF_TYPE
    if (class1 == SELF_TYPE && class2 == SELF_TYPE) return SELF_TYPE;
    // SELF_TYPE and T
    if (class1 == SELF_TYPE) return lca(curr_class_ptr->get_name(), class2);
    // T and SELF_TYPE
    if (class2 == SELF_TYPE) return lca(class1, curr_class_ptr->get_name());
    // T and T'
    for (Symbol parent = class1; parent != Object; parent = class_map[parent].parent)
    {
        if (is_child(class2, parent)) return parent;
    }

    return Object;
}

Symbol ClassTable::get_parent(Symbol class_name)
{
    return class_map[class_name].parent;
}

void ClassTable::set_curr_class_ptr(Class_ class_ptr)
{
    curr_class_ptr = class_ptr;
}

Class_ ClassTable::get_curr_class_ptr()
{
    return curr_class_ptr;
}

Class_ ClassTable::get_class(Symbol class_name)
{
    return class_map[class_name].c;
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
    error_reporter = new ErrorReporter();

    /* ClassTable constructor may do some semantic analysis */
    ClassTable *class_table = new ClassTable(classes);
    if (error_reporter->errors())
    {
        cerr << "Compilation halted due to static semantic errors." << endl;
        exit(1);
    }

    // if code reaches here, then the map is well-formed so now check for cycles
    if (class_table->check_for_cycles())
    {
        cerr << "Compilation halted due to static semantic errors." << endl;
        exit(1);
    }


    // go through classes and collect all methods in a method table
    FeatureTable *feature_table = new FeatureTable();

    // Install all of our classes in the feature, starting with the basic classes
    feature_table->set_class_table(class_table);
    feature_table->populate(classes);

    // check for existence of Main class and main() method with no args within it
    if (!class_table->class_exists(Main))
    {
        error_reporter->semant_error() << "Main class not defined." << endl;
    }
    else
    {
        // check that main method exists in it using feature table
        if (!feature_table->method_exists_in_class(main_meth, Main))
        {
            error_reporter->semant_error(class_table->get_class(Main)) << "No 'main' method in class Main" << endl;
        }
        else
        {
            method_class *main_method = feature_table->get_methods(Main)[main_meth];
            if (main_method->get_formals()->len() != 0)
            {
                error_reporter->semant_error(class_table->get_class(Main), main_method) << "main() method should not have any arguments." << endl;
            }
        }
    }

    feature_table->add_inherited_features(class_table);

    SemanticAnalyzer *semantic_analyzer = new SemanticAnalyzer();
    SymbolTable<Symbol, Symbol> *symbol_table = new SymbolTable<Symbol, Symbol>();
    semantic_analyzer->set_symbol_table(symbol_table);
    semantic_analyzer->set_class_table(class_table);
    semantic_analyzer->set_feature_table(feature_table);
    semantic_analyzer->traverse(classes);

    if (error_reporter->errors())
    {
        cerr << "Compilation halted due to static semantic errors." << endl;
        exit(1);
    }
}

SemanticAnalyzer::SemanticAnalyzer()
{
    // nothing to initialize
}

void SemanticAnalyzer::set_symbol_table(SymbolTable<Symbol, Symbol> *symbol_tab)
{
    symbol_table = symbol_tab;
}

void SemanticAnalyzer::set_class_table(ClassTable *class_tab)
{
    class_table = class_tab;
}

void SemanticAnalyzer::set_feature_table(FeatureTable *feature_tab)
{
    feature_table = feature_tab;
}

void SemanticAnalyzer::traverse(Classes classes)
{
    // here iterate through all the classes and populate the symbol table
    for (int i = classes->first(); classes->more(i); i = classes->next(i))
    {
        symbol_table->enterscope(); // new scope per class
        Class_ class_ptr = classes->nth(i);
        // add current class to symbol table
        symbol_table->addid(self, new Symbol(SELF_TYPE));
        class_table->set_curr_class_ptr(class_ptr);
        Features features = class_ptr->get_features();
        add_inherited_attributes(class_ptr->get_name());
        //cerr << "class: " << class_ptr->get_name() << endl;
        check_attributes(features, class_ptr->get_name()); // first, check all attributes
        check_methods(features, class_ptr->get_name()); // then go through methods one by one
        symbol_table->exitscope(); // exit the scope for the class
    }
}

void SemanticAnalyzer::check_attributes(Features features, Symbol class_name)
{
    for (int j = features->first(); features->more(j); j = features->next(j))
    {
        Feature feature_ptr = features->nth(j);
        //cerr << "class: " << class_name << feature_ptr << endl;
        int type = get_type_of_feature(feature_ptr);
        if (type == ATTRIBUTE)
        {
            attr_class *attribute = dynamic_cast<attr_class *>(feature_ptr);
            check_attribute(attribute, class_name);
        }
    }
}


// REGRADE DIFF - added a check for SELF_TYPE here.
// Also moved the line where we added attribute to symbol_table up to before we evaluate the init.
void SemanticAnalyzer::check_attribute(attr_class *attribute, Symbol class_name)
{
    // check whether exists in parent classes or not

    if (attribute->get_name() == self)
    {
        error_reporter->semant_error(class_table->get_curr_class_ptr(), attribute) << "Attempting to assign to self in attribute declaration." << endl;
    }
    else if (is_attr_in_parent_classes(attribute, class_name))
    {
        error_reporter->semant_error(class_table->get_curr_class_ptr(), attribute) << "Attribute " << attribute->get_name() << " already defined in ancestor classes." << endl;
    }
    else
    {
        // check in same class whether attribute has been defined or not
        if(symbol_table->lookup(attribute->get_name()) == NULL)
        {
            Symbol type = attribute->get_type();
            if (type == SELF_TYPE)
            {
            type = SELF_TYPE;
            // DEBUG cerr << "SELF TYPE attribute: " << attribute->get_name() << " is now labelled: " << type <<  endl;
            }
            else if (!class_table->class_exists(type))
            {
                error_reporter->semant_error(class_table->get_curr_class_ptr(), attribute) << "Class " << type << " of attribute " << attribute->get_name() << " is undefined." << endl;
                type = Object;
            }
            symbol_table->addid(attribute->get_name(), new Symbol(type));
            Symbol expr_type = (attribute->get_init_expr())->eval(class_table, feature_table, symbol_table);

            if (expr_type != No_type)  // init expression is defined
            {
                if (class_table->is_child(expr_type, type))
                {
                    symbol_table->addid(attribute->get_name(), new Symbol(type));
                }
                else
                {
                    error_reporter->semant_error(class_table->get_curr_class_ptr(), attribute) << "Inferred type " << expr_type << " of initialization of attribute " << attribute->get_name() << " does not conform to declared type " << type << "." << endl;
                    symbol_table->addid(attribute->get_name(), new Symbol(type));
                }
            }
        }
        else
        {
            if(attribute->get_name() == self)
            {
                error_reporter->semant_error(class_table->get_curr_class_ptr(), attribute) << "'self' cannot be the name of an attribute." << endl;
            }
            else
            {
                error_reporter->semant_error(class_table->get_curr_class_ptr(), attribute) << "Redefining attribute " << attribute->get_name() << " not allowed." << endl;
            }
        }
    }
}

// Adds all the attributes inherited but not put in scope yet. No error checking required here.
void SemanticAnalyzer::add_inherited_attributes(Symbol class_name)
{
    for (Symbol parent = class_table->get_parent(class_name); parent != No_class; parent = class_table->get_parent(parent))
    {
        std::map<Symbol, Symbol> all_attributes = feature_table->get_attributes(parent);
        for (std::map<Symbol, Symbol>::iterator it = all_attributes.begin(); it != all_attributes.end(); ++it)
        {
            if (symbol_table->lookup(it->first) == NULL)
            {
                symbol_table->addid(it->first, new Symbol(it->second));
            }
        }
    }
}

bool SemanticAnalyzer::is_attr_in_parent_classes(attr_class *attribute, Symbol class_name)
{
    while(class_table->get_parent(class_name) != Object)
    {
        Symbol parent_name = class_table->get_parent(class_name);
        if (feature_table->get_attributes(parent_name).count(attribute->get_name()) > 0)
        {
            return true;
        }

        class_name = parent_name;
    }

    return false;
}

feature_type SemanticAnalyzer::get_type_of_feature(Feature feature)
{
    attr_class *attribute = dynamic_cast<attr_class *>(feature);

    if (attribute == 0)
    {
        return METHOD;
    }
    else
    {
        return ATTRIBUTE;
    }
}

void SemanticAnalyzer::check_methods(Features features, Symbol class_name)
{
    for (int j = features->first(); features->more(j); j = features->next(j))
    {
        Feature feature_ptr = features->nth(j);
        int type = get_type_of_feature(feature_ptr);
        if (type == METHOD)
        {
            method_class *method = dynamic_cast<method_class *>(feature_ptr);
            //cerr << "currently on method: " << method->get_name() << endl;
            check_method(method, class_name);
            //cerr << "finished method: " << method->get_name() << endl;
        }
    }
}

void SemanticAnalyzer::check_method(method_class *method, Symbol class_name)
{
    symbol_table->enterscope(); // new scope per method
    // check whether exists in parent classes or not
    if (method_redefined_with_different_signature(method, class_name))
    {

    }
    else
    {
        // get formals (arguments) and add to current scope so that they are defined in the expression
        Formals formals = method->get_formals();
        for (int j = formals->first(); formals->more(j); j = formals->next(j))
        {
            Formal curr_formal = formals->nth(j);
            if (curr_formal->get_name() == self)
            {
                error_reporter->semant_error(class_table->get_curr_class_ptr(), method) << "Attempting to bind to self in a method formal." << endl;
            }
            else if (curr_formal->get_type() == SELF_TYPE)
            {
                error_reporter->semant_error(class_table->get_curr_class_ptr(), method) << "Attempting to bind SELF_TYPE to variable in formal." << endl;
            }
            else if (symbol_table->probe(curr_formal->get_name()) == NULL)
            {
                if (valid_type(curr_formal->get_type()))
                {
                    symbol_table->addid(curr_formal->get_name(), new Symbol(curr_formal->get_type()));
                }
                else
                {
                    error_reporter->semant_error(class_table->get_curr_class_ptr(), method) << "Class " << curr_formal->get_type() << " of formal parameter " << curr_formal->get_name() << " is undefined." << endl;
                    symbol_table->addid(curr_formal->get_name(), new Symbol(Object));
                }
            }
            else
            {
                error_reporter->semant_error(class_table->get_curr_class_ptr(), method) << "Redefining formal " << curr_formal->get_name() << " not allowed." << endl;
            }
        }
        Symbol expr_type = (method->get_expr())->eval(class_table, feature_table, symbol_table);

        if (class_table->is_child(expr_type, method->get_return_type()))
        {
            symbol_table->addid(method->get_name(), new Symbol(expr_type));
        }
        else
        {
            error_reporter->semant_error(class_table->get_curr_class_ptr(), method) << "Inferred return type " << expr_type << " of method " << method->get_name() << " does not conform to declared return type " << method->get_return_type() << "." << endl;
        }
    }
    symbol_table->exitscope(); // exit scope for method
}

bool SemanticAnalyzer::valid_type(Symbol type)
{
    return (class_table->class_exists(type));
}

bool SemanticAnalyzer::method_redefined_with_different_signature(method_class *method, Symbol class_name)
{
    while (class_table->get_parent(class_name) != Object)
    {
        Symbol parent_name = class_table->get_parent(class_name);
        if (feature_table->get_methods(parent_name).count(method->get_name()) > 0)
        {
            method_class *other_method = feature_table->get_methods(parent_name)[method->get_name()];
            if (!have_identical_signatures(method, other_method))
                {
                    return true;
                }
        }

        class_name = parent_name;
    }

    return false;
}

bool SemanticAnalyzer::have_identical_signatures(method_class *method_one, method_class *method_two)
{
    // check same length of arguments
    if (method_one->get_formals()->len() != method_two->get_formals()->len())
    {
        error_reporter->semant_error(class_table->get_curr_class_ptr(), method_one)
            << "Incompatible number of formal parameters in redefined method " << method_one->get_name() << endl;
        return false;
    }

    // check same types of arguments
    Formals formals_one = method_one->get_formals();
    Formals formals_two = method_two->get_formals();
    for (int j = formals_one->first(); formals_one->more(j); j = formals_one->next(j))
    {
        Formal curr_formal = formals_one->nth(j);
        Formal other_formal = formals_two->nth(j);
        if (curr_formal->get_type() != other_formal->get_type())
        {
            error_reporter->semant_error(class_table->get_curr_class_ptr(), method_one)
                << "In redefined method " << method_one->get_name()
                << ", parameter type " << curr_formal->get_type()
                << " is different from original type " << other_formal->get_type() << "." << endl;
            return false;
        }
    }

    // check return type is the same for both methods
    if (method_one->get_return_type() != method_two->get_return_type())
    {
        error_reporter->semant_error(class_table->get_curr_class_ptr(), method_one) << "In redefined method " << method_one->get_name() << ", return type " << method_one->get_return_type() << " is different from original return type " << method_two->get_return_type() << "." << endl;
        return false;
    }

    return true;
}

FeatureTable::FeatureTable()
{
    // nothing to initialize
}

std::map<Symbol, method_class *> FeatureTable::get_methods(Symbol class_name)
{
    if (features[class_name] == NULL)
    {
        features_struct *new_features = new features_struct;
        features[class_name] = new_features;
    }

    return features[class_name]->methods;
}

std::map<Symbol, Symbol> FeatureTable::get_attributes(Symbol class_name)
{
    if (features[class_name] == NULL)
    {
        features_struct *new_features = new features_struct;
        features[class_name] = new_features;
    }

    return features[class_name]->attributes;
}

bool FeatureTable::method_exists_in_class(Symbol method_name, Symbol class_name)
{
    return (features[class_name]->methods.count(method_name) > 0);
}

void FeatureTable::add_method(Symbol class_name, method_class *method_ptr, Class_ c)
{
    Symbol method_name = method_ptr->get_name();
    if (features.count(class_name) > 0)
    {
    if (method_exists_in_class(method_name, class_name))
    {
        error_reporter->semant_error(class_table->get_curr_class_ptr(), method_ptr)
            << "Method " << method_name << " redefined in class." << endl;
    }
    else
    {
        features[class_name]->methods[method_name] = method_ptr;
    }
    }
    else
    {
        features_struct *new_features = new features_struct;
        new_features->methods[method_name] = method_ptr;
        new_features->c = c;
        features[class_name] = new_features;
    }
}

void FeatureTable::add_attribute(Symbol class_name, attr_class *attr_ptr, Class_ c)
{
    Symbol attribute_name = attr_ptr->get_name();

    if (features.count(class_name) > 0)
    {
        if (features[class_name]->attributes.count(attribute_name) > 0)
        {
            error_reporter->semant_error(class_table->get_curr_class_ptr(), attr_ptr) << "Attribute " << attribute_name << " redefined in class." << endl;
        }
        else
        {
            features[class_name]->attributes[attribute_name] = attr_ptr->get_type();
        }
    }
    else
    {
        features_struct *new_features = new features_struct;
        new_features->attributes[attribute_name] = attr_ptr->get_type();
        new_features->c = c;
        features[class_name] = new_features;
    }
}

void FeatureTable::check_valid_dispatch_arguments(method_class *method_defn,
                                                  std::vector<Symbol> *arg_types,
                                                  ClassTable *class_table,
                                                  tree_node *t)
{
    // check same number of arguments
    if ((size_t)method_defn->get_formals()->len() != arg_types->size())
    {
        error_reporter->semant_error(class_table->get_curr_class_ptr(), t) << "Method " << method_defn->get_name() << " called with wrong number of arguments." << endl;
        return;
    }

    // check valid args provided
    Formals formals = method_defn->get_formals();
    for (int j = formals->first(); formals->more(j); j = formals->next(j))
    {
        Formal curr_formal = formals->nth(j);
        if (!class_table->is_child((*arg_types)[j], curr_formal->get_type()))
        {
            error_reporter->semant_error(class_table->get_curr_class_ptr(), t)
                << "In call of method " << method_defn->get_name()
                << ", type " << (*arg_types)[j] << " of parameter "
                << curr_formal->get_name() << " does not conform to declared type "
                << curr_formal->get_type() << "." << endl;
        }
    }
}

void FeatureTable::install_features_from_class(Class_ class_ptr)
{
    Features features_list = class_ptr->get_features();

    for (int j = features_list->first(); features_list->more(j); j = features_list->next(j))
    {
        Feature feature_ptr = features_list->nth(j);
        // try to cast to method class type
        method_class *method_ptr = dynamic_cast<method_class *>(feature_ptr);

        if (method_ptr != 0)
        {
            // a method
            add_method(class_ptr->get_name(), method_ptr, class_ptr);
        }
        else
        {
            // not a method, must be attribute
            attr_class *attr_ptr = dynamic_cast<attr_class *>(feature_ptr);
            add_attribute(class_ptr->get_name(), attr_ptr, class_ptr);
        }
    }
}

void FeatureTable::populate(Classes classes) {

    // First, populate with basic classes

    install_features_from_class(class_table->get_class(IO));
    install_features_from_class(class_table->get_class(Object));
    install_features_from_class(class_table->get_class(Int));
    install_features_from_class(class_table->get_class(Bool));
    install_features_from_class(class_table->get_class(Str));

    // Then the rest of the classes
    for (int i = classes->first(); classes->more(i); i = classes->next(i))
    {
        Class_ class_ptr = classes->nth(i);
        install_features_from_class(class_ptr);
    }
}

void FeatureTable::set_class_table(ClassTable *class_tab)
{
    class_table = class_tab;
}

// We pass in the feature maps for a child and an ancestor. Any missing methods
// or attributes in the child_features are added.
//
// No error checking is done here.
void FeatureTable::add_missing_features(features_struct *child_features,
                                        features_struct *anc_features)
{
    if (anc_features != NULL)
    {
        std::map<Symbol, method_class *> anc_methods = anc_features->methods;
        std::map<Symbol, Symbol> anc_attr = anc_features->attributes;
        //methods

        for (std::map<Symbol, method_class *>::iterator it = anc_methods.begin(); it != anc_methods.end(); ++it)
        {
            if (child_features->methods.count(it->first) == 0)
            {
                child_features->methods[it->first] = it->second;
            }
        }
        //attributes
        for (std::map<Symbol, Symbol>::iterator it = anc_attr.begin(); it != anc_attr.end(); ++it)
        {
            if (child_features->attributes.count(it->first) == 0)
            {
                child_features->attributes[it->first] = it->second;
            }
        }
    }
}


// Goes through each class in our feature table
// For each class, we go through its ancestors and add all methods and attributes not already in its features map.
// We do not need to do any error checking here, since any inheritance problems are checked in traverse.
void FeatureTable::add_inherited_features(ClassTable *class_tab)
{
    for (std::map<Symbol, features_struct *>::iterator it = features.begin(); it != features.end(); ++it)
    {
        Symbol curr_class = it->first;
        for (Symbol parent = class_tab->get_parent(curr_class); parent != No_class && curr_class != Object; parent = class_tab->get_parent(parent))
        {
            add_missing_features(features[curr_class], features[parent]);
        }
    }
}

// REGRADE DIFF - When we assign something, we DON'T change the type of the object! The expression's type doesn't replace the object type!
Symbol assign_class::eval(ClassTable *class_table,
                          FeatureTable *feature_table,
                          SymbolTable<Symbol,
                          Symbol> *symbol_table)
{
    // check that name is defined in symbol table, and that expression is valid type

    if (name == self)
    {
        set_type(Object);
        error_reporter->semant_error(class_table->get_curr_class_ptr(), this) << "Attempting to assign to self." << endl;

        return Object;
    }

    Symbol expr_type = expr->eval(class_table, feature_table, symbol_table);

    if (symbol_table->lookup(name) != NULL)
    {
        Symbol type_of_attr = *(symbol_table->lookup(name));
        // the attribute being assigned to is defined in the symbol table
        if (!class_table->is_child(expr_type, type_of_attr))
        {
            error_reporter->semant_error(class_table->get_curr_class_ptr(), this) << "Type " << expr_type << " of assigned expression does not conform to declared return type " << type_of_attr << "." << endl;
        }
        else
        {
            // add to symbol table - NOPE
            // symbol_table->addid(name, new Symbol(expr_type));
        }
    }
    else
    {
        error_reporter->semant_error(class_table->get_curr_class_ptr(), this) << "Attribute " << name << " being assigned to is not defined." << endl;
    }

    set_type(expr_type);
    return expr_type;
}

Symbol static_dispatch_class::eval(ClassTable *class_table,
                                   FeatureTable *feature_table,
                                   SymbolTable<Symbol, Symbol> *symbol_table)
{
    Symbol specified_parent = type_name;
    Symbol expr_type = expr->eval(class_table, feature_table, symbol_table);
    // check whether expr_type is <= of the explicitly defined parent type

    if (!class_table->class_exists(specified_parent))
    {
        error_reporter->semant_error(class_table->get_curr_class_ptr(), this)
            << "Specified static type is not defined." << endl;
    }
    else
    {
        if (!feature_table->method_exists_in_class(name, specified_parent))
        {
            error_reporter->semant_error(class_table->get_curr_class_ptr(), this)
                << "Method " << name << " not defined in class " << specified_parent << "." << endl;
            set_type(Object);

            return Object;
        }
        else
        {
            // check that args are valid
            std::vector<Symbol> *arg_types = new std::vector<Symbol>();

            for (int j = actual->first(); actual->more(j); j = actual->next(j))
            {
                Expression curr_expr = actual->nth(j);
                arg_types->push_back(curr_expr->eval(class_table, feature_table, symbol_table));
            }

            // check_valid_dispatch_arguments also reports the relevant errors
            feature_table->check_valid_dispatch_arguments(feature_table->get_methods(specified_parent)[name], arg_types, class_table, this);
        }
    }

    // figure out what type to return
    // gets the statically declared return type of the winky() method in class B
    // but not sufficient
    Symbol ret_type;
    Symbol method_ret_type = feature_table->get_methods(specified_parent)[name]->get_return_type();

    if (specified_parent == SELF_TYPE)
    {
        error_reporter->semant_error(class_table->get_curr_class_ptr(), this)
            << "Static type of dispatch cannot be SELF_TYPE." << endl;

        method_ret_type = Object;
    }
    else if (!class_table->is_child(expr_type, specified_parent))
    {
        error_reporter->semant_error(class_table->get_curr_class_ptr(), this)
            << "Expression type " << expr_type
            << " does not conform to declared static dispatch type "
            << specified_parent << "." << endl;

        method_ret_type = Object;
    }

    if (method_ret_type == SELF_TYPE)
    {
        ret_type = expr_type;
    }
    else
    {
        if (class_table->class_exists(method_ret_type))
        {
            ret_type = method_ret_type;
        }
        else
        {
            error_reporter->semant_error(class_table->get_curr_class_ptr(), this)
                << "Return type of dispatch is not a defined class.";

            ret_type = Object;
        }
    }

    set_type(ret_type);
    return ret_type;
}

Symbol dispatch_class::eval(ClassTable *class_table,
                            FeatureTable *feature_table,
                            SymbolTable<Symbol, Symbol> *symbol_table)
{
    // check that the method exists in the class, and that it has the same arguments
    //cerr << "in dispatch_class" << endl;
    Symbol expr_type = expr->eval(class_table, feature_table, symbol_table);
    //cerr << "evaluated left-most expr" << endl;
    Symbol curr_class;

    if (expr_type == SELF_TYPE)
    {
        curr_class = (class_table->get_curr_class_ptr())->get_name();
    }
    else
    {
        curr_class = expr_type;
    }

    bool method_exists = true;
    //cerr << "a" << endl;

    if (!class_table->class_exists(curr_class))
    {
        error_reporter->semant_error(class_table->get_curr_class_ptr(), this)
            << "Class " << curr_class << " is not defined." << endl;
    }
    else
    {
        if (!feature_table->method_exists_in_class(name, curr_class))
        {
            method_exists = false;
            error_reporter->semant_error(class_table->get_curr_class_ptr(), this)
                << "Dispatch to undefined method " << name << "." << endl;
        }
        else
        {
            // check that args are valid
            std::vector<Symbol> *arg_types = new std::vector<Symbol>();

            // eval each argument to the dispatch
            for (int j = actual->first(); actual->more(j); j = actual->next(j))
            {
                Expression curr_expr = actual->nth(j);
                arg_types->push_back(curr_expr->eval(class_table, feature_table, symbol_table));
            }

            // check_valid_dispatch_arguments also reports the relevant errors
            feature_table->check_valid_dispatch_arguments(feature_table->get_methods(curr_class)[name],
                                                          arg_types,
                                                          class_table,
                                                          this);
        }
    }

    //cerr << "b" << endl;

    // figure out what type to return
    Symbol ret_type;
    if (!method_exists)
    {
        ret_type = Object;
    }
    else
    {
        Symbol method_ret_type = feature_table->get_methods(curr_class)[name]->get_return_type();
        if (method_ret_type == SELF_TYPE)
        {
            ret_type = expr_type;
        }
        else
        {
            if (class_table->class_exists(method_ret_type))
            {
                ret_type = method_ret_type;
            }
            else
            {
                error_reporter->semant_error(class_table->get_curr_class_ptr(), this)
                    << "Return type " << method_ret_type
                    << "of dispatch is not a defined class." << endl;

                ret_type = Object;
            }
        }
    }

    //cerr << "c" << endl;
    set_type(ret_type);
    //cerr << "d" << endl;

    return ret_type;
}

Symbol cond_class::eval(ClassTable *class_table,
                        FeatureTable *feature_table,
                        SymbolTable<Symbol, Symbol> *symbol_table)
{
    Symbol pred_type = pred->eval(class_table, feature_table, symbol_table);

    if (pred_type != Bool)
    {
        error_reporter->semant_error(class_table->get_curr_class_ptr(), this)
            << "Predicate of if-then must be a boolean." << endl;
    }

    Symbol then_type = then_exp->eval(class_table, feature_table, symbol_table);
    Symbol else_type = else_exp->eval(class_table, feature_table, symbol_table);
    set_type(class_table->lca(then_type, else_type));

    return class_table->lca(then_type, else_type);
}

Symbol loop_class::eval(ClassTable *class_table,
                        FeatureTable *feature_table,
                        SymbolTable<Symbol, Symbol> *symbol_table)
{
    Symbol pred_type = pred->eval(class_table, feature_table, symbol_table);

    if (pred_type != Bool)
    {
        error_reporter->semant_error(class_table->get_curr_class_ptr(), this)
            << "Predicate of while loop must be a boolean." << endl;
    }

    Symbol body_type = body->eval(class_table, feature_table, symbol_table);
    set_type(Object);

    return Object;
}

Symbol typcase_class::eval(ClassTable *class_table,
                           FeatureTable *feature_table,
                           SymbolTable<Symbol, Symbol> *symbol_table)
{
    // check that variables in all branches have different types, also that none of the cases involve self
    std::set<Symbol> *types = new std::set<Symbol>();

    for (int i = cases->first(); cases->more(i); i = cases->next(i))
    {
        Symbol name = cases->nth(i)->get_name();
        Symbol type = cases->nth(i)->get_type();
        if (name == self)
        {
            error_reporter->semant_error(class_table->get_curr_class_ptr(), this)
                << "Attempting to bind to self in case branch." << endl;
        }
        else if (types->count(type) > 0)
        {
            error_reporter->semant_error(class_table->get_curr_class_ptr(), this)
                << "Two or more branches have the same type defined." << endl;
        }
        else
        {
            types->insert(type);
        }
    }

    // QN - since the value of expr0 is assigned to one of the id's, do we need to check that all the id's have types that can be assigned the type of the value of expr0?
    // QN - do we need to enterscope() here on case branch? I think we need to..implemented below already.
    Symbol expr_type = expr->eval(class_table, feature_table, symbol_table);

    // find union class of all the expressions of all the branches
    symbol_table->enterscope();
    Case curr_case = cases->nth(cases->first());
    symbol_table->addid(curr_case->get_name(), new Symbol(expr_type));

    Symbol ret_type = curr_case->get_expr()->eval(class_table, feature_table, symbol_table);
    symbol_table->exitscope();

    for (int i = cases->first(); cases->more(i); i = cases->next(i))
    {
        symbol_table->enterscope();
        Case curr_case = cases->nth(i);
        if (!class_table->class_exists(curr_case->get_type()))
        {
            error_reporter->semant_error(class_table->get_curr_class_ptr(), this)
                << "Class " << curr_case->get_type()
                << " of case bound identifier " << curr_case->get_name()
                << " is undefined." << endl;

            ret_type = Object;
            symbol_table->addid(curr_case->get_name(), new Symbol(Object));
        }
        else
        {
            symbol_table->addid(curr_case->get_name(), new Symbol(curr_case->get_type()));
        }
        ret_type = class_table->lca(ret_type, curr_case->get_expr()->eval(class_table, feature_table, symbol_table));
        symbol_table->exitscope();
    }

    set_type(ret_type);

    return ret_type;
}

Symbol block_class::eval(ClassTable *class_table,
                         FeatureTable *feature_table,
                         SymbolTable<Symbol, Symbol> *symbol_table)
{
    //cerr << "entered block" << endl;
    Symbol last;

    for (int i = body->first(); body->more(i); i = body->next(i))
    {
        if ( !(body->more(body->next(i))) )
        {
            last = body->nth(i)->eval(class_table, feature_table, symbol_table);
            set_type(last);

            break;
        }

        body->nth(i)->eval(class_table, feature_table, symbol_table);
    }

    // cerr << "exited block" << endl;
    return last;
}

Symbol let_class::eval(ClassTable *class_table,
                       FeatureTable *feature_table,
                       SymbolTable<Symbol, Symbol> *symbol_table)
{
    symbol_table->enterscope();
    Symbol type_of_attr;
    type_of_attr = type_decl;

    if (!class_table->class_exists(type_of_attr))
    {
        error_reporter->semant_error(class_table->get_curr_class_ptr(), this)
            << "Class " << type_of_attr
            << " of let bound identifier "
            << identifier << " is undefined." << endl;

        type_of_attr = Object;
    }

    // eval-ing this init expression will recursively add all the formals defined in this let expression to the current scope
    Symbol init_expr_type = init->eval(class_table, feature_table, symbol_table);

    if (identifier == self)
    {
        error_reporter->semant_error(class_table->get_curr_class_ptr(), this)
            << "Attempting to bind to self in let expression." << endl;
    }
    else if(init_expr_type != No_type)
    {
        // init expression defined
        // check valid type
        if (!class_table->is_child(init_expr_type, type_of_attr))
        {
            error_reporter->semant_error(class_table->get_curr_class_ptr(), this)
                << "Inferred type " << init_expr_type
                << " of initialization of identifier does not conform to identifier's declared type " << type_decl
                << "." << endl;
            symbol_table->addid(identifier, new Symbol(type_decl));
        }
        else
        {
            // add to symbol table
            symbol_table->addid(identifier, new Symbol(type_of_attr));
        }
    }
    else // init expression not defined
    {
        symbol_table->addid(identifier, new Symbol(type_of_attr));
    }


    // at this point, all the formals have been added to the symbol table
    Symbol let_return_type = body->eval(class_table, feature_table, symbol_table);
    symbol_table->exitscope();
    set_type(let_return_type);

    return let_return_type;
}

Symbol plus_class::eval(ClassTable *class_table,
                        FeatureTable *feature_table,
                        SymbolTable<Symbol, Symbol> *symbol_table)
{
    Symbol e1_type = e1->eval(class_table, feature_table, symbol_table);
    Symbol e2_type = e2->eval(class_table, feature_table, symbol_table);

    if (e1_type != Int || e2_type != Int)
    {
        error_reporter->semant_error(class_table->get_curr_class_ptr(), this)
            << "Cannot add non-int values." << endl;
    }

    set_type(Int);
    return Int;
}

Symbol sub_class::eval(ClassTable *class_table,
                       FeatureTable *feature_table,
                       SymbolTable<Symbol, Symbol> *symbol_table)
{
    Symbol e1_type = e1->eval(class_table, feature_table, symbol_table);
    Symbol e2_type = e2->eval(class_table, feature_table, symbol_table);

    if (e1_type != Int || e2_type != Int)
    {
        error_reporter->semant_error(class_table->get_curr_class_ptr(), this)
            << "Cannot subtract non-int values." << endl;
    }

    set_type(Int);
    return Int;
}

Symbol mul_class::eval(ClassTable *class_table,
                       FeatureTable *feature_table,
                       SymbolTable<Symbol, Symbol> *symbol_table)
{
    Symbol e1_type = e1->eval(class_table, feature_table, symbol_table);
    Symbol e2_type = e2->eval(class_table, feature_table, symbol_table);

    if (e1_type != Int || e2_type != Int)
    {
        error_reporter->semant_error(class_table->get_curr_class_ptr(), this)
            << "Cannot multiply non-int values." << endl;
    }

    set_type(Int);
    return Int;
}

Symbol divide_class::eval(ClassTable *class_table,
                          FeatureTable *feature_table,
                          SymbolTable<Symbol, Symbol> *symbol_table)
{
    Symbol e1_type = e1->eval(class_table, feature_table, symbol_table);
    Symbol e2_type = e2->eval(class_table, feature_table, symbol_table);

    if (e1_type != Int || e2_type != Int)
    {
        error_reporter->semant_error(class_table->get_curr_class_ptr(), this)
            << "Cannot divide non-int values." << endl;
    }

    set_type(Int);
    return Int;
}

Symbol neg_class::eval(ClassTable *class_table,
                       FeatureTable *feature_table,
                       SymbolTable<Symbol, Symbol> *symbol_table)
{
    // applies to integers
    Symbol expr_type = e1->eval(class_table, feature_table, symbol_table);

    if (expr_type != Int)
    {
        error_reporter->semant_error(class_table->get_curr_class_ptr(), this)
            << "Cannot take negation of non-integer." << endl;
    }

    set_type(Int);
    return Int;
}

Symbol lt_class::eval(ClassTable *class_table, FeatureTable *feature_table, SymbolTable<Symbol, Symbol> *symbol_table) {
    Symbol e1_type = e1->eval(class_table, feature_table, symbol_table);
    Symbol e2_type = e2->eval(class_table, feature_table, symbol_table);
    if(e1_type != Int || e2_type != Int) {
    error_reporter->semant_error(class_table->get_curr_class_ptr(), this) << "Cannot compare/order non-int values using 'less than' operator." << endl;
    }
    set_type(Bool);
    return Bool;
}

Symbol eq_class::eval(ClassTable *class_table,
                      FeatureTable *feature_table,
                      SymbolTable<Symbol, Symbol> *symbol_table)
{
    Symbol e1_type = e1->eval(class_table, feature_table, symbol_table);
    Symbol e2_type = e2->eval(class_table, feature_table, symbol_table);

    if (e1_type != e2_type)
    {
        error_reporter->semant_error(class_table->get_curr_class_ptr(), this)
            << "Cannot compare values of different types using 'equal to' operator." << endl;
    }

    set_type(Bool);
    return Bool;
}

Symbol leq_class::eval(ClassTable *class_table,
                       FeatureTable *feature_table,
                       SymbolTable<Symbol, Symbol> *symbol_table)
{
    Symbol e1_type = e1->eval(class_table, feature_table, symbol_table);
    Symbol e2_type = e2->eval(class_table, feature_table, symbol_table);

    if (e1_type != Int || e2_type != Int)
    {
        error_reporter->semant_error(class_table->get_curr_class_ptr(), this)
            << "Cannot compare/order non-int values using 'less than or equal to' operator." << endl;
    }

    set_type(Bool);
    return Bool;
}

Symbol comp_class::eval(ClassTable *class_table,
                        FeatureTable *feature_table,
                        SymbolTable<Symbol, Symbol> *symbol_table)
{
    // applies to booleans
    Symbol expr_type = e1->eval(class_table, feature_table, symbol_table);

    if (expr_type != Bool)
    {
        error_reporter->semant_error(class_table->get_curr_class_ptr(), this) << "Cannot take complement of non-boolean." << endl;
    }

    set_type(Bool);
    return Bool;
}

Symbol int_const_class::eval(ClassTable *class_table,
                             FeatureTable *feature_table,
                             SymbolTable<Symbol, Symbol> *symbol_table)
{
    set_type(Int);
    return Int;
}

Symbol bool_const_class::eval(ClassTable *class_table,
                              FeatureTable *feature_table,
                              SymbolTable<Symbol, Symbol> *symbol_table)
{
    set_type(Bool);
    return Bool;
}

Symbol string_const_class::eval(ClassTable *class_table,
                                FeatureTable *feature_table,
                                SymbolTable<Symbol, Symbol> *symbol_table)
{
    set_type(Str);
    return Str;
}

Symbol new__class::eval(ClassTable *class_table,
                        FeatureTable *feature_table,
                        SymbolTable<Symbol, Symbol> *symbol_table)
{
    //cerr << "in new_class" << endl;
    if (type_name == SELF_TYPE)
    {
        set_type(SELF_TYPE);
        return SELF_TYPE;
    }

    if (class_table->class_exists(type_name))
    {
        set_type(type_name);
        return type_name;
    }
    else
    {
        error_reporter->semant_error(class_table->get_curr_class_ptr(), this)
            << "Attempting to call 'new' on an undefined class " << type_name << "." << endl;
        set_type(Object);

        return Object;
    }
}

Symbol isvoid_class::eval(ClassTable *class_table,
                          FeatureTable *feature_table,
                          SymbolTable<Symbol, Symbol> *symbol_table)
{
    e1->eval(class_table, feature_table, symbol_table);
    set_type(Bool);

    return Bool;
}

Symbol no_expr_class::eval(ClassTable *class_table,
                           FeatureTable *feature_table,
                           SymbolTable<Symbol, Symbol> *symbol_table)
{
    set_type(No_type);
    return No_type;
}

// REGRADE DIFF - We allow objects to be of type Object. Holly cow, how did we miss this.
Symbol object_class::eval(ClassTable *class_table,
                          FeatureTable *feature_table,
                          SymbolTable<Symbol, Symbol> *symbol_table)
{
    if (name == self)
    {
        set_type(SELF_TYPE);
        return SELF_TYPE;
    }

    Symbol *obj_type = symbol_table->lookup(name);
    Symbol ret_type;

    if (obj_type == NULL)
    {
        error_reporter->semant_error(class_table->get_curr_class_ptr(), this) << "Undeclared identifier " << name << "." << endl;
        set_type(Object);
        ret_type = Object;
    }
    else
    {
        Symbol obj = *obj_type;
        /*
        if (obj == Object)
        {
            error_reporter->semant_error(class_table->get_curr_class_ptr(), this) << "Object cannot have type Object." << endl;
            set_type(Object);
            ret_type = Object;
        }
        */
        set_type(obj);
        ret_type = obj;
    }

    return ret_type;
}
