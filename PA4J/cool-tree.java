// -*- mode: java -*- 
//
// file: cool-tree.m4
//
// This file defines the AST
//
//////////////////////////////////////////////////////////

import java.util.Enumeration;
import java.io.PrintStream;
import java.util.Vector;


/** Defines simple phylum Program */
abstract class Program extends TreeNode {
    protected Program(int lineNumber) {
        super(lineNumber);
    }
    public abstract void dump_with_types(PrintStream out, int n);
    public abstract void semant();

}


/** Defines simple phylum Class_ */
abstract class Class_ extends TreeNode {
    protected Class_(int lineNumber) {
        super(lineNumber);
    }
    public abstract void dump_with_types(PrintStream out, int n);

}


/** Defines list phylum Classes
    <p>
    See <a href="ListNode.html">ListNode</a> for full documentation. */
class Classes extends ListNode {
    public final static Class elementClass = Class_.class;
    /** Returns class of this lists's elements */
    public Class getElementClass() {
        return elementClass;
    }
    protected Classes(int lineNumber, Vector elements) {
        super(lineNumber, elements);
    }
    /** Creates an empty "Classes" list */
    public Classes(int lineNumber) {
        super(lineNumber);
    }
    /** Appends "Class_" element to this list */
    public Classes appendElement(TreeNode elem) {
        addElement(elem);
        return this;
    }
    public TreeNode copy() {
        return new Classes(lineNumber, copyElements());
    }
}


/** Defines simple phylum Feature */
abstract class Feature extends TreeNode {
    protected Feature(int lineNumber) {
        super(lineNumber);
    }
    public abstract void dump_with_types(PrintStream out, int n);
    public abstract AbstractSymbol getType();
    public abstract AbstractSymbol getName();
    //Here, we pass around the classTable the class name.  
    //Attrs and Methods can be looked up in classTable from the className;
    public abstract void typeCheck(ClassTable classTable,AbstractSymbol parent,AbstractSymbol className);
                                                      
}


/** Defines list phylum Features
    <p>
    See <a href="ListNode.html">ListNode</a> for full documentation. */
class Features extends ListNode {
    public final static Class elementClass = Feature.class;
    /** Returns class of this lists's elements */
    public Class getElementClass() {
        return elementClass;
    }
    protected Features(int lineNumber, Vector elements) {
        super(lineNumber, elements);
    }
    /** Creates an empty "Features" list */
    public Features(int lineNumber) {
        super(lineNumber);
    }
    /** Appends "Feature" element to this list */
    public Features appendElement(TreeNode elem) {
        addElement(elem);
        return this;
    }
    public TreeNode copy() {
        return new Features(lineNumber, copyElements());
    }
}


/** Defines simple phylum Formal */
abstract class Formal extends TreeNode {
    protected Formal(int lineNumber) {
        super(lineNumber);
    }
    public abstract void dump_with_types(PrintStream out, int n);
    public abstract AbstractSymbol getName();
    public abstract AbstractSymbol getType();
}


/** Defines list phylum Formals
    <p>
    See <a href="ListNode.html">ListNode</a> for full documentation. */
class Formals extends ListNode {
    public final static Class elementClass = Formal.class;
    /** Returns class of this lists's elements */
    public Class getElementClass() {
        return elementClass;
    }
    protected Formals(int lineNumber, Vector elements) {
        super(lineNumber, elements);
    }
    /** Creates an empty "Formals" list */
    public Formals(int lineNumber) {
        super(lineNumber);
    }
    /** Appends "Formal" element to this list */
    public Formals appendElement(TreeNode elem) {
        addElement(elem);
        return this;
    }
    public TreeNode copy() {
        return new Formals(lineNumber, copyElements());
    }
}


/** Defines simple phylum Expression */
abstract class Expression extends TreeNode {
    protected Expression(int lineNumber) {
        super(lineNumber);
    }
    private AbstractSymbol type = null;                                 
    public AbstractSymbol get_type() { return type; }           
    public Expression set_type(AbstractSymbol s) { type = s; return this; } 
    public abstract void dump_with_types(PrintStream out, int n);
    //Here, we pass around the classTable the class name.  

    //Attrs and Methods can be looked up in classTable from the className;
    public abstract AbstractSymbol typeCheck(ClassTable classTable,AbstractSymbol className);
    public void dump_type(PrintStream out, int n) {
        if (type != null)
            { out.println(Utilities.pad(n) + ": " + type.getString()); }
        else
            { out.println(Utilities.pad(n) + ": _no_type"); }
    }

}


/** Defines list phylum Expressions
    <p>
    See <a href="ListNode.html">ListNode</a> for full documentation. */
class Expressions extends ListNode {
    public final static Class elementClass = Expression.class;
    /** Returns class of this lists's elements */
    public Class getElementClass() {
        return elementClass;
    }
    protected Expressions(int lineNumber, Vector elements) {
        super(lineNumber, elements);
    }
    /** Creates an empty "Expressions" list */
    public Expressions(int lineNumber) {
        super(lineNumber);
    }
    /** Appends "Expression" element to this list */
    public Expressions appendElement(TreeNode elem) {
        addElement(elem);
        return this;
    }
    public TreeNode copy() {
        return new Expressions(lineNumber, copyElements());
    }
}


/** Defines simple phylum Case */
abstract class Case extends TreeNode {
    protected Case(int lineNumber) {
        super(lineNumber);
    }
    public abstract void dump_with_types(PrintStream out, int n);
    public abstract AbstractSymbol getType();
    public abstract AbstractSymbol typeCheck (ClassTable classTable, AbstractSymbol className);


}


/** Defines list phylum Cases
    <p>
    See <a href="ListNode.html">ListNode</a> for full documentation. */
class Cases extends ListNode {
    public final static Class elementClass = Case.class;
    /** Returns class of this lists's elements */
    public Class getElementClass() {
        return elementClass;
    }
    protected Cases(int lineNumber, Vector elements) {
        super(lineNumber, elements);
    }
    /** Creates an empty "Cases" list */
    public Cases(int lineNumber) {
        super(lineNumber);
    }
    /** Appends "Case" element to this list */
    public Cases appendElement(TreeNode elem) {
        addElement(elem);
        return this;
    }
    public TreeNode copy() {
        return new Cases(lineNumber, copyElements());
    }
}


/** Defines AST constructor 'programc'.
    <p>
    See <a href="TreeNode.html">TreeNode</a> for full documentation. */
class programc extends Program {
    protected Classes classes;
    /** Creates "programc" AST node. 
      *
      * @param lineNumber the line in the source file from which this node came.
      * @param a0 initial value for classes
      */
    public programc(int lineNumber, Classes a1) {
        super(lineNumber);
        classes = a1;
    }
    public TreeNode copy() {
        return new programc(lineNumber, (Classes)classes.copy());
    }
    public void dump(PrintStream out, int n) {
        out.print(Utilities.pad(n) + "programc\n");
        classes.dump(out, n+2);
    }

    
    public void dump_with_types(PrintStream out, int n) {
        dump_line(out, n);
        out.println(Utilities.pad(n) + "_program");
        for (Enumeration e = classes.getElements(); e.hasMoreElements(); ) {
            // sm: changed 'n + 1' to 'n + 2' to match changes elsewhere
        ((Class_)e.nextElement()).dump_with_types(out, n + 2);
        }
    }
    /** This method is the entry point to the semantic checker.  You will
        need to complete it in programming assignment 4.
    <p>
        Your checker should do the following two things:
    <ol>
    <li>Check that the program is semantically correct
    <li>Decorate the abstract syntax tree with type information
        by setting the type field in each Expression node.
        (see tree.h)
    </ol>
    <p>
    You are free to first do (1) and make sure you catch all semantic
        errors. Part (2) can be done in a second stage when you want
    to test the complete compiler.
    */
    public void semant() {
    /* ClassTable constructor may do some semantic analysis */
    ClassTable classTable = new ClassTable(classes);
    
    /* some semantic analysis code may go here */

        /* Fill each class_c's attribute and method tables, since these
           have scope over the entire class */

    for (Enumeration e = classes.getElements(); e.hasMoreElements(); ) {
        ((class_c)e.nextElement()).fillNameTables(classTable);
        }


        /* We check that class Main is defined */
    if (classTable.lookup(TreeConstants.Main)==null) {
        PrintStream es = classTable.semantError();
        es.println("Class Main is not defined.");
        }   
    /* check for existance of main() in class Main */
    else if ((classTable.lookup(TreeConstants.Main)).getMethod(TreeConstants.main_meth) == null) {
        PrintStream es = classTable.semantError(classTable.lookup(TreeConstants.Main));
        es.println("Method Main.main() is not defined.");
        }
    /* Then we check that it has no parameters */
    else if(((classTable.lookup(TreeConstants.Main)).getMethod(TreeConstants.main_meth)).getFormals().getLength()!=0) {
        PrintStream es = classTable.semantError(classTable.lookup(TreeConstants.Main));
        es.println("Method Main.main() should not have any formal parameters.");
        }

        /* Now do the typecheck / namecheck for all classes */
        for (Enumeration e = classes.getElements(); e.hasMoreElements(); ) {
        ((class_c)e.nextElement()).typeCheck(classTable);      
        } 

    if (classTable.errors()) {
        System.err.println("Compilation halted due to static semantic errors.");
        System.exit(1);
    }
    }

}


/** Defines AST constructor 'class_c'.
    <p>
    See <a href="TreeNode.html">TreeNode</a> for full documentation. */
class class_c extends Class_ {
    protected AbstractSymbol name;
    protected AbstractSymbol parent;
    protected Features features;
    protected AbstractSymbol filename;
    // protected boolean traversed = false; //helps in the path-to-root algorithm 
    protected SymbolTable objTable;
    protected SymbolTable mthdTable;
    // protected SymbolTable attrTable; //might be needed if we don't exit scopes in objTable cleanly, or we run multi-threaded

    /** Creates "class_c" AST node. 
      *
      * @param lineNumber the line in the source file from which this node came.
      * @param a0 initial value for name
      * @param a1 initial value for parent
      * @param a2 initial value for features
      * @param a3 initial value for filename
      */
    public class_c(int lineNumber, AbstractSymbol a1, AbstractSymbol a2, Features a3, AbstractSymbol a4) {
        super(lineNumber);
        name = a1;
        parent = a2;
        features = a3;
        filename = a4;
    objTable = new SymbolTable();
    objTable.enterScope(); //This one should have multiple levels
    mthdTable = new SymbolTable();
    mthdTable.enterScope(); //This one should be only 1 level
    }

    public TreeNode copy() {
        return new class_c(lineNumber, copy_AbstractSymbol(name), copy_AbstractSymbol(parent), (Features)features.copy(), copy_AbstractSymbol(filename));
    }

    public void dump(PrintStream out, int n) {
        out.print(Utilities.pad(n) + "class_c\n");
        dump_AbstractSymbol(out, n+2, name);
        dump_AbstractSymbol(out, n+2, parent);
        features.dump(out, n+2);
        dump_AbstractSymbol(out, n+2, filename);
    }

    
    public AbstractSymbol getFilename() { return filename; }
    public AbstractSymbol getName()     { return name; }
    public AbstractSymbol getParent()   { return parent; }

    //This fills up the SymbolTables of this class on the first pass
    //through the AST, since methods and attributes can be defined
    //after usage.
    //After being filled with attributes, objTable is the initial state to be
    //passed around to different scopes to check the definition of 
    //identifier names.  

    public void fillNameTables(ClassTable ct) {
        for (Enumeration e = features.getElements(); e.hasMoreElements();) {
        Feature curF = (Feature)e.nextElement();
        if(curF instanceof method) {
        //Only add method if it is not already defined
        if (mthdTable.lookup(curF.getName())==null)
            mthdTable.addId(curF.getName(),curF);
        else {
            PrintStream es = ct.semantError(curF,name);
            es.println("Method " + curF.getName() + 
                   " redefined in class " + name);
        }
         
        }
        if(curF instanceof attr) {
        if ((objTable.lookup(curF.getName())==null) &&
            (curF.getName()!=TreeConstants.self)) {
        //Only add attribute if it is not already defined and 
        //is not self
            objTable.addId(curF.getName(),curF.getType());
        }
        else {
            if (curF.getName() != TreeConstants.self) {
            PrintStream es = ct.semantError(curF,name);
            es.println("Attribute " + curF.getName() + 
                   " redefined in class " + name);
            }
        }
        }   
        }
 
        //We also add self to this class's objTable, with type SELF_TYPE
        objTable.addId(TreeConstants.self,TreeConstants.SELF_TYPE);
    }
    
    public method getMethod(AbstractSymbol name) {
    return (method)mthdTable.lookup(name);
    }

    public SymbolTable getObjTable() {
    return objTable;
    }

    public AbstractSymbol getObj(AbstractSymbol name) {
    //This will look up the type associated with a name in the
        //Object table.  We will use this to look for inherited
        //attributes from superclasses.
    //Note that this usage imposes a constraint of sequential processing 
    //and proper exiting of scopes, since we want the objTables for 
        //classes we are NOT currently typechecking to contain just the 
    //initial value, which is the list of attributes.

    return (AbstractSymbol)objTable.lookup(name);
    }
 
    //Here is the typechecking + name checking code.
    public void typeCheck(ClassTable ct) {
        for (Enumeration e = features.getElements(); e.hasMoreElements();) {
        ((Feature)e.nextElement()).typeCheck(ct,parent,name);
        }
    }

    public void dump_with_types(PrintStream out, int n) {
        dump_line(out, n);
        out.println(Utilities.pad(n) + "_class");
        dump_AbstractSymbol(out, n + 2, name);
        dump_AbstractSymbol(out, n + 2, parent);
        out.print(Utilities.pad(n + 2) + "\"");
        Utilities.printEscapedString(out, filename.getString());
        out.println("\"\n" + Utilities.pad(n + 2) + "(");
        for (Enumeration e = features.getElements(); e.hasMoreElements();) {
        ((Feature)e.nextElement()).dump_with_types(out, n + 2);
        }
        out.println(Utilities.pad(n + 2) + ")");
    }

}


/** Defines AST constructor 'method'.
    <p>
    See <a href="TreeNode.html">TreeNode</a> for full documentation. */
class method extends Feature {
    protected AbstractSymbol name;
    protected Formals formals;
    protected AbstractSymbol return_type;
    protected Expression expr;
    /** Creates "method" AST node. 
      *
      * @param lineNumber the line in the source file from which this node came.
      * @param a0 initial value for name
      * @param a1 initial value for formals
      * @param a2 initial value for return_type
      * @param a3 initial value for expr
      */
    public method(int lineNumber, AbstractSymbol a1, Formals a2, AbstractSymbol a3, Expression a4) {
        super(lineNumber);
        name = a1;
        formals = a2;
        return_type = a3;
        expr = a4;
    }
    public TreeNode copy() {
        return new method(lineNumber, copy_AbstractSymbol(name), (Formals)formals.copy(), copy_AbstractSymbol(return_type), (Expression)expr.copy());
    }
    public void dump(PrintStream out, int n) {
        out.print(Utilities.pad(n) + "method\n");
        dump_AbstractSymbol(out, n+2, name);
        formals.dump(out, n+2);
        dump_AbstractSymbol(out, n+2, return_type);
        expr.dump(out, n+2);
    }

    public AbstractSymbol getName() {
    return name;
    }
    public AbstractSymbol getType() {
        return return_type;
    }
    public Formals getFormals() {
    return formals;
    }
    public void typeCheck (ClassTable classTable, AbstractSymbol parent, AbstractSymbol className) {
    SymbolTable objTable = (classTable.lookup(className)).getObjTable();
    
    //Check that if this class redefines a method from an ancestor,
    //it is done properly
    method inherited = classTable.lookupMthd(name,parent);
    if (inherited != null) {
        Formals inhFormals = inherited.getFormals();
        AbstractSymbol inhType = inherited.getType();

        if (return_type != inhType) {
        PrintStream es = classTable.semantError(this,className);
        es.println("Redefined method " + name 
               + " has different return type " + return_type +
               " than original return type " + inhType);
        }

        if (inhFormals.getLength() != formals.getLength() ) {
        PrintStream es = classTable.semantError(this,className);
        es.println("Redefined method " + name 
               + " has different number of args as previous declaration" );
        }
        else {
        //both have same number of formals
        Enumeration ie = inhFormals.getElements();
        for (Enumeration e = formals.getElements(); e.hasMoreElements();) {
            Formal curFormal = (Formal)e.nextElement();
            Formal inhFormal = (Formal)ie.nextElement();
            if (curFormal.getType() != inhFormal.getType()) {
            PrintStream es = classTable.semantError(this,className);
            es.println("Formal " + curFormal.getName() + " of redefined method " + name 
                   + " has different type " + curFormal.getType() +
                   " than original type " + inhFormal.getType());
            }
        }
        }

    }

    objTable.enterScope(); //Create a scope for the formal parameters
    
    for (Enumeration e = formals.getElements(); e.hasMoreElements();) {
        Formal curFormal = (Formal)e.nextElement();

        if (curFormal.getName() == TreeConstants.self) {
        PrintStream es = classTable.semantError(this,className);
        es.println("'self' used as formal parameter");
        }
        else //add everything but self.
        objTable.addId(curFormal.getName(),curFormal.getType());
        
        //check that no formal has type declared as SELF_TYPE
        if (curFormal.getType() == TreeConstants.SELF_TYPE) {
        PrintStream es = classTable.semantError(curFormal,className);
        es.println("formal " + curFormal.getName() + 
               " cannot have SELF_TYPE as its declared type");
        } 
        //check that the type declaration of each formal is valid
        else if (!classTable.isDefined(curFormal.getType())) {
        PrintStream es = classTable.semantError(curFormal,className);
        es.println("formal " + curFormal.getName() + ":" + 
               curFormal.getType() + " has undefined type.");
        }          
        }
    
    AbstractSymbol typeE = expr.typeCheck(classTable, className);
    if (!classTable.isDefined(return_type)) {
        PrintStream es = classTable.semantError(this,className);
        es.println("Method return type " + return_type + " is undefined.");
    }
    else if (!classTable.conforms(typeE,return_type,className)) {
        PrintStream es = classTable.semantError(this,className);
        es.println("Inferred type " + typeE + " of method body does not conform to declared return type " + return_type);
    }
        objTable.exitScope();
    }

    public void dump_with_types(PrintStream out, int n) {
        dump_line(out, n);
        out.println(Utilities.pad(n) + "_method");
        dump_AbstractSymbol(out, n + 2, name);
        for (Enumeration e = formals.getElements(); e.hasMoreElements();) {
        ((Formal)e.nextElement()).dump_with_types(out, n + 2);
        }
        dump_AbstractSymbol(out, n + 2, return_type);
    expr.dump_with_types(out, n + 2);
    }

}


/** Defines AST constructor 'attr'.
    <p>
    See <a href="TreeNode.html">TreeNode</a> for full documentation. */
class attr extends Feature {
    protected AbstractSymbol name;
    protected AbstractSymbol type_decl;
    protected Expression init;
    /** Creates "attr" AST node. 
      *
      * @param lineNumber the line in the source file from which this node came.
      * @param a0 initial value for name
      * @param a1 initial value for type_decl
      * @param a2 initial value for init
      */
    public attr(int lineNumber, AbstractSymbol a1, AbstractSymbol a2, Expression a3) {
        super(lineNumber);
        name = a1;
        type_decl = a2;
        init = a3;
    }
    public TreeNode copy() {
        return new attr(lineNumber, copy_AbstractSymbol(name), copy_AbstractSymbol(type_decl), (Expression)init.copy());
    }
    public void dump(PrintStream out, int n) {
        out.print(Utilities.pad(n) + "attr\n");
        dump_AbstractSymbol(out, n+2, name);
        dump_AbstractSymbol(out, n+2, type_decl);
        init.dump(out, n+2);
    }

    public AbstractSymbol getName() {
    return name;
    }
    public AbstractSymbol getType() {
        return type_decl;
    }
    public void typeCheck (ClassTable classTable, AbstractSymbol parent, AbstractSymbol className) {
    AbstractSymbol exprType = init.typeCheck(classTable,className);
    

    if (name == TreeConstants.self) {
        PrintStream es = classTable.semantError(this,className);
        es.println("'self' used as an attribute");
    }

    //Check that this attribute isn't already defined in an ancestor
    else if (classTable.lookupObj(name,parent)!=TreeConstants.No_type) {
        PrintStream es = classTable.semantError(this,className);
        es.println("Attr " + name +            
               " is defined in ancestor class.");
    }

    if (!classTable.isDefined(type_decl)) {
        PrintStream es = classTable.semantError(this,className);
        es.println("Declared type " + type_decl + " of attr " + name +
               " is undefined.");
    }
    else {
        //Class is defined;
        if (exprType!=TreeConstants.No_type) {
        //Check the type conformity of the init expression to type_decl
        if (!classTable.conforms(exprType,type_decl,className)) {
            PrintStream es = classTable.semantError(this,className);
            es.println("Inferred type " + exprType + " does not conform to declared type " + type_decl + " in attr init");
        }
        } 
    }
    }
    
    public void dump_with_types(PrintStream out, int n) {
        dump_line(out, n);
        out.println(Utilities.pad(n) + "_attr");
        dump_AbstractSymbol(out, n + 2, name);
        dump_AbstractSymbol(out, n + 2, type_decl);
    init.dump_with_types(out, n + 2);
    }

}


/** Defines AST constructor 'formalc'.
    <p>
    See <a href="TreeNode.html">TreeNode</a> for full documentation. */
class formalc extends Formal {
    protected AbstractSymbol name;
    protected AbstractSymbol type_decl;
    /** Creates "formalc" AST node. 
      *
      * @param lineNumber the line in the source file from which this node came.
      * @param a0 initial value for name
      * @param a1 initial value for type_decl
      */
    public formalc(int lineNumber, AbstractSymbol a1, AbstractSymbol a2) {
        super(lineNumber);
        name = a1;
        type_decl = a2;
    }
    
    public AbstractSymbol getName() {
    return name;
    }
    
    public AbstractSymbol getType() {
    return type_decl;
    }

    public TreeNode copy() {
        return new formalc(lineNumber, copy_AbstractSymbol(name), copy_AbstractSymbol(type_decl));
    }
    public void dump(PrintStream out, int n) {
        out.print(Utilities.pad(n) + "formalc\n");
        dump_AbstractSymbol(out, n+2, name);
        dump_AbstractSymbol(out, n+2, type_decl);
    }
    
    
    public void dump_with_types(PrintStream out, int n) {
        dump_line(out, n);
        out.println(Utilities.pad(n) + "_formal");
        dump_AbstractSymbol(out, n + 2, name);
        dump_AbstractSymbol(out, n + 2, type_decl);
    }

}


/** Defines AST constructor 'branch'.
    <p>
    See <a href="TreeNode.html">TreeNode</a> for full documentation. */
class branch extends Case {
    protected AbstractSymbol name;
    protected AbstractSymbol type_decl;
    protected Expression expr;
    /** Creates "branch" AST node. 
      *
      * @param lineNumber the line in the source file from which this node came.
      * @param a0 initial value for name
      * @param a1 initial value for type_decl
      * @param a2 initial value for expr
      */
    public branch(int lineNumber, AbstractSymbol a1, AbstractSymbol a2, Expression a3) {
        super(lineNumber);
        name = a1;
        type_decl = a2;
        expr = a3;
    }
    public TreeNode copy() {
        return new branch(lineNumber, copy_AbstractSymbol(name), copy_AbstractSymbol(type_decl), (Expression)expr.copy());
    }
    public void dump(PrintStream out, int n) {
        out.print(Utilities.pad(n) + "branch\n");
        dump_AbstractSymbol(out, n+2, name);
        dump_AbstractSymbol(out, n+2, type_decl);
        expr.dump(out, n+2);
    }

    public AbstractSymbol getType() {
    return type_decl;
    }

    public void dump_with_types(PrintStream out, int n) {
        dump_line(out, n);
        out.println(Utilities.pad(n) + "_branch");
        dump_AbstractSymbol(out, n + 2, name);
        dump_AbstractSymbol(out, n + 2, type_decl);
    expr.dump_with_types(out, n + 2);
    }

     public AbstractSymbol typeCheck (ClassTable classTable, AbstractSymbol className)
     {
    SymbolTable objTable = (classTable.lookup(className)).getObjTable();
        objTable.enterScope();

        if (!classTable.isDefined(type_decl)) {
            PrintStream es = classTable.semantError(this,className);
            es.println("Declared type " + type_decl + " of branch variable " + name +
                       " is undefined.");
        }

    if (type_decl == TreeConstants.SELF_TYPE) {
        PrintStream es = classTable.semantError(this,className);
        es.println("'SELF_TYPE' as declared type in case branch");
        type_decl = TreeConstants.Object_;
    }

    if (name == TreeConstants.self) {
        PrintStream es = classTable.semantError(this,className);
        es.println("binding 'self' in case");
    }
    else //everything but self
        objTable.addId(name,type_decl);
    
    AbstractSymbol branchType = expr.typeCheck(classTable,className);
    objTable.exitScope();
        
    //this.set_type(branchType);
    return branchType;
                                        
     }
}


/** Defines AST constructor 'assign'.
    <p>
    See <a href="TreeNode.html">TreeNode</a> for full documentation. */
class assign extends Expression {
    protected AbstractSymbol name;
    protected Expression expr;
    /** Creates "assign" AST node. 
      *
      * @param lineNumber the line in the source file from which this node came.
      * @param a0 initial value for name
      * @param a1 initial value for expr
      */
    public assign(int lineNumber, AbstractSymbol a1, Expression a2) {
        super(lineNumber);
        name = a1;
        expr = a2;
    }
    public TreeNode copy() {
        return new assign(lineNumber, copy_AbstractSymbol(name), (Expression)expr.copy());
    }
    public void dump(PrintStream out, int n) {
        out.print(Utilities.pad(n) + "assign\n");
        dump_AbstractSymbol(out, n+2, name);
        expr.dump(out, n+2);
    }

    public AbstractSymbol typeCheck (ClassTable classTable, AbstractSymbol className) {
        AbstractSymbol e2T = expr.typeCheck(classTable, className);

    if (name == TreeConstants.self) {
        PrintStream es = classTable.semantError(this,className);
        es.println("assign to 'self'");
    }

    //lookupObj walks the ancestors classes for us
    AbstractSymbol type = classTable.lookupObj(name,className); 
    if (type == TreeConstants.No_type) {
        PrintStream es = classTable.semantError(this,className);
        es.println("Assign to undeclared identifier: " + name);
        this.set_type(TreeConstants.Object_);
        return TreeConstants.Object_;
    }

    if(!classTable.isDefined(type)){
        PrintStream es = classTable.semantError(this,className);
            es.println("Invalid type: " + type + " for assign identifier "+ name);
        this.set_type(TreeConstants.Object_);
            return TreeConstants.Object_;
    }

        if (!classTable.conforms(e2T, type, className)) {
            PrintStream es = classTable.semantError(this,className);
            es.println("Non-conforming arguments: " + type + " <- " + e2T);
        }

        this.set_type(e2T);
        return e2T;
    
    }
    
    public void dump_with_types(PrintStream out, int n) {
        dump_line(out, n);
        out.println(Utilities.pad(n) + "_assign");
        dump_AbstractSymbol(out, n + 2, name);
    expr.dump_with_types(out, n + 2);
    dump_type(out, n);
    }

}


/** Defines AST constructor 'static_dispatch'.
    <p>
    See <a href="TreeNode.html">TreeNode</a> for full documentation. */
class static_dispatch extends Expression {
    protected Expression expr;
    protected AbstractSymbol type_name;
    protected AbstractSymbol name;
    protected Expressions actual;
    /** Creates "static_dispatch" AST node. 
      *
      * @param lineNumber the line in the source file from which this node came.
      * @param a0 initial value for expr
      * @param a1 initial value for type_name
      * @param a2 initial value for name
      * @param a3 initial value for actual
      */
    public static_dispatch(int lineNumber, Expression a1, AbstractSymbol a2, AbstractSymbol a3, Expressions a4) {
        super(lineNumber);
        expr = a1;
        type_name = a2;
        name = a3;
        actual = a4;
    }
    public TreeNode copy() {
        return new static_dispatch(lineNumber, (Expression)expr.copy(), copy_AbstractSymbol(type_name), copy_AbstractSymbol(name), (Expressions)actual.copy());
    }
    public void dump(PrintStream out, int n) {
        out.print(Utilities.pad(n) + "static_dispatch\n");
        expr.dump(out, n+2);
        dump_AbstractSymbol(out, n+2, type_name);
        dump_AbstractSymbol(out, n+2, name);
        actual.dump(out, n+2);
    }


    public AbstractSymbol typeCheck (ClassTable classTable, AbstractSymbol className) {
    method mthd;
    Enumeration fe;
    boolean errors=false;
    AbstractSymbol e0Type = expr.typeCheck(classTable, className);
    
    /*** GENERAL NOTE ***/
    /* A lot of the ugly branching is to ensure that all expression
       got checked, even in error cases */

    if (type_name == TreeConstants.SELF_TYPE) {
            PrintStream es = classTable.semantError(this,className);
            es.println("Cannot statically dispatch method " + name + 
               " from SELF_TYPE");
        errors = true;
    }
    else {
        if (!classTable.isDefined(type_name)) {
        PrintStream es = classTable.semantError(this,className);
        es.println("Cannot statically dispatch method " + name + 
               " from undefined class " + type_name);
        errors = true;
        }
        else {
        if (!classTable.conforms(e0Type,type_name,className)) {
            PrintStream es = classTable.semantError(this,className);
            es.println(e0Type + "@" + type_name + " does not conform "
                   + "in static dispatch");
            errors = true;
        }
        }
    }
    
    //This next block sets the appropriate values of mthd and
    //fe
    
    if (errors) {
        //The declared type_name was bad
        mthd = null;
        fe = (new Formals(0)).getElements(); //Create dummy 0-element enumeration of formals
    }
    else {
        //Do the lookup of this method.  classTable.lookupMthd will
        //walk the class hierarchy for us
        mthd = classTable.lookupMthd(name,type_name);
        if (mthd==null) {
        //Mthd not defined
        PrintStream es = classTable.semantError(this,className);
        es.println("Method " + name + " not defined in class " +
               type_name + " or any of its ancestors");
        errors = true;
        fe = (new Formals(0)).getElements(); //Create dummy 0-element enumeration of formals
        }
        else {
        //Mthd is defined.
        //Check that it's got the same number of params as we have
        //args
        if (mthd.getFormals().getLength() != actual.getLength()) {
            PrintStream es = classTable.semantError(this,className);
            es.println("Method " + name + " in class " +
                   type_name + " expects " +
                   mthd.getFormals().getLength() + " arguments, but got "
                   + actual.getLength());
            errors = true;
            fe = (new Formals(0)).getElements(); //Create dummy 0-element enumeration of formals
        }
        else
            fe = mthd.getFormals().getElements();
        }
    }
    
    
        Enumeration ae = actual.getElements();
    //Now go element by element
    while (ae.hasMoreElements()) {
        Formal curFormal; 
        if (fe.hasMoreElements())
        curFormal = (Formal)fe.nextElement(); 
        else
        curFormal = null;
        Expression curExp = (Expression)ae.nextElement();
        AbstractSymbol expType = curExp.typeCheck(classTable,className);

        //check that the type declaration of each formal is valid
        //We only want to run conform on valid types
        //Don't need to print error message for undefined type or
        //SELF_TYPE used for formal parameters again, since it is
        //done when typechecking the method declaration.
        if ((curFormal != null) && 
        classTable.isDefined(curFormal.getType())) { 
        if (!classTable.conforms(expType, curFormal.getType(), className)) {
            PrintStream es = classTable.semantError(this,className);
            es.println("Inferred type " + expType + " of param " +
                   curFormal.getName() + 
                   " does not conform to expected type " +
                   curFormal.getType());
            errors=true;
        }
        }
    }
    
    if (errors) {
        this.set_type(TreeConstants.Object_);
        return TreeConstants.Object_;
    }
    else if (mthd.getType() == TreeConstants.SELF_TYPE){
        this.set_type(e0Type);
        return e0Type;
    }
    else {
        this.set_type(mthd.getType());
        return mthd.getType();
    }
       
    }

    public void dump_with_types(PrintStream out, int n) {
        dump_line(out, n);
        out.println(Utilities.pad(n) + "_static_dispatch");
    expr.dump_with_types(out, n + 2);
        dump_AbstractSymbol(out, n + 2, type_name);
        dump_AbstractSymbol(out, n + 2, name);
        out.println(Utilities.pad(n + 2) + "(");
        for (Enumeration e = actual.getElements(); e.hasMoreElements();) {
        ((Expression)e.nextElement()).dump_with_types(out, n + 2);
        }
        out.println(Utilities.pad(n + 2) + ")");
    dump_type(out, n);
    }

}


/** Defines AST constructor 'dispatch'.
    <p>
    See <a href="TreeNode.html">TreeNode</a> for full documentation. */
class dispatch extends Expression {
    protected Expression expr;
    protected AbstractSymbol name;
    protected Expressions actual;
    /** Creates "dispatch" AST node. 
      *
      * @param lineNumber the line in the source file from which this node came.
      * @param a0 initial value for expr
      * @param a1 initial value for name
      * @param a2 initial value for actual
      */
    public dispatch(int lineNumber, Expression a1, AbstractSymbol a2, Expressions a3) {
        super(lineNumber);
        expr = a1;
        name = a2;
        actual = a3;
    }
    public TreeNode copy() {
        return new dispatch(lineNumber, (Expression)expr.copy(), copy_AbstractSymbol(name), (Expressions)actual.copy());
    }
    public void dump(PrintStream out, int n) {
        out.print(Utilities.pad(n) + "dispatch\n");
        expr.dump(out, n+2);
        dump_AbstractSymbol(out, n+2, name);
        actual.dump(out, n+2);
    }


    public AbstractSymbol typeCheck (ClassTable classTable, AbstractSymbol className) {
        Enumeration fe;
    AbstractSymbol fromType;
    boolean errors=false;
    AbstractSymbol e0Type = expr.typeCheck(classTable, className);
    
    //If expr is self, we want to start searching from the current class.
    if (e0Type == TreeConstants.SELF_TYPE) 
        fromType = className;
    else
        fromType = e0Type;

    //Do the lookup of this method.  classTable.lookupMthd will
    //walk the class hierarchy for us
    method mthd = classTable.lookupMthd(name,fromType);
    if (mthd==null) {
            PrintStream es = classTable.semantError(this,className);
            es.println("Method " + name + " not defined in class " +
               fromType + " or any of its ancestors");
        errors = true;
        fe = (new Formals(0)).getElements(); //Create dummy 0-element enumeration of formals
    }
    //Method exists.
    //Now check the conformity of each argument to each formal
    //parameter  
    //First check that they've got the same number of args
    else {
        if (mthd.getFormals().getLength() != actual.getLength()) {
        PrintStream es = classTable.semantError(this,className);
        es.println("Method " + name + " in class " +
               fromType + " expects " +
               mthd.getFormals().getLength() + " arguments, but got "
               + actual.getLength());
        errors = true;
        fe = (new Formals(0)).getElements(); //Create dummy 0-element enumeration of formals
        }
        else 
        fe = mthd.getFormals().getElements();
    }

        Enumeration ae = actual.getElements();
    //Now go element by element
    while (ae.hasMoreElements()) {
        Formal curFormal; 
        if (fe.hasMoreElements())
        curFormal = (Formal)fe.nextElement(); 
        else
        curFormal = null;
        Expression curExp = (Expression)ae.nextElement();
        AbstractSymbol expType = curExp.typeCheck(classTable,className);

        //check that the type declaration of each formal is valid
        //We only want to run conform on valid types
        //Don't need to print error message for undefined type or
        //SELF_TYPE used for formal parameters again, since it is
        //done when typechecking the method declaration.
        if ((curFormal != null) && 
        classTable.isDefined(curFormal.getType())) { 
        if (!classTable.conforms(expType, curFormal.getType(), className)) {
            PrintStream es = classTable.semantError(this,className);
            es.println("Inferred type " + expType + " of param " +
                   curFormal.getName() + 
                   " does not conform to expected type " +
                   curFormal.getType());
            errors=true;
        }
        }
    }
    
    if (errors) {
        this.set_type(TreeConstants.Object_);
        return TreeConstants.Object_;
    }
    else if (mthd.getType() == TreeConstants.SELF_TYPE){
        this.set_type(e0Type);
        return e0Type;
    }
    else {
        this.set_type(mthd.getType());
        return mthd.getType();
    }
       
    }
    
    public void dump_with_types(PrintStream out, int n) {
        dump_line(out, n);
        out.println(Utilities.pad(n) + "_dispatch");
    expr.dump_with_types(out, n + 2);
        dump_AbstractSymbol(out, n + 2, name);
        out.println(Utilities.pad(n + 2) + "(");
        for (Enumeration e = actual.getElements(); e.hasMoreElements();) {
        ((Expression)e.nextElement()).dump_with_types(out, n + 2);
        }
        out.println(Utilities.pad(n + 2) + ")");
    dump_type(out, n);
    }

}


/** Defines AST constructor 'cond'.
    <p>
    See <a href="TreeNode.html">TreeNode</a> for full documentation. */
class cond extends Expression {
    protected Expression pred;
    protected Expression then_exp;
    protected Expression else_exp;
    /** Creates "cond" AST node. 
      *
      * @param lineNumber the line in the source file from which this node came.
      * @param a0 initial value for pred
      * @param a1 initial value for then_exp
      * @param a2 initial value for else_exp
      */
    public cond(int lineNumber, Expression a1, Expression a2, Expression a3) {
        super(lineNumber);
        pred = a1;
        then_exp = a2;
        else_exp = a3;
    }
    public TreeNode copy() {
        return new cond(lineNumber, (Expression)pred.copy(), (Expression)then_exp.copy(), (Expression)else_exp.copy());
    }
    public void dump(PrintStream out, int n) {
        out.print(Utilities.pad(n) + "cond\n");
        pred.dump(out, n+2);
        then_exp.dump(out, n+2);
        else_exp.dump(out, n+2);
    }

    public AbstractSymbol typeCheck (ClassTable classTable, AbstractSymbol className) {
    AbstractSymbol e1T = pred.typeCheck(classTable, className);
        AbstractSymbol e2T = then_exp.typeCheck(classTable, className);
    AbstractSymbol e3T = else_exp.typeCheck(classTable, className);

        if ((e1T != TreeConstants.Bool)) {
            PrintStream es = classTable.semantError(this,className);
            es.println("Non-Bool argument: if" + e1T + " then " + e2T + " else " + e3T);
        }
    
        this.set_type(classTable.lub(e2T, e3T, className));
        return classTable.lub(e2T, e3T, className);

    }
    
    public void dump_with_types(PrintStream out, int n) {
        dump_line(out, n);
        out.println(Utilities.pad(n) + "_cond");
    pred.dump_with_types(out, n + 2);
    then_exp.dump_with_types(out, n + 2);
    else_exp.dump_with_types(out, n + 2);
    dump_type(out, n);
    }

}


/** Defines AST constructor 'loop'.
    <p>
    See <a href="TreeNode.html">TreeNode</a> for full documentation. */
class loop extends Expression {
    protected Expression pred;
    protected Expression body;
    /** Creates "loop" AST node. 
      *
      * @param lineNumber the line in the source file from which this node came.
      * @param a0 initial value for pred
      * @param a1 initial value for body
      */
    public loop(int lineNumber, Expression a1, Expression a2) {
        super(lineNumber);
        pred = a1;
        body = a2;
    }
    public TreeNode copy() {
        return new loop(lineNumber, (Expression)pred.copy(), (Expression)body.copy());
    }
    public void dump(PrintStream out, int n) {
        out.print(Utilities.pad(n) + "loop\n");
        pred.dump(out, n+2);
        body.dump(out, n+2);
    }

    
    public void dump_with_types(PrintStream out, int n) {
        dump_line(out, n);
        out.println(Utilities.pad(n) + "_loop");
    pred.dump_with_types(out, n + 2);
    body.dump_with_types(out, n + 2);
    dump_type(out, n);
    }

    public AbstractSymbol typeCheck (ClassTable classTable, AbstractSymbol className) {

     AbstractSymbol e1T = pred.typeCheck(classTable, className);
        AbstractSymbol e2T = body.typeCheck(classTable, className);

        if ((e1T != TreeConstants.Bool)) {
            PrintStream es = classTable.semantError(this,className);
            es.println("Non-Bool argument: while " + e1T + " loop " + e2T + " pool");
        }

        this.set_type(TreeConstants.Object_);
        return TreeConstants.Object_;

    }

}


/** Defines AST constructor 'typcase'.
    <p>
    See <a href="TreeNode.html">TreeNode</a> for full documentation. */
class typcase extends Expression {
    protected Expression expr;
    protected Cases cases;
    /** Creates "typcase" AST node. 
      *
      * @param lineNumber the line in the source file from which this node came.
      * @param a0 initial value for expr
      * @param a1 initial value for cases
      */
    public typcase(int lineNumber, Expression a1, Cases a2) {
        super(lineNumber);
        expr = a1;
        cases = a2;
    }
    public TreeNode copy() {
        return new typcase(lineNumber, (Expression)expr.copy(), (Cases)cases.copy());
    }
    public void dump(PrintStream out, int n) {
        out.print(Utilities.pad(n) + "typcase\n");
        expr.dump(out, n+2);
        cases.dump(out, n+2);
    }
    
    public void dump_with_types(PrintStream out, int n) {
        dump_line(out, n);
        out.println(Utilities.pad(n) + "_typcase");
    expr.dump_with_types(out, n + 2);
        for (Enumeration e = cases.getElements(); e.hasMoreElements();) {
        ((Case)e.nextElement()).dump_with_types(out, n + 2);
        }
    dump_type(out, n);
    }
    
    public AbstractSymbol typeCheck (ClassTable classTable, AbstractSymbol className) {
    AbstractSymbol e1T = expr.typeCheck(classTable, className);
        AbstractSymbol e2T = null; 
    Vector vtypes= new Vector();

    for (Enumeration e = cases.getElements(); e.hasMoreElements();) {
        Case curCase = (Case)e.nextElement();
        if(vtypes.contains(curCase.getType()))      {
            PrintStream es = classTable.semantError(curCase,className);
            es.println("Duplicate type in case branch: " + curCase.getType());
        }
        else
        vtypes.add(curCase.getType());

        if(e2T==null)       {
               e2T=curCase.typeCheck(classTable,className);
            }
        else  {
           AbstractSymbol temp = curCase.typeCheck(classTable, className);
           e2T =classTable.lub(e2T, temp, className); 
        }
        }
    
    this.set_type(e2T); 
    return e2T;
    }
}


/** Defines AST constructor 'block'.
    <p>
    See <a href="TreeNode.html">TreeNode</a> for full documentation. */
class block extends Expression {
    protected Expressions body;
    /** Creates "block" AST node. 
      *
      * @param lineNumber the line in the source file from which this node came.
      * @param a0 initial value for body
      */
    public block(int lineNumber, Expressions a1) {
        super(lineNumber);
        body = a1;
    }
    public TreeNode copy() {
        return new block(lineNumber, (Expressions)body.copy());
    }
    public void dump(PrintStream out, int n) {
        out.print(Utilities.pad(n) + "block\n");
        body.dump(out, n+2);
    }

    
    public void dump_with_types(PrintStream out, int n) {
        dump_line(out, n);
        out.println(Utilities.pad(n) + "_block");
        for (Enumeration e = body.getElements(); e.hasMoreElements();) {
        ((Expression)e.nextElement()).dump_with_types(out, n + 2);
        }
    dump_type(out, n);
    }
    
    public AbstractSymbol typeCheck (ClassTable classTable, AbstractSymbol className){
    AbstractSymbol e1T=null;
    for (Enumeration e = body.getElements(); e.hasMoreElements();) {
            e1T=((Expression)e.nextElement()).typeCheck(classTable, className);
        }
    
    this.set_type(e1T);
    return e1T;
    }
}


/** Defines AST constructor 'let'.
    <p>
    See <a href="TreeNode.html">TreeNode</a> for full documentation. */
class let extends Expression {
    protected AbstractSymbol identifier;
    protected AbstractSymbol type_decl;
    protected Expression init;
    protected Expression body;
    /** Creates "let" AST node. 
      *
      * @param lineNumber the line in the source file from which this node came.
      * @param a0 initial value for identifier
      * @param a1 initial value for type_decl
      * @param a2 initial value for init
      * @param a3 initial value for body
      */
    public let(int lineNumber, AbstractSymbol a1, AbstractSymbol a2, Expression a3, Expression a4) {
        super(lineNumber);
        identifier = a1;
        type_decl = a2;
        init = a3;
        body = a4;
    }
    public TreeNode copy() {
        return new let(lineNumber, copy_AbstractSymbol(identifier), copy_AbstractSymbol(type_decl), (Expression)init.copy(), (Expression)body.copy());
    }
    public void dump(PrintStream out, int n) {
        out.print(Utilities.pad(n) + "let\n");
        dump_AbstractSymbol(out, n+2, identifier);
        dump_AbstractSymbol(out, n+2, type_decl);
        init.dump(out, n+2);
        body.dump(out, n+2);
    }

    
    public void dump_with_types(PrintStream out, int n) {
        dump_line(out, n);
        out.println(Utilities.pad(n) + "_let");
    dump_AbstractSymbol(out, n + 2, identifier);
    dump_AbstractSymbol(out, n + 2, type_decl);
    init.dump_with_types(out, n + 2);
    body.dump_with_types(out, n + 2);
    dump_type(out, n);
    }

    public AbstractSymbol typeCheck (ClassTable classTable, AbstractSymbol className) {
    SymbolTable objTable = (classTable.lookup(className)).getObjTable();
    boolean errors = false;
    AbstractSymbol exprType = init.typeCheck(classTable,className); 
    objTable.enterScope();

    if (!classTable.isDefined(type_decl)) {
        PrintStream es = classTable.semantError(this,className);
        es.println("Declared type " + type_decl + " of let variable " + identifier +
               " is undefined.");
        errors = true;
    }
    else {
        //Declared Type is defined
        if (exprType != TreeConstants.No_type) {
        //Check the type conformity of the init expression to
        //type_decl, unless there was no init
        if (!classTable.conforms(exprType,type_decl,className)) {
            PrintStream es = classTable.semantError(this,className);
            es.println("Inferred type " + exprType + " does not conform to declared type " + type_decl + " in let init for var " + identifier );
            errors = true;
        }
        } 
    }
        /* Even if type is bad, still insert the name:type since we'll
       check the type upon use, unless the name is 'self' */
    if (identifier == TreeConstants.self) {
        PrintStream es = classTable.semantError(this,className);
        es.println("binding 'self' in a 'let'");
    }
    else
        objTable.addId(identifier,type_decl); 

    //Now we type check the body with the new Env
    AbstractSymbol bodyType = body.typeCheck(classTable,className);

    objTable.exitScope();

    this.set_type(bodyType);
    return bodyType;

    }
}


/** Defines AST constructor 'plus'.
    <p>
    See <a href="TreeNode.html">TreeNode</a> for full documentation. */
class plus extends Expression {
    protected Expression e1;
    protected Expression e2;
    /** Creates "plus" AST node. 
      *
      * @param lineNumber the line in the source file from which this node came.
      * @param a0 initial value for e1
      * @param a1 initial value for e2
      */
    public plus(int lineNumber, Expression a1, Expression a2) {
        super(lineNumber);
        e1 = a1;
        e2 = a2;
    }
    public TreeNode copy() {
        return new plus(lineNumber, (Expression)e1.copy(), (Expression)e2.copy());
    }
    public void dump(PrintStream out, int n) {
        out.print(Utilities.pad(n) + "plus\n");
        e1.dump(out, n+2);
        e2.dump(out, n+2);
    }

    
    public void dump_with_types(PrintStream out, int n) {
        dump_line(out, n);
        out.println(Utilities.pad(n) + "_plus");
    e1.dump_with_types(out, n + 2);
    e2.dump_with_types(out, n + 2);
    dump_type(out, n);
    }
    
    public AbstractSymbol typeCheck (ClassTable classTable, AbstractSymbol className) {
    AbstractSymbol e1T = e1.typeCheck(classTable, className);
    AbstractSymbol e2T = e2.typeCheck(classTable, className);

    if ((e1T != TreeConstants.Int) || (e2T != TreeConstants.Int)) {
        PrintStream es = classTable.semantError(this,className);
        es.println("Non-Int arguments: " + e1T + " + " + e2T);
    }

    this.set_type(TreeConstants.Int);
    return TreeConstants.Int;
    }
}


/** Defines AST constructor 'sub'.
    <p>
    See <a href="TreeNode.html">TreeNode</a> for full documentation. */
class sub extends Expression {
    protected Expression e1;
    protected Expression e2;
    /** Creates "sub" AST node. 
      *
      * @param lineNumber the line in the source file from which this node came.
      * @param a0 initial value for e1
      * @param a1 initial value for e2
      */
    public sub(int lineNumber, Expression a1, Expression a2) {
        super(lineNumber);
        e1 = a1;
        e2 = a2;
    }
    public TreeNode copy() {
        return new sub(lineNumber, (Expression)e1.copy(), (Expression)e2.copy());
    }
    public void dump(PrintStream out, int n) {
        out.print(Utilities.pad(n) + "sub\n");
        e1.dump(out, n+2);
        e2.dump(out, n+2);
    }
    
    
    public void dump_with_types(PrintStream out, int n) {
        dump_line(out, n);
        out.println(Utilities.pad(n) + "_sub");
    e1.dump_with_types(out, n + 2);
    e2.dump_with_types(out, n + 2);
    dump_type(out, n);
    }
    
    public AbstractSymbol typeCheck (ClassTable classTable, AbstractSymbol className) {
    AbstractSymbol e1T = e1.typeCheck(classTable, className);
    AbstractSymbol e2T = e2.typeCheck(classTable, className);

    if ((e1T != TreeConstants.Int) || (e2T != TreeConstants.Int)) {
        PrintStream es = classTable.semantError(this,className);
        es.println("Non-Int arguments: " + e1T + " - " + e2T);
    }

    this.set_type(TreeConstants.Int);
    return TreeConstants.Int;
    }   
}


/** Defines AST constructor 'mul'.
    <p>
    See <a href="TreeNode.html">TreeNode</a> for full documentation. */
class mul extends Expression {
    protected Expression e1;
    protected Expression e2;
    /** Creates "mul" AST node. 
      *
      * @param lineNumber the line in the source file from which this node came.
      * @param a0 initial value for e1
      * @param a1 initial value for e2
      */
    public mul(int lineNumber, Expression a1, Expression a2) {
        super(lineNumber);
        e1 = a1;
        e2 = a2;
    }
    public TreeNode copy() {
        return new mul(lineNumber, (Expression)e1.copy(), (Expression)e2.copy());
    }
    public void dump(PrintStream out, int n) {
        out.print(Utilities.pad(n) + "mul\n");
        e1.dump(out, n+2);
        e2.dump(out, n+2);
    }

    
    public void dump_with_types(PrintStream out, int n) {
        dump_line(out, n);
        out.println(Utilities.pad(n) + "_mul");
    e1.dump_with_types(out, n + 2);
    e2.dump_with_types(out, n + 2);
    dump_type(out, n);
    }
    
    public AbstractSymbol typeCheck (ClassTable classTable, AbstractSymbol className) {
    AbstractSymbol e1T = e1.typeCheck(classTable, className);
    AbstractSymbol e2T = e2.typeCheck(classTable, className);

    if ((e1T != TreeConstants.Int) || (e2T != TreeConstants.Int)) {
        PrintStream es = classTable.semantError(this,className);
        es.println("Non-Int arguments: " + e1T + " * " + e2T);
    }

    this.set_type(TreeConstants.Int);
    return TreeConstants.Int;
    }
}



/** Defines AST constructor 'divide'.
    <p>
    See <a href="TreeNode.html">TreeNode</a> for full documentation. */
class divide extends Expression {
    protected Expression e1;
    protected Expression e2;
    /** Creates "divide" AST node. 
      *
      * @param lineNumber the line in the source file from which this node came.
      * @param a0 initial value for e1
      * @param a1 initial value for e2
      */
    public divide(int lineNumber, Expression a1, Expression a2) {
        super(lineNumber);
        e1 = a1;
        e2 = a2;
    }
    public TreeNode copy() {
        return new divide(lineNumber, (Expression)e1.copy(), (Expression)e2.copy());
    }
    public void dump(PrintStream out, int n) {
        out.print(Utilities.pad(n) + "divide\n");
        e1.dump(out, n+2);
        e2.dump(out, n+2);
    }

    
    public void dump_with_types(PrintStream out, int n) {
        dump_line(out, n);
        out.println(Utilities.pad(n) + "_divide");
    e1.dump_with_types(out, n + 2);
    e2.dump_with_types(out, n + 2);
    dump_type(out, n);
    }
    
    public AbstractSymbol typeCheck (ClassTable classTable, AbstractSymbol className) {
    AbstractSymbol e1T = e1.typeCheck(classTable, className);
    AbstractSymbol e2T = e2.typeCheck(classTable, className);

    if ((e1T != TreeConstants.Int) || (e2T != TreeConstants.Int)) {
        PrintStream es = classTable.semantError(this,className);
        es.println("Non-Int arguments: " + e1T + " / " + e2T);
    }

    this.set_type(TreeConstants.Int);
    return TreeConstants.Int;

    }
}


/** Defines AST constructor 'neg'.
    <p>
    See <a href="TreeNode.html">TreeNode</a> for full documentation. */
class neg extends Expression {
    protected Expression e1;
    /** Creates "neg" AST node. 
      *
      * @param lineNumber the line in the source file from which this node came.
      * @param a0 initial value for e1
      */
    public neg(int lineNumber, Expression a1) {
        super(lineNumber);
        e1 = a1;
    }
    public TreeNode copy() {
        return new neg(lineNumber, (Expression)e1.copy());
    }
    public void dump(PrintStream out, int n) {
        out.print(Utilities.pad(n) + "neg\n");
        e1.dump(out, n+2);
    }

    
    public void dump_with_types(PrintStream out, int n) {
        dump_line(out, n);
        out.println(Utilities.pad(n) + "_neg");
    e1.dump_with_types(out, n + 2);
    dump_type(out, n);
    }
        
    public AbstractSymbol typeCheck (ClassTable classTable, AbstractSymbol className) {
    AbstractSymbol e1T = e1.typeCheck(classTable, className);

    if (e1T != TreeConstants.Int) {
        PrintStream es = classTable.semantError(this,className);
        es.println("'~' expression got arg of type " + e1T + ", expected Int");
    }
    this.set_type(TreeConstants.Int);
    return TreeConstants.Int;

    }
}


/** Defines AST constructor 'lt'.
    <p>
    See <a href="TreeNode.html">TreeNode</a> for full documentation. */
class lt extends Expression {
    protected Expression e1;
    protected Expression e2;
    /** Creates "lt" AST node. 
      *
      * @param lineNumber the line in the source file from which this node came.
      * @param a0 initial value for e1
      * @param a1 initial value for e2
      */
    public lt(int lineNumber, Expression a1, Expression a2) {
        super(lineNumber);
        e1 = a1;
        e2 = a2;
    }
    public TreeNode copy() {
        return new lt(lineNumber, (Expression)e1.copy(), (Expression)e2.copy());
    }
    public void dump(PrintStream out, int n) {
        out.print(Utilities.pad(n) + "lt\n");
        e1.dump(out, n+2);
        e2.dump(out, n+2);
    }

    
    public void dump_with_types(PrintStream out, int n) {
        dump_line(out, n);
        out.println(Utilities.pad(n) + "_lt");
    e1.dump_with_types(out, n + 2);
    e2.dump_with_types(out, n + 2);
    dump_type(out, n);
    }


    public AbstractSymbol typeCheck (ClassTable classTable, AbstractSymbol className) {
    AbstractSymbol e1T = e1.typeCheck(classTable, className);
    AbstractSymbol e2T = e2.typeCheck(classTable, className);



    if ((e1T != TreeConstants.Int) || (e2T != TreeConstants.Int)) {
        PrintStream es = classTable.semantError(this,className);
        es.println("Non-Int arguments: " + e1T + " < " + e2T);
    }
        
    this.set_type(TreeConstants.Bool);
    return TreeConstants.Bool;

    }

}


/** Defines AST constructor 'eq'.
    <p>
    See <a href="TreeNode.html">TreeNode</a> for full documentation. */
class eq extends Expression {
    protected Expression e1;
    protected Expression e2;
    /** Creates "eq" AST node. 
      *
      * @param lineNumber the line in the source file from which this node came.
      * @param a0 initial value for e1
      * @param a1 initial value for e2
      */
    public eq(int lineNumber, Expression a1, Expression a2) {
        super(lineNumber);
        e1 = a1;
        e2 = a2;
    }
    public TreeNode copy() {
        return new eq(lineNumber, (Expression)e1.copy(), (Expression)e2.copy());
    }
    public void dump(PrintStream out, int n) {
        out.print(Utilities.pad(n) + "eq\n");
        e1.dump(out, n+2);
        e2.dump(out, n+2);
    }

    
    public void dump_with_types(PrintStream out, int n) {
        dump_line(out, n);
        out.println(Utilities.pad(n) + "_eq");
    e1.dump_with_types(out, n + 2);
    e2.dump_with_types(out, n + 2);
    dump_type(out, n);
    }

    public AbstractSymbol typeCheck (ClassTable classTable, AbstractSymbol className) {
    AbstractSymbol e1T = e1.typeCheck(classTable, className);
    AbstractSymbol e2T = e2.typeCheck(classTable, className);
    
    if ((e1T == TreeConstants.Int) || (e2T == TreeConstants.Int)) {
        if (e1T != e2T) {
        PrintStream es = classTable.semantError(this,className);
        es.println("'=' expression got args of type " + e1T +
               " and " + e2T + 
               ".  Can only compare 2 expressions of type Intx");
        }
    }

    if ((e1T == TreeConstants.Bool) || (e2T == TreeConstants.Bool)) {
        if (e1T != e2T) {
        PrintStream es = classTable.semantError(this,className);
        es.println("'==' expression got args of type " + e1T +
               " and " + e2T + 
               ".  Can only compare 2 expressions of type Bool");
        }
    }

    if ((e1T == TreeConstants.Str) || (e2T == TreeConstants.Str)) {
        if (e1T != e2T) {
        PrintStream es = classTable.semantError(this,className);
        es.println("'==' expression got args of type " + e1T +
               " and " + e2T + 
               ".  Can only compare 2 expressions of type String");
        }
    }
    this.set_type(TreeConstants.Bool);
    return TreeConstants.Bool;
    }
}


/** Defines AST constructor 'leq'.
    <p>
    See <a href="TreeNode.html">TreeNode</a> for full documentation. */
class leq extends Expression {
    protected Expression e1;
    protected Expression e2;
    /** Creates "leq" AST node. 
      *
      * @param lineNumber the line in the source file from which this node came.
      * @param a0 initial value for e1
      * @param a1 initial value for e2
      */
    public leq(int lineNumber, Expression a1, Expression a2) {
        super(lineNumber);
        e1 = a1;
        e2 = a2;
    }
    public TreeNode copy() {
        return new leq(lineNumber, (Expression)e1.copy(), (Expression)e2.copy());
    }
    public void dump(PrintStream out, int n) {
        out.print(Utilities.pad(n) + "leq\n");
        e1.dump(out, n+2);
        e2.dump(out, n+2);
    }

    
    public void dump_with_types(PrintStream out, int n) {
        dump_line(out, n);
        out.println(Utilities.pad(n) + "_leq");
    e1.dump_with_types(out, n + 2);
    e2.dump_with_types(out, n + 2);
    dump_type(out, n);
    }
    public AbstractSymbol typeCheck (ClassTable classTable, AbstractSymbol className) {
    AbstractSymbol e1T = e1.typeCheck(classTable, className);
    AbstractSymbol e2T = e2.typeCheck(classTable, className);

    if ((e1T != TreeConstants.Int) || (e2T != TreeConstants.Int)) {
        PrintStream es = classTable.semantError(this,className);
        es.println("Non-Int arguments: " + e1T + " <= " + e2T);
    }

    this.set_type(TreeConstants.Bool);
    return TreeConstants.Bool;

    }
}


/** Defines AST constructor 'comp'.
    <p>
    See <a href="TreeNode.html">TreeNode</a> for full documentation. */
class comp extends Expression {
    protected Expression e1;
    /** Creates "comp" AST node. 
      *
      * @param lineNumber the line in the source file from which this node came.
      * @param a0 initial value for e1
      */
    public comp(int lineNumber, Expression a1) {
        super(lineNumber);
        e1 = a1;
    }
    public TreeNode copy() {
        return new comp(lineNumber, (Expression)e1.copy());
    }
    public void dump(PrintStream out, int n) {
        out.print(Utilities.pad(n) + "comp\n");
        e1.dump(out, n+2);
    }

    
    public void dump_with_types(PrintStream out, int n) {
        dump_line(out, n);
        out.println(Utilities.pad(n) + "_comp");
    e1.dump_with_types(out, n + 2);
    dump_type(out, n);
    }
    public AbstractSymbol typeCheck (ClassTable classTable, AbstractSymbol className) {
    AbstractSymbol e1T = e1.typeCheck(classTable, className);

    if (e1T != TreeConstants.Bool) {
        PrintStream es = classTable.semantError(this,className);
        es.println("'not' expression got arg of type " + e1T + ", expected Bool");
    }
    this.set_type(TreeConstants.Bool);
    return TreeConstants.Bool;

    }
}


/** Defines AST constructor 'int_const'.
    <p>
    See <a href="TreeNode.html">TreeNode</a> for full documentation. */
class int_const extends Expression {
    protected AbstractSymbol token;
    /** Creates "int_const" AST node. 
      *
      * @param lineNumber the line in the source file from which this node came.
      * @param a0 i:q
      * nitial value for token
      */
    public int_const(int lineNumber, AbstractSymbol a1) {
    super(lineNumber);
        token = a1;
    }
    public TreeNode copy() {
        return new int_const(lineNumber, copy_AbstractSymbol(token));
    }
    public void dump(PrintStream out, int n) {
        out.print(Utilities.pad(n) + "int_const\n");
        dump_AbstractSymbol(out, n+2, token);
    }

    public void dump_with_types(PrintStream out, int n) {
        dump_line(out, n);
        out.println(Utilities.pad(n) + "_int");
    dump_AbstractSymbol(out, n + 2, token);
    dump_type(out, n);
    }
    public AbstractSymbol typeCheck(ClassTable ct, AbstractSymbol clsN) {
    this.set_type(TreeConstants.Int);
    return TreeConstants.Int;

    }


}


/** Defines AST constructor 'bool_const'.
    <p>
    See <a href="TreeNode.html">TreeNode</a> for full documentation. */
class bool_const extends Expression {
    protected Boolean val;
    /** Creates "bool_const" AST node. 
      *
      * @param lineNumber the line in the source file from which this node came.
      * @param a0 initial value for val
      */
    public bool_const(int lineNumber, Boolean a1) {
        super(lineNumber);
        val = a1;
    }
    public TreeNode copy() {
        return new bool_const(lineNumber, copy_Boolean(val));
    }
    public void dump(PrintStream out, int n) {
        out.print(Utilities.pad(n) + "bool_const\n");
        dump_Boolean(out, n+2, val);
    }

    
    public void dump_with_types(PrintStream out, int n) {
        dump_line(out, n);
        out.println(Utilities.pad(n) + "_bool");
    dump_Boolean(out, n + 2, val);
    dump_type(out, n);
    }
    public AbstractSymbol typeCheck (ClassTable classTable, AbstractSymbol className) {
    this.set_type(TreeConstants.Bool);
    return TreeConstants.Bool;
    }
}


/** Defines AST constructor 'string_const'.
    <p>
    See <a href="TreeNode.html">TreeNode</a> for full documentation. */
class string_const extends Expression {
    protected AbstractSymbol token;
    /** Creates "string_const" AST node. 
      *
      * @param lineNumber the line in the source file from which this node came.
      * @param a0 initial value for token
      */
    public string_const(int lineNumber, AbstractSymbol a1) {
        super(lineNumber);
        token = a1;
    }
    public TreeNode copy() {
        return new string_const(lineNumber, copy_AbstractSymbol(token));
    }
    public void dump(PrintStream out, int n) {
        out.print(Utilities.pad(n) + "string_const\n");
        dump_AbstractSymbol(out, n+2, token);
    }

    
    public void dump_with_types(PrintStream out, int n) {
        dump_line(out, n);
        out.println(Utilities.pad(n) + "_string");
    out.print(Utilities.pad(n + 2) + "\"");
    Utilities.printEscapedString(out, token.getString());
    out.println("\"");
    dump_type(out, n);
    }
    public AbstractSymbol typeCheck (ClassTable classTable, AbstractSymbol className) {
    this.set_type(TreeConstants.Str);
    return TreeConstants.Str;
    }
}


/** Defines AST constructor 'new_'.
    <p>
    See <a href="TreeNode.html">TreeNode</a> for full documentation. */
class new_ extends Expression {
    protected AbstractSymbol type_name;
    /** Creates "new_" AST node. 
      *
      * @param lineNumber the line in the source file from which this node came.
      * @param a0 initial value for type_name
      */
    public new_(int lineNumber, AbstractSymbol a1) {
        super(lineNumber);
        type_name = a1;
    }
    public TreeNode copy() {
        return new new_(lineNumber, copy_AbstractSymbol(type_name));
    }
    public void dump(PrintStream out, int n) {
        out.print(Utilities.pad(n) + "new_\n");
        dump_AbstractSymbol(out, n+2, type_name);
    }

    
    public void dump_with_types(PrintStream out, int n) {
        dump_line(out, n);
        out.println(Utilities.pad(n) + "_new");
    dump_AbstractSymbol(out, n + 2, type_name);
    dump_type(out, n);
    }
    public AbstractSymbol typeCheck (ClassTable classTable, AbstractSymbol className) {
    if (classTable.isDefined(type_name)) {
        this.set_type(type_name);
        return type_name;
    }
    else {
        PrintStream es = classTable.semantError(this,className);
        es.println("new type " + type_name +
               " is undefined.");   
        this.set_type(TreeConstants.Object_);
        return TreeConstants.Object_;
    }

    }
}


/** Defines AST constructor 'isvoid'.
    <p>
    See <a href="TreeNode.html">TreeNode</a> for full documentation. */
class isvoid extends Expression {
    protected Expression e1;
    /** Creates "isvoid" AST node. 
      *
      * @param lineNumber the line in the source file from which this node came.
      * @param a0 initial value for e1
      */
    public isvoid(int lineNumber, Expression a1) {
        super(lineNumber);
        e1 = a1;
    }
    public TreeNode copy() {
        return new isvoid(lineNumber, (Expression)e1.copy());
    }
    public void dump(PrintStream out, int n) {
        out.print(Utilities.pad(n) + "isvoid\n");
        e1.dump(out, n+2);
    }

    
    public void dump_with_types(PrintStream out, int n) {
        dump_line(out, n);
        out.println(Utilities.pad(n) + "_isvoid");
    e1.dump_with_types(out, n + 2);
    dump_type(out, n);
    }
    public AbstractSymbol typeCheck (ClassTable classTable, AbstractSymbol className) {
    e1.typeCheck(classTable,className);
    this.set_type (TreeConstants.Bool);
    return TreeConstants.Bool;
    }
}


/** Defines AST constructor 'no_expr'.
    <p>
    See <a href="TreeNode.html">TreeNode</a> for full documentation. */
class no_expr extends Expression {
    /** Creates "no_expr" AST node. 
      *
      * @param lineNumber the line in the source file from which this node came.
      */
    public no_expr(int lineNumber) {
        super(lineNumber);
    }
    public TreeNode copy() {
        return new no_expr(lineNumber);
    }
    public void dump(PrintStream out, int n) {
        out.print(Utilities.pad(n) + "no_expr\n");
    }

    
    public void dump_with_types(PrintStream out, int n) {
        dump_line(out, n);
        out.println(Utilities.pad(n) + "_no_expr");
    dump_type(out, n);
    }
    public AbstractSymbol typeCheck (ClassTable classTable, AbstractSymbol className) {
    this.set_type(TreeConstants.No_type);
    return TreeConstants.No_type;
    }
}


/** Defines AST constructor 'object'.
    <p>
    See <a href="TreeNode.html">TreeNode</a> for full documentation. */
class object extends Expression {
    protected AbstractSymbol name;
    /** Creates "object" AST node. 
      *
      * @param lineNumber the line in the source file from which this node came.
      * @param a0 initial value for name
      */
    public object(int lineNumber, AbstractSymbol a1) {
        super(lineNumber);
        name = a1;
    }
    public TreeNode copy() {
        return new object(lineNumber, copy_AbstractSymbol(name));
    }
    public void dump(PrintStream out, int n) {
        out.print(Utilities.pad(n) + "object\n");
        dump_AbstractSymbol(out, n+2, name);
    }

    public AbstractSymbol typeCheck(ClassTable classTable,AbstractSymbol className) {
    //lookupObj walks the ancestors classes for us
    AbstractSymbol type = classTable.lookupObj(name,className); 
    if (type == TreeConstants.No_type) {
        PrintStream es = classTable.semantError(this,className);
        es.println("Undeclared identifier: " + name);
        this.set_type(TreeConstants.Object_);
        return TreeConstants.Object_;
    }

    if(!classTable.isDefined(type))
    {
        PrintStream es = classTable.semantError(this,className);
            es.println("Invalid type: " + type + " for variable "+ name);
        this.set_type(TreeConstants.Object_);
            return TreeConstants.Object_;
    }

    this.set_type(type);
    return type;
    }
    
    public void dump_with_types(PrintStream out, int n) {
        dump_line(out, n);
        out.println(Utilities.pad(n) + "_object");
    dump_AbstractSymbol(out, n + 2, name);
    dump_type(out, n);
    }

}


