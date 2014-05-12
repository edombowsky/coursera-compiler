import java.io.PrintStream;
import java.util.Enumeration;
import java.util.Vector;




/** This class may be used to contain the semantic information such as
 * the inheritance graph.  You may use it or not as you like: it is only
 * here to provide a container for the supplied methods.  */
class ClassTable {
    private int semantErrors;
    private PrintStream errorStream;
    private SymbolTable classTable;  // just a container that associates
                                     // class name with class_c constructor

    /** Creates data structures representing basic Cool classes (Object,
     * IO, Int, Bool, String).  Please note: as is this method does not
     * do anything useful; you will need to edit it to make if do what
     * you want.
     * */
    private void installBasicClasses() {
    AbstractSymbol filename 
        = AbstractTable.stringtable.addString("<basic class>");
    
    // The following demonstrates how to create dummy parse trees to
    // refer to basic Cool classes.  There's no need for method
    // bodies -- these are already built into the runtime system.

    // IMPORTANT: The results of the following expressions are
    // stored in local variables.  You will want to do something
    // with those variables at the end of this method to make this
    // code meaningful.

    // The Object class has no parent class. Its methods are
    //        cool_abort() : Object    aborts the program
    //        type_name() : Str        returns a string representation 
    //                                 of class name
    //        copy() : SELF_TYPE       returns a copy of the object

    class_c Object_class = 
        new class_c(0, 
               TreeConstants.Object_, 
               TreeConstants.No_class,
               new Features(0)
               .appendElement(new method(0, 
                          TreeConstants.cool_abort, 
                          new Formals(0), 
                          TreeConstants.Object_, 
                          new no_expr(0)))
               .appendElement(new method(0,
                          TreeConstants.type_name,
                          new Formals(0),
                          TreeConstants.Str,
                          new no_expr(0)))
               .appendElement(new method(0,
                          TreeConstants.copy,
                          new Formals(0),
                          TreeConstants.SELF_TYPE,
                          new no_expr(0))),
               filename);
    
    // The IO class inherits from Object. Its methods are
    //        out_string(Str) : SELF_TYPE  writes a string to the output
    //        out_int(Int) : SELF_TYPE      "    an int    "  "     "
    //        in_string() : Str            reads a string from the input
    //        in_int() : Int                "   an int     "  "     "

    class_c IO_class = 
        new class_c(0,
               TreeConstants.IO,
               TreeConstants.Object_,
               new Features(0)
               .appendElement(new method(0,
                          TreeConstants.out_string,
                          new Formals(0)
                          .appendElement(new formalc(0,
                                     TreeConstants.arg,
                                     TreeConstants.Str)),
                          TreeConstants.SELF_TYPE,
                          new no_expr(0)))
               .appendElement(new method(0,
                          TreeConstants.out_int,
                          new Formals(0)
                          .appendElement(new formalc(0,
                                     TreeConstants.arg,
                                     TreeConstants.Int)),
                          TreeConstants.SELF_TYPE,
                          new no_expr(0)))
               .appendElement(new method(0,
                          TreeConstants.in_string,
                          new Formals(0),
                          TreeConstants.Str,
                          new no_expr(0)))
               .appendElement(new method(0,
                          TreeConstants.in_int,
                          new Formals(0),
                          TreeConstants.Int,
                          new no_expr(0))),
               filename);

    // The Int class has no methods and only a single attribute, the
    // "val" for the integer.

    class_c Int_class = 
        new class_c(0,
               TreeConstants.Int,
               TreeConstants.Object_,
               new Features(0)
               .appendElement(new attr(0,
                        TreeConstants.val,
                        TreeConstants.prim_slot,
                        new no_expr(0))),
               filename);

    // Bool also has only the "val" slot.
    class_c Bool_class = 
        new class_c(0,
               TreeConstants.Bool,
               TreeConstants.Object_,
               new Features(0)
               .appendElement(new attr(0,
                        TreeConstants.val,
                        TreeConstants.prim_slot,
                        new no_expr(0))),
               filename);

    // The class Str has a number of slots and operations:
    //       val                              the length of the string
    //       str_field                        the string itself
    //       length() : Int                   returns length of the string
    //       concat(arg: Str) : Str           performs string concatenation
    //       substr(arg: Int, arg2: Int): Str substring selection

    class_c Str_class =
        new class_c(0,
               TreeConstants.Str,
               TreeConstants.Object_,
               new Features(0)
               .appendElement(new attr(0,
                        TreeConstants.val,
                        TreeConstants.Int,
                        new no_expr(0)))
               .appendElement(new attr(0,
                        TreeConstants.str_field,
                        TreeConstants.prim_slot,
                        new no_expr(0)))
               .appendElement(new method(0,
                          TreeConstants.length,
                          new Formals(0),
                          TreeConstants.Int,
                          new no_expr(0)))
               .appendElement(new method(0,
                          TreeConstants.concat,
                          new Formals(0)
                          .appendElement(new formalc(0,
                                     TreeConstants.arg, 
                                     TreeConstants.Str)),
                          TreeConstants.Str,
                          new no_expr(0)))
               .appendElement(new method(0,
                          TreeConstants.substr,
                          new Formals(0)
                          .appendElement(new formalc(0,
                                     TreeConstants.arg,
                                     TreeConstants.Int))
                          .appendElement(new formalc(0,
                                     TreeConstants.arg2,
                                     TreeConstants.Int)),
                          TreeConstants.Str,
                          new no_expr(0))),
               filename);

    /* Do somethind with Object_class, IO_class, Int_class,
           Bool_class, and Str_class here */
    //Add each of these to classTable and also fill the
    //method and attribute names of each of the basic classes.
        classTable.addId(TreeConstants.Object_, Object_class);
    Object_class.fillNameTables(this);
        classTable.addId(TreeConstants.IO, IO_class);
    IO_class.fillNameTables(this);
        classTable.addId(TreeConstants.Int, Int_class);
    Int_class.fillNameTables(this);
        classTable.addId(TreeConstants.Bool, Bool_class);
    Bool_class.fillNameTables(this);
        classTable.addId(TreeConstants.Str, Str_class);
    Str_class.fillNameTables(this);
    }
    
    public ClassTable(Classes cls) {

        //Add loop check on insert

        //Check path to root after full graph formed

        //make a wrapper class to keep scopes / traversed?


    semantErrors = 0;
    errorStream = System.err;
    /* fill this in */
    classTable = new SymbolTable();
        classTable.enterScope(); // initialize classTable
                                 // NO MORE SCOPES AFTER THIS ONE!!!
    installBasicClasses(); //TODO: What to do with SELF_TYPE?
        for (Enumeration e = cls.getElements(); e.hasMoreElements(); ) {
            class_c cur_cls = (class_c)e.nextElement();

            //Some error handling first

        //Redefining a basic type
            if ((cur_cls.getName()==TreeConstants.SELF_TYPE) ||
            (cur_cls.getName()==TreeConstants.Object_) ||
                (cur_cls.getName()==TreeConstants.Int) ||
                (cur_cls.getName()==TreeConstants.IO) ||
                (cur_cls.getName()==TreeConstants.Bool) ||
                (cur_cls.getName()==TreeConstants.Str)) {
              PrintStream es = semantError(cur_cls);
              es.println("Cannot redefine basic class " + cur_cls.getName() + ".");
              System.err.println("Compilation halted due to static semantic errors.");
          System.exit(1);

            }

            //Inheriting from basic types SELF_TYPE, Int, String, Bool
            if ((cur_cls.getParent()==TreeConstants.SELF_TYPE) ||
                (cur_cls.getParent()==TreeConstants.Int) ||
                (cur_cls.getParent()==TreeConstants.Bool) ||
                (cur_cls.getParent()==TreeConstants.Str)) {
              PrintStream es = semantError(cur_cls);
              es.println("Class " + cur_cls.getName() + " cannot inherit from class " + cur_cls.getParent() + ".");
              System.err.println("Compilation halted due to static semantic errors.");
          System.exit(1);

            }

            //Redefining previously defined type
            if ((classTable.lookup(cur_cls.getName())!=null)) { 
              PrintStream es = semantError(cur_cls);
              es.println("Class " + cur_cls.getName() + " cannot be re-defined.");
              System.err.println("Compilation halted due to static semantic errors.");
          System.exit(1);
            }

   
            classTable.addId(cur_cls.getName(),cur_cls);

        /* Now we check if this new addition formed a loop.  This helps
               keep the invariant that we never add a class which forms a
               loop to the inheritance graph */

            if(inheritanceLoops(cur_cls.getName())) {         
              PrintStream es = semantError(cur_cls);
              es.println("Class " + cur_cls.getName() + " forms inheritance loop");
              System.err.println("Compilation halted due to static semantic errors.");
          System.exit(1);
            }
        }


    /* Now we have to walk from each class to root to make sure graph is
           well formed.  (Can optimize, but won't yet) */
    for (Enumeration e = cls.getElements(); e.hasMoreElements(); ) {
            class_c cur_cls = (class_c)e.nextElement();
        if (noPathToRoot(cur_cls)) {
              PrintStream es = semantError(cur_cls);
              es.println("Class " + cur_cls.getName() + " has UNDEFINED ancestor class.");
              System.err.println("Compilation halted due to static semantic errors.");
          System.exit(1);
            }
    }
    }

    private boolean noPathToRoot(class_c cls) {
    /* Walk the parent pointers to see if there is no path to root */
    class_c cur_class = cls;
        while (cur_class != null) {
        AbstractSymbol parent = cur_class.getParent();
            if (parent == TreeConstants.Object_)
        return false;  //Reached root, so no loops.
        cur_class = (class_c)classTable.lookup(parent);
        }
        /* If we reach here, we've reached an undefined class.  */
        return true;
    }

    private boolean inheritanceLoops(AbstractSymbol class_name) {
    /* Walk the parent pointers.  The OK termination conditions are
       reaching Object_ or an as-yet undefined class.  The error 
       condition is reaching yourself */
    class_c cur_class = (class_c)classTable.lookup(class_name);
        while (cur_class != null) {
        AbstractSymbol parent = cur_class.getParent();
            if (parent == TreeConstants.Object_)
        return false;  //Reached root, so no loops.
            if (parent == class_name)
                return true;   //Reached start point, so there is a loop
        cur_class = (class_c)classTable.lookup(parent);
        }
        /* If we reach here, we've reached an as-yet undefined class.  
           This means no loop (yet) */
        return false;

    }

    /* Simple accessor */
    public class_c lookup(AbstractSymbol class_name) {
    return (class_c)classTable.lookup(class_name);
    }

    /* isDefined also handles SELF_TYPE */
    public boolean isDefined(AbstractSymbol class_name) {
    if (class_name == TreeConstants.SELF_TYPE)
        return true;
    if (classTable.lookup(class_name)==null)
        return false;
    else
        return true;
    }

    /* checks type conformity */
    /* now handles SELF_TYPE.  Remember that SELF_TYPE is not a member
       of classTable */
    public boolean conforms(AbstractSymbol subC, 
                        AbstractSymbol superC, 
                            AbstractSymbol call_class) {
    class_c cur_class, super_class;

        if ((subC == TreeConstants.SELF_TYPE) && (superC == TreeConstants.SELF_TYPE))
        return true; //SELF_TYPEs coming from same class conform

    if (superC == TreeConstants.SELF_TYPE) 
        return false; //Otherwise, SELF_TYPE as the super class always false;

    if (subC == TreeConstants.SELF_TYPE)
        cur_class = (class_c)classTable.lookup(call_class);
    else
        cur_class = (class_c)classTable.lookup(subC);

        super_class = (class_c)classTable.lookup(superC);

        if ((cur_class == null) || (super_class == null)) {
        PrintStream es = semantError((class_c)classTable.lookup(call_class));
        es.println("Undefined type in class " + call_class + 
                       ".  May be " + cur_class + " or " + super_class);
        return false;  //subC or superC is not defined.  Cannot conform 
    }
    //Search stops at Object node, whose parent is null
        while (cur_class != null) {
            if (cur_class.getName() == superC)
        return true;  //Found a match, so conforms
        cur_class = (class_c)classTable.lookup(cur_class.getParent());
        }
        /* If we reach this point, we've not found the superC class.  */
        return false;
    }

    /* finds least-upper bound between 2 classes */
    /* now handles SELF_TYPE.  Remember that SELF_TYPE is not a member
       of classTable */
    public AbstractSymbol lub(AbstractSymbol cls1, 
                          AbstractSymbol cls2, 
                              AbstractSymbol call_class) {
    class_c cur_class, class2;
    
        if ((cls1 == TreeConstants.SELF_TYPE) && (cls2 == TreeConstants.SELF_TYPE))
        return cls1; //lub of 2 SELF_TYPEs is the SELF_TYPE

    if (cls1 == TreeConstants.SELF_TYPE)
        cls1 = call_class;

        if (cls2 == TreeConstants.SELF_TYPE)
        cls2 = call_class;

        cur_class = (class_c)classTable.lookup(cls1); 
    class2 = (class_c)classTable.lookup(cls2);

    if ((cur_class==null) || (class2==null)) {
        //trying to take lub of undefined class
        PrintStream es = semantError((class_c)classTable.lookup(call_class));
        es.println("Case statement in class " + call_class + 
                       "has undefined branch-types.  Maybe " + cls1 +
               " or " + cls2);
        return TreeConstants.Object_;
    }

    //Now walk the ancestors of cls1 to see which one cls2 conforms to
        while(cur_class != null) {
        if (conforms(cls2,cur_class.getName(),call_class))
        return cur_class.getName();
        cur_class = (class_c)classTable.lookup(cur_class.getParent());
        }

    //If we reach here, there is a problem with the ancestor tree
    //since everything should conform to Object at least.
        PrintStream es = semantError((class_c)classTable.lookup(call_class));
        es.println("Malformed inheritance tree detected by least-upper-bound called in class " + call_class);
    es.println("May be caused by undefined type in type attribute or method formal param.");
    return TreeConstants.Object_;
    }

    /* TODO:  WRITE lookupObj(name, className) and lookupMthd(name, className)
       that walk up the class inheritance hierarchy */

    public AbstractSymbol lookupObj(AbstractSymbol name, AbstractSymbol clsN) {
    //This method will run through the ancestors of clsN and try to
    //find name in their objTables.  Note that only the objTable of
    //clsN should have nested scopes.  The other classes should have a
    //objTables in the initialized state, meaning it's filled only
    //with attr's.
    

    class_c cur_class = (class_c)classTable.lookup(clsN);
    while (cur_class != null) {
        AbstractSymbol type = (AbstractSymbol)cur_class.getObj(name);
        //System.err.println(cur_class.getName());
        if (type != null)
        return type;
        cur_class = (class_c)classTable.lookup(cur_class.getParent());
    }
    //If we reached here, we know that the name was not defined in 
    //any accessible scope.
    //We'll return No_type to indicate this

    return TreeConstants.No_type;

    }

    public method lookupMthd(AbstractSymbol name, AbstractSymbol clsN) {
    //This method will run through the ancestors of clsN and try to
    //find name in their mthdTables. 
    //Since mthdTables are not modified after being initially
    //populated in the 1st pass of fillNameTables(), they should
    //contain each class' methods
    

    class_c cur_class = (class_c)classTable.lookup(clsN);
    while (cur_class != null) {
        method mthd = (method)cur_class.getMethod(name);
        if (mthd != null)
        return mthd;
        cur_class = (class_c)classTable.lookup(cur_class.getParent());
    }
    //If we reached here, we know that the method was not defined in 
    //any class in the ancestor chain.
    //We'll return null to indicate this

    return null;

    }


    /** Prints line number and file name of the given class.
     *
     * Also increments semantic error count.
     *
     * @param c the class
     * @return a print stream to which the rest of the error message is
     * to be printed.
     *
     * */
    public PrintStream semantError(class_c c) {
    return semantError(c.getFilename(), c);
    }

    public PrintStream semantError(AbstractSymbol cn) {
    return semantError((class_c)classTable.lookup(cn));
    }

    public PrintStream semantError(TreeNode t, AbstractSymbol cn) {
    class_c cls = (class_c)classTable.lookup(cn);
    return semantError(cls.getFilename(),t);
    }


    /** Prints the file name and the line number of the given tree node.
     *
     * Also increments semantic error count.
     *
     * @param filename the file name
     * @param t the tree node
     * @return a print stream to which the rest of the error message is
     * to be printed.
     *
     * */
    public PrintStream semantError(AbstractSymbol filename, TreeNode t) {
    errorStream.print(filename + ":" + t.getLineNumber() + ": ");
    return semantError();
    }

    /** Increments semantic error count and returns the print stream for
     * error messages.
     *
     * @return a print stream to which the error message is
     * to be printed.
     *
     * */
    public PrintStream semantError() {
    semantErrors++;
    return errorStream;
    }

    /** Returns true if there are any static semantic errors. */
    public boolean errors() {
    return semantErrors != 0;
    }
}
