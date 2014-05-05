class Main inherits IO {
    exitCode : Bool <- false;

    command : String;
    --stack : List <- new List.cons(" ").cons("2").cons("3").cons("+");
    stack : List <- new List;
    convert : Converter <- new Converter;
    numInt1 : Int;
    numInt2 : Int;
    num1 : String;
    num2 : String ;
    token1 : String;
    token2 : String;
    item : String;
    main() : Object{
    {
        --out_string("\n");
        --out_string("!!!!!!!!!!!!!!!!MyCool Stack Machine!!!!!!!!!!!!!!!!!");
        --out_string("\n");
        while (not exitCode)
        loop
        {
            --      out_string(">");
            --      command <- in_string();

            if (command = "e") then
            {

                --      if( not stack.isNil()) then
                {
                    --      item <- stack.head();
                    convert  <- new Converter;
                    if (item = "+") then
                    {

                        stack <- stack.tail();
                        num1 <- stack.head();

                        stack <- stack.tail();
                        num2 <- stack.head();

                        stack <- stack.tail();
                        numInt1 <- convert.stringToInt(num1);
                        numInt2 <- convert.stringToInt(num2);
                        num1 <- convert.intToString(numInt1+numInt2);

                        stack <- stack.cons(num1);

                    }
                    else if (item = "s") then
                    {
                        stack <- stack.tail();

                        if (not stack.isNil()) then
                        {
                            token1 <- stack.head();
                            stack <- stack.tail();
                            if (not stack.isNil()) then
                            {

                                token2 <- stack.head();
                                stack <- stack.tail();
                                stack <- stack.cons(token1);
                                stack <- stack.cons(token2);
                            }
                            else
                            {
                                true;
                            }
                            fi;
                        }
                        else
                        {
                            true;
                        }
                        fi;
                    }
                    else
                    {
                        true; --nothing happens since the first item on stack is a number
                    }
                    fi fi;

                }
                else
                {
                    true;
                }
                fi;
                true;
            }
            else if (command = "d") then
            {
                (new ReadStack).readAndDisplay(stack);
                true;
            }
            else if (command = "x") then
            {
                exitCode <- true;
            }
            else -- pushes in stack
            {
                stack <- stack.cons(command);
                false;
            }
            fi fi fi;
        }
        pool;

        out_string("Program Ended\n");
        }
    };
};
class List {
    isNil() : Bool { true };
    head()  : String { { abort(); ""; } };
    tail()  : List { { abort(); self; } };

    cons(i : String) : List {
        (new Cons).init(i, self)
    };

};


class Cons inherits List {

    car : String;

    cdr : List;

    isNil() : Bool { false };

    head()  : String { car };

    tail()  : List { cdr };

    init(i : String, rest : List) : List {
        {
            ar <- i;
            cdr <- rest;
            self;
        }
    };

};
class Converter inherits IO {

    stringToInt(s : String) : Int {
        if s.length() = 0 then 0 else
        if s.substr(0,1) = "-" then ~a2i_aux(s.substr(1,s.length()-1)) else
        if s.substr(0,1) = "+" then a2i_aux(s.substr(1,s.length()-1)) else
            a2i_aux(s)
        fi fi fi
    };
 
    a2i_aux(s : String) : Int {
        (let int : Int <- 0 in
            {
                (let j : Int <- s.length() in
                    (let i : Int <- 0 in
                    while i < j loop
                        {
                            int <- int * 10 + c2i(s.substr(i,1));
                            i <- i + 1;
                        }
                    pool
                    )
                );
                int;
            }
        )
    };

    intToString(i : Int) : String {
    if i = 0 then "0" else
        if 0 < i then i2a_aux(i) else
            "-".concat(i2a_aux(i * ~1))
        fi fi
    };

    (* i2a_aux is an example using recursion.  *)

    i2a_aux(i : Int) : String {
        if i = 0 then "" else
            (let next : Int <- i / 10 in
                i2a_aux(next).concat(i2c(i - next * 10))
            )
        fi
    };
    c2i(char : String) : Int {
        if char = "0" then 0 else
        if char = "1" then 1 else
        if char = "2" then 2 else
        if char = "3" then 3 else
        if char = "4" then 4 else
        if char = "5" then 5 else
        if char = "6" then 6 else
        if char = "7" then 7 else
        if char = "8" then 8 else
        if char = "9" then 9 else
        { abort(); 0; }  (* the 0 is needed to satisfy the
                              typchecker *)
        fi fi fi fi fi fi fi fi fi fi
    };

    (*
       i2c is the inverse of c2i.
    *)
    i2c(i : Int) : String {
        if i = 0 then "0" else
        if i = 1 then "1" else
        if i = 2 then "2" else
        if i = 3 then "3" else
        if i = 4 then "4" else
        if i = 5 then "5" else
        if i = 6 then "6" else
        if i = 7 then "7" else
        if i = 8 then "8" else
        if i = 9 then "9" else
        { abort(); ""; }  -- the "" is needed to satisfy the typchecker
        fi fi fi fi fi fi fi fi fi fi
     };
};

class ReadStack inherits IO {

    readAndDisplay(stack : List) : Object
    {
        if not stack.isNil()
        then
        {
            out_string(stack.head());
            out_string("\n");
            readAndDisplay(stack.tail());
            true;
        }
        else
            true
        fi
    };
};
