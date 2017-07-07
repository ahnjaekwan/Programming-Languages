// import lists and other data structures from the Java standard library
import java.util.*;

// PROBLEM 1

// a type for arithmetic Expressions
interface Exp {
    double eval(); 	                       // Problem 1a
    List<Instr> compile(); 	               // Problem 1c
}

class Num implements Exp {
    public Num(double value) { //initialize in Constructor
        this.val = value;
    }
    public double eval() { //just return
        return this.val;
    }
    public List<Instr> compile() {
        List<Instr> expr = new LinkedList<Instr>();
        expr.add(new Push(val));
        return expr;
    }

    protected double val;

    public boolean equals(Object o) { return (o instanceof Num) && ((Num)o).val == this.val; }

    public String toString() { return "" + val; }
}

class BinOp implements Exp {
    public BinOp(Exp left1, Op op1, Exp right1) { //initialize in Constructor
        this.left = left1;
        this.right = right1;
        this.op = op1;
    }
    public double eval() { //evaluate by using calculate() function in enum Op
        return this.op.calculate(this.left.eval(), this.right.eval());
    }
    public List<Instr> compile() {
        List<Instr> expr = new LinkedList<Instr>();
        expr.addAll(this.left.compile());
        expr.addAll(this.right.compile());
        expr.add(new Calculate(this.op));
        return expr;
    }

    protected Exp left, right;
    protected Op op;

    public boolean equals(Object o) {
    	if(!(o instanceof BinOp))
    		return false;
    	BinOp b = (BinOp) o;
    	return this.left.equals(b.left) && this.op.equals(b.op) &&
		    	this.right.equals(b.right);
    }

    public String toString() {
		return "BinOp(" + left + ", " + op + ", " + right + ")";
    }
}

// a representation of four arithmetic operators
enum Op {
    ADD, SUB, MULT, DIV;

	double calculate(double x, double y) {
        switch(this) {
            case ADD:   return x + y;
            case SUB:  return x - y;
            case MULT:  return x * y;
            case DIV: return x / y;
        }
        throw new AssertionError("Unknown Op: " + this);
    }
}

// a type for arithmetic instructions
interface Instr {
    //double exec(double d1, double d2);
    //This is an alternative way
    //It works with correct inputs but it occurs an error with incorrect inputs
    
    void exec(Stack<Double> s); //define a new function to help execute() function in Instrs
}

class Push implements Instr {
    public Push(double value) { //initialize in Constructor
        this.val = value;
    }
    /* //alternative option
    public double exec(double d1, double d2) {
        return val;
    }
    */
    public void exec(Stack<Double> s) { //just push value into stack
        s.push(this.val); //auto-boxing
    }

    protected double val;

	public boolean equals(Object o) { return (o instanceof Push) && ((Push)o).val == this.val; }

    public String toString() {
		return "Push " + val;
    }
}

class Calculate implements Instr {
    public Calculate(Op op1) { //initialize in Constructor
        this.op = op1;
    }
    /*
    public double exec(double left, double right) {
        BinOp binop = new BinOp(new Num(left), op, new Num(right));
        return binop.eval();
    }
    */
    public void exec(Stack<Double> s) { //pop top two elements from the stack and evaluate with given operation and push the result back into stack
        double right = s.pop();
        double left = s.pop();
        s.push(new BinOp(new Num(left), this.op, new Num(right)).eval()); //auto-boxing
    }

    protected Op op;

    public boolean equals(Object o) { return (o instanceof Calculate) && 
    						  ((Calculate)o).op.equals(this.op); }

    public String toString() {
		return "Calculate " + op;
    }    
}

class Instrs {
    protected List<Instr> instrs;

    public Instrs(List<Instr> instrs) { this.instrs = instrs; }

    public double execute() {
        /* //alternative option
        Stack<Num> st = new Stack<Num>();
        for (Instr i : instrs) {
            Num right, left;
            if(st.empty()) {
                right = new Num(1.0);
                left = new Num(1.0);
            } else {
                right = st.pop();
                if(st.empty()) {
                    left = new Num(1.0);
                } else {
                    left = st.pop();
                }
            }
            st.push(left);
            st.push(right);
            st.push(new Num(i.exec(left.eval(), right.eval())));
        }
        return st.pop().eval();
        */
        Stack<Double> st = new Stack<Double>(); //create a new stack of double
        for (Instr i : instrs) {
            i.exec(st); //update stack on each iteration by using for-each
        }
        return st.pop(); //final value is a result
    }  // Problem 1b
}


class CalcTest {
    public static void main(String[] args) {
	// // a test for Problem 1a
	    Exp exp =
	        new BinOp(new BinOp(new Num(1.0), Op.ADD, new Num(2.0)),
	        	  	Op.MULT,
		       	    new Num(3.0));
		assert(exp.eval() == 9.0);

	// // a test for Problem 1b
		List<Instr> is = new LinkedList<Instr>();
		is.add(new Push(1.0));
		is.add(new Push(2.0));
		is.add(new Calculate(Op.ADD));
		is.add(new Push(3.0));
		is.add(new Calculate(Op.MULT));
		Instrs instrs = new Instrs(is);
		assert(instrs.execute() == 9.0);

	// // a test for Problem 1c
		assert(exp.compile().equals(is));
    }
}


// PROBLEM 2

// the type for a set of strings
interface Set {
    int size();
    boolean contains(String s);
    void add(String s);
}

// an implementation of Set using a linked list
class ListSet implements Set {
    protected SNode head;
    public int size() { //figure out the size by using count() function in SNode
        return this.head.count();
    }
    public boolean contains(String s) { //investigate starting from the head
        return this.head.isThis(s);
    }
    public void add(String s) { //use addNode(String s) function in SNode to add a new node
        this.head = this.head.addNode(s);
        return;
    }

    ListSet() {
        this.head = new SEmpty();
    }
}

// a type for the nodes of the linked list
interface SNode {
    int count();
    boolean isThis(String s);
    SNode addNode(String s);
    //should return a new node, representing the current node's replacement in the list after the addition of a new element
}

// represents an empty node (which ends a linked list)
class SEmpty implements SNode {
    SEmpty() {}
    
    public int count() { //if empty, size is zero
        return 0;
    }
    public boolean isThis(String s) { //this means ListSet reached at the end without finding the proper node
        return false;
    }
    public SNode addNode(String s) { //add node at the end
        SNode newSE = new SElement(s, this);
        return newSE;
    }
}

// represents a non-empty node
class SElement implements SNode {
    protected String elem;
    protected SNode next;

    SElement(String elem, SNode next) {
        this.elem = elem;
        this.next = next;
    }

    public int count() { //if not empty, increment by 1 and move to the next node
        return 1 + next.count();
    }
    public boolean isThis(String s) { //b/c ListSet is sorted, compare elements by using compareTo
        if(this.elem.compareTo(s) == 0) { //found!
            return true;
        } else if(this.elem.compareTo(s) > 0) { //not found!
            return false;
        } else { //keep propagating to the next node
            return this.next.isThis(s);
        }
    }
    public SNode addNode(String s) { //propagate until finding the right position and if found, add the new node
        if(this.elem.compareTo(s) == 0) { //if there already exists the same one in the set, skip this one
            return this;
        } else if(this.elem.compareTo(s) > 0) { //here is the right position to insert a new node
            SNode newSE = new SElement(s, this);
            return newSE;
        } else { //keep propagating to the next node until finding the right position
            this.next = this.next.addNode(s); //**the most important code**: update the next recursively next node, not the current node            
            return this; //always return this so that list can be still at the head
        }
    }
}

class CalcTest2 {
    public static void main(String[] args) {
    // // test for count
        ListSet ls = new ListSet();
        assert(ls.size() == 0);
    // // test for add
        ls.add("hello");
        assert(ls.size() == 1);
        ls.add("hello");
        assert(ls.size() == 1);
        ls.add("hi");
        assert(ls.size() == 2);
    // // test for contains
        assert(ls.contains("hi"));
        assert(ls.contains("hello"));
        assert(!ls.contains("bye"));
    // //
    }
}
