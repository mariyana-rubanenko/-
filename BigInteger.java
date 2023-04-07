package big_arithmetic;

import java.util.ArrayDeque;
import java.util.Arrays;
import java.lang.Math;

class OwnBigInteger implements Comparable<OwnBigInteger> {

    static long defaultBase = 10;
    long base = 1000000000;
    long[] arr;
    boolean isNegative;
    int len;

    public OwnBigInteger(String s) {
        this.base = defaultBase;
        len = s.length();
        int i=0,j;
        if(s.charAt(0) == '-'){
            isNegative = true;
            i++;
            len--;
        }
        else{
            isNegative = false;
        }
        arr = new long[len];
        for(j=len-1;j>=0;j--,i++){
            arr[j] = Character.digit(s.charAt(i),10);
        }

        arr = convertBase(arr,10,1000000000);
        this.base = 1000000000;
        this.len = arr.length;
    }

    public OwnBigInteger(){
        len = 0;
        arr = new long[1000];
    }

    public OwnBigInteger(long arr[]){
        this(arr,1000000000);
    }

    public OwnBigInteger(long arr[], long base){
        this.arr = arr;
        this.len = arr.length;
        this.base = base;
    }

    public OwnBigInteger(long x) {
        this(x,1000000000);
    }

    public OwnBigInteger(long x, long base) {
        this();
        this.base = base;
        int i = 0;
        if(x==0){
            arr[i] = 0;
            i++;
        }
        if(x<0){
            isNegative = true;
            x *=-1;
        }
        while(x>0)
        {
            isNegative = false;
            arr[i] = x % base;
            x = x / base;
            i++;
        }
        len = i;
    }

    public static OwnBigInteger add(OwnBigInteger a, OwnBigInteger b) {
        if(a.base != b.base)
            throw new NumberFormatException();
        long[] out = new long[Math.max(a.len,b.len)+1];
        long sum = 0;
        long carry = 0;
        int i=0;
        while(i<a.len && i<b.len)
        {
            sum = a.arr[i] + b.arr[i] + carry;
            out[i] =sum % a.base;
            carry = sum /a.base;
            i++;
        }
        while(i<a.len)
        {
            sum = a.arr[i] + carry;
            out[i] = sum % a.base;
            carry = sum / a.base;
            i++;
        }
        while(i<b.len)
        {
            sum = b.arr[i] + carry;
            out[i] = sum % b.base;
            carry = sum / a.base;
            i++;
        }
        if(carry>0)
            out[i] = carry;

        return new OwnBigInteger(out[out.length-1]==0?Arrays.copyOfRange(out,0,out.length-1):out,a.base);
    }

    public static OwnBigInteger subtract(OwnBigInteger a, OwnBigInteger b) {
        if(a.base != b.base)
            throw new NumberFormatException();
        long carry = 0;
        OwnBigInteger zero = new OwnBigInteger(0);
        long[] diff = new long[Math.max(a.len,b.len)]; // Max length will be max of length of both the number
        OwnBigInteger x =a ,y =b;
        if(a.compareTo(b)<0)
        {
            x = b;
            y = a;
        }
        if(x.compareTo(zero) ==0)
            return y;
        if(y.compareTo(zero)==0)
            return x;

        for(int i=0; i<y.len; i++)
        {
            long sub = x.arr[i] - y.arr[i] - carry;
            if(sub < 0){
                sub += x.base;
                carry = 1;
            }
            else{
                carry = 0;
            }
            diff[i] = sub;
        }
        for(int j=y.len; j<x.len; j++)
        {
            long sub = x.arr[j] - carry;
            if(sub < 0){
                sub += x.base;
                carry = 1;
            }
            else{
                carry = 0;
            }
            diff[j] = sub;
        }

        int k = diff.length-1;
        while(k>=0 && diff[k] == 0)
            k--;
        if(k == -1)
            return new OwnBigInteger(0,a.base);
        if(k == 0)
            return  new OwnBigInteger(diff[0],a.base);

        OwnBigInteger output = new OwnBigInteger(Arrays.copyOfRange(diff,0,k+1),a.base);

        if(a.compareTo(b)<0)
        {
            output.isNegative = true;
        }
        return output;
    }

    public static OwnBigInteger product(OwnBigInteger a, OwnBigInteger b) {
        if(a.base != b.base)
            throw new NumberFormatException();
        long[] product = new long[a.len+b.len];
        OwnBigInteger zero = new OwnBigInteger(0,a.base);

        if(a.compareTo(zero)==0 || b.compareTo(zero)==0)
            return zero;

        long carry;
        for(int i=0; i<b.len ; i++){
            carry=0;
            for(int j=0; j<a.len ; j++){
                product[i+j] += carry + a.arr[j] * b.arr[i];
                carry = product[i+j] / a.base;
                product[i+j] = product[i+j] % a.base;
            }
            product[i + a.len] = carry;
        }

        OwnBigInteger output;
        if(product[product.length-1]==0)
            output = new OwnBigInteger(Arrays.copyOfRange(product,0,product.length-1),a.base);
        else
            output = new OwnBigInteger(product,a.base);

        if(a.isNegative ^ b.isNegative)
            output.isNegative = true;
        else
            output.isNegative = false;

        return output;
    }

    public static OwnBigInteger power(OwnBigInteger a, long n) {
        if( n == 0)
            return new OwnBigInteger(1,a.base);
        else if(n % 2 == 0 )
            return product(power(a,n/2),power (a,n/2));
        else
            return product(a,product(power(a,n/2),power (a,n/2)));
    }

    public static OwnBigInteger divide(OwnBigInteger a, OwnBigInteger b) {
        if(a.base != b.base)
            throw new NumberFormatException();
        if(b.compareTo(new OwnBigInteger(0,b.base)) ==0)
            return null;
        OwnBigInteger left = new OwnBigInteger(0,a.base);
        OwnBigInteger right = new OwnBigInteger(Arrays.copyOf(a.arr,a.len),a.base);
        OwnBigInteger prevMid = new OwnBigInteger(0,a.base);

        OwnBigInteger temp1,temp2;

        while(true){
            temp1 = subtract(right,left);
            temp2 = temp1.by2();
            OwnBigInteger mid = add(left,temp2);
            if(product(b,mid).compareTo(a) == 0 || prevMid.compareTo(mid)==0)
                return mid;
            if(product(b,mid).compareTo(a) < 0)
                left = mid;
            else
                right = mid;
            prevMid =mid;
        }
    }

    public static OwnBigInteger mod(OwnBigInteger a, OwnBigInteger b) {
        if(a.base != b.base)
            throw new NumberFormatException();
        OwnBigInteger ZERO = new OwnBigInteger(0,a.base);
        OwnBigInteger ONE = new OwnBigInteger(1,a.base);
        if(b.compareTo(ZERO) ==0)
            return null;
        if(a.compareTo(b) == 0 )
            return ZERO;
        if(b.compareTo(ONE) == 0)
            return ZERO;

        OwnBigInteger temp1 = divide(a,b);
        OwnBigInteger temp2 = product(b,temp1);
        OwnBigInteger temp3 = subtract(a,temp2);

        return (temp3);
    }

    public static OwnBigInteger squareRoot(OwnBigInteger a) {
        OwnBigInteger ZERO = new OwnBigInteger(0,a.base);
        OwnBigInteger ONE = new OwnBigInteger(1,a.base);
        if(a.compareTo(ZERO)==0 || a.compareTo(ONE)==0)
            return a;

        OwnBigInteger start = ONE;
        OwnBigInteger end = a;
        OwnBigInteger result = ZERO;
        OwnBigInteger prevMid = ZERO;

        while(start.compareTo(end) <=0){
            OwnBigInteger mid = (add(start,end)).by2();

            OwnBigInteger prod = product(mid,mid);
            if(prod.compareTo(a)== 0 || prevMid.compareTo(mid) == 0 )
                return  mid;

            if(prod.compareTo(a)<0){
                start = add(mid,ONE);
                result = mid;
            }
            else{
                end = subtract(mid,ONE);
            }
            prevMid = mid;
        }
        return result;
    }

    public int compareTo(OwnBigInteger other) {
        if(!this.isNegative && !other.isNegative)
            return unsignedCompareTo(other);
        else if(this.isNegative && other.isNegative)
            return -1*unsignedCompareTo(other);
        else if (this.isNegative && !other.isNegative)
            return -1;
        else
            return 1;
    }

    public int unsignedCompareTo(OwnBigInteger other) {
        if (this.len<other.len) {
            return -1;
        } else if (this.len>other.len) {
            return +1;
        } else {
            return compareMagnitude(other);
        }
    }

    public int compareMagnitude(OwnBigInteger other) {
        for (int i = this.len - 1; i >= 0; i--) {
            if (this.arr[i] < other.arr[i]) {
                return -1;
            } else if (this.arr[i] > other.arr[i]) {
                return 1;
            }
        }
        return 0;
    }

    public void printList() {
        StringBuilder output = new StringBuilder();
        output.append(base+":");
        if(isNegative)
            output.append(" -");
        for(int i =0; i<len; i++){
            output.append(" "+arr[i]);
        }
        System.out.print(output);
    }

    public long base() { return base; }
    public OwnBigInteger convertBase(int newBase) {
        OwnBigInteger thisNum = new OwnBigInteger(this.arr);
        long[] newNum = convertBase(thisNum.arr,(int)thisNum.base,newBase);
        OwnBigInteger result = new OwnBigInteger(newNum,newBase);
        return result;
    }

    public String toStringWithoutBaseChange() {
        StringBuilder output = new StringBuilder();
        for(int i=len-1; i>=0; i--)
            output.append(arr[i]);
        return String.valueOf(output);
    }

    public String toString() {
        StringBuilder output = new StringBuilder();
        for(int i=len-1; i>=0; i--)
            output.append(arr[i]);
        return String.valueOf(output);
    }

    public long[] convertBase(long[] thisNumArr, int currentBase,int newBase){
        OwnBigInteger ZERO = new OwnBigInteger(0,currentBase);
        int arrSize = 0;
        OwnBigInteger thisNum = new OwnBigInteger(thisNumArr,currentBase);
        OwnBigInteger b = new OwnBigInteger(newBase,currentBase);
        arrSize = (int) Math.ceil((thisNum.len+1)/Math.log10(newBase)+1);
        long[] newNum = new long[1000];
        int i =0;

        while(thisNum.compareTo(ZERO) > 0){
            newNum[i] = Long.parseLong(mod(thisNum,b).toStringWithoutBaseChange());
            thisNum = divide(thisNum,b);
            i++;
        }

        //removing trailing zeros
        int k = newNum.length -1;
        while(k>=0 && newNum[k] == 0)
            k--;
        if(k == -1)
            return new long[]{0};
        if(k == 0)
            return new long[]{newNum[0]};

        return Arrays.copyOfRange(newNum,0,k+1);
    }

    public OwnBigInteger convertBaseTo10() {
        OwnBigInteger ZERO = new OwnBigInteger(0,10);
        OwnBigInteger thisNum = new OwnBigInteger(this.arr,10);
        int arrSize = 0;
        arrSize = (int) Math.ceil((len+1)/Math.log10(10)+1);
        OwnBigInteger newNum = ZERO;
        for(int i=0 ; i< thisNum.len ; i++){
            long temp =(long)(thisNum.arr[i] * Math.pow(base, i));
            newNum = add(newNum,new OwnBigInteger(temp,10));
        }
        return newNum;
    }

    public OwnBigInteger by2() {
        OwnBigInteger ZERO = new OwnBigInteger(0,this.base);
        OwnBigInteger one = new OwnBigInteger(1,this.base);
        if(this.compareTo(one)==0 || this.compareTo(ZERO) == 0)
            return ZERO;
        long[] output = Arrays.copyOf(this.arr,this.len);
        long carry = 0;
        for(int i=len-1; i>=0 ; i-- ){
            output[i] = output[i] + carry;
            if(output[i] % 2 == 1)
                carry = this.base;
            else
                carry = 0;
            output[i] = output[i] / 2;
        }

        //Removing trailing zeros
        OwnBigInteger by2;
        if(output[output.length-1]==0) {
            by2 = new OwnBigInteger(Arrays.copyOfRange(output, 0, output.length - 1),this.base);
        }
        else
            by2 = new OwnBigInteger(output,this.base);
        return  by2;
    }

    public static OwnBigInteger evaluatePostfix(String[] expr) {
        ArrayDeque<OwnBigInteger> stack = new ArrayDeque<>();

        for(int i=0 ; i<expr.length ; i++){
            if(Character.isDigit(expr[i].charAt(0))){
                stack.push(new OwnBigInteger(expr[i]));
            }
            else{
                OwnBigInteger val1 = stack.pop();
                OwnBigInteger val2 = stack.pop();

                switch(expr[i]){
                    case "+":
                        stack.push(add(val2,val1));
                        break;

                    case "-":
                        stack.push(subtract(val2,val1));
                        break;

                    case "/":
                        stack.push(divide(val2,val1));
                        break;

                    case "*":
                        stack.push(product(val2,val1));
                        break;

                    case "%":
                        stack.push(mod(val2,val1));
                        break;

                    case "^":
                        stack.push(power(val2,Long.parseLong(val1.toString())));
                        break;

                }

            }

        }

        return stack.pop();
    }

    public static OwnBigInteger evaluateInfix(String[] expr) {

        ArrayDeque<OwnBigInteger> values = new ArrayDeque<>(); //using array deque to use as a stack
        ArrayDeque<String> operators = new ArrayDeque<>();

        for(int i=0 ; i<expr.length; i++){
            if(Character.isDigit(expr[i].charAt(0))){
                values.push(new OwnBigInteger(expr[i]));
            }
            else if(expr[i].equals("(")){
                operators.push(expr[i]);
            }
            else if(expr[i].equals(")")){
                while(!operators.peek().equals("(")){
                    values.push(evaluate(operators.pop(),values.pop(),values.pop()));
                }
                operators.pop();
            }else{
                while(!operators.isEmpty() && hasPrecedence(expr[i],operators.peek())){
                    values.push(evaluate(operators.pop(),values.pop(),values.pop()));
                }
                operators.push(expr[i]);
            }
        }

        while (!operators.isEmpty()){
            values.push(evaluate(operators.pop(),values.pop(),values.pop()));
        }

        return values.pop();
    }

    public static boolean hasPrecedence(String operator1, String operator2){
        if(operator2.equals("(") || operator2.equals(")")){
            return false;
        }
        if((operator1.equals("*") || operator1.equals("/")) && (operator2.equals("+") || operator2.equals("-"))){
            return false;
        }else{
            return true;
        }
    }

    public static OwnBigInteger evaluate(String operator, OwnBigInteger a, OwnBigInteger b){
        switch(operator){
            case "+":
                return add(b,a);

            case "-":
                return (subtract(b,a));

            case "/":
                return  (divide(b,a));

            case "*":
                return  (product(b,a));

            case "%":
                return (mod(b,a));

            case "^":
                return  (power(b,Long.parseLong(a.toString())));

        }
        return new OwnBigInteger(0);
    }


    public static void main(String[] args) {
        OwnBigInteger s = new OwnBigInteger("1000000000500000000000");
        OwnBigInteger t = new OwnBigInteger(12);

        //String[] input = {"10","+","2","*","6"};
        String[] input = { "98765432109876543210987654321",  "5432109876543210987654321", "345678901234567890123456789012", "*", "+", "246801357924680135792468013579", "*", "12345678910111213141516171819202122", "191817161514131211109876543210", "13579", "24680", "*", "-", "*", "+", "7896543", "*", "157984320", "+" };

        OwnBigInteger p = evaluatePostfix(input);
        System.out.println((p.isNegative?"-":"")+p);
        System.out.println(squareRoot(p));
        (p.convertBase(1000)).printList();

    }

}
