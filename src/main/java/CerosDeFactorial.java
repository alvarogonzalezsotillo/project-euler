import java.util.*;

public class CerosDeFactorial{


  public static int numeroDeFactores( int n, int factor ){
    int ret = 0;
    while( n % factor == 0 ){
      ret += 1;
      n /= factor
    }
    return ret;
  }
  
  public static int numeroDeFactores( int from, int to, int factor ){
    int ret = 0;
    for( int n = from ; n <= to ; n++ ){
      ret += numeroDeFactores(n,factor);
    }
    return ret;
  }
  
  public static int numeroDeDiez( int from, int to ){
    int cincos = numeroDeFactores(from,to,5);
    int doses = numeroDeFactores(from,to,2);
    return Math.min(cincos,doses)
  }
  
  public static void main(String args[]){
    Scanner in = new Scanner(System.in);
    int casos = in.nextInt();
    for( int c = 0 ; c < casos ; c++ ){
      int n = in.nextInt();
      int solucion = numeroDeDiez(2,n);
      System.out.println( solucion )
    }
  }
  
  


}