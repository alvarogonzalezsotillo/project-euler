
import java.util.*;

/**
https://www.aceptaelreto.com/problem/statement.php?id=138&potw=1

El factorial de un número n (representado como n!) es el resultado de multiplicar todos los números entre 1 y el propio n:

n! = 1 · 2 · …  · (n - 1) · n = (n - 1)! · n

Además, por convenio, el factorial de 0 es 1 (es decir, 0! = 1).

Es sabido que el factorial crece muy rápidamente, de forma que para n's pequeños se obtienen n! con un gran número de dígitos; por ejemplo, el factorial de 24 es 620.448.401.733.239.439.360.000, un número que está muy lejos del máximo soportado en los tipos int de los lenguajes de programación tradicionales de 32 bits.

El escenario empeora rápidamente; 70! es el primero que rompe la barrera de los 100 dígitos, mientras que 100! tiene ya 158.

Es por esto que hoy, en vez de intentar calcular el factorial de semejantes números, nos conformaremos con saber cuántos ceros tienen al final.

Entrada
La entrada comenzará con un número natural indicando el número de casos de prueba. Cada caso de prueba aparecerá en una línea independiente con un número natural entre 0 y 2³¹ - 1.

Salida
Para cada caso de prueba, el programa mostrará el número de dígitos a 0 consecutivos que aparecen a la derecha (dígitos menos significativos) de su factorial.
*/

public class CerosDeFactorial{

  interface Solucion{
    long numeroDeDiez( long from, long to );
  }

  static class OpcionLenta implements Solucion{
    long numeroDeFactores( long n, long factor ){
      long ret = 0;
      while( n % factor == 0 ){
        ret += 1;
        n /= factor;
      }
      return ret;
    }
    
    long numeroDeFactores( long from, long to, long factor ){
      int ret = 0;
      for( long n = from ; n <= to ; n++ ){
        ret += numeroDeFactores(n,factor);
      }
      return ret;
    }
    
    public long numeroDeDiez( long from, long to ){
      long cincos = numeroDeFactores(from,to,5);
      long doses = numeroDeFactores(from,to,2);
      return Math.min(cincos,doses);
    }
  }
  
  
  static class OpcionRapida implements Solucion{  
    private long _potencias[];
    
    public OpcionRapida(){
        int maxExp = (int)(Math.log(Integer.MAX_VALUE)/Math.log(5))+1;
        _potencias = new long[maxExp+1];
        _potencias[0] = 1;
        potenciaDeCinco(maxExp);
    }
    
    long potenciaDeCinco( int exp ){
      if( _potencias[exp] == 0 ){
        _potencias[exp] = potenciaDeCinco(exp-1)*5;
      }
      return _potencias[exp];
    }
    
    public long numeroDeDiez( long from, long to ){
    
      int exp = 1;
      long div = potenciaDeCinco(exp);
      long ret = 0;
      while( to >= div ){
        ret += to/div;
        exp = exp+1;
        div = potenciaDeCinco(exp);
      }
      
      return ret;
    }
  }
  
  public static void main(String args[]){
    Scanner in = new Scanner(System.in);
    Solucion s = new OpcionRapida();
    int casos = in.nextInt();
    for( int c = 0 ; c < casos ; c++ ){
      long n = in.nextLong();
      long solucion = s.numeroDeDiez(2,n);
      System.out.println( solucion );
    }
  }
}