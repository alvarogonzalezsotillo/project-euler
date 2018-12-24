package aer;

import java.io.ByteArrayInputStream;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Scanner;

/**
  * Created by alvaro on 31/01/17.
  */
class Pruebas384{

  public static void logPrueba(Object ... args ){
    Object[] p = new Object[args.length-1];
    System.arraycopy(args,1,p,0,p.length );
    System.out.printf( args[0].toString(), p );
  }

  public static String inventarSecuenciaDePrueba(int sobrinos) {
    Long array[] = new Long[sobrinos];
    for( int i = 0 ; i < array.length ; i++ ){
      array[i] = new Long(i);
    }
    List<Long> list = Arrays.asList(array);
    Collections.shuffle( list );

    StringBuffer buf = new StringBuffer();
    buf.append(sobrinos);
    buf.append("\n");
    for ( Long l: list ) {
      buf.append(l);
      buf.append(" ");
    }
    return buf.toString();
  }

  public static void pruebaDescendente() {
    StringBuffer buf = new StringBuffer();
    int n = 500000;
    buf.append(n);
    buf.append("\n");
    for( int i = n ; i > 0 ; i-- ){
      buf.append(i + " ");
    }
    String s = buf.toString();
    boolean b = AceptaElReto384.haySecuenciaPerdedora(new Scanner(new ByteArrayInputStream(s.getBytes())));
    System.out.println(b);
  }

  public static void pruebaAscendente() {
    StringBuffer buf = new StringBuffer();
    int n = 10;
    buf.append(n);
    buf.append("\n");
    for( int i = 1 ; i <= n ; i++ ){
      buf.append(i + " ");
    }
    String s = buf.toString();
    //logPrueba(s + "\n" );
    boolean b = AceptaElReto384.haySecuenciaPerdedora(new Scanner(new ByteArrayInputStream(s.getBytes())));
    //logPrueba(b + "\n");
  }

  public static void pruebaLenta() {
    StringBuffer buf = new StringBuffer();
    int n = 200000;
    if( n % 2 != 0 ) throw new IllegalArgumentException();
    buf.append(n);
    buf.append("\n");
    for( int i = n/2 ; i >=0 ; i-- ){
      buf.append( (i*2) + " " + (i*2+1) + " ");
    }
    String s = buf.toString();
    System.out.println( "Secuencia lenta generada");
    //logPrueba( s + "\n");
    boolean b = AceptaElReto384.haySecuenciaPerdedora(new Scanner(new ByteArrayInputStream(s.getBytes())));
    //logPrueba(b + "\n");
  }


  public static void ficheroDePrueba() {
    for (int i = 0; i < 10; i += 1) {
      System.out.println(inventarSecuenciaDePrueba(100000));
    }
  }

  public static void prueba() {
    for (int i = 0; i < 10000; i += 1) {
      String s = inventarSecuenciaDePrueba(10);
      System.out.println( "\n\n*********************************");
      System.out.println( s );
      System.out.println( "rapido*****************************");
      boolean b1 = AceptaElReto384.haySecuenciaPerdedora(new Scanner(new ByteArrayInputStream(s.getBytes())));
      System.out.println( "lento*****************************");
      boolean b2 = haySecuenciaPerdedoraLento(new Scanner(new ByteArrayInputStream(s.getBytes())));
      if (b1 != b2) {
        throw new IllegalStateException("rapido:" + b1 + "  lento " + b2 + "  " + s);
      }
    }
  }

  public static void medirTiempo( Runnable r ){
    long ini = System.currentTimeMillis();
    r.run();
    long end = System.currentTimeMillis();
    System.out.println( (end-ini) + "ms" );
  }


  public static boolean haySecuenciaPerdedoraLento(Scanner in) {
    int sobrinos = in.nextInt();
    long[] alturas = new long[sobrinos];
    for (int s = 0; s < sobrinos; s += 1) {
      alturas[s] = in.nextLong();
    }

    for (int tercero = sobrinos-1; tercero >= 2; tercero -= 1) {
      for (int segundo = tercero-1; segundo >= 1; segundo -= 1) {
        for (int primero = segundo-1; primero >= 0; primero -= 1) {
          long a1 = alturas[primero];
          long a2 = alturas[segundo];
          long a3 = alturas[tercero];

          if (a1 < a2 && a1 < a3 && a2 > a3) {
            //logPrueba("primero:%d (%d) segundo:%d (%d) tercero: %d (%d)\n", primero, a1, segundo, a2, tercero, a3);
            return true;
          }
        }
      }
    }
    //logPrueba("No se encuentra\n");
    return false;
  }

  public static void compruebaSiDescendente(long[] array, int len){
    if( array.length == 0 ){
      return;
    }
    long previous = array[0];
    for( int i = 0 ; i < len ; i++  ){
      long l = array[i];
      if(l > previous ){
        System.out.println( "Minimos:" + Arrays.asList(Arrays.stream(array).mapToObj(Long::valueOf).toArray(Long[]::new)) );
        throw new IllegalArgumentException();
      }
      previous = l;
    }
  }

  public static void main(String args[]){
    Pruebas384.medirTiempo( Pruebas384::prueba );
    Pruebas384.medirTiempo( Pruebas384::pruebaDescendente );
    Pruebas384.medirTiempo( Pruebas384::pruebaAscendente );
    Pruebas384.medirTiempo( Pruebas384::pruebaLenta );

  }

}