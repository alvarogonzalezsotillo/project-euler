package aer;

import java.util.*;








/**
 * Created by alvaro on 26/01/17.
 * https://www.aceptaelreto.com/problem/statement.php?id=384
 */
public class AceptaElReto384 {


    /*
     La primera línea contiene un número indicando el número de sobrinos que hay ese día en casa (como mínimo 3 y hasta 500.000).
     La segunda línea contendrá las alturas de cada uno de ellos en la configuración aleatoria en la que se han colocado.
     Las alturas no se repiten, y serán números entre 1 y 10^9.
     */
    private static final int MUCHOS = 500000;
    private static long[] minimos = new long[MUCHOS];
    private static long[] maximos = new long[MUCHOS];


    public static boolean haySecuenciaPerdedora(Scanner in) {
        int sobrinos = in.nextInt();

        int rango = -1;

        for (long s = 0; s < sobrinos; s += 1) {
            long altura = in.nextLong();
            //log( "Miro altura: %s\n", altura );
            if (rango < 0 || ((altura < minimos[rango]) && (minimos[rango] < maximos[rango]))) {

                // NUEVO RANGO POR NUEVO MINIMO
                rango += 1;
                minimos[rango] = altura;
                maximos[rango] = altura;
                //log( "  nuevo mínimo:%s\n", altura);
                continue;
            }

            if (altura < minimos[rango] && minimos[rango] == maximos[rango]) {
                // AMPLIO UN RANGO RECIEN ESTRENADO POR ABAJO
                minimos[rango] = altura;
                maximos[rango] = altura;
                //log( "  nuevo rango: %s\n", altura);
                continue;
            }

            if (altura > maximos[rango]) {
                // AMPLIO EL RANGO ACTUAL POR ARRIBA
                maximos[rango] = altura;
            }

            // COMPRUEBO SI EL VALOR ENCAJA EN ALGUN RANGO COMO INTERMEDIO
            // LOS RANGOS ESTÁN ORDENADOS POR MÍNIMO DESCENDENTE
            //log( "hay %s rangos para probar\n", (rango+1) );
            for (int r = rango; r >= 0 && minimos[r] < altura; r -= 1) {
                //logPrueba( "Compruebo rango %s %s con %s\n", minimos[r], maximos[r], altura );
                if (altura > minimos[r] && altura < maximos[r]) {
                    // LEO HASTA FIN DE LINEA PARA COLOCAR EL SIGUIENTE CASO
                    in.nextLine();
                    //Pruebas384.compruebaSiDescendente(minimos,rango+1);
                    return true;
                }
            }
        }
        //Pruebas384.compruebaSiDescendente(minimos,rango+1);
        return false;
    }

    private static void log(Object... args) {
        Object[] p = new Object[args.length - 1];
        System.arraycopy(args, 1, p, 0, p.length);
        System.out.printf(args[0].toString(), p);
    }

    private static String solucion(boolean b) {
        return b ? "ELEGIR OTRA" : "SIEMPRE PREMIO";
    }


    public static void main(String[] args) {


        Scanner in = new Scanner(System.in);
        while (in.hasNextLong()) {
            System.out.println(solucion(haySecuenciaPerdedora(in)));
        }

    }
}


