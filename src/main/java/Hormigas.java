import java.util.Scanner;
/**
 * http://muchoporprogramar.wordpress.com/2014/02/17/hormigas-itinerantes/
 */
class Main
{

	private static int solucionaMinimo(int longitud, int hormigas[])
	{
        double distanciaMasGrandeAExtremo = 0;
        for (int h = 0; h < hormigas.length; h++)
        {
            double distanciaMasCortaAExtremoDeHormiga = Math.min(hormigas[h],longitud-hormigas[h]);
            distanciaMasGrandeAExtremo = Math.max(distanciaMasGrandeAExtremo, distanciaMasCortaAExtremoDeHormiga);
        }
        return (int)distanciaMasGrandeAExtremo;
	}

	private static int solucionaMaximo(int longitud, int hormigas[])
	{
		double distanciaMasGrandeAExtremo = 0;
		for (int h = 0; h < hormigas.length; h++)
		{
			distanciaMasGrandeAExtremo = Math.max(distanciaMasGrandeAExtremo, hormigas[h]);
			distanciaMasGrandeAExtremo = Math.max(distanciaMasGrandeAExtremo, longitud - hormigas[h]);
		}
		return (int)distanciaMasGrandeAExtremo;
	}

	private static void soluciona(int longitud, int hormigas[])
	{
		int minimo = solucionaMinimo(longitud, hormigas);
		int maximo = solucionaMaximo(longitud, hormigas);
		System.out.println(minimo + " " + maximo);
	}

	public static void main(String[] args)
	{
		Scanner sc = new Scanner(System.in);
		int numeroDeCasos = sc.nextInt();

		for (int caso = 0; caso < numeroDeCasos; caso++)
		{
			int longitud = sc.nextInt();
			int numeroDeHormigas = sc.nextInt();
			int hormigas[] = new int[numeroDeHormigas];
			for (int h = 0; h < numeroDeHormigas; h++)
			{
				hormigas[h] = sc.nextInt();
			}

			soluciona(longitud, hormigas);
		}


	}
}