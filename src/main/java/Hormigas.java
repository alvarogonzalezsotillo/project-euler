import java.util.*;

class Main
{

	private static int solucionaMinimo(int longitud, int hormigas[])
	{
		double mitad = 1. * longitud / 2;
		int hormigaMasCercanaAMitad = 0;
		for (int h = 0; h < hormigas.length; h++)
		{
			if (Math.abs(mitad - hormigas[h]) < Math.abs(mitad - hormigas[hormigaMasCercanaAMitad]))
			{
				hormigaMasCercanaAMitad = h;
			}
		}

		int posicionHormigaCercanaAMitad = hormigas[hormigaMasCercanaAMitad];

		System.err.println("phcam:" + posicionHormigaCercanaAMitad + " hmcam:" + hormigaMasCercanaAMitad);

		if (posicionHormigaCercanaAMitad > mitad)
		{
			return longitud - posicionHormigaCercanaAMitad;
		}
		else{
			return posicionHormigaCercanaAMitad;
		}
	}

	private static int solucionaMaximo(int longitud, int hormigas[])
	{
		double mitad = 1. * longitud / 2;
		int hormigasSimuladas[] = new int[hormigas.length];
		System.arraycopy(hormigas, 0, hormigasSimuladas, 0, hormigas.length);
		int sentidoHormigas[] = new int[hormigas.length];

		for (int h = 0; h < hormigas.length; h++)
		{
			if (hormigasSimuladas[h] < mitad)
				sentidoHormigas[h] = 1;
			else
				sentidoHormigas[h] = -1;
		}

		boolean hormigaEnElPalo = true;
		int pasos = 0;
		while (hormigaEnElPalo)
		{
			pasos++;
			int nextHormigasSimuladas[] = new int[hormigasSimuladas.length];

			for (int h = 0; h < hormigasSimuladas.length; h++)
			{
				nextHormigasSimuladas[h] = hormigasSimuladas[h] + sentidoHormigas[h];
			}

			for (int h1 = 0; h1 < hormigasSimuladas.length; h1++)
			{
				for (int h2 = h1+1; h2 < hormigasSimuladas.length; h2++)
				{
					boolean cruce = nextHormigasSimuladas[h1] < nextHormigasSimuladas[h2] &&
									hormigasSimuladas[h1] > nextHormigasSimuladas[h2];

					boolean toque = nextHormigasSimuladas[h1] == nextHormigasSimuladas[h2];

					if (cruce)
					{
						nextHormigasSimuladas[h1] = hormigasSimuladas[h1];
						nextHormigasSimuladas[h2] = hormigasSimuladas[h2];
						hormigasSimuladas[h1] *= -1;
						hormigasSimuladas[h2] *= -1;
					}
					else if (toque)
					{
						hormigasSimuladas[h1] *= -1;
						hormigasSimuladas[h2] *= -1;
					}
				}
			}

			System.arraycopy(nextHormigasSimuladas, 0, hormigasSimuladas, 0, hormigasSimuladas.length);

			hormigaEnElPalo = false;
			for (int h = 0; h < hormigasSimuladas.length; h++)
			{
				if (hormigasSimuladas[h] > 0 && hormigasSimuladas[h] < longitud)
				{
					hormigaEnElPalo = true;
				}
			}

			System.err.println("Paso " + pasos + " --> " + Arrays.asList(hormigasSimuladas));
		}

		return pasos;
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