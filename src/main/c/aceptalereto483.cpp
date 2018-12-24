#include <iostream>
#include <climits>
#include <stdio.h>

using namespace std;


const int MUCHOS=500000;

long minimos[MUCHOS];
long maximos[MUCHOS];

const string OTRA="ELEGIR OTRA";
const string PREMIO="SIEMPRE PREMIO";



const string* detectaSecuenciaPerdedora(){
  int sobrinos;
  if( scanf("%d", &sobrinos ) != 1 ){
    return NULL;
  }

  int rango = -1;

  for (long s = 0; s < sobrinos; s += 1) {
    long altura;
    if( scanf( "%ld", &altura ) != 1 ){
      return NULL;
    }
    
    if (rango < 0 || ( (altura < minimos[rango]) && (minimos[rango] < maximos[rango]) ) )   {

      // NUEVO RANGO POR NUEVO MINIMO
      rango += 1;
      minimos[rango] = altura;
      maximos[rango] = altura;
      continue;
    }

    if( altura < minimos[rango] && minimos[rango] == maximos[rango] ){
      // AMPLIO UN RANGO RECIEN ESTRENADO POR ABAJO
      minimos[rango] = altura;
      maximos[rango] = altura;
      continue;
    }

    if (altura > maximos[rango]) {
      // AMPLIO EL RANGO ACTUAL POR ARRIBA
      maximos[rango] = altura;
    }

    // COMPRUEBO SI EL VALOR ENCAJA EN ALGUN RANGO COMO INTERMEDIO
    // LOS RANGOS ESTÁN ORDENADOS EN MÍNIMO DECRECIENTE
    for (int r = rango; r >=0 && minimos[r]< altura ; r -= 1) {
      if (altura < maximos[r]) {
        // LEO HASTA FIN DE LINEA PARA COLOCAR EL SIGUIENTE CASO
        while( fgetc(stdin) != '\n' );
        return &OTRA;
      }
    }
  }
  return &PREMIO;

}


int main( int, char *[] ){
  while(true){
    const string* ret = detectaSecuenciaPerdedora();
    if( ret ){
      cout << *ret << endl;
    }

    else{
      break;
    }
  }
}  
