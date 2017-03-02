#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

/*
 * rilevaTempiTransizioni : 
 * 
 * listaStati: l'array con la sequenza degli stati
 * tempiTransizione l'array con i tempi di transizione fra gli stati di listaStati
 * out_from : (OUTPUT) stato di partenza
 * in_from : (OUTPUT) stato di arrivo
 */
void rilevaTempiTransizioni(int *listaEventi, double *tempiTransizione, int *quantiEventi,
                            int *max_valore_evento,
                            int *out_mm) {
  
  int base, curs, i;
  int *eventiAttivati = (int*) calloc( 1 , (*max_valore_evento)+10 );
  double cumulativo_tempo;
  
  printf("\n TROVATI %d eventi in una stringa lunga %d",*max_valore_evento,*quantiEventi);
  
  // Per ogni evento guarda cosa trova a seguire
//  for(base = 0; base < ( *quantiEventi - 1 ) ; base++ ) { -1
    
    cumulativo_tempo = 0;
    // pulisci gli eventi attivati
    for(i = 0; i <= *max_valore_evento ; i++  ) *(eventiAttivati+i) = 0; 
    
 //   for(curs = base + 1; curs < ( *quantiEventi )  && *(listaEventi+base)!=*(listaEventi+curs) ; curs++ ) { -1
      
      // incrementa il cumulativo del tempo con il tempo 
      // fra i due eventi successivi
      // cumulativo_tempo = *(tempiTransizione+curs) - *(tempiTransizione+base);   -1
      
      // se lo stato NON e' gia' stato "raggiunto" precedentemente
      // (altrimenti non entrare nell'if, e chiudi il loop, passando oltre)
      //      printf("\n occorrenza per %d = %d, offset = %d (base=%d,curs=%d)", 
      //             *(listaEventi+curs)  ,*( eventiAttivati + *(listaEventi+curs) ),*(listaEventi+curs),base,curs);
      
      
      // if( *( eventiAttivati + *(listaEventi+curs) ) == 0 ) {   -1
      
      //        *(out_mm + *(listaEventi+curs) + *(listaEventi+base) * (*max_valore_evento) ) = cumulativo_tempo;
      
      //        printf("\n Inserisco %d ==(%lf)==> %d   [%d]",
      //               *(listaEventi+base),
      //               cumulativo_tempo,
      //               *(listaEventi+curs),
      //               *(listaEventi+curs) + *(listaEventi+base) * (*max_valore_evento));
      
      // marca lo stato come "raggiunto"
      //        *(eventiAttivati + *(listaEventi+curs) ) = 1;      -1
      
      // }   -1
 //   } -1

 // }  -1
  printf("\n FINE!");
  
}
void old_rilevaTempiTransizioni(int *listaEventi, double *tempiTransizione, int *quantiEventi,
                            int *max_valore_evento,
                            int *out_mm) {

  int base, curs, i;
  int *eventiAttivati = (int*) calloc( 1 , (*max_valore_evento)+10 );
  double cumulativo_tempo;
  
  printf("\n TROVATI %d eventi in una stringa lunga %d",*max_valore_evento,*quantiEventi);
  
  // Per ogni evento guarda cosa trova a seguire
  for(base = 0; base < ( *quantiEventi - 1 ) ; base++ ) {
    
    cumulativo_tempo = 0;
    // pulisci gli eventi attivati
    for(i = 0; i < (*max_valore_evento)+5; i++  ) *(eventiAttivati+i) = 0;
    
    for(curs = base + 1; curs < ( *quantiEventi )  && *(listaEventi+base)!=*(listaEventi+curs) ; curs++ ) {
      
      // incrementa il cumulativo del tempo con il tempo 
      // fra i due eventi successivi
      // cumulativo_tempo = *(tempiTransizione+curs) - *(tempiTransizione+base);   -1
      
      // se lo stato NON e' gia' stato "raggiunto" precedentemente
      // (altrimenti non entrare nell'if, e chiudi il loop, passando oltre)
//      printf("\n occorrenza per %d = %d, offset = %d (base=%d,curs=%d)", 
//             *(listaEventi+curs)  ,*( eventiAttivati + *(listaEventi+curs) ),*(listaEventi+curs),base,curs);
      
      
      // if( *( eventiAttivati + *(listaEventi+curs) ) == 0 ) {   -1
        
//        *(out_mm + *(listaEventi+curs) + *(listaEventi+base) * (*max_valore_evento) ) = cumulativo_tempo;
        
//        printf("\n Inserisco %d ==(%lf)==> %d   [%d]",
//               *(listaEventi+base),
//               cumulativo_tempo,
//               *(listaEventi+curs),
//               *(listaEventi+curs) + *(listaEventi+base) * (*max_valore_evento));
        
        // marca lo stato come "raggiunto"
//        *(eventiAttivati + *(listaEventi+curs) ) = 1;      -1
        
      // }   -1
    }
    printf("\n--------------------");
  }
  printf("\n FINE!");
  
}

