#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//


// [[Rcpp::export]]
List transitionsTime(NumericVector listaEventi, NumericVector tempiTransizione, int max_valore_evento) {

  int quantiEventi = listaEventi.size();
  int base, curs, i;
  int *eventiAttivati = (int*) calloc( (max_valore_evento)+10 , sizeof(int)  );
  int pos_output = 0;
  NumericVector output_from;
  NumericVector output_to;
  NumericVector output_time;
  
  // Per ogni evento guarda cosa trova a seguire
  for(base = 0; base <  quantiEventi - 1  ; base++ ) {
    
    // pulisci gli eventi attivati
    for(i = 0; i <= max_valore_evento ; i++  ) *(eventiAttivati+i) = 0;
    
    for(curs = base + 1; curs <  quantiEventi ; curs++ ) {

      if((int)listaEventi[base]==(int)listaEventi[curs] ) { 
        output_from.push_back((int)listaEventi[base]);
        output_to.push_back((int)listaEventi[curs]);
        output_time.push_back(   (int)tempiTransizione[curs] - (int)tempiTransizione[base]  );
        break; 
      }

      // se lo stato NON e' gia' stato "raggiunto" precedentemente
      // (altrimenti non entrare nell'if, e chiudi il loop, passando oltre)
      if( *( eventiAttivati + (int)listaEventi[curs]  ) == 0 ) {

        // marca lo stato come "raggiunto"
        *(eventiAttivati + (int)listaEventi[curs] ) = 1;
        output_from.push_back((int)listaEventi[base]);
        output_to.push_back((int)listaEventi[curs]);
        output_time.push_back(   (int)tempiTransizione[curs] - (int)tempiTransizione[base]  );
      }
      
    }
  }
  free(eventiAttivati);
  
  List ret;
  ret["from"] = output_from;
  ret["to"] = output_to;
  ret["time"] = output_time;
  return ret;
}
