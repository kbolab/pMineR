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

// [[Rcpp::export]]
int getInterestingSinglePatientData( DataFrame inputDF) {
  CharacterVector b = inputDF["C100_EVENTO"];
  int n = b[0].size();
  return(n);
}

// [[Rcpp::export]]
List filterPatProcess( DataFrame PatProcess , 
                       CharacterVector arrayEventiDaRimuovere,
                       CharacterVector arrayEventiDaTenere, 
                       int eventColumnNumber) {
  
  std::string genString1;
  std::string genString2;
  NumericVector rigaDaTenere;
  List newPatProcess;
  
  StringVector colonnaEvento = PatProcess[ eventColumnNumber ];
  
  int i,t;
  int daEliminare = 0;

  // Per ogni evento della colonna  
  for( i=0; i < colonnaEvento.size(); i++ ) {
    printf("\n-");
    // VErifica se e' uno degli elementi da rimuovere. Quando li 
    // trovi popola il vettore ''rigaDaEliminare''
    if(arrayEventiDaRimuovere.size()>1) {
      daEliminare = 0;
      for( t = 1; t < arrayEventiDaRimuovere.size(); t++ ) {
        genString1 = colonnaEvento(i) ; genString2 = arrayEventiDaRimuovere(t);
        if( genString1 == genString2  ) {  daEliminare = 1; printf("  => %d",i); }
      }
    }
    if(arrayEventiDaTenere.size()>1) {
      daEliminare = 1;
      for( t = 1; t < arrayEventiDaTenere.size(); t++ ) {
        genString1 = colonnaEvento(i) ; genString2 = arrayEventiDaTenere(t);
        if( genString1 == genString2  ) {  daEliminare = 0; }
      }
    }
    
    // Lista quelle da tenere
    if( daEliminare == 0 ) {
      rigaDaTenere.push_back( i+1 );
    }
  }
  
  // Costruisci la lista da ritornare
  List ret;
  ret["rigaDaTenere"] = rigaDaTenere;
  ret["rigaDaTenere2"] = rigaDaTenere;
  return ret;
}
