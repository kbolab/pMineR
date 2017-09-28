#include <Rcpp.h>
#include <time.h>
#include <stdio.h>
#include <string.h>

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
int c_IsASCII(String fileName) {
  int c;
  FILE *file;
  file = fopen(fileName.get_cstring(), "r");
  if (file) {
    while ((c = getc(file)) != EOF) {
      if(c > 127 ) return( -1 );
    }
    fclose(file);
  }
  return(0);
}
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

time_t convertDate(std::string stringaDaData) {
  char str_anno[4]; char str_mese[2]; char str_giorno[2];  
  struct tm when = {0};  
  
  when.tm_year = (stringaDaData[6]-'0')*1000 + (stringaDaData[7]-'0')*100 + (stringaDaData[8]-'0')*10 + (stringaDaData[9]-'0');
  when.tm_mon = (stringaDaData[3]-'0')*10 + (stringaDaData[4]-'0');
  when.tm_mday = (stringaDaData[0]-'0')*10 + (stringaDaData[1]-'0');
  when.tm_hour = (stringaDaData[11]-'0')*10 + (stringaDaData[12]-'0');
  when.tm_min = (stringaDaData[14]-'0')*10 + (stringaDaData[15]-'0');
  when.tm_sec = (stringaDaData[17]-'0')*10 + (stringaDaData[18]-'0');
  

  // str_anno[0] = stringaData[6]; str_anno[1] = stringaData[7]; str_anno[2] = stringaData[8]; str_anno[3] = stringaData[9];
  // str_mese[0] = stringaData[3]; str_mese[1] = stringaData[4];
  // str_giorno[0] = stringaData[0]; str_giorno[1] = stringaData[2];
  
  time_t converted = mktime(&when);
  return(converted);
}

/*
 * ID_act_group              : il la lista contente le matrici dei Log dei pazienti
 * MMatrix                   : la matrice da compilare
 * eventColumnNumber         : la posizione della colonna dell'evento clinico nell' Event Log
 * posizioneBegin            : la posizione del BEGIN nella MMatrix
 * posizioneEnd              : la posizione dell'END nella MMatrix
 * posDataInizio             : la posizione della colonna della DATA nell' EventLog
 * rowNames                  : 
 * separatore                : il separatore usato per separare i nomi degli Eventi Clinici
 * listaOccorrenzeTempi      : l'array delle occorrenze che verra' compilato (i delta t x ogni transizione)
 * namesListaOccorrenzeTempi : l'array con i 'names()' della 'listaOccorrenzeTEmpi'
 */
// [[Rcpp::export]]
void c_buildMMMatrices_and_other_structures_v2(
    List ID_act_group, 
    NumericMatrix MMatrix , 
    int eventColumnNumber,
    int posizioneBegin,
    int posizioneEnd,
    int posDataInizio,
    StringVector rowNames,
    String separatore,
    StringVector listaOccorrenzeTempi,
    StringVector namesListaOccorrenzeTempi
  ) {
  int numRiga, numeroRigheMatrice;
  int cx;
  int deltaMinuti;
  DataFrame dataFramePaziente;
  StringVector arrayNomiPazienti = ID_act_group.names();
  Rcpp::List MMTimeOcc;
  char buffer[100];

  for( int IndexPatient = 0; IndexPatient < ID_act_group.length();  IndexPatient++   ) {
    // print del paziente che stai analizzando ora
    // Rcpp::Rcout << "\n" << arrayNomiPazienti(IndexPatient);
    // su ogni elemento del percorso clinico
    // t e' il "tempo" in senso di "step"
    dataFramePaziente = ID_act_group[ IndexPatient ];
    // Numero di stati del paziente
    numeroRigheMatrice = dataFramePaziente.nrow();
    // array delle transazioni (colonna degli eventi del data.frame)
    StringVector arrayStatiTrack = dataFramePaziente[  eventColumnNumber  ];
    IntegerVector posizioneIntMatrice = match(arrayStatiTrack,rowNames);
    StringVector arrayDate = dataFramePaziente[  posDataInizio  ];
    
    for(numRiga = 0;  numRiga < numeroRigheMatrice ; numRiga++ ) {
      // vedi se devi legare il BEGIN
      if( numRiga == 0) {
        MMatrix( posizioneBegin , (posizioneIntMatrice[numRiga]-1) )++ ;
      }
      // vedi se devi legare l'END   
      if( numRiga == (numeroRigheMatrice-1) ) {
        MMatrix( (posizioneIntMatrice[numRiga]-1) , posizioneEnd)++ ;
      }      
      // Ora sta a tutti gli altri
      if( numRiga > 0 & numRiga < (numeroRigheMatrice-1) ) {
        // converti la data in stringa
        std::string stringaDaData = Rcpp::as<std::string>(arrayDate[numRiga-1]);
        std::string stringaAData = Rcpp::as<std::string>(arrayDate[numRiga]);
        std::string nomeStatoFrom = Rcpp::as<std::string>(arrayStatiTrack[numRiga-1]);
        std::string nomeStatoTo = Rcpp::as<std::string>(arrayStatiTrack[numRiga]);
        
        // Aggiorna la matrice di transizione
        MMatrix( (posizioneIntMatrice[numRiga-1]-1) , (posizioneIntMatrice[numRiga]-1) )++ ;
        
        // Calcola il delta date fra il precedente ed ora
        deltaMinuti = (int)(difftime( convertDate(stringaAData),convertDate(stringaDaData) )/60 );

        std::string nomeCompleto;
        nomeCompleto.append(nomeStatoFrom);
        nomeCompleto.append(separatore);
        nomeCompleto.append(nomeStatoTo);
        
        // printf("\n Nome: %s",nomeStatoTo.c_str());
        StringVector Vect_nomeCompleto(1);

        Vect_nomeCompleto[0] = nomeCompleto;

        IntegerVector posMatriceOccTempi = match(Vect_nomeCompleto,namesListaOccorrenzeTempi);
        
        // prendi il contenuto della matrice
        std::string val_arr_occorr = Rcpp::as<std::string>(listaOccorrenzeTempi[ posMatriceOccTempi[0]-1 ]);
        cx = snprintf ( buffer, 100, "%d", deltaMinuti   );
        if(val_arr_occorr!="")  val_arr_occorr.append(",");
        val_arr_occorr.append(buffer);
        
        listaOccorrenzeTempi[ posMatriceOccTempi[0]-1 ] = val_arr_occorr;
      }
    }
  }
  return;
}

