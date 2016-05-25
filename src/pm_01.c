#include <stdio.h>
#include <string.h>
#include <math.h>

struct bagListStruct {
  char *strPunt;
  struct bagListStruct *next;
};
struct footPrintStruct {
  int *footPrint;
  int numOfElement;
};
struct dependenciesStruct {
  char *pre;
  char *post;
  struct dependenciesStruct *next;
};

struct bagListStruct *bagListFather;
struct bagListStruct *blCursor;
struct dependenciesStruct *depFather;
struct dependenciesStruct *depCursor;
struct footPrintStruct *fp;

int varGlobal = 1;
int varGlobal_v2 = 1;
int deepLevel = 0; // for debug of recursive functions

void printBagList() {
  struct bagListStruct *tmpCursor;
  int numOfElement = fp->numOfElement;
  
  for( struct bagListStruct *nn = bagListFather; nn != NULL; nn = nn->next ) {
    if(bagListFather!=nn) printf("\n-%s-",nn->strPunt);
  }
}

/*
 * addStringToBag : aggiunge una stringa alla 'bag of words'
 * 
 * stringa: la parola da aggiungere
 * numOfElement: il numero di elementi massimi (che dovrebbe corrispondere alla lunghezza della parola)
 */
void addAssociationToBag( char *pre, char *post) {
  
  int numOfElement = fp->numOfElement;
  depCursor->next = (struct dependenciesStruct *)calloc(1,sizeof(struct dependenciesStruct));

  depCursor->next->post = (char *)calloc(1,numOfElement+2);
  depCursor->next->pre = (char *)calloc(1,numOfElement+2);
  depCursor->next->next = NULL;
  for(int i=0; i < numOfElement-1; i++) {
    *(depCursor->next->post + i) ='\0';
    *(depCursor->next->pre + i) ='\0';
  }
  for(int i=0; i < numOfElement; i++){
    *(depCursor->next->post + i) = *(post + i);
    *(depCursor->next->pre + i) = *(pre + i);
  }

  depCursor = depCursor->next;
  varGlobal_v2++;
}
void addStringToBag( char *stringa) {
  
  int numOfElement = fp->numOfElement;
  
  blCursor->next = (struct bagListStruct *)calloc(1,sizeof(struct bagListStruct));
  blCursor->next->strPunt = (char *)calloc(1,numOfElement+2);
  blCursor->next->next = NULL;
  for(int i=0; i < numOfElement-1; i++) *(blCursor->next->strPunt + i) ='\0';
  for(int i=0; i < numOfElement; i++) *(blCursor->next->strPunt + i) = *(stringa + i);
  
  blCursor = blCursor->next;
//  printf("\n");
//  for( struct bagListStruct *nn = bagListFather; nn->next!=NULL; nn = nn->next ) {
//    printf("%s,",nn->strPunt);
//  }
}

/*
 * checkWord : verifica la validità di una parola di lunghezza variabile
 * 
 * stringa: la parola da verificare
 */
int checkWord( char *stringa ) {
  char valore;
  int pos = strlen(stringa);
//  printf("\n-------------------");
//  printf("\nstringa = %s",stringa);
  for( int first = 0; first < pos; first++ ) {
    if( *(stringa+first)=='1'  ) {
      for( int second = first; second < pos; second++ ) {
        if( *(stringa+second)=='1'  ) {
          valore = fp->footPrint[   first*(fp->numOfElement) + second    ];
//          printf("\n first=%d, second=%d, numOfElement=%d",first,second,pos);
//          printf("\n-%s- valore='%d'",stringa,valore);
          if(valore!=4) return(-1);  // se il valore è != da '#'
        }
      }
    }
  }
//  printf("\n-------------------");
  return(0);
}

int str_len(char *p) {
  int i;
  for( i =0; *(p+i)!='\0';i++) ;;;;
  return(i);
}
/*
 * buildWords : cerca ricorsivamente le parole papabili partendo da '0' e '1'
 * 
 * stringa: la parola fino ad ora costruita (e validata)
 * pos: il numero  di elementi costituenti 'stringa' (potrebbe essere calcolata, ma così è più veloce)
 * numOfElement: il numero di elementi massimi
 */
void buildWords( char *stringa) {
  int pos,numOfElement;
  deepLevel++;
  // assegnazione preventiva delle variabili
  pos = strlen( stringa );
  numOfElement = fp->numOfElement;

  // fai un check della parola: se non è valida ritorna
  if(pos>0) {
    if( checkWord( stringa ) == -1 ) {
      deepLevel--;
      return;
    }
  }
//  if( checkWord( stringa ) == -1 ) printf("\nFAIL!");
  
  // caso noto: la lunghezza della parola è giunta al massimo
  if( pos == (numOfElement) ) {

    // verifica che non sia il caso banale di tutti '0', se sì esci
    int sommaUni=0;
    for(int i=0; i<=pos; i++)  {if(*(stringa+i)=='1') sommaUni++; }
    if(sommaUni==0) {
      deepLevel--;
      return;
    }
    
    varGlobal++;  

    // se non è uscito è perchè la parola contiene almeno un '1', quindi è da aggiungere
    addStringToBag( stringa );
//    printf("\n Stringa=%s",stringa);
    deepLevel--;
    return;
  }
  
  // allunga la stringa aggiungendo '1' e testala
  // alla fine riduci nuovamente la stringa rimuovendo l'ultimo digit
  *(stringa+pos) = '1';  
  buildWords(  stringa  );
  *(stringa+pos) = '\0';

  // allunga la stringa aggiungendo '0' e testala
  // alla fine riduci nuovamente la stringa rimuovendo l'ultimo digit  
  *(stringa+pos) = '0';
  buildWords(  stringa  );
  *(stringa+pos) = '\0';
//  if(deepLevel<=2) printf("\n stringa=%s (%d)",stringa,deepLevel);
  deepLevel--;
}
/*
 * recruitePossibleWords : funzione che restituisce un set di parole 'papabili'
 * 
 * footPrint: array contenente la matrice di footPrint
 * numOfElement: il numero di elementi (numero di righe della matrice di footPrint)
 */
void recruitePossibleWords( int *footPrint ) {
  char *stringa;
  int i;

  // preparazione e pulizia dell'area di memoria che ospiterà la stringa
  stringa = (char *)calloc( (fp->numOfElement)+4, sizeof(char));
  for(int i=0; i<= fp->numOfElement +1 ; i++) *(stringa+i)=0;
  
  // lancia la funzione ricorsiva per la ricerca delle parole
  // ( dovrebbe popolare la 'bag of words')
  buildWords( stringa );

}
void preliminaryOperations( int *footPrint, int *numOfElement) {
  // alloca una footPrint Struct
  fp = (struct footPrintStruct *)calloc(1,sizeof(struct footPrintStruct));
  fp->numOfElement = *numOfElement;
  fp->footPrint = footPrint;
  
  // alloca il primo elemento della lista lineare della 'bag of words'
  bagListFather = (struct bagListStruct *)calloc(1,sizeof(struct bagListStruct));
  bagListFather->strPunt = (char*)calloc(1,*numOfElement+5);
  bagListFather->next=NULL; 
  // prepare il cursore della lista lineare
  blCursor = bagListFather;
  
  // alloca il primo elemento della lista lineare della 'bag of words'
  depFather = (struct dependenciesStruct *)calloc(1,sizeof(struct dependenciesStruct));
  depFather->pre = NULL;
  depFather->post = NULL;
  depFather->next=NULL; 
  depCursor = depFather;
  
  return;
}
void findDependences( ) {
  struct bagListStruct *possPre;
  struct bagListStruct *possPost;
  int iPre, iPost, valore, mismatch;
  
  // per ogni elemento della bag of list possibile PRE
  for( possPre = bagListFather->next; possPre != NULL; possPre = possPre->next) {
    // per ogni elemento della bag of list possibile POST
    for( possPost = bagListFather->next; possPost != NULL; possPost = possPost->next) {
      mismatch = 0;
      // se son diversi (non può essere riflessivo, il confronto)
      if(possPre != possPost) {

        // verifica che per ogni elemento della stringa PRE a '1'
        for( iPre=0; iPre < fp->numOfElement ; iPre++ ) {
          if(   *(possPre->strPunt+iPre) =='1') {
            // verifica che per ogni elemento della stringa POST a '1'
            for( iPost = 0; iPost < fp->numOfElement ; iPost++ ){
              if( *(possPost->strPunt+iPost)=='1'  ) {
                // deve essere che sono in relazione di '->'
                valore = fp->footPrint[   iPre*(fp->numOfElement) + iPost    ];
                if( valore != 2) {
                  mismatch = 1;
                }
              }
            }
          }
        }
      } // if they are the same (again)
      else {
        mismatch=1;
      }
      // se puoi, aggiungile nella lista delle associazioni papabili
      if(mismatch==0) {
        addAssociationToBag( possPre->strPunt , possPost->strPunt );
//        printf("\nchk: %s vs %s   =  ",possPre->strPunt,possPost->strPunt);
      }      
    }
  }
}

int convertBinaryStringToInteger(char  *stringa ) {
  int valore = 0;
  int powerOf2 = 0;

  for(int i = str_len(stringa)-1; i>=0; i--) {
    if(*(stringa+i)=='1') valore+=pow(2,powerOf2);
    powerOf2++;
  }
  return(valore);
}
void composeResult( int *arrayPre,int *arrayPost   ) {
  int pre, post;
  int position=0;
  for( depCursor = depFather->next; depCursor!=NULL; depCursor = depCursor->next ) {
      pre = convertBinaryStringToInteger(  depCursor->pre );
      post = convertBinaryStringToInteger(  depCursor->post );
      *(arrayPre+position) = pre;
      *(arrayPost+position) = post;
      position++;
  }
}


/*
 * getSets : resittuisce le assocazioni utili
 * 
 * footPrint: array contenente la matrice di footPrint
 * numOfElement: il numero di elementi (numero di righe della matrice di footPrint)
 */
void getSets(  int *footPrint, 
               int *numOfElement, 
               int *whatDoYouWantToKnow, 
               int *howManyDependences,
               int *arrayPre,
               int *arrayPost ) {
  int a;
  
  // preliminary operations: calloc for global pointers
  preliminaryOperations( footPrint , numOfElement );
    
  // get the possible words (by reciproval '#' );
  recruitePossibleWords(  footPrint   );
  
  // get the possible association (by consequential '->')
  findDependences(  );

  // compose the result in the *pointers transforming the binary in decimal
  if(*whatDoYouWantToKnow==1) composeResult( arrayPre, arrayPost );
  
  // give me back, anyway, the number of dependencies
  *howManyDependences = varGlobal_v2;

  // debug
//  printBagList();
  
}