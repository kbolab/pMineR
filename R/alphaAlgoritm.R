#' AlphaAlgoritm class
#' 
#' @description  An implementation of an AlphaAlgorith (AA) model. Written with closures it provides also a set of
#'               ancillary methods for building/handling models. The methods are:
#'              \itemize{
#'              \item \code{alphaAlgorithm( ... ) } is the costructor
#'              \item \code{loadDataset( ... ) } loads data into an AA object.
#'              \item \code{trainModel( ... ) } trains an AA model
#'              \item \code{getModel() } returns the AA model (XML or graphical way, via grViz script)
#'              \item \code{replay( ... ) } submits to the model a set of given sequences and returns the fitting
#'              \item \code{play( ... ) } generates a wished number of sequences
#'              \item \code{plot() } plots the graph
#'              \item \code{distanceFrom( ... ) } calculates the distance between two different AA model objects
#'              }
#'              There are two ways to use this class: directly using the methods previously 
#'              listed or via wrapping functions (called PM.<method name>). In the examples section you will find an example of both.
#' @param parameters.list a list containing possible parameters to tune the model. At the moment no parameters are admitted for this model: this is implemented for further improvements.
#' @useDynLib pMineR    
#' @import stringr XML graphics          
#' @export
#' @examples \dontrun{
#' # -----------------------------------------------
#' #  USING THE METHODS of the class
#' # -----------------------------------------------
#' obj.L<-dataLoader();   # create a Loader
#' 
#' # Load a .csv using "DES" and "ID" as column names to indeicate events 
#' # and Patient's ID
#' obj.L$loader(nomeFile = "./otherFiles/test_02.csv",IDName = "ID",
#' EVENTName = "DES")
#' 
#' # now create an object AlphaAlgorithm
#' obj.AA<-alphaAlgorithm();    
#' 
#' # load the data into AA model
#' obj.AA$loadDataset( obj.L$getData() );  
#' 
#' # train the model
#' obj.AA$trainModel();  
#' 
#' # plot the model 
#' obj.AA$plot();  
#' 
#' # -----------------------------------------------
#' #  USING THE WRAPPER Functions
#' # -----------------------------------------------
#' # Instantiate a loader
#' obj.LD<-LD.builder()
#' 
#' # Load a CSV into the loader
#' LD.load.csv(loader.obj = obj.LD ,nomeFile = "./otherFiles/test_02.csv",
#'      IDName = "ID",EVENTName = "DES")
#' 
#' # Instantiate a PM model
#' obj.PM <-PM.builder(kindOfObject = "alphaAlgorithm")
#' 
#' # Load the PM model
#' PM.loadDataset(PM.obj = obj.PM,dataList = LD.getData(loader.obj = obj.LD))
#'
#' # train it
#' PM.trainModel(PM.obj = obj.PM)
#' 
#' # plot the model 
#' PM.plot(PM.obj = obj.PM)
#' }
alphaAlgorithm<-function(  parameters.list = NA ) {

  MMatrix<-''
  MMatrix.perc<-NA
  MMatrix.perc.noLoop<-NA  
  footPrint<-''
  model.grViz<-'';
  model.XML<-'';
  is.dataLoaded<-FALSE
  parameters<-NA
  obj.log<-NA
  istanceClass<-list()
  
  # ***************************************************************************************************
  # WRAPPING METHODS
  # *************************************************************************************************** 
  #===========================================================  
  # loadDataset
  #===========================================================  
  loadDataset<-function( dataList ) {
    transMatrix<-dataList$MMatrix
    footPrintTable<-dataList$footPrint
    
    MMatrix<<-transMatrix
    footPrint<<-footPrintTable
    
    # calcola la matrice delle percentuali e quella delle percentuali senza i loop
    # MMatrix.perc
    MM<-MMatrix;
    for( i in seq( 1 , nrow(MM)) ) {if(sum(MM[i,])>0)  {MM[i,]<-MM[i,]/sum(MM[i,]);} } 
    MMatrix.perc<<-MM
    
    # MMatrix.perc.noLoop
    MM<-MMatrix;
    diag(MM)<-0;
    for( i in seq( 1 , nrow(MM)) ) {if(sum(MM[i,])>0)  {MM[i,]<-MM[i,]/sum(MM[i,]);} } 
    MMatrix.perc.noLoop<<-MM    
    
    is.dataLoaded<<-TRUE
  }
  #===========================================================  
  # replay
  #===========================================================  
  replay<-function( wordSequence.raw ) {
    # initialize variables
    res<-list(); 
    status.ok<-0; status.warning<-0; status.error<-0; 
    resTable<-c();
    # build a PETRI NETWORK OBJECT
    objPN<-petriNetworkModel();
    # Load a model into the PETRI NETWORK OBJECT
    objPN$loadModel.xml( xmlModel = model.XML )
    
    # now loop for each word and check if it complains with the PETRI NETWORK
    for(PatID in names(wordSequence.raw)) {
      singleRes<-objPN$replay( wordSequence.raw[[ PatID ]] )
      # build a quick summary table
      if(singleRes$status=="ok") status.ok<-status.ok+1;
      if(singleRes$status=="warning") status.warning<-status.warning+1;
      if(singleRes$status=="error") status.error<-status.error+1;
      # build the final resuls structure
      resTable<-rbind(resTable,c(PatID,singleRes$status,singleRes$msg))
    }

    # finalize the summary table
    res<-list()
    res$resultTable<-resTable
    res$summaryTable<-c(status.ok,status.warning,status.error)
    res$summaryTable<-rbind(res$summaryTable,res$summaryTable/sum(res$summaryTable))
    colnames(res$resultTable)<-c("PatID","status","msg")
    colnames(res$summaryTable)<-c("ok","warning","error")
    return(res)
  }
  #===========================================================
  # play
  #===========================================================
  play<-function(numberOfPlays = 1 ) {
    res<-list()
    if(is.dataLoaded == FALSE) obj.log$sendLog(msg= "data is not yet loaded in model!" , type="NMI" )
        
    for(i in seq(1,numberOfPlays)) {
      # build a PETRI NETWORK OBJECT
      objPN<-petriNetworkModel();
      # Load a model into the PETRI NETWORK OBJECT
      objPN$loadModel.xml( xmlModel = model.XML )
      # play!
      res[[i]]<-objPN$play();
    }
    return(res)
  }  
  #===========================================================  
  # trainModel
  #===========================================================  
  trainModel<-function() {
    if(is.dataLoaded == FALSE) {
      obj.log$sendLog(msg= "data is not yet loaded in model!" , type="NMI" )
    }
    # 1) TL
    TL<-build.TL();  
    # 2) TI
    TI<-built.TI();
    # 3) TO
    TO<-built.TO();
    # 4) XL
    xl<-build.xl(TL);
    # 5) YL 
    yl<-build.yl(xl);
    # 6) PL
    pl<-build.pl(yl)
    # 7) FL
    res<-build.fl(yl,TI,TO,TL);
    fl<-res$fl;
    listaNodi<-res$listaNodi; 
    rigaBEGIN<-res$rigaBEGIN;
    rigaEND<-res$rigaEND; 
    stringaNodiComplessi<-res$stringaNodiComplessi;
    
    # now plot it
    a<-paste(c("digraph boxes_and_circles {
             
             # a 'graph' statement
             graph [overlap = true, fontsize = 10]
             
             # several 'node' statements
             node [shape = box,
             fontname = Helvetica]
             ",listaNodi,"
             
             node [shape = circle,
             fixedsize = true,
             width = 0.5] // sets as circles
             'BEGIN'; 'END';
             
             # several edge
             ",rigaBEGIN,"
             ",rigaEND,"
             ",stringaNodiComplessi,"
    }"), collapse='')    

    # converti quanto restituito in un XML
    listaDaPassare<-list(
      "fl"=fl,
      "listaNodi"=res$listaNodi,
      "listaTokenPos"=res$listaTokenPos,
      "stringaNodiComplessi"=res$stringaNodiComplessi,
      "arrayNodiComplessi"=res$arrayNodiComplessi,
      "arrayTokenUtili"=res$arrayTokenUtili,
      "rigaBEGIN"=rigaBEGIN,
      "rigaEND"=rigaEND
    )
    strutturaXML <- convert2XML(  nodeList = listaDaPassare )

    model.grViz<<-a;
    model.XML<<-strutturaXML
  }
  #===========================================================  
  # getModel
  #===========================================================  
  getModel<-function(kindOfOutput) {
    if(kindOfOutput=="XML") return( model.XML )
    if(kindOfOutput=="grViz") return( model.grViz )
    obj.log$sendLog(msg= "The requested model is not available yet" , type="NMI" )    
  }
  #===========================================================  
  # plot
  #===========================================================  
  plot<-function(){
    grViz( getModel(kindOfOutput = "grViz" ))
  }  
  #=================================================================================
  # distanceFrom - WRAPPER Function
  # Funzione WRAPPER per il calcolo delle distanze rispetto ad un oggetto omologo
  #=================================================================================   
  distanceFrom<-function( objToCheck, metric="default") {
    if( metric == "default" ) return( distanceFrom.default( objToCheck = objToCheck) )
    obj.log$sendLog(msg= "The requested metric is not yet available" , type="NMI" )
  }  
  #===========================================================  
  # getLogObj
  #===========================================================  
  getLogObj<-function() {
    return(obj.log)
  }
  #===========================================================
  # setLogObj
  #===========================================================  
  setLogObj<-function( objLog ) {
    obj.log<<-objLog
  }    
  
  
  # ***************************************************************************************************
  # MODEL SPECIFIC PUBLIC METHODS
  # *************************************************************************************************** 
  
  
  # ***************************************************************************************************
  # PRIVATE METHODS
  # *************************************************************************************************** 
  #===========================================================
  # distanceFrom.default
  # MEtrica di default. In questo caso la metrica di default fa una 
  # semplice comparison fra le footprint tables. pesando come 1 ogni 
  # diversita' di simbolo ad eccezione di una inversione di direzion che 
  # vale 2.
  #===========================================================
  distanceFrom.default<-function( objToCheck ) {
    
    ext.MM <- objToCheck$getModel(kindOfOutput = "MMatrix.perc")
    int.MM <- MMatrix.perc
    
    combinazioni<-calcolaMatriceCombinazioni( ext.MM = ext.MM, int.MM = int.MM)
    
    distance<-sum(abs(combinazioni$ext - combinazioni$int))
    return( list(   "distance" = distance )      )
  }   
  #===========================================================
  # calcolaMatriceCombinazioni
  # Funzione di comodo che calcola in una matrice le differenze di probabilita'
  # fra due FSM. Di fatto e' un pre-processing per funzioni che calcolano metriche
  #===========================================================
  calcolaMatriceCombinazioni<-function( ext.MM, int.MM) {
    unione.nomi<-unique(c(colnames(ext.MM),colnames(int.MM)))
    combinazioni<-expand.grid(unione.nomi,unione.nomi)
    combinazioni<-cbind( combinazioni,  rep(0,nrow(combinazioni)  ) )
    combinazioni<-cbind( combinazioni,  rep(0,nrow(combinazioni)  ) )
    colnames(combinazioni)<-c("from","to","int","ext")
    combinazioni$from<-as.character(combinazioni$from)
    combinazioni$to<-as.character(combinazioni$to)
    
    for(riga in seq(1,nrow(combinazioni))) {
      
      if(  combinazioni[riga, "from"] %in%  colnames(int.MM)  &
           combinazioni[riga, "to"] %in%  colnames(int.MM)
      ) {
        combinazioni[riga, "int"]<-int.MM[  combinazioni[riga, "from"] , combinazioni[riga, "to"]     ]
      }
      if(  combinazioni[riga, "from"] %in%  colnames(ext.MM)  &
           combinazioni[riga, "to"] %in%  colnames(ext.MM)
      ) {
        combinazioni[riga, "ext"]<-ext.MM[  combinazioni[riga, "from"] , combinazioni[riga, "to"]     ]
      }
    }    
    return(combinazioni)  
  }  
  #===========================================================  
  # build.X series of function
  #===========================================================  
  build.TL<-function() {
    res<-colnames(MMatrix)[!(colnames(MMatrix) %in% c("BEGIN","END"))]
    return(res);
  }
  built.TI<-function() {
    res<-colnames(footPrint)[which(as.character(footPrint["BEGIN",])=="->")]
    return(res)
  }
  built.TO<-function() {
    res<-colnames(footPrint)[which(as.character(footPrint["END",])=="<-")]
    return(res);
  }
  build.xl<-function(TL) {
    a<-footPrint;   xl.l<-list(); 
    a<-a[ -c( which(rownames(a)=="BEGIN"),which(rownames(a)=="END")   ),-c(which(colnames(a)=="BEGIN"),which(colnames(a)=="END"))  ]
    
    a[which( a=='||',arr.ind = T )]<-1
    a[which( a=='->',arr.ind = T )]<-2
    a[which( a=='<-',arr.ind = T )]<-3
    a[which( a=='#',arr.ind = T )]<-4
    
    numberOfVariables<-nrow(a) 
    #    print(colnames(a))
    var1<-0; var2<-0; var3<-0;
    res<-.C("getSets",as.integer( as.array(t(a)) ), as.integer( numberOfVariables ), as.integer(0) ,as.integer(var1),as.integer(var2),as.integer(var3))
    
    numberOfDepencencies <- res[[4]]
    var2<-rep(0,numberOfDepencencies); var3<-rep(0,numberOfDepencencies);
    res<-.C("getSets",as.integer( as.array(t(a)) ), as.integer( numberOfVariables ), as.integer(1) ,as.integer(var1),as.integer(var2),as.integer(var3))
    
    counter<-1;
    xl<-list();
    for(i in seq(1,numberOfDepencencies)) {
      bw.from<-dectobin(res[[5]][i]); bw.to<-dectobin(res[[6]][i]);
      bw.from<- c(rep(0,numberOfVariables-length(bw.from)),bw.from)
      bw.to<- c(rep(0,numberOfVariables-length(bw.to)),bw.to)
      pos.from = which(bw.from==1);  pos.to = which(bw.to==1);
      desc.from = colnames(a)[pos.from];  desc.to = colnames(a)[pos.to];
      
      if(length(desc.to)>0 & length(desc.from)>0) {  
        xl[[counter]]<-list( "from"=desc.from, "to"=desc.to  )
        counter<-counter+1;
      }
    }
    return(xl);
  }  
  build.yl<-function(xl) {
    yl<-list();
    counter<-1
    for( index in seq(1,length(xl))) {
      daScartare<-FALSE;
      for( ii in seq(1,length(xl))) {
        if(is.included( xl[[index]]$from, xl[[ii]]$from)==TRUE &
           is.included( xl[[index]]$to, xl[[ii]]$to)==TRUE) {
          
          if(setequal(xl[[index]]$from, xl[[ii]]$from) ==TRUE &
             setequal(xl[[index]]$to, xl[[ii]]$to) ==TRUE  &
             daScartare==FALSE) {
            daScartare = FALSE;
          } else  {
            daScartare = TRUE;
          }
        }
      }
      if(daScartare==FALSE) {
        yl[[counter]]<-xl[[index]]
        counter<-counter+1
      }
    }
    return(yl);
  }
  build.pl<-function(yl) {
    pl<-yl
    pl[[length(pl)+1]]<-"BEGIN"
    pl[[length(pl)+1]]<-"END"
    return(pl);
  }
  build.fl<-function(yl,TI,TO,TL) {
    fl<-list();
    counter<-1;
    for( i in seq(1,length(yl))) {
      for( fromNode in yl[[i]]$from ) {
        fl[[counter]]<-list(  "from"=fromNode, "to"=list( "from"=yl[[i]]$from,"to"=yl[[i]]$to   )     )
        counter<-counter+1
      }
    }
    # poi cicla sui TO
    for( i in seq(1,length(yl))) {
      for( toNode in yl[[i]]$to ) {
        fl[[counter]]<-list( "from"=list( "from"=yl[[i]]$from,"to"=yl[[i]]$to   ), "to"=toNode     )
        counter<-counter+1
      }
    }
    # poi tocca ai TI
    fl[[counter]]<-list("from"="BEGIN", "to"=TI)
    fl[[counter+1]]<-list("from"=TO, "to"="END")
    
    # grafica il risultato
    listaNodiComplessi<-list();
    #listaNodi<-c("'BEGIN'; 'END'");
    listaNodi<-c(); incipit<-'';
    for(i in TL) {
      listaNodi<-paste( c( listaNodi, incipit ," '",i,"'"   ),collapse = ''   )
      incipit<-';'
    }
    
    listaTokenPos<-c(); incipit<-''; stringaNodiComplessi<-''; arrayTokenUtili<-c();
    arrayNodiComplessi<-c();
    for(i in seq(1,length(fl))) {
      if(i>1) incipit<-";"
      if(  is.list(fl[[i]]$from) == TRUE  ) listaTokenPos<-paste( c( listaTokenPos, incipit," ","'",i,"'"   ),collapse = ''   )
      if(  is.list(fl[[i]]$to) == TRUE  ) listaTokenPos<-paste( c( listaTokenPos, incipit," ","'",i,"'"   ),collapse = ''   )
      
      # 'from'BEGIN'
      if(length(fl[[i]]$from)==1){
        if( fl[[i]]$from=="BEGIN"  ) {
          rigaBEGIN<-c('');
          for( kk in seq(1,length( fl[[i]]$to ))) {
            rigaBEGIN<-paste( c(rigaBEGIN, " BEGIN->", "'",fl[[i]]$to[kk],"'" ),collapse=''   ); 
          }
        }
      }
      # 'from'END'
      if(length(fl[[i]]$to)==1) {
        if( fl[[i]]$to=="END"  ) {
          rigaEND<-c('');
          for( kk in seq(1,length( fl[[i]]$from ))) {
            rigaEND<-paste( c(rigaEND, "'",fl[[i]]$from[kk],"'","->END "),collapse=''   ); 
          }
        }
      }
      if( is.list(fl[[i]]$from) | is.list(fl[[i]]$to) & i < (length(fl)-2)) {
        if(is.list(fl[[i]]$from) ) {listaDaCercare<-fl[[i]]$from; statoDaLinkare<-fl[[i]]$to}
        if(is.list(fl[[i]]$to) ) {listaDaCercare<-fl[[i]]$to; statoDaLinkare<-fl[[i]]$from}
        #cerca se gia' esiste nella lista dei nodi complessi
        esiste<-FALSE
        indiceDaUsare<-0;
        if(length(listaNodiComplessi)==0)   listaNodiComplessi[[1]]<-listaDaCercare
        
        for(qwe in seq(1,length(listaNodiComplessi))) {
          if( identical( listaNodiComplessi[[qwe]] , listaDaCercare  ) ) {
            esiste<-TRUE
            indiceDaUsare<-qwe
          }
        }
        if(esiste==FALSE) {
          listaNodiComplessi[[  length(listaNodiComplessi)+1  ]]<-listaDaCercare
          indiceDaUsare<-length(listaNodiComplessi)
        }
        # associa il nodo della posizione indiceDaUsare
        if(is.list(fl[[i]]$from) ) {
          stringaNodiComplessi<-paste( c( stringaNodiComplessi," ",indiceDaUsare,"->","'",statoDaLinkare,"'")  ,collapse = ''  );
          if(! ( statoDaLinkare %in% arrayNodiComplessi)) arrayNodiComplessi<-c(arrayNodiComplessi,statoDaLinkare)
        }
        if(is.list(fl[[i]]$to) ) {
          stringaNodiComplessi<-paste( c( stringaNodiComplessi," ","'",statoDaLinkare,"'","->",indiceDaUsare)  ,collapse = ''  );
          if(! ( statoDaLinkare %in% arrayNodiComplessi)) arrayNodiComplessi<-c(arrayNodiComplessi,statoDaLinkare)
          # array token usati
          if(! ( indiceDaUsare %in% arrayTokenUtili)) {arrayTokenUtili<-c(arrayTokenUtili,indiceDaUsare) }
        }        
      }
    }
    return(  list(
      "fl" = fl, 
      "listaNodi"=listaNodi ,
      "stringaNodiComplessi"=stringaNodiComplessi,
      "rigaBEGIN"=rigaBEGIN,
      "rigaEND"=rigaEND,
      "arrayNodiComplessi"=arrayNodiComplessi,
      "arrayTokenUtili"=arrayTokenUtili
    )  )
  }
  #===========================================================  
  # convert2XML - future
  # Converte la struttura interna in formato XML
  #===========================================================  
  convert2XML<-function( nodeList  ) {
    testo<-textObj();
    linkArr_03<-c();
    
    linkArr_01<-str_split(string = str_c(nodeList$stringaNodiComplessi," ",nodeList$rigaBEGIN," ",nodeList$rigaEND),pattern = " ")[[1]]
    for(i in seq(1, length(linkArr_01))) {
      if( str_length( linkArr_01[i])>3 ) {
        linkArr_02<-str_split(string = linkArr_01[i],pattern = "->")[[1]]
        pre<-str_replace_all(string = linkArr_02[1],pattern = "\'",replacement = "")
        post<-str_replace_all(string = linkArr_02[2],pattern = "\'",replacement = "")
        linkArr_03<-rbind(linkArr_03, c( pre,post)   ); 
      }
    }
    
    nodeArr_01<-nodeList$arrayTokenUtili
    nodeArr_02<- nodeList$arrayNodiComplessi
    nodeArr_01<-c(nodeArr_01, "BEGIN")
    nodeArr_01<-c(nodeArr_01, "END")    

    testo$add("<xml>");
    testo$add("\t<graphStructure>");
    testo$add("\t\t<nodeList>")
    for(i in seq(1,length(nodeArr_02))) { testo$add(c("\t\t\t<node name='",nodeArr_02[i],"' type='state'></node>"))  }
    for(i in seq(1,length(nodeArr_01))) { testo$add(c("\t\t\t<node name='",nodeArr_01[i],"' type='tokenPlace'></node>"))  }
    testo$add("\t\t</nodeList>")
    testo$add("\t\t<links>")
    for(i in seq(1,nrow(linkArr_03))) {
      testo$add(c("\t\t\t<link from='",linkArr_03[i,1],"' to='",linkArr_03[i,2],"'></link>"))        
    }
    testo$add("\t\t</links>")
    testo$add("\t</graphStructure>");
    testo$add("</xml>");
    return( testo$get() );
  }  
  #===========================================================
  # setIstanceClass
  #===========================================================
  setInstanceClass<-function( className, classType = "default") {
    istanceClass[[classType]]<-className
  }  
  #===========================================================
  # costructor
  # E' il costruttore della classe
  #===========================================================
  costructor<-function( parametersFromInput = NA ) {
    MMatrix<<-''
    MMatrix.perc<<-''
    MMatrix.perc.noLoop<<-''      
    footPrint<<-''
    model.grViz<<-'';
    model.XML<<-'';
    is.dataLoaded<<-FALSE
    parameters<<-parameters
    obj.log<<-logHandler();    
    setInstanceClass(className = "alphaAlgorithm")
  }
  #===========================================================
  costructor( parametersFromInput = parameters.list);
  #===========================================================
  return( list(
    "trainModel"=trainModel,
    "getModel"=getModel,
    "loadDataset"=loadDataset,
    "replay"=replay,
    "play"=play,
    "plot"=plot,
    "distanceFrom"=distanceFrom,
    "getLogObj"=getLogObj,
    "setLogObj"=setLogObj    
  ) )
}