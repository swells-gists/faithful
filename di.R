#rstudio::viewer("http://localhost:8100")
#setwd('/Users/swells/sandbox/deployr-ui/stage/faithful')
rstudio::viewer("http://localhost:3000")

#Sys.setenv(di = "/Users/swells/viewstore/revo-oss/deployr/deployr-cli/bin/di")
#setwd("/Users/swells/viewstore/revo-oss/deployr/deployr-cli/bin/")

diPath <- "/Users/swells/viewstore/revo-oss/deployr/deployr-cli/bin/di";

envORIG<-function(argv) {
  shell<-paste(c("deployr --no-colors env"), collapse=" ")
  echo(system(shell))
}
env<-function(argv) {
  shell<-paste(c("di --no-colors &"), collapse=" ")
  system(shell)
}


about<-function(argv) {
  shell<-paste(c("/Users/swells/viewstore/revo-oss/deployr/deployr-cli/bin/di about --no-color"), collapse=" ")
  system(shell)
}

dihelp<-function(argv) {
  print('help')
  shell<-paste(c("/Users/swells/viewstore/revo-oss/deployr/deployr-cli/bin/di --no-color help"), collapse=" ")
  system(shell)
}


env<-function(argv) {
  shell<-paste(c("/Users/swells/viewstore/revo-oss/deployr/deployr-cli/bin/di --no-colors"), collapse=" ")
  system(shell)
}

appPush<-function(argv="-rsp") {
  shell<-paste(c(diPath, "app --no-colors push", argv), collapse=" ")
  #print(shell)
  system(shell)  
}

#code<-function(filename, argv, outputs, ...) {
codeORIG<-function(filename, ...) {
  filename<-paste(c(getwd(), filename), collapse="/")
  #shell<-paste(c("/Users/swells/viewstore/revo-oss/deployr/deployr-cli/bin/di --no-colors code", filename, argv=""), collapse=" ")
  #system(shell) 
  run(filename, ...)
}

#run<-function(filename, argv, inputs, ...) {
#run<-function(filename, inames, ...) {
script<-function(filename, ...) {
  arg <- deparse(substitute(filename))
  dots <- substitute(list(...))[-1]
  c(arg, sapply(dots, deparse))
  inames<-sapply(dots, deparse)
  inargs <- list(...)
  
  if(length(inargs)) {
    inputs<-""
    for(i in 1:length(inargs)) {      
      name<-inames[i] #reflection(inames[i])
      
      #print(paste("name  --->: ", name))
      #print(paste("value --->", inargs[[i]] ))
      
      if (!is.null(name)) {
        value = inargs[[i]];        
        encoded<-doTopLevelJson(value, name) 
        #print(encoded)
        
        if (length(inargs) > 1) {
          len<-nchar(encoded)
          inputs<-paste(inputs, substr(encoded, 1, len), sep = ",") 
        } else {
          inputs<-encoded
        }
      } else {
        print(paste("No name provided for input value '", inargs[[i]], "' ignoring...."))
      }
    }
    if (length(inargs) > 1) {
      inputs<-substr(inputs, 2, nchar(inputs))
    }
  }
  
  #print(inputs)
  
  if (!is.null(inputs)) {
    inputs<-paste("'", inputs, "'")
    argv<-""
    argv<- paste(argv, '-i', inputs, sep=" ")
  }
  
  if (!is.null(outputs)) {
    #argv<-paste(argv, "-o", outputs, sep=" ")
  }
  
  shell<-paste(c("deployr --no-colors run", filename, argv), collapse=" ")
  print(shell)
  #system(shell)
}

code<-function(filename, ...) {
  filename<-paste(c(getwd(), filename), collapse="/")
  arg <- deparse(substitute(filename))
  dots <- substitute(list(...))[-1]
  c(arg, sapply(dots, deparse))
  inames<-sapply(dots, deparse)
  inargs <- list(...)
  
  if(length(inargs)) {
    inputs<-""
    for(i in 1:length(inargs)) {      
      name<-inames[i] #reflection(inames[i])
      
      #print(paste("name  --->: ", name))
      #print(paste("value --->", inargs[[i]] ))
      
      if (!is.null(name)) {
        value = inargs[[i]];        
        encoded<-doTopLevelJson(value, name) 
        #print(encoded)
        
        if (length(inargs) > 1) {
          len<-nchar(encoded)
          inputs<-paste(inputs, substr(encoded, 1, len), sep = ",") 
        } else {
          inputs<-encoded
        }
      } else {
        print(paste("No name provided for input value '", inargs[[i]], "' ignoring...."))
      }
    }
    if (length(inargs) > 1) {
      inputs<-substr(inputs, 2, nchar(inputs))
    }
  }
  
  #print(inputs)
  
  if (!is.null(inputs)) {
    inputs<-paste("'", inputs, "'")
    argv<-""
    argv<- paste(argv, '-i', inputs, sep=" ")
  }
  
  #if (!is.null(outputs)) {
  #argv<-paste(argv, "-o", outputs, sep=" ")
  #}
  
  #shell<-paste(c("di --no-colors run", filename, argv), collapse=" ")
  type<-'script';
  isScript<-FALSE
  if (!isScript) {
    type<-'code'
  }
  
  shell<-paste(c(diPath, "--no-colors", type, filename, argv), collapse=" ")
  #print(shell)
  system(shell)
}

runORIG<-function(filename, argv, outputs, ...) {
  inargs <- list(...)
  if(length(inargs)) {
    inputs<-""
    for(i in 1:length(inargs)) {      
      name<-names(inargs[[i]])
      if (!is.null(name)) {
        value = inargs[[i]];
        encoded<-doTopLevelJson(value, name)        
        len<-nchar(encoded)
        inputs<-paste(inputs, substr(encoded, 1, len), sep = ",")        
      } else {
        print(paste("No name provided for input value '", inargs[[i]], " ignoring...."))
      }
    }
    inputs<-substr(inputs, 2, nchar(inputs))
  }
  
  #print(inputs)
  
  if (!is.null(inputs)) {
    inputs<-paste("'", inputs, "'")
    argv<- paste(argv, '-i', inputs, sep=" ")
  }
  
  if (!is.null(outputs)) {
    argv<-paste(argv, "-o", outputs, sep=" ")
  }
  
  shell<-paste(c("deployr --no-colors run", filename, argv), collapse=" ")
  #print(shell)
  system(shell)
  
  if (TRUE) {
    fileName<-"/Users/swells/sandbox/meteor/leaderboard/foobar.txt"
    conn<-file(fileName, open="r")
    linn<-readLines(conn)
    for (i in 1:length(linn)){
      eval(parse(text=linn[i]))
    }
    close(conn)
  }
}

push<-function(filename, dest, argv='-p') {
  shell<-paste(c("deployr --no-colors push", filename, dest, argv), collapse=" ")
  system(shell)  
}


#deployr<-list(env=env, run=run, push=push, apps=apps)
di<-list(about=about, help=dihelp, env=env, script=script, code=code, push=push, appPush=appPush)

###########################################################




getDeployrJsonPrimitive<-function(x) {
  c<-class(x);
  clss<-c[1]; 
  if(clss == 'POSIXct')return(class(x)[1]);
  if(clss == 'POSIXlt')return(class(x)[1]);
  if(clss == 'ordered')return(class(x)[1]);
  if(clss == 'Date')return('Date');
  if(is.data.frame(x))return('data.frame');
  if(is.list(x))return('list');
  if(is.matrix(x))return('matrix');
  if(is.factor(x))return('factor');
  if(is.logical(x))return('logical');
  if(is.character(x))return('character');
  if(is.integer(x))return('integer');
  if(is.double(x))return('numeric');
  return('unknown');
}

getVectorJson <- function(name,x,doName=FALSE) { 
  if(doName) {
    return( paste("{'name'",":'",name,"',","'type':'vector','value':[",paste(x, collapse = ','),"]}",sep="") );
  } else {
    return( paste("{'",name,"'",":{'type':'vector','value':[",paste(x, collapse = ','),"]}}",sep="") );
  }
}

getCharacterVectorJson <- function(name,x,doName=FALSE) {
  if(doName) {
    return( paste("{'name'",":'",name,"',","'type':'vector','value':[",paste(shQuote(x), collapse = ','),"]}",sep="") );
  } else {
    return( paste("{'",name,"'",":{'type':'vector','value':[",paste(shQuote(x), collapse=","),"]}}",sep="") ); 
  }    
}

doDataFrameJson <- function(x,name,doName=FALSE) {
  results = "";
  n <- names(x);
  if(doName) {
    results <- paste("{'name'",":'",name,"','type':'dataframe','value':[",sep="");
  } else {
    results <- paste("{'",name,"':{'type':'dataframe','value':[",sep="");
  }
  len <- length(n);
  for(i in 1:len  ) {
    value = x[[i]];
    rclass2 <- getDeployrJsonPrimitive(value);
    vectorValue <- "";
    if(rclass2 == "character") {
      vectorValue <- getCharacterVectorJson(n[i],value,TRUE);
    }
    else if(rclass2 == "numeric" || rclass2 == "integer") {
      vectorValue <- getVectorJson(n[i],value,TRUE);
    }
    if( i > 1 ) {
      results <- paste(results,",",sep="")
    }
    results <- paste(results,vectorValue,sep="")
  }
  if(doName) {
    results <- paste(results,"]}",sep="")
  } else {
    results <- paste(results,"]}}",sep="")
  }
  return( results );
}

doListJson <- function(x,name,doName=FALSE) {
  if(doName) {
    results <- paste("{'name'",":'",name,"','type':'list','value':[",sep="");
  } else {
    results <- paste("{'",name,"':{'type':'list','value':[",sep="");
  }
  len <- length(x);
  for(i in 1:len  ) {
    rObject = x[[i]];
    vectorValue <- doTopLevelJson(rObject,as.character(i),TRUE);
    if( i > 1 ) {
      results <- paste(results,",",sep="")
    }
    results <- paste(results,vectorValue,sep="")
  }
  if(doName) {
    results <- paste(results,"]}",sep="")
  } else {
    results <- paste(results,"]}}",sep="")
  }
  return( results );
}

doNumericIntegerJson<-function(x,name,doName=FALSE) {
  results = "";
  if(length(x) == 1) {
    if(doName) {
      #results <- paste("{'name'",":'",name,"','type':'primitive','value':",x,"}",sep="");
      #results <- paste('{"name"', ':"', name, '","type":"primitive","value":', x, "}",sep="");
      results <- paste('{"name"', ':"', name, '","type":"numeric","value":', x, "}",sep="");
    } else {
      #results <- paste("{'",name,"':{'type':'primitive','value':",x,"}}",sep="");
      #results <- paste('{"', name, '":{"type": "primitive", "value":', x, "}}", sep="");
      results <- paste('{"', name, '":{"type": "numeric", "value":', x, "}}", sep="");
    }
  } else {
    results <- getVectorJson(name,x,doName);
  }
  return(results)
}

doCharacterJson<-function(x,name,doName=FALSE) {
  results = "";
  if(length(x) == 1) {
    if(doName) {
      #results<-paste("{'name'",":'",name,"','type':'primitive','value':'",x,"'}",sep="");
      results<-paste('{"name"', ':"', name, '", "type": "primitive", "value": "', x, '"}', sep="");
    } else {
      #results<-paste("{'",name,"':{'type':'primitive','value':'",x,"'}}",sep="");
      results<-paste('{"', name, '":{"type":"primitive","value":"', x, '"}}', sep="");
    }
  } else {
    results <- getCharacterVectorJson(name,x,doName);
  }
  return(results)
}

doLogicalJson<-function(x,name,doName=FALSE) {
  results = "";
  if(length(x) == 1) {
    if (x) {
      x<-"true"
    } else {
      x<-"false"
    }
    
    if(doName) {
      #results <- paste("{'name'",":'",name,"','type':'primitive','value':",x,"}",sep="");
      #results <- paste('{"name"',':"', name,'", "type": "primitive", "value":', x, '}', sep="");
      esults <- paste('{"name"',':"', name,'", "type": "logical", "value":', x, '}', sep="");
    } else {
      #results <- paste("{'",name,"':{'type':'primitive','value':",x,"}}",sep="");
      #results <- paste('{"',name,'":{"type":"primitive","value":',x,'}}', sep="");
      results <- paste('{"',name,'":{"type":"logical","value":',x,'}}', sep="");
    }
  } else {
    results <- getVectorJson(name,x,doName);
  }
  return(results)
}

doFactorJson<-function(x,name,doName=FALSE) {
  ordered <- FALSE;
  if(is.ordered(x)) {
    ordered <- TRUE
  }
  if(doName) {
    results <- paste("{'name'",":'",name,"','type':'factor','ordered':",ordered,",'value':[",sep="");
  } else {
    results <- paste("{'",name,"':{'type':'factor','ordered':",ordered,",'value':[",sep="");
  }
  values <- paste(x,collapse=",");
  results = paste(results, values, "],'labels':[", sep="");
  levels <- paste(levels(x),collapse=",");
  if(doName) {
    results = paste(results, levels, "]}", sep="");
  } else {
    results = paste(results, levels, "]}}", sep="");
  }
  return( results );
  return( results );
}

doMatrixJson<-function(x,name,doName=FALSE) {
  if(doName) {
    results <- paste("{'name'",":'",name,"','type':'matrix','value':[[",sep="");
  } else {
    results <- paste("{'",name,"':{'type':'matrix','value':[[",sep="");
  }
  nrows <- nrow(x);
  for(i in 1:nrows) {
    subResults <- paste(mdat[i,],collapse=",");
    if( i != nrows ) {
      results <- paste(results, subResults, "],[",sep="");
    } else {
      results <- paste(results, subResults, "]",sep="");
    }
  }
  if(doName) {
    results <- paste(results, "]}",sep="");
  } else {
    results <- paste(results, "]}}",sep="");
  }
  return( results );
}

doDateJson <- function(x,name,doName=FALSE) {
  if(doName) {
    return( paste("{'name':'",name,"','type':'date','format':'yyyy-MM-dd','value':'",x,"'}",sep="") );
  } else {
    return( paste("{'",name,"':{'type':'date','format':'yyyy-MM-dd','value':'",x,"'}}",sep="") );
  }
}

doPosixJson <- function(x,name,doName=FALSE) {
  if(doName) {
    return( paste("{'name':'",name,"','type':'date','format':'yyyy-MM-dd HH:mm:ss Z','value':'",strftime(x,usetz=TRUE),"'}",sep="") );
  } else {
    return( paste("{'",name,"':{'type':'date','format':'yyyy-MM-dd HH:mm:ss Z','value':'",strftime(x,usetz=TRUE),"'}}",sep="") );
  }
}

doTopLevelJson <- function(x,name,doName=FALSE) {
  results = "";
  rclass <- getDeployrJsonPrimitive(x);
  if(rclass == "numeric" || rclass == "integer") {
    results <- doNumericIntegerJson(x,name,doName);
  } else if(rclass == "character") {
    results <- doCharacterJson(x,name,doName)
  } else if(rclass == "data.frame") {
    results <- doDataFrameJson(x,name,doName)
  } else if(rclass == "list") {
    results <- doListJson(x,name,doName)
  } else if(rclass == "factor" || rclass == "ordered") {
    results <- doFactorJson(x,name,doName)
  } else if(rclass == "matrix") {
    results <- doMatrixJson(x,name,doName)
  } else if(rclass == "logical") {
    results <- doLogicalJson(x,name,doName)
  } else if(rclass == "Date") {
    results <- doDateJson(x,name,doName)
  } else if(rclass == "POSIXct") {
    results <- doPosixJson(x,name,doName)
  }
  return( results );
}
