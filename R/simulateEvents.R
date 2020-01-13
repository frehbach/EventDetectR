#'Imposes simulated events on the top of the data
#'
#'Simulates Events on columns of a data frame or a matrix by applying different transformations.
#'The events of type sinusoidal, square, binomial or ramp can be used.
#'
#'@param Data Data frame or matrix containing the data to which the events will be introduced
#'@param Params Numeric vector or vector of strings indicating the column names (in case Data is a data frame) or the column
#'numbers (in case Data is a matrix) of the parameters in which an event will be simulated
#'@param Event_type String vector indicating which type of transformation the parameters will undergo.
#'Current valid options include sinusoidal, square, ramp and slowsinusoidal. If Params contains more that one
#'element and Event_type only contains one element the same transformation will be applied to all given Params
#'@param Event_strength (Optional) Numeric Vector indicating the amplitude. Only valid for sinusoidal and square
#'transformations. When specified for other type of transformations it will have no effect. However it must have
#'the same number of elements as Params.
#'@param Start_index Numeric, indicates the index where the event should start
#'@param Event_duration Numeric, indicates the number of steps the transformation should last. Default is 100
#'@param Percentage (Optional) Numeric value from 0 to 1. Alternative input indicating the percentage of data that
#'should be affected by the transformation. Either Event_duration or Percentage should be especified.
#'
#'@return Matrix or data frame containing the selected columns with simulated events
#'
#'@export
#'
#'@import imputeTS
#'
#'@examples
#'
#'#Generate event of type sinusoidal and ramp on two columns of the stationBData data set
#'simupar<-c("B_PH_VAL","B_TEMP_VAL")
#'SimulatedEvents<-simulateEvents(stationBData,
#'                                 simupar,Event_type = c("sinusoidal","ramp"),
#'                                 Start_index = 2500)
#'
#'#When specifiying Event_strength the lenght of the vector needs to match the number
#'#of elements in Params.
#'SimulatedEvents<-simulateEvents(stationBData,
#'                                 simupar,Event_type = c("sinusoidal","ramp"),
#'                                 Start_index = 2500,
#'                                 Percentage = 0.2,
#'                                 Event_strength = c(4,1))
#'
simulateEvents<-function(Data,
                         Params,
                         Event_type,
                         Event_strength = NULL,
                         Start_index = NULL,
                         Event_duration = NULL,
                         Percentage = NULL){
    #Check length(params)=length(Eventtype) or Event_type= 1 element
    if(!length(Params)==length(Event_type)){
        if(length(Event_type)==1){
            Event_type<-rep(Event_type, each=length(Params))}
        else{stop("Params and Event_type should have the same dimension or
                  Event_type need to be a one element character vector")}}
    #X must be of type matrix or dataset
    if(!(is.data.frame(Data)||is.matrix(Data))){
        stop ("X must be of type matix or data frame")
    }else{
        data_extracted<-subset(Data,select = Params)}
    #Check if Event_duration was given if not default to 100
    if(is.null(Event_duration))
    {Event_duration<-100}
    #Check that Start_index is given
    if(is.null(Start_index)){
        Start_index <- 1}
    #Check for start and end index/ check if percentage is given
    if(!is.null(Percentage)){
        if(0<Percentage & Percentage<=1){
            index_length<-round(nrow(Data)*Percentage)
            End_index<-(Start_index+index_length)-1
            if(nrow(Data)<End_index){
                End_index<-nrow(Data)
            }else{End_index<-End_index}
        }else {stop("Percentage should be a value between 0 and 1")}
    }else {End_index<-Start_index+Event_duration}
    #Check if Event_strength is given, if it has correct length or set to default
    if(is.null(Event_strength)){
        Event_strength<-rep(1,length(Params))
    }else{
        if(!length(Event_strength)==length(Params))
        {stop("Event_strength and Params should have the same number of elements")}
    }

    #sigma vector
    sigma_vector<-apply(data_extracted,2,sd, na.rm = TRUE)
    steps<-End_index-Start_index
    #loop counting the position in vector Event type = Parloop
    for(Parloop in 1:length(Params))
    {
        #loop for each sample between Start_index and End_index
        for(iter in 0:steps)
        {
            Emag<-Event_strength[Parloop]
            if(Event_type[Parloop]=="sinusoidal")
            {#sinusoidal case
                data_extracted[Start_index+iter,Parloop]<-data_extracted[Start_index+iter,Parloop]+(Event_strength[Parloop]*sin(pi/2*((2*iter)+1))*sigma_vector[Parloop])
            }else{
                if(Event_type[Parloop]=="square") #This looks more like a saw
                {#square case
                    data_extracted[Start_index+iter,Parloop]<-data_extracted[Start_index+iter,Parloop]+(Event_strength[Parloop]*sigma_vector[Parloop])
                }else {
                    if (Event_type[Parloop]=="ramp")
                    {#ramp case
                        data_extracted[Start_index+iter,Parloop]<-data_extracted[Start_index+iter,Parloop]+((iter/7.0)*sigma_vector[Parloop])
                    }else{
                        if(Event_type[Parloop]=="slowsinusoidal")
                        {#slowsinusoidal
                            t_points<-seq(0,steps)
                            Tao<-steps/2
                            f_sin<-sin(((2*pi)/Tao)*t_points)
                            data_extracted[Start_index+iter,Parloop]<-f_sin[iter+1]*sigma_vector[Parloop]*Event_strength[Parloop]+data_extracted[Start_index,Parloop]
                        }else{
                        #if(Event_type[Parloop]=="binomial")
                        #{#binomial case
                            #data_extracted[Start_index+iter,Parloop]<-data_extracted[Start_index+iter,Parloop]+
                                #(15*sigma_vector[Parloop]*factorial(20)/factorial(20-iter)/factorial(iter)*0.5^2)
                        #}else{
                                stop("Event_type should be type sinusoidal, square, ramp, binomial or slowsinusoidal. Try again with a valid Event_type")

                        }
                    }
                }
            }
        }#for(iter in Start_index:End_index)
    }#Parloop for close
return(data_extracted)
        }#end function
