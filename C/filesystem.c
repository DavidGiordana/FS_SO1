#include "filesystem.h"

/**
* @author David Giordana
* @author Gabriel Antelo
* @author Maximiliano Ibalborde
*/

//mensajeros de worker
mqd_t workerMessenger[NUM_WORKERS];
mq_at attrWorker[NUM_WORKERS];

//mensajeros de clientes
mqd_t clientList[MAX_CLIENTS];
mq_at attrClient;

//Variables de proteccion de memoria
pthread_mutex_t mutex_worker = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t mutex_client = PTHREAD_MUTEX_INITIALIZER;
sem_t semaph_worker[5];
int waitingplus[4];

//Lleva el conteo de los clientes
int clientId = 0;

/**
* Lanza el programa
**/
int main(void){
    int i;
    //Desenlaza las mqueues de clientes en caso de no ser la primera ejecucion
    char temp[4];
    for (i = 0; i < MAX_CLIENTS; i++){
        sprintf(temp, "/c%d", i);
        mq_unlink(temp);
    }

    //Crea los workers
    int *n;
    pthread_t workerThreads[NUM_WORKERS];
    for(i = 0 ; i < NUM_WORKERS ; i++){
        //Setea los atributos del mensajero
        attrWorker[i].mq_flags   = 0;
    	attrWorker[i].mq_maxmsg  = 10;
    	attrWorker[i].mq_msgsize = BUFF_SIZE;
    	attrWorker[i].mq_curmsgs = 0;
        //Crea el mensajero de worker
        mq_unlink(WORKER_ID(i));
        workerMessenger[i] = mq_open(WORKER_ID(i), O_RDWR | O_CREAT | O_EXCL, S_IRWXU | S_IRWXG, &attrWorker[i]);

        //inicializa el semaforo
        sem_init(&semaph_worker[i], 0, 0);
        waitingplus[i] = 0;

        //Crea el hilo del worker
        n = (int*) malloc(sizeof(int));
        *n = i;
        pthread_create(&workerThreads[i], NULL, worker, n);
    }

    //Setea los atributos de los mensajeros de cliente
    attrClient.mq_flags   = 0;
    attrClient.mq_maxmsg  = 1;
    attrClient.mq_msgsize = BUFF_SIZE;
    attrClient.mq_curmsgs = 0;

    //Llama al dispatcher de clientes
    dispatcher();

    pthread_join(workerThreads[0], NULL);
	return 0;
}

/**
* Recibe las conexiones de los clientes y lanza hilos para atenderlos
*/
void dispatcher(){
    //Variables del servidor
    int list_s;
    int conn_s = -1;
    struct sockaddr_in servaddr;
    //Intenta inicializar el servidor
    if ( (list_s = socket(AF_INET, SOCK_STREAM, 0)) < 0 ) {
        fprintf(stderr, "FILESERV: Error creating listening socket.\n");
    }
    memset(&servaddr, 0, sizeof(servaddr));
    servaddr.sin_family      = AF_INET;
    servaddr.sin_addr.s_addr = htonl(INADDR_ANY);
    servaddr.sin_port        = htons(PORT);
    if (setsockopt(list_s, SOL_SOCKET, SO_REUSEADDR, &(int){ 1 }, sizeof(int)) < 0)
        fprintf(stderr, "setsockopt(SO_REUSEADDR) failed\n");
    if ( bind(list_s, (struct sockaddr *) &servaddr, sizeof(servaddr)) < 0 ) {
        fprintf(stderr, "FILESERV: Error calling bind()\n");
    }
    if ( listen(list_s, 10) < 0 ) {
        fprintf(stderr, "FILESERV: Error calling listen()\n");
    }

    int clientCounter = 0;
    while (1) {
        //Error en la conexion
        if ( (conn_s = accept(list_s, NULL, NULL) ) < 0 ) {
            fprintf(stderr, "FILESERV: Error calling accept()\n");
            continue;
        }
        pthread_t clientThread;
        //Empaca la informacion que requiere el hilo cliente
        int* data = (int*)malloc(sizeof(int) * 2);
        data[0] = conn_s;
        data[1] = clientCounter++;
        //Lanza el cliente
        pthread_create(&clientThread, NULL, client, data);
    }
}

/**
* Atiende las solicitudes de los clientes
**/
void* client(void* arg){
    //Desempaca la información
    int* data = (int*)arg;
    int sockDescriptor = data[0];
    int clientID = data[1];
    free(arg);

    //Asigna el worker
    int worker = getWorker();
    DEBUG("Se creo el cliente con el id:\"%d\"\n",clientID);
    DEBUG("Worker %d asignado al cliente %d\n", worker, clientID);

    //Crea las variables para trabajar
    char entrada[BUFF_SIZE];
    char salida[BUFF_SIZE];
    char msgcontent[BUFF_SIZE];
    int res;
    int conn = 0; //Indica si se estableció conexion

    //Espera a que se loguee el usuario
    do{
        memset(entrada , '\0' , BUFF_SIZE);
        res = read(sockDescriptor, entrada, BUFF_SIZE);

        //Error de conexion
        if (res < 0) {
            close(sockDescriptor);
            return NULL;
        }

        //Intenta conectar
        Parser_Package *temp = clientRequestParser(entrada);
        if (temp -> com == CON){
            conn = 1;
            sprintf(salida, "OK %d %s", clientID , "\n\0");
            write(sockDescriptor, salida, strlen(salida));
        }else{
            sprintf(salida, "%s", "ERROR: Aun no se ha conectado\n\0");
            write(sockDescriptor, salida, strlen(salida));
        }
        freeParser_Package(temp);
    }while(conn == 0);

    //Crea el mensajero de cliente
    char desc[3];
    sprintf(desc, "/c%d", clientID);
    clientList[clientID] = mq_open(desc, O_RDWR | O_CREAT | O_EXCL, S_IRWXU | S_IRWXG, &attrClient);

    //Recibe, procesa y responde las solicitudes
    while(conn != 0) {
        memset(entrada , '\0' , BUFF_SIZE);
        memset(msgcontent , '\0' , BUFF_SIZE);
        memset(salida , '\0' , BUFF_SIZE);

        //Lee mensaje de entrada
        res = read(sockDescriptor, entrada, BUFF_SIZE);
        if (res < 0) {
            close(sockDescriptor);
            break;
        }

        //Envia mensaje a worker y se bloquea
        sprintf(msgcontent, "%d %s", clientID, entrada);
        mq_send(workerMessenger[worker], msgcontent, strlen(msgcontent), PRIOCLIENT);
        sem_post(&semaph_worker[worker]);

        //Recibe mensaje de worker
        DEBUG("Nuevo cliente id: %d\n", clientID);
        memset(entrada , '\0' , BUFF_SIZE);
        mq_receive(clientList[clientID], entrada, BUFF_SIZE, NULL);
        //Responde al cliente o se cierra segun corresponda
        if(!strncmp(entrada , "BYE" , 3)){
            sprintf(salida, "OK \n");
            conn = 0;
        }
        else{
            sprintf(salida, "%s\n", entrada);
        }
        write(sockDescriptor, salida, strlen(salida));
    }
    close(sockDescriptor);
    return NULL;
}

/**
* WORKER
**/
void *worker(void *arg){
    //Archivero
    FileS fileList[MAX_FILES];
    int cantFiles = 0;

    //Informacion para comunicaciones
    int workerId = *(int*)arg;
    free(arg);
    int msgcounter = 0;
    int safe = 0; //Bandera que impide solicitar más de una vez información a un worker

    //Solo se usa a la hora de recibir mensajes de un worker o cliente
    char *msgcontent = malloc(BUFF_SIZE * sizeof(char));

    //Variable usada para la splitNS
    char message[BUFF_SIZE];

    //contiene el id del cliente a responder la solicitud
    int clientId;

    //Informacion parseada para procesar
    Parser_Package *parsedInfo = NULL;

    //Buffer para responder al cliente
    char salida[BUFF_SIZE];

    //Contiene el id del proximo worker
    int nextWorker = -1;

    //Cache usada para el procesamiento (durante las iteraciones)
    char cache[BUFF_SIZE];

    //buffer temporal para lectura y escritura de archivos
    char tempbuf[BUFF_SIZE];

    int aux = -1; //Variable auxiliar

    while(1){
        if(!availableMessages(workerId) && parsedInfo == NULL && !waitingplus[workerId]){

            PRINT("El Worker %d se bloqueó\n", workerId);

            pthread_mutex_lock(&mutex_worker);
            waitingplus[workerId] = 1;
            pthread_mutex_unlock(&mutex_worker);

            sem_wait(&semaph_worker[workerId]);

            PRINT("El Worker %d se desbloqueó\n", workerId);
        }

        if (parsedInfo != NULL){
            switch (parsedInfo -> com) {
                case LSD:
                    if (msgcounter == 0){
                        filelistToString(cache, fileList, cantFiles);
                        msgcounter++;
                    }
                    else if (msgcounter == 5){
                        sprintf(salida, "OK %s", cache);
                        mq_send(clientList[clientId], salida, strlen(salida) + 1, 0);
                        clearVar(&msgcounter, &parsedInfo, &safe, salida, cache);
                    }
                    else if (safe == 0){
                        sprintf(salida, "%d %d %s", workerId, GIVE_FILE_REQ, parsedInfo->str);

                        nextWorker = nextWorkerToSend(workerId, msgcounter);

                        pthread_mutex_lock(&mutex_worker);
                        waitingplus[nextWorker] = 0;
                        pthread_mutex_unlock(&mutex_worker);

                        PRINT("Desbloqueo del worker %d\n", nextWorker);

                        mq_send(workerMessenger[nextWorker], salida, strlen(salida), PRIOWORKER);
                        sem_post(&semaph_worker[nextWorker]);
                        safe = 1;
                    }
                    break;
                case DEL:
                    if (msgcounter == 0){
                        aux = deleteFile(fileList, parsedInfo->str, cantFiles);
                        sprintf(cache, "%d", aux);
                        if(aux != -1)
                            msgcounter = 5;
                        else{
                            msgcounter++;
                        }
                    }
                    else if (msgcounter == 5){
                            aux = atoi(cache);
                            if(aux == 1){
                                sprintf(salida, "OK");
                            }
                            else if (aux == -1){
                                sprintf(salida, "ERROR: El archivo \"%s\" no existe", parsedInfo->str);
                            }
                            else if (aux == 2){
                                sprintf(salida, "ERROR - El archivo está en uso");
                            }
                            mq_send(clientList[clientId], salida, strlen(salida) + 1, 0);
                            clearVar(&msgcounter, &parsedInfo, &safe, salida, cache);
                        }
                    else if(safe == 0){
                        sprintf(salida, "%d %d %s", workerId, DELETE_FILE_REQ, parsedInfo->str);

                        nextWorker = nextWorkerToSend(workerId, msgcounter);

                        pthread_mutex_lock(&mutex_worker);
                        waitingplus[nextWorker] = 0;
                        pthread_mutex_unlock(&mutex_worker);

                        PRINT("Desbloqueo del worker %d\n", nextWorker);

                        mq_send(workerMessenger[nextWorker], salida, strlen(salida), PRIOWORKER);
                        sem_post(&semaph_worker[nextWorker]);
                        safe = 1;
                    }
                    break;
                case CRE:
                    if (msgcounter == 0){
                        if(hasTheFile(fileList, parsedInfo->str, cantFiles)){
                            sprintf(salida, "ERROR: El archivo \"%s\" ya existe", parsedInfo->str);
                            mq_send(clientList[clientId], salida, strlen(salida) + 1, 0);
                            clearVar(&msgcounter, &parsedInfo, &safe, salida, cache);
                        }
                        else{
                            msgcounter++;
                        }
                    }
                    else if (msgcounter == 5){
                        if (atoi(cache) == 0){
                            FileS *temp = createFile(parsedInfo->str);
                            fileList[cantFiles] = *temp;
                            cantFiles++;
                            sprintf(salida, "OK");
                        }
                        else{
                            sprintf(salida, "ERROR: El archivo \"%s\" ya existe", parsedInfo->str);
                        }
                        waitingplus[workerId] = 0;
                        mq_send(clientList[clientId], salida, strlen(salida) + 1, 0);
                        clearVar(&msgcounter, &parsedInfo, &safe, salida, cache);
                    }

                    else if(safe == 0){
                        sprintf(salida, "%d %d %s", workerId, HAS_FILE_REQ, parsedInfo->str);
                        nextWorker = nextWorkerToSend(workerId, msgcounter);

                        pthread_mutex_lock(&mutex_worker);
                        waitingplus[nextWorker] = 0;
                        pthread_mutex_unlock(&mutex_worker);

                        PRINT("Desbloqueo del worker %d\n", nextWorker);

                        mq_send(workerMessenger[nextWorker], salida, strlen(salida), PRIOWORKER);
                        sem_post(&semaph_worker[nextWorker]);

                        safe = 1;
                    }
                    break;
                case OPN:
                    if (msgcounter == 0){
                        if(hasTheFile(fileList, parsedInfo->str, cantFiles)){
                            aux = openFile(fileList, parsedInfo->str, clientId, cantFiles);
                            sprintf(cache, "%d", aux);
                            msgcounter = 5;
                        }
                        else{
                            msgcounter++;
                        }
                    }
                    else if (msgcounter == 5){
                            aux = atoi(cache);
                            if(aux == 1){
                                sprintf(salida, "ERROR: El archivo está en uso");
                            }
                            else if (aux == 0){
                                sprintf(salida, "OK FD %d", lastFD);
                            }
                            else{
                                sprintf(salida, "ERROR: El archivo \"%s\" no existe", parsedInfo->str);
                            }
                            mq_send(clientList[clientId], salida, strlen(salida) + 1, 0);
                            clearVar(&msgcounter, &parsedInfo, &safe, salida, cache);
                        }
                    else if(safe == 0){
                        sprintf(salida, "%d %d %d %s", workerId, OPEN_FILE_REQ, clientId, parsedInfo->str);

                        nextWorker = nextWorkerToSend(workerId, msgcounter);

                        pthread_mutex_lock(&mutex_worker);
                        waitingplus[nextWorker] = 0;
                        pthread_mutex_unlock(&mutex_worker);

                        PRINT("Desbloqueo del worker %d\n", nextWorker);

                        mq_send(workerMessenger[nextWorker], salida, strlen(salida), PRIOWORKER);
                        sem_post(&semaph_worker[nextWorker]);
                        safe = 1;
                    }
                    break;
                case WRT:
                    if (msgcounter == 0){
                        aux = writeFile(fileList, parsedInfo->fd, clientId, cantFiles, parsedInfo->size, parsedInfo->str);
                        sprintf(cache, "%d", aux);
                        if(aux != -1)
                            msgcounter = 5;
                        else{
                            msgcounter++;
                        }
                    }
                    else if (msgcounter == 5){
                            aux = atoi(cache);
                            if(aux == 1){
                                sprintf(salida, "OK");
                            }
                            else if (aux == -1){
                                sprintf(salida, "ERROR: El archivo \"%s\" no existe", parsedInfo->str);
                            }
                            else if (aux == 2){
                                sprintf(salida, "ERROR: Espacio insuficiente");
                            }
                            else if (aux == 3){
                                sprintf(salida, "ERROR: El archivo está en uso");
                            }
                            mq_send(clientList[clientId], salida, strlen(salida) + 1, 0);
                            clearVar(&msgcounter, &parsedInfo, &safe, salida, cache);
                        }
                    else if(safe == 0){
                        sprintf(salida, "%d %d %d %d %d %s", workerId, WRITE_FILE_REQ, parsedInfo->fd, clientId, parsedInfo->size, parsedInfo->str);

                        nextWorker = nextWorkerToSend(workerId, msgcounter);

                        pthread_mutex_lock(&mutex_worker);
                        waitingplus[nextWorker] = 0;
                        pthread_mutex_unlock(&mutex_worker);

                        PRINT("Desbloqueo del worker %d\n", nextWorker);

                        mq_send(workerMessenger[nextWorker], salida, strlen(salida), PRIOWORKER);
                        sem_post(&semaph_worker[nextWorker]);
                        safe = 1;
                    }
                    break;
                case REA:
                    if (msgcounter == 0){
                        memset(tempbuf, '\0', BUFF_SIZE);
                        aux = readFile(fileList, parsedInfo->fd, clientId, cantFiles, parsedInfo->size, tempbuf);
                        sprintf(cache, "%d", aux);
                        if(aux != -1){
                            msgcounter = 5;
                        }
                        else{
                            msgcounter++;
                        }
                    }
                    else if (msgcounter == 5){
                            aux = atoi(cache);
                            if(aux == 1){
                                sprintf(salida, "OK SIZE %d %s", strlen(tempbuf), tempbuf);
                            }
                            else if (aux == -1){
                                sprintf(salida, "ERROR: El archivo \"%s\" no existe", parsedInfo->str);
                            }
                            else if (aux == 2){
                                sprintf(salida, "ERROR: Ha excedido la informacion a leer");
                            }
                            else if (aux == 3){
                                sprintf(salida, "ERROR: El archivo está en uso");
                            }
                            mq_send(clientList[clientId], salida, strlen(salida) + 1, 0);
                            clearVar(&msgcounter, &parsedInfo, &safe, salida, cache);
                        }
                    else if(safe == 0){
                        sprintf(salida, "%d %d %d %d %d", workerId, READ_FILE_REQ, parsedInfo->fd, clientId, parsedInfo->size);

                        nextWorker = nextWorkerToSend(workerId, msgcounter);

                        pthread_mutex_lock(&mutex_worker);
                        waitingplus[nextWorker] = 0;
                        pthread_mutex_unlock(&mutex_worker);

                        PRINT("Desbloqueo del worker %d\n", nextWorker);

                        mq_send(workerMessenger[nextWorker], salida, strlen(salida), PRIOWORKER);
                        sem_post(&semaph_worker[nextWorker]);
                        safe = 1;
                    }
                    break;
                case CLO:
                    if (msgcounter == 0){
                        aux = closeFile(fileList, parsedInfo->fd, clientId, cantFiles);
                        sprintf(cache, "%d", aux);
                        if(aux != -1)
                            msgcounter = 5;
                        else{
                            msgcounter++;
                        }
                    }
                    else if (msgcounter == 5){
                            aux = atoi(cache);
                            if(aux == 1){
                                sprintf(salida, "OK");
                            }
                            else if (aux == -1){
                                sprintf(salida, "Es descriptor no corresponde a un archivo abierto");
                            }
                            else if (aux == 2){
                                sprintf(salida, "ERROR: El archivo está en uso");
                            }
                            mq_send(clientList[clientId], salida, strlen(salida) + 1, 0);
                            clearVar(&msgcounter, &parsedInfo, &safe, salida, cache);
                        }
                    else if(safe == 0){
                        sprintf(salida, "%d %d %d %d", workerId, CLOSE_FILE_REQ, clientId, parsedInfo->fd);

                        nextWorker = nextWorkerToSend(workerId, msgcounter);

                        pthread_mutex_lock(&mutex_worker);
                        waitingplus[nextWorker] = 0;
                        pthread_mutex_unlock(&mutex_worker);

                        PRINT("Desbloqueo del worker %d\n", nextWorker);

                        mq_send(workerMessenger[nextWorker], salida, strlen(salida), PRIOWORKER);
                        sem_post(&semaph_worker[nextWorker]);
                        safe = 1;
                    }
                    break;
                case BYE:
                    if (msgcounter == 0){
                        closeAllFilesOf(fileList, cantFiles, clientId);
                        msgcounter++;
                    }
                    else if (msgcounter == 5){
                        sprintf(salida, "BYE");
                        mq_send(clientList[clientId], salida, strlen(salida) + 1, 0);
                        clearVar(&msgcounter, &parsedInfo, &safe, salida, cache);
                    }
                    else if (safe == 0){
                        sprintf(salida, "%d %d %d", workerId, CLOSE_ALL_REQ, clientId);

                        nextWorker = nextWorkerToSend(workerId, msgcounter);

                        pthread_mutex_lock(&mutex_worker);
                        waitingplus[nextWorker] = 0;
                        pthread_mutex_unlock(&mutex_worker);

                        PRINT("Desbloqueo del worker %d\n", nextWorker);

                        mq_send(workerMessenger[nextWorker], salida, strlen(salida), PRIOWORKER);
                        sem_post(&semaph_worker[nextWorker]);
                        safe = 1;
                    }
                    break;
                default:
                    sprintf(salida, "ERROR: Parse Error");
                    mq_send(clientList[clientId], salida, strlen(salida) + 1, 0);
                    clearVar(&msgcounter, &parsedInfo, &safe, salida, cache);
                    break;
            }
        }
        //Lee Solicitud de cliente en caso de ser necesario
        else if(parsedInfo == NULL){
            msgcontent = getClientMessage(workerId);
            if(msgcontent != NULL){
                DEBUG("Worker %d parsea el mensaje del cliente \"%s\"\n", workerId, msgcontent);
                splitNS(&clientId, message, msgcontent);
                parsedInfo = clientRequestParser(message);
                printParser_Package(*parsedInfo);
            }
        }
        //Lee mensaje de worker en caso de ser necesario
        msgcontent = getWorkerMessage(workerId);
        if (msgcontent != NULL){
            /*
            Estructura de un mensaje entre worker
            "workerId workerRequest datos"
            */
            int tempWorkerId; //Identificador del worker que envio el mensaje
            int workReq; //Identificador interno de requerimiento
            int tempClientId; //Identificador del cliente que generó la solicitud
            char temporalData[BUFF_SIZE]; //Caché para responder a worker o almacena el resto del mensaje
            int temporalFileDescriptor; //Descriptor de archivo
            int bufSize; //Tamaño del buffer pasado
            int k; //Variable auxiliar
            char str[BUFF_SIZE]; //Buffer temporal

            char trash[BUFF_SIZE]; //Variable para almacenar la basura

            DEBUG("Worker %d recibe el mensaje de otro worker \"%s\"\n", workerId, msgcontent);

            splitNS(&tempWorkerId, message, msgcontent);
            splitNS(&workReq, temporalData, message);

            if(workReq == OPEN_FILE_REQ){
                splitNS(&tempClientId, temporalData, temporalData);
            }

            switch(workReq){
                case 0:
                    DEBUG("Error: Codigo Interno de mensaje de worker inválido\n");
                    break;
                case HAS_FILE_REQ:
                    sprintf(temporalData, "%d %d %d", workerId, HAS_FILE_RES, hasTheFile(fileList, temporalData, cantFiles));
                    mq_send(workerMessenger[tempWorkerId], temporalData, strlen(temporalData) + 1, PRIOWORKER);
                    memset(temporalData, '\0', BUFF_SIZE);
                    break;
                case HAS_FILE_RES:
                    safe = 0;
                    msgcounter++;
                    k = strlen(cache);
                    sprintf(cache + k, "%d", atoi(temporalData));
                    memset(temporalData, '\0', BUFF_SIZE);
                    break;
                case GIVE_FILE_REQ:
                    memset(str, '\0', BUFF_SIZE);
                    filelistToString(str, fileList, cantFiles);
                    sprintf(temporalData, "%d %d %s", workerId, GIVE_FILE_RES, str);
                    mq_send(workerMessenger[tempWorkerId], temporalData, strlen(temporalData) + 1, PRIOWORKER);
                    memset(temporalData, '\0', BUFF_SIZE);
                    break;
                case GIVE_FILE_RES:
                    safe = 0;
                    msgcounter++;
                    k = strlen(cache);
                    sprintf(cache + k, "%s", temporalData);
                    memset(temporalData, '\0', BUFF_SIZE);
                    break;
                case OPEN_FILE_REQ:
                    if (hasTheFile(fileList, temporalData, cantFiles)){
                        aux = openFile(fileList, temporalData, tempClientId, cantFiles);
                        sprintf(temporalData, "%d %d %d", workerId, OPEN_FILE_RES, aux);
                        mq_send(workerMessenger[tempWorkerId], temporalData, strlen(temporalData) + 1, PRIOWORKER);
                    }
                    else{
                        aux = 2;
                        sprintf(temporalData, "%d %d %d", workerId, OPEN_FILE_RES, aux);
                        mq_send(workerMessenger[tempWorkerId], temporalData, strlen(temporalData) + 1, PRIOWORKER);
                    }
                    break;
                case OPEN_FILE_RES:
                    memset(cache, '\0', BUFF_SIZE);
                    safe = 0;
                    msgcounter++;
                    k = atoi(temporalData);
                    if (k != 2){
                        msgcounter = 5;
                    }
                    sprintf(cache, "%d", k);
                    memset(temporalData, '\0', BUFF_SIZE);
                    break;
                case CLOSE_FILE_REQ:
                    splitNS(&k, temporalData, temporalData);
                    aux = closeFile(fileList, atoi(temporalData), k, cantFiles);
                    sprintf(temporalData, "%d %d %d", workerId, CLOSE_FILE_RES, aux);
                    mq_send(workerMessenger[tempWorkerId], temporalData, strlen(temporalData) + 1, PRIOWORKER);
                    break;
                case CLOSE_FILE_RES:
                    safe = 0;
                    msgcounter++;
                    k = atoi(temporalData);
                    if (k != -1){
                        msgcounter = 5;
                        sprintf(cache, "%d", k);
                    }
                    memset(temporalData, '\0', BUFF_SIZE);
                    break;
                case DELETE_FILE_REQ:
                    aux = deleteFile(fileList, temporalData, cantFiles);
                    sprintf(temporalData, "%d %d %d", workerId, DELETE_FILE_RES, aux);
                    mq_send(workerMessenger[tempWorkerId], temporalData, strlen(temporalData) + 1, PRIOWORKER);
                    break;
                case DELETE_FILE_RES:
                    safe = 0;
                    msgcounter++;
                    k = atoi(temporalData);
                    if (k != -1){
                        msgcounter = 5;
                        sprintf(cache, "%d", k);
                    }
                    memset(temporalData, '\0', BUFF_SIZE);
                    break;
                case WRITE_FILE_REQ:
                    memset(str, '\0', BUFF_SIZE);
                    splitNNNS(&temporalFileDescriptor, &tempClientId, &bufSize, str, temporalData);
                    aux = writeFile(fileList, temporalFileDescriptor, tempClientId, cantFiles, bufSize, str);
                    memset(temporalData, '\0', BUFF_SIZE);
                    memset(str, '\0', BUFF_SIZE);
                    sprintf(temporalData, "%d %d %d", workerId, WRITE_FILE_RES, aux);
                    mq_send(workerMessenger[tempWorkerId], temporalData, strlen(temporalData) + 1, PRIOWORKER);
                    break;
                case WRITE_FILE_RES:
                    safe = 0;
                    msgcounter++;
                    k = atoi(temporalData);
                    if (k != -1){
                        msgcounter = 5;
                        sprintf(cache, "%d", k);
                    }
                    memset(temporalData, '\0', BUFF_SIZE);
                    break;
                case READ_FILE_REQ:
                    memset(tempbuf, '\0', BUFF_SIZE);
                    splitNNNS(&temporalFileDescriptor, &tempClientId, &bufSize, trash, temporalData);
                    aux = readFile(fileList, temporalFileDescriptor, tempClientId, cantFiles, bufSize, tempbuf);
                    memset(temporalData, '\0', BUFF_SIZE);
                    sprintf(temporalData, "%d %d %d %s", workerId, READ_FILE_RES, aux, tempbuf);
                    DEBUG("DATA: temporalData contiene \"%s\"\n", temporalData);
                    mq_send(workerMessenger[tempWorkerId], temporalData, strlen(temporalData) + 1, PRIOWORKER);
                    break;
                case READ_FILE_RES:
                    safe = 0;
                    msgcounter++;
                    splitNS(&k, temporalData, temporalData);
                    if (k != -1){
                        msgcounter = 5;
                        sprintf(cache, "%d", k);
                        sprintf(tempbuf, temporalData, strlen(temporalData));
                    }
                    memset(temporalData, '\0', BUFF_SIZE);
                    break;
                case CLOSE_ALL_REQ:
                    splitNS(&k, temporalData, temporalData);
                    closeAllFilesOf(fileList, cantFiles, k);
                    sprintf(temporalData, "%d %d ok", workerId, CLOSE_ALL_RES);
                    mq_send(workerMessenger[tempWorkerId], temporalData, strlen(temporalData) + 1, PRIOWORKER);
                    memset(temporalData, '\0', BUFF_SIZE);
                    break;
                case CLOSE_ALL_RES:
                    safe = 0;
                    msgcounter++;
                    break;
            }
        }
    }
	return NULL;
}

/**
* Dado un string extrae el numero que está al principio y retorna el resto
*
* number: Numero al principio del string
* rest: resto del string (ignirando el espacio intermedio)
* originalString: Cadena a analizar
*
* El String debe tener la forma "number rest"
**/
void splitNS(int *number, char *rest, char *originalString){
    int i, n = strlen(originalString);
    //Crea arreglo temporal para le numero
    char temp[6];
    memset(temp, '\0', 6);
    //Extrae el número
    for(i = 0; i < n; i++){
        if (originalString[i] != ' '){
            temp[i] = originalString[i];
        }
        else{
            break;
        }
    }
    *number = atoi(temp);
    //Genera rest
    sprintf(rest, "%s", originalString + i + 1);
}

/*
* Splitea una cadena de la forma (a b c rest)
*
* a: Primer numero de la cadena
* b: sengundo numero de la cadena
* c: tercer numero de la cadena
* rest: resto del string
* originalString: Cadena a analizar
*/
void splitNNNS(int *a, int *b, int *c, char *rest, char *originalString){
    int n = strlen(originalString);
    //Crea las cadenas temporales
    char temp1[n], temp2[n];
    memset(temp1, '\0', n);
    memset(temp2, '\0', n);
    //Extrae la informacion
    splitNS(a, rest, originalString);
    splitNS(b, temp1, rest);
    splitNS(c, temp2, temp1);
    //Genera rest
    n = strlen(rest);
    memset(rest, '\0', n);
    strcpy(rest, temp2);
}

/**
* Limpia la información basura de un worker para poder procesar una nueva solicitud
*
* msgcounter: contador de mensaje
* parsedInfo: Paquete de parseo
* safe: varaible de seguridad
* salida: buffer de salida
* cache: Caché interna del worker
**/
void clearVar(int *msgcounter, Parser_Package** parsedInfo , int* safe , char* salida , char* cache){
    *msgcounter = 0;
    freeParser_Package(*parsedInfo);
    *parsedInfo = NULL;
    *safe = 0;
    memset(cache, '\0', BUFF_SIZE);
    memset(salida, '\0', BUFF_SIZE);
}

/**
* Dado el identificador de un worker indica si tiene mensajes
*
* workerId: Identificador de worker
**/
int availableMessages(workerId){
    mq_getattr(workerMessenger[workerId], &attrWorker[workerId]);
    return ((int)attrWorker[workerId].mq_curmsgs) != 0;
}

/**
* Recibe un mensaje de un Cliente, Retorna @see NULL en caso de no haber ninguno
*
* workId: Identificador del worker
**/
char* getClientMessage(int workId){
    if(!availableMessages(workId)){
        return NULL;
    }

    unsigned int prio;
    char* msgcontent = malloc((BUFF_SIZE + 1) * sizeof(char));
    memset(msgcontent, '\0', BUFF_SIZE + 1);
    mq_receive(workerMessenger[workId], msgcontent, BUFF_SIZE + 1, &prio);
    if(prio == PRIOWORKER || prio == PRIOREWORK){
        mq_send(workerMessenger[workId], msgcontent, strlen(msgcontent) + 1, PRIOREWORK);
        free(msgcontent);
    }
    else if(prio == PRIOCLIENT || prio == PRIORECLIE){
        return msgcontent;
    }
    return NULL;
}

/**
* Recibe un mensaje de un Worker, Retorna @see NULL en caso de no haber ninguno
*
* workId: Identificador del worker
**/
char* getWorkerMessage(int workId){
    if(!availableMessages(workId)){
        return NULL;
    }

    unsigned int prio;
    char* msgcontent = malloc((BUFF_SIZE + 1) * sizeof(char));
    memset(msgcontent, '\0', BUFF_SIZE + 1);
    mq_receive(workerMessenger[workId], msgcontent, BUFF_SIZE + 1, &prio);

    if(prio == PRIOCLIENT || prio == PRIORECLIE){
        mq_send(workerMessenger[workId], msgcontent, strlen(msgcontent) + 1, PRIORECLIE);
        free(msgcontent);
    }
    else if(prio == PRIOWORKER || prio == PRIOREWORK){
        return msgcontent;
    }
    return NULL;
}

/**
* Retorna el indice de un worker al azar
**/
int getWorker(){
    srand(time(NULL));
    return rand() % 5;
}

/**
* Retorna el siguiente indice de worker para emviar el mensaje
* Este método es utilizado para simular una ronda
*
* ownId: Id del worker que invoca el método
* counter: contador de la ronda (ownId - 1 - 2 - 3 - 4)
**/
int nextWorkerToSend(int ownId, int counter){
    return (ownId + counter) % 5;
}

/**
* Retorna 1 si el archivo existe
*
* filelist: Lista de archivos
* name: Nombre del archivo
* cantFiles: cantidad de archivos de la lista
**/
int hasTheFile(FileS *filelist, char *name, int cantFiles){
    int i;
    for (i = 0; i < cantFiles; i++){
        if (filelist[i].name != NULL){
            if (!strcmp(filelist[i].name, name))
                return 1;
        }
    }
    return 0;
}

/**
* Dada una lista de archivos y una cadena escribe en el String los nombres de los archivos
*
* buf: cadema de uso temporal para almacenar los nombres ya procesados
* files: arreglo de estructuras de archivos
* cantFiles: camtidad de archivos del arreglo a procesar
**/
void filelistToString(char *buf, FileS *files, int cantFiles){
    int i;
    for (i = 0; i < cantFiles; i++){
        if (files[i].name != NULL){
            sprintf(buf + strlen(buf), "%s ", files[i].name);
        }
    }
    return;
}
