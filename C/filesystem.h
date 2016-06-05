#ifndef __PARS_H__
#define __PARS_H__

#include <sys/stat.h>
#include <sys/types.h>
#include <arpa/inet.h>
#include <mqueue.h>
#include <semaphore.h>
#include <pthread.h>
#include <unistd.h>
#include <errno.h>
#include <time.h>

//Archivos del sistema de archivos
#include "parser.h"
#include "files.h"

/**
* @author David Giordana
* @author Gabriel Antelo
* @author Maximiliano Ibalborde
*/

typedef struct mq_attr mq_at;

#define MAX_FILES 50 //Cantidad de archivos
#define NUM_WORKERS 5 //Cantidad de workers
#define BUFF_SIZE 2048 //Tamaño del buffer
#define PORT 8001 //Puerto del servidor
#define MAX_CLIENTS 32 //Cantidad máxima de clientes

//Retorna el identificador del worker
#define WORKER_ID(n) (n==0 ? "/work1" : (n==1 ? "/work2" : (n==2 ? "/work3" : (n==3 ? "/work4" : "/work5"))))

//Prioridades de mensajes
#define PRIOREWORK 4
#define PRIOWORKER 3
#define PRIORECLIE 2
#define PRIOCLIENT 1

//Identificadores de mensajes internos de worker
#define HAS_FILE_REQ 1
#define HAS_FILE_RES 2
#define GIVE_FILE_REQ 3
#define GIVE_FILE_RES 4
#define OPEN_FILE_REQ 5
#define OPEN_FILE_RES 6
#define CLOSE_FILE_REQ 7
#define CLOSE_FILE_RES 8
#define DELETE_FILE_REQ 9
#define DELETE_FILE_RES 10
#define WRITE_FILE_REQ 11
#define WRITE_FILE_RES 12
#define READ_FILE_REQ 13
#define READ_FILE_RES 14
#define CLOSE_ALL_REQ 15
#define CLOSE_ALL_RES 16

/**
* Metodos del archivo
**/

/**
* Recibe las conexiones de los clientes y lanza hilos para atenderlos
*/
void dispatcher();

/**
* Atiende las solicitudes de los clientes
**/
void* client(void* arg);

/**
* WORKER
**/
void *worker(void *arg);

/**
* Dado un string extrae el numero que está al principio y retorna el resto
*
* number: Numero al principio del string
* rest: resto del string (ignirando el espacio intermedio)
* originalString: Cadena a analizar
*
* El String debe tener la forma "number rest"
**/
void splitNS(int *number, char *rest, char *originalString);

/*
* Splitea una cadena de la forma (a b c rest)
*
* a: Primer numero de la cadena
* b: sengundo numero de la cadena
* c: tercer numero de la cadena
* rest: resto del string
* originalString: Cadena a analizar
*/
void splitNNNS(int *a, int *b, int *c, char *rest, char *originalString);

/**
* Limpia la información basura de un worker para poder procesar una nueva solicitud
*
* msgcounter: contador de mensaje
* parsedInfo: Paquete de parseo
* safe: varaible de seguridad
* salida: buffer de salida
* cache: Caché interna del worker
**/
void clearVar(int *msgcounter, Parser_Package** parsedInfo , int* safe , char* salida , char* cache);

/**
* Dado el identificador de un worker indica si tiene mensajes
*
* workerId: Identificador de worker
**/
int availableMessages(workerId);

/**
* Recibe un mensaje de un Cliente, Retorna @see NULL en caso de no haber ninguno
*
* workId: Identificador del worker
**/
char* getClientMessage(int workId);

/**
* Recibe un mensaje de un Worker, Retorna @see NULL en caso de no haber ninguno
*
* workId: Identificador del worker
**/
char* getWorkerMessage(int workId);

/**
* Retorna el indice de un worker al azar
**/
int getWorker();

/**
* Retorna el siguiente indice de worker para emviar el mensaje
* Este método es utilizado para simular una ronda
*
* ownId: Id del worker que invoca el método
* counter: contador de la ronda (ownId - 1 - 2 - 3 - 4)
**/
int nextWorkerToSend(int ownId, int counter);

/**
* Retorna 1 si el archivo existe
*
* filelist: Lista de archivos
* name: Nombre del archivo
* cantFiles: cantidad de archivos de la lista
**/
int hasTheFile(FileS *filelist, char *name, int cantFiles);

/**
* Dada una lista de archivos y una cadena escribe en el String los nombres de los archivos
*
* buf: cadema de uso temporal para almacenar los nombres ya procesados
* files: arreglo de estructuras de archivos
* cantFiles: camtidad de archivos del arreglo a procesar
**/
void filelistToString(char *buf, FileS *files, int cantFiles);

/**
* Si está definido CONSOLE en el MakeFile activa las impresiones de debug
**/
#ifdef CONSOLE
	#define DEBUG(...) printf("DEBUG MESSAGE: "),printf(__VA_ARGS__)
#else
	# define DEBUG do {} while (0)
#endif

/*
void DEBUG(char* str,...){
	#ifdef CONSOLE
		printf("DEBUG MESSAGE: ");
	    printf(str);
	#endif
}
*/

/**
* Si está definido PRINTS en el MakeFile activa las impresiones de bloqueo
**/
#ifdef PRINTS
	#define PRINT(...) printf("LOCK MESSAGE: "),printf(__VA_ARGS__)
#else
	# define PRINT(...) do {} while (0)
#endif

#define INTERNAL_WORKER_MESSAGE(x) (\
	if(x == HAS_FILE_REQ)\
		"HAS_FILE_REQ";\
	if(x == HAS_FILE_RES)\
		"HAS_FILE_RES";\
	if(x == GIVE_FILE_REQ)\
		"GIVE_FILE_REQ";\
	if(x == GIVE_FILE_RES)\
		"GIVE_FILE_RES";\
	if(x == OPEN_FILE_REQ)\
		"OPEN_FILE_REQ";\
	if(x == OPEN_FILE_RES)\
		"OPEN_FILE_RES";\
	if(x == CLOSE_FILE_REQ)\
		"CLOSE_FILE_REQ";\
	if(x == CLOSE_FILE_RES)\
		"CLOSE_FILE_RES";\
	if(x == DELETE_FILE_REQ)\
		"DELETE_FILE_REQ";\
	if(x == DELETE_FILE_RES)\
		"DELETE_FILE_RES";\
	if(x == WRITE_FILE_REQ)\
		"WRITE_FILE_REQ";\
	if(x == WRITE_FILE_RES)\
		"WRITE_FILE_RES";\
	if(x == READ_FILE_REQ)\
		"READ_FILE_REQ";\
	if(x == READ_FILE_RES)\
		"READ_FILE_RES";\
	if(x == CLOSE_ALL_REQ)\
		"CLOSE_ALL_REQ";\
	if(x == CLOSE_ALL_RES)\
		"CLOSE_ALL_RES";\
	else\
		"ERROR";\
	)




#endif
