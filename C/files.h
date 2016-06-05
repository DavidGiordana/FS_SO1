#ifndef __FILES_H__
#define __FILES_H__

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>

/**
* @author David Giordana
* @author Gabriel Antelo
* @author Maximiliano Ibalborde
*/

#define MIN(X, Y) (((X) < (Y)) ? (X) : (Y))

//Datos de los archivos
#define TITLE_SIZE 16
#define BODY_SIZE 10//128

extern int lastFD;

/**
* Estructura que representa un archivo
**/
typedef struct FileStr_{
	char *name; //Nombre
	char *content; //Contenido
	int descriptor; //Descriptor
	int prop; //Propoetario
	int index; //Apuntador para la lectura
} FileS;

/*
* Retorna el indice de un archivo en base a su descriptor de archivo
*
* fileList: Lista de archivos
* cantFiles: Cantidad de arhivos en la Lista
* fd: Descriptor de archivo
*
* indice en caso de existir el archivo, -1 en caso de no existir
*/
int getFileIndexByFd(FileS* fileList, int cantFiles, int fd);

/*
* Retorna el indice de un archivo en base a su nombre de archivo
*
* fileList: Lista de archivos
* cantFiles: Cantidad de arhivos en la Lista
* name: Nombrede archivo
*
* indice en caso de existir el archivo, -1 en caso de no existir
*/
int getFileIndexByName(FileS* fileList, int cantFiles, char* name);

/**
* Crea un archivo en base al nombre del mismo
*
* name: Nombre del archivo
*/
FileS *createFile(char *name);

/**
* Intenta abrir un archivo
*
* fileList: Lista de archivos
* name: Nombre del archivo a abrir
* user: usuario que está abriendo el archivo
* cantFiles: cantidad de archivos de la lista
*
* 1 -> Error: el archivo ya está abierto
* 0 -> Se abrió correctamente el archivo
* -1 -> Error: El archivo no está en la lista
*/
int openFile(FileS *fileList, char *name, int user, int cantFiles);

/**
* Intenta cerrar un archivo
*
* filelist: lista de archivos a analizar
* fd: Descriptor de archivo
* user: Identificador de Usuario
* cantFiles: cantidad de archivos en la lista
*
* 1 -> Cierre exitoso
* 2 -> Cierre no autorizado
* -1 -> El archivo no se encuentra en la lista
**/
int closeFile(FileS *fileList, int fd, int user, int cantFiles);

/**
* Cierra todos los archivos de un usuario
*
* filelist: lista de archivos a analizar
* cantFiles: cantidad de archivos en la lista
* usrId: Identificador de Usuario
**/
void closeAllFilesOf(FileS* fileList, int cantFiles, int usrId);

/**
* Intenta eliminar un archivo
*
* filelist: lista de archivos a analizar
* name: Nombre del archivo a borrar
* cantFiles: cantidad de archivos en la lista
*
* 1 -> Eliminado con exito
* 2 -> Error: El archivo está abierto
* -1 -> Error: El archivo no está en la lista
**/
int deleteFile(FileS *fileList, char *name, int cantFiles);

/**
* Intenta escribir en un archivo
*
* filelist: lista de archivos a analizar
* fd: Descriptor de archivo
* usrId: Identificador de Usuario
* cantFiles: cantidad de archivos en la lista
* bufSize: Tamaño del buffer a escribir
* buf: Buffer a escribir
*
* 1 -> Escritura correcta
* 2 -> Error: espacio insuficiente
* 3 -> Error: acceso denegado
* -1 -> Error: El archivo no está en la lista
**/
int writeFile(FileS* fileList, int fd, int usrId, int cantFiles, int bufSize, char* buf);

/**
* Intenta leer un archivo
*
* filelist: lista de archivos a analizar
* fd: Descriptor de archivo
* usrId: Identificador de Usuario
* cantFiles: cantidad de archivos en la lista
* bufSize: Tamaño del buffer a escribir
* buf: Buffer a escribir
*
* 1 -> Lectura correcta
* 2 -> Error: espacio insuficiente
* 3 -> Error: acceso denegado
* -1 -> Error: El archivo no está en la lista
**/
int readFile(FileS* fileList, int fd, int usrId, int cantFiles, int bufSize, char *buf);

#endif
