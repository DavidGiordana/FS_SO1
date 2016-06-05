#include "files.h"

/**
* @author David Giordana
* @author Gabriel Antelo
* @author Maximiliano Ibalborde
*/

//Indica cual fue el ultimo descriptor de archivo usado
// 0 Representa el descriptor nulo
lastFD = 0;

//Lock para archivos
pthread_mutex_t file_mutex = PTHREAD_MUTEX_INITIALIZER;

/*
* Retorna el indice de un archivo en base a su descriptor de archivo
*
* fileList: Lista de archivos
* cantFiles: Cantidad de arhivos en la Lista
* fd: Descriptor de archivo
*
* indice en caso de existir el archivo, -1 en caso de no existir
*/
int getFileIndexByFd(FileS* fileList, int cantFiles, int fd){
    int i;
    for (i = 0; i < cantFiles; i++)
        if (fileList[i].name != NULL)
            if(fileList[i].descriptor == fd)
                return i;
    return -1;
}

/*
* Retorna el indice de un archivo en base a su nombre de archivo
*
* fileList: Lista de archivos
* cantFiles: Cantidad de arhivos en la Lista
* name: Nombrede archivo
*
* indice en caso de existir el archivo, -1 en caso de no existir
*/
int getFileIndexByName(FileS* fileList, int cantFiles, char* name){
    int i;
    for (i = 0; i < cantFiles; i++)
        if (fileList[i].name != NULL)
            if(!strcmp(fileList[i].name, name))
                return i;
    return -1;
}

/**
* Crea un archivo en base al nombre del mismo
*
* name: Nombre del archivo
*/
FileS *createFile(char *name){
    //Crea la estructura
    FileS *temp = malloc(sizeof(FileS));
    temp->name = malloc((TITLE_SIZE + 1) * sizeof(char));
    temp->content = malloc((BODY_SIZE + 1) * sizeof(char));
    //Limpia valores basura
    memset(temp->name, '\0', TITLE_SIZE + 1);
    memset(temp->content, '\0', BODY_SIZE + 1);
    //Inicializa la estructura
    sprintf(temp->name, "%s", name);
    sprintf(temp->content, "%s", "");
    return temp;
}

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
int openFile(FileS *fileList, char *name, int user, int cantFiles){
    int i = getFileIndexByName(fileList, cantFiles, name);
    //Archivo no en lista
    if(i == -1)
        return -1;
    if (fileList[i].descriptor != 0){
        //Error el archivo ya está abierto
        return 1;
    }
    else{
        pthread_mutex_lock(&file_mutex);
        fileList[i].prop = user;
        lastFD = lastFD + 1;
        fileList[i].descriptor = lastFD;
        fileList[i].index = 0;
        pthread_mutex_unlock(&file_mutex);
        //Ok
        return 0;
    }
}

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
int closeFile(FileS *fileList, int fd, int user, int cantFiles){
    int i = getFileIndexByFd(fileList, cantFiles, fd);
    //Archivo no en lista
    if(i == -1)
        return -1;

    if (fileList[i].prop == user){
            fileList[i].prop = -1;
            fileList[i].descriptor = 0;
            //Cierre exitoso
            return 1;
    }
    //Cierre no autorizado
    else if ((fileList[i].descriptor == fd) && (fileList[i].prop != user))
        return 2;
    return -1;
}

/**
* Cierra todos los archivos de un usuario
*
* filelist: lista de archivos a analizar
* cantFiles: cantidad de archivos en la lista
* usrId: Identificador de Usuario
**/
void closeAllFilesOf(FileS* fileList, int cantFiles, int usrId){
    int i;
    for (i = 0; i < cantFiles; i++)
        if ((fileList[i].name != NULL) && (fileList[i].prop == usrId)){
            fileList[i].prop = -1;
            fileList[i].descriptor = 0;
        }
}

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
int deleteFile(FileS *fileList, char *name, int cantFiles){
    int i = getFileIndexByName(fileList, cantFiles, name);
    //Archivo no en lista
    if(i == -1)
        return -1;
    if (fileList[i].descriptor == 0){
            fileList[i].prop = -1;
            fileList[i].descriptor = -1;
            free(fileList[i].name);
            free(fileList[i].content);
            fileList[i].name = NULL;
            //Eliminado con exito
            return 1;
    }
    //error: archivo abierto
    else
        return 2;
}

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
int writeFile(FileS* fileList, int fd, int usrId, int cantFiles, int bufSize, char* buf){
    int i = getFileIndexByFd(fileList, cantFiles, fd);
    //Archivo no en lista
    if(i == -1)
        return -1;
    if(fileList[i].prop == usrId){
        if (strlen(buf) < bufSize)
            return 2;
        int contentSize = strlen(fileList[i].content);
        int availableBuffer = BODY_SIZE - contentSize;
        if(bufSize <= availableBuffer){
            strncpy(fileList[i].content + contentSize, buf, bufSize);
            //copiado exitoso
            return 1;
        }
        else{
            //Espacio insuficiente, acceso denegado
            return 2;
        }
    }
    else{
        //No se puede escribir, permiso denegado
        return 3;
    }
}

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
* 2 -> Error: acceso denegado
* -1 -> Error: El archivo no está en la lista
**/
int readFile(FileS* fileList, int fd, int usrId, int cantFiles, int bufSize, char *buf){
    int i = getFileIndexByFd(fileList, cantFiles, fd);
    //Archivo no en lista
    if(i == -1)
        return -1;
    //copiado exitoso
    if(fileList[i].prop == usrId){
        int availableBuffer = strlen(fileList[i].content) - fileList[i].index;
        if(availableBuffer == 0){
            memset(buf, '\0', bufSize);
            return 1;
        }
        bufSize = MIN(bufSize, availableBuffer);
        strncpy(buf, fileList[i].content + fileList[i].index, bufSize);
        fileList[i].index += bufSize;
        return 1;
    }
    //No se puede escribir, permiso denegado
    else
        return 3;
}
