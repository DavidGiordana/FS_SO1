#include "parser.h"

/**
* @author David Giordana
* @author Gabriel Antelo
* @author Maximiliano Ibalborde
*/

/**
* Retorna el indice de un comando
*
* str: Cadena de tres caracteres para determinar comando
**/
int getCommand(char* str){
    if(!strcmp(str , "CON")){
        return CON;
    }
    else if(!strcmp(str , "LSD")){
        return LSD;
    }
    else if(!strcmp(str , "DEL")){
        return DEL;
    }
    else if(!strcmp(str , "CRE")){
        return CRE;
    }
    else if(!strcmp(str , "OPN")){
        return OPN;
    }
    else if(!strcmp(str , "WRT")){
        return WRT;
    }
    else if(!strcmp(str , "REA")){
        return REA;
    }
    else if(!strcmp(str , "CLO")){
        return CLO;
    }
    else if(!strcmp(str , "BYE")){
        return BYE;
    }
    return 0;
}

/**
* Parsea una cadena
*
* originalStr: Cadena a parsear
*
* -> Parser_Package
**/
Parser_Package *clientRequestParser(char *originalStr){
    //Elimina caracteres basura
    //printf("%s\n", originalStr);
    originalStr = clearExtraDataString(originalStr);
    //printf("%s\n", originalStr);
    //Variable temporales para el struct
    int cmd = 0;
    int fd = 0;
    int size = 0;
    char* str = malloc(sizeof(char) * (BUFF_SIZE + 1));
    //Variables para la extraccion
    char* stringAnalize; //Cadena a analizar
    char* stopped;
    char* box [5] = {}; // Variable temporal para almacenar la primera subcadena
    char* rest ; //Variable temporal para almacenar el resto de la cadena

    Parser_Package *package = (Parser_Package *)malloc(sizeof(Parser_Package));

    //En caso de ser una cadena vacia retorna un paquete con comando erroneo
    if(strlen(originalStr) == 0){
        package -> com = cmd;
        package -> fd      = fd;
        package -> size    = size;
        package -> str     = NULL;
        return package;
    }

    //Corta si no puede trabajar, No se puede reservar espacio para el paquete de datos
    else if((stringAnalize = (char *)malloc(sizeof(char) * (strlen(originalStr) + 1))) == NULL) {
        package -> com = cmd;
        package -> fd      = fd;
        package -> size    = size;
        package -> str     = NULL;
        return package;
    }
    strcpy(stringAnalize , originalStr);

    //Extrae el comando
    box[0] = strtok_r(stringAnalize, TOK, &rest);
    cmd = getCommand(box[0]);

    //Procesa la informacion
    switch (cmd) {
        case 0:
            break;
        case DEL:
            box[0] = strtok_r(NULL, TOK, &rest);
            if(box[0] == NULL)
                cmd = 0;
            else
                strcpy(str , box[0]);
            break;
        case CRE:
            box[0] = strtok_r(NULL, TOK, &rest);
            if(box[0] == NULL)
                cmd = 0;
            else
                strcpy(str , box[0]);
            break;
        case OPN:
            box[0] = strtok_r(NULL, TOK, &rest);
            if(box[0] == NULL)
                cmd = 0;
            else{
                strcpy(str , box[0]);
            }
            break;
        case WRT:
            box[0] = strtok_r(NULL, TOK, &rest);
            box[1] = strtok_r(NULL, TOK, &rest);
            box[2] = strtok_r(NULL, TOK, &rest);
            box[3] = strtok_r(NULL, TOK, &rest);
            box[4] = strtok_r(NULL, TOK, &rest);

            //Si hay fallo de parseo
            if(box[0] == NULL || box[2] == NULL || box[4] == NULL){
                cmd = 0;
            }
            else if(!strcmp("FD" , box[0]) && !strcmp("SIZE",box[2])){
                fd = (int)strtol(box[1], &stopped, 10);
                if(strlen(stopped) != 0){
                    cmd = 0;
                    break;
                }
                size = (int)strtol(box[3], &stopped, 10);
                if(strlen(stopped) != 0){
                    cmd = 0;
                    break;
                }
                if((strlen(box[4]) < size)){
                    cmd = 0;
                    break;
                }
                strcpy(str , box[4]);
            }
            else
                cmd = 0;
            break;
        case REA:
            box[0] = strtok_r(NULL, TOK, &rest);
            box[1] = strtok_r(NULL, TOK, &rest);
            box[2] = strtok_r(NULL, TOK, &rest);
            box[3] = strtok_r(NULL, TOK, &rest);
            //Si hay fallo de parseo
            if(box[1] == NULL || box[3] == NULL){
                cmd = 0;
            }
            else if(!strcmp("FD" , box[0]) && !strcmp("SIZE",box[2])){
                fd = (int)strtol(box[1], &stopped, 10);
                if(strlen(stopped) != 0){
                    cmd = 0;
                    break;
                }
                size = (int)strtol(box[3], &stopped, 10);
                if(strlen(stopped) != 0){
                    cmd = 0;
                    break;
                }
            }
            else
                cmd = 0;
            break;
        case CLO:
            box[0] = strtok_r(NULL, TOK, &rest);
            box[1] = strtok_r(NULL, TOK, &rest);
            //Si hay fallo de parseo
            if(box[0] == NULL || box[1] == NULL){
                cmd = 0;
            }
            else if(!strcmp("FD" , box[0])){
                fd = (int)strtol(box[1], &stopped, 10);
                if(strlen(stopped) != 0){
                    cmd = 0;
                    break;
                }
            }
            else
                cmd = 0;
            break;
    }
    //revenue
    package -> com = cmd;
    package -> fd      = fd;
    package -> size    = size;
    package -> str     = str;
	return package;
}

/**
* Imprime el contenido de un Parser_Package
*
* p: Paquete de parseo a imprimir
**/
void printParser_Package(Parser_Package p){
    #ifdef PARSED_INFO
    printf("Se parseó el paquete:\n");
    printf("COM: %d\n", p.com);
    printf("FD: %d\n", p.fd);
    printf("SIZE: %d\n", p.size);
    printf("STR: %s\n", p.str);
    #endif
}

/**
* Libera un Parser_Package
*
* pack: Paquete de parseo a liberar
**/
void freeParser_Package(Parser_Package *pack){
    if(pack != NULL){
        if(pack->str != NULL){
            free(pack->str);
        }
        free(pack);
    }
    return;
}

/**
* Indica si un caracter está en una cadena
*
* c: Caracter a comprobar
* str: Cadena a analizar
*/
int isIn(char c, char* str){
    printf("%c -> %d", c, strchr(str, c) != NULL);
    return strchr(str, c) != NULL;
    /*int size = strlen(str);
    int i;
    for(i = 0; i < size; i++){
        if(str[i] == c){
            return 1;
        }
    }
    return 0;*/
}

/**
* Elimina la basura de un String y lo retorna
*
* str: Cadena a trabajar
**/
char* clearExtraDataString(char* str){
    int size = strlen(str);
    int initOffset = 0;
    int endOffset = 0;
    int i;

    for(i = 0; i < size; i++){
        if(isIn(str[i], TOK)){
            initOffset++;
        }
        else{
            break;
        }
    }
    for(i = size-1; i > initOffset; i--){
        if(isIn(str[i], TOK)){
            endOffset++;
        }
        else{
            break;
        }
    }
    str[size - endOffset] = '\0';
    return str + initOffset;
}
