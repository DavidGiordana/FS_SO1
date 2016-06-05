#ifndef __PARSER_H__
#define __PARSER_H__

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/**
* @author David Giordana
* @author Gabriel Antelo
* @author Maximiliano Ibalborde
*/

#define TOK " \r\n" //Token para split
#define BUFF_SIZE 2048 //Tamaño de buffer

/**
* Indices de comando
**/
#define CON 1
#define LSD 2
#define DEL 3
#define CRE 4
#define OPN 5
#define WRT 6
#define REA 7
#define CLO 8
#define BYE 9

/**
* Estructura con la informacion de un comando
**/
typedef struct Parser_Package_{
	unsigned int com; //Comando
	int fd; //File descriptor
	int size; //Size
	char *str; //Other info
} Parser_Package;

/**
* Funciones incluidas
**/

/**
* Retorna el indice de un comando
*
* str: Cadena de tres caracteres para determinar comando
**/
int getCommand(char* str);

/**
* Parsea una cadena
*
* originalStr: Cadena a parsear
*
* -> Parser_Package
**/
Parser_Package *clientRequestParser(char *estomellego);

/**
* Imprime el contenido de un Parser_Package
*
* p: Paquete de parseo a imprimir
**/
void printParser_Package(Parser_Package p);

/**
* Librera un Parser_Package
*
* pack: Paquete de parseo a liberar
**/
void freeParser_Package(Parser_Package *pack);

/**
* Indica si un caracter está en una cadena
*
* c: Caracter a comprobar
* str: Cadena a analizar
*/
int isIn(char c, char* str);

/**
* Elimina la basura de un String y lo retorna
*
* str: Cadena a trabajar
**/
char* clearExtraDataString(char* str);

#endif
